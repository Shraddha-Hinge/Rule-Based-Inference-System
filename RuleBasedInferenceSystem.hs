import System.IO
import Control.Concurrent
import Control.Exception (catch, IOException)
import Control.Monad (forM)
import Text.Read (readMaybe)

data Token =
    TVar String
  | TAnd
  | TOr
  | TNot
  | TOParen
  | TCParen
  | TImplies
  | TIaoi
  | TTrue
  | TFalse
  deriving (Show)


data Predicate =
      T
    | F
    | Var String
    | Not Predicate
    | And Predicate Predicate
    | Or Predicate Predicate
    | Implies Predicate Predicate
    | Iaoi Predicate Predicate
    deriving (Show)


type VarAssign = [(String, Bool)]

eval :: Predicate -> VarAssign -> Bool
eval T _ = True
eval F _ = False
eval (Var x) assignments = case lookup x assignments of
    Just val -> val
    Nothing -> error ("Variable " ++ x ++ " not found in assignments")
eval (Not p) assignments = not (eval p assignments)
eval (And p q) assignments = eval p assignments && eval q assignments
eval (Or p q) assignments = eval p assignments || eval q assignments
eval (Implies p q) assignments = not (eval p assignments) || eval q assignments
eval (Iaoi p q) assignments = eval p assignments == eval q assignments


type TStream = [Token]

tokenize :: String -> TStream
tokenize [] = []
tokenize ('(':rem) = TOParen : tokenize rem
tokenize (')':rem) = TCParen : tokenize rem
tokenize ('&':rem) = TAnd : tokenize rem
tokenize ('|':rem) = TOr : tokenize rem
tokenize ('~':rem) = TNot : tokenize rem
tokenize ('-':'>':rem) = TImplies : tokenize rem
tokenize ('<':'-':'>':rem) = TIaoi : tokenize rem
tokenize ('T':rem) = TTrue : tokenize rem
tokenize ('F':rem) = TFalse : tokenize rem
tokenize (c:rem) | c `elem` ['a'..'z'] = TVar [c] : tokenize rem
tokenize (c:rem) | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c=='[' || c==']'= tokenize rem
                | otherwise = error ("Other character: " ++ [c])


parse :: TStream -> Predicate
parse = fst . parseIaoi


parseIaoi :: TStream -> (Predicate, TStream)
parseIaoi tokens =
    let (p1, rem1) = parseImplies tokens
    in case rem1 of
        (TIaoi:rem2) ->
            let (p2, rem3) = parseIaoi rem2
            in (Iaoi p1 p2, rem3)
        _ -> (p1, rem1)


parseImplies :: TStream -> (Predicate, TStream)
parseImplies tokens =
    let (p1, rem1) = parseOr tokens
    in case rem1 of
        (TImplies:rem2) ->
            let (p2, rem3) = parseImplies rem2
            in (Implies p1 p2, rem3)
        _ -> (p1, rem1)


parseOr :: TStream -> (Predicate, TStream)
parseOr tokens =
    let (p1, rem1) = parseAnd tokens
    in case rem1 of
        (TOr:rem2) ->
            let (p2, rem3) = parseOr rem2
            in (Or p1 p2, rem3)
        _ -> (p1, rem1)


parseAnd :: TStream -> (Predicate, TStream)
parseAnd tokens =
    let (p1, rem1) = parseNot tokens
    in case rem1 of
        (TAnd:rem2) ->
            let (p2, rem3) = parseAnd rem2
            in (And p1 p2, rem3)
        _ -> (p1, rem1)


parseNot :: TStream -> (Predicate, TStream)
parseNot (TNot:rem) =
    let (p, rem1) = parseNot rem
    in (Not p, rem1)
parseNot tokens = parseAtomic tokens


parseAtomic :: TStream -> (Predicate, TStream)
parseAtomic (TOParen:rem) =
    let (p, rem1) = parseIaoi rem
    in case rem1 of
        (TCParen:rem2) -> (p, rem2)
        _ -> error "Mismatched parentheses"
parseAtomic (TVar x:rem) = (Var x, rem)
parseAtomic (TTrue:rem) = (T, rem)
parseAtomic (TFalse:rem) = (F, rem)
parseAtomic (TNot:rem) =
    let (p, rem1) = parseNot rem
    in (Not p, rem1)
parseAtomic _ = error "Invalid predicate"

 
filterUniq :: [String] -> [String]
filterUniq [] = []
filterUniq (x:xs)
  | x `elem` xs = filterUniq xs
  | otherwise = x : filterUniq xs


uniqueElem :: Predicate -> [String]
uniqueElem pred = filterUniq (collectVariables pred)
  where
    collectVariables :: Predicate -> [String]
    collectVariables T = []
    collectVariables F = []
    collectVariables (Var x) = [x]
    collectVariables (Not p) = collectVariables p
    collectVariables (And p q) = collectVariables p ++ collectVariables q
    collectVariables (Or p q) = collectVariables p ++ collectVariables q
    collectVariables (Implies p q) = collectVariables p ++ collectVariables q
    collectVariables (Iaoi p q) = collectVariables p ++ collectVariables q

    
generateAssignments :: [String] -> [VarAssign]
generateAssignments [] = [[]]
generateAssignments (var:vars) = [(var, val) : rem | val <- [True, False], rem <- generateAssignments vars]


any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs)
  | p x       = True
  | otherwise = any' p xs


isSatisfiable :: Predicate -> Bool
isSatisfiable pred = any' (\assignments -> eval pred assignments) (generateAssignments (uniqueElem pred))


splitList :: String -> [String]
splitList [] = [""]
splitList (c:cs)
    | c == ','  = "" : rem
    | otherwise = (c : head rem) : tail rem
    where rem = splitList cs

isSatisfiableSingle :: String -> Bool
isSatisfiableSingle predStr = isSatisfiable (parse (tokenize predStr))

isSatisfiableMulticore :: [String] -> IO [Bool]
isSatisfiableMulticore predicates = do
    results <- forM predicates (\predStr -> do
        mvar <- newEmptyMVar
        _ <- forkIO (do
            let result = isSatisfiableSingle predStr
            putMVar mvar result)
        return mvar)
    mapM takeMVar results



handleReadFileError :: IOException -> IO String
handleReadFileError _ = do
    putStrLn "Error: The file does not exist."
    return ""
    
    
main :: IO ()
main = do
    putStrLn "Enter a symbolic predicate file path:"
    filePath <- getLine
    input <- catch (readFile filePath) handleReadFileError
    if null input
        then putStrLn "No input because file does not exist"
        else do
            let separatedPred = splitList input  
            results <- isSatisfiableMulticore separatedPred
            putStrLn $ "Satisfiability of each predicate: " ++ show results
            putStrLn $ "Are all predicates satisfiable? " ++ show (all id results)
