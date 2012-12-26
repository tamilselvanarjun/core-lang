import Data.Char
import Debug.Trace

main = do putStrLn "hi"

data Expr a
	=  EVar Name
   | ENum Int
   | EConstr Int Int
   | EAp (Expr a) (Expr a)
   | ECase (Expr a) [Alter a]
   | ELam [a] (Expr a)
   | ELet IsRec [(a, Expr a)] (Expr a)
   deriving (Show)

type Alter a =  (Int, [a], (Expr a))
type CoreAlt = Alter Name

type Name = String
type IsRec = Bool

-- Don't quite understand the need for type parametrization of expressions here
-- 
type CoreExpr = Expr Name

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- pg. 19 EAp (EAp (EVar "+") (EVar "x")) (EVar "y")

recursive = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = map fst defns

rhssOf defns = map snd defns
-- [name | (name, rhs) <- defns]

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e = False

-- pg. 21 Example: 
-- [("main", [], (EAp (EVar "double") (ENum 21))),
-- ..

-- TODO: Check that this was copied correctly 
preludeDefs :: CoreProgram
preludeDefs = [
  ("I", ["x"], EVar "x"),
  ("K", ["x","y"], EVar "x"),
  ("K1",["x","y"], EVar "y"),
  ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))),
  ("compose", ["f","g","x"], EAp (EVar "f") (EAp (EVar "g") (EVar "x"))),
  ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))]


-- Basic attempt at pretty printing before proceeding with the chapter
pprint' :: CoreProgram -> String
pprint' defns = concat $ map (\x -> (printScDefn x) ++ "\n") defns

printScDefn :: CoreScDefn -> String
printScDefn (name, args, _) = name ++ " " ++ (foldl (\x y -> x ++ y ++ " ") "" args) ++ "= "

-- In ghci, to see results:
-- do putStrLn $ pprint preludeDefs

--iNil :: Iseq
--iStr :: String -> Iseq
--iNewline :: Iseq -- Why shouldn't this be like indent
--iAppend :: Iseq -> Iseq -> Iseq
--iIndent :: Iseq -> Iseq -- Indent the verb here
--iDisplay :: Iseq -> String

--pprExpr :: CoreExpr -> Iseq
--pprExpr (EVar v) = iStr v
--pprExpr EAp (Expr e1) (Expr e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprExpr e2)

---- Exercise 1.2

--iConcat :: [Iseq] -> Iseq
--iConcat seqs = foldl iAppend iNil seqs

--iInterleave :: Iseq -> [Iseq] -> Iseq
--iInterleave seq seqs = iConcat $ map (\x -> seq `iAppend` x) (tail seqs)

--pprint prog = iDisplay (pprProgram prog)

-- TODO: Copy pprExpr (ELet)
-- TODO: Write pprExpr for case, lambda, ENum
-- TODO: Write pprProgram
-- TODO: Copy Section 1.5.3, ...

-- A parser for Jane

-- TODO: Read files in as Strings in the main monad
parse :: String -> CoreProgram
parse = syntax . clex -- lex is a function in Prelude

-- Allowed characters: [a-zA-Z0-9_], ;, =, [+|-|*|/], (, ),
--- keywords (let, letrec, where, in, \ ., Pack{}, <,>, ->, case) are lexed seperately?

-- Lexer

type Token = String

clex :: String -> [Token]
clex (c:cs) | isWhiteSpace c = clex cs
clex (c:cs) | isDigit c = num_token : clex rest_cs
                          where num_token = c : takeWhile isDigit cs
                                rest_cs = dropWhile isDigit cs
clex (c:cs) | isAlpha c = var_token : clex rest_cs
                          where var_token = c : takeWhile isIdChar cs
                                rest_cs = dropWhile isIdChar cs
clex (c:cs) = [c] : clex cs
clex [] = []

isIdChar c = isAlpha c || isDigit c || (c == '_')
isWhiteSpace = isSpace

-- TODO: Exercise 1.{9,10,11}
-- 11 involves getting the line numbers: token == (num, [char])

-- clex "main = map_small 2;"

-- Parser

type Parser a = [Token] -> [(a, [Token])]


pSat :: (String -> Bool) -> Parser String
pSat fn (tok:toks)
  | fn tok = [(tok, toks)]
  | otherwise = []
pSat fn [] = []

--pLit :: String -> Parser String
--pLit str (tok:toks)
--  | (str == tok) = [(str, toks)]
--  | otherwise = []
--pLit str [] = []

pLit str = pSat ((==) str)

--pVar :: Parser String
--pVar (tok:toks) = [(tok, toks)]
--pVar [] = []

keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pVar = pSat (\x -> not $ x `elem` keywords)

pApply :: Parser a -> (a -> b) -> Parser b
pApply parser fn toks = map (\x -> ((fn (fst x)), snd x)) (parser toks)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-- Test: pApply pGreeting (mapTuple $ map toUpper) ["goodbye", "James", "!"]
-- [(("GOODBYE","JAMES"),[])]

pNum :: Parser Int
pNum = pApply (pSat $ all isDigit) read

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p2 toks ++ p1 toks

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks =
  [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks,
                             (v2, toks2) <- p2 toks1]

-- Simple language

pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")
--pGreeting = pThen mk_pair pHelloOrGoodbye pVar
--            where
--            mk_pair hg name = (hg, name)

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks =
  [ (combine v1 v2 v3, toks3) | (v1, toks1) <- p1 toks,
                                (v2, toks2) <- p2 toks1,
                                (v3, toks3) <- p3 toks2]

pThen4 combine p1 p2 p3 p4 toks =
  pThen3 (\x -> combine (fst x) (snd x)) (pThen (\x y -> (x, y)) p1 p2) p3 p4 toks

-- TODO: What's better way to write pThen4?
-- Using pThen3

pGreeting = pThen3 mk_greeting
                   pHelloOrGoodbye
                   pVar
                   (pLit "!")
            where
              mk_greeting hg name exclamation = (hg, name)

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pEmpty :: a -> Parser a
pEmpty val toks = [(val, toks)]

--- TODO: Understand Ex 1.19
pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p toks = (pOneOrMoreHelper p [] toks [])

pOneOrMoreHelper :: Parser a -> [a] -> [Token] -> [([a], [Token])] -> [([a], [Token])]
pOneOrMoreHelper p v toks resultP = let parsed = (p toks)
                                        (val, toks1) = (head parsed)
                                        v1 = (val : v)
                                        resultP' = resultP ++ [(v1, toks1)]
                                    in
                                    if (length parsed) >= 1 then
                                      pOneOrMoreHelper p v1 toks1 resultP'
                                    else
                                      resultP
-- Test: pOneOrMore (pLit "goodbye") ["goodbye", "goodbye", "!"]

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep mainP sepP toks = (pOneOrMoreHelper p [] toks []) where
                                    p = pThen take_first mainP sepP
                                    take_first a b = a

-- Test: pOneOrMoreWithSep (pLit "goodbye") (pLit ",") ["goodbye", ",", "goodbye", ",", "!"]

syntax :: [Token] -> CoreProgram
syntax =  take_first_parse . pProgram
          where
          take_first_parse ((prog,[]) : others) = prog
          take_first_parse (parse : others) = take_first_parse others
          take_first_parse other = error "Core Syntax error"

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr
      where
      mk_sc name args eq expr = (name, args, expr)


pENum = pApply pNum (\x -> ENum x)
pEVar = pApply pVar (\x -> EVar x)

-- ELet IsRec [(a, Expr a)] (Expr a)

pLet = pThen4 make_let (pLit "let") pDefns (pLit "in") pExpr
       where
       pDefns = pOneOrMoreWithSep pDefn (pLit ";")
       pDefn = pThen3 make_defn pVar (pLit "=") pExpr
       make_defn var _ expr = (var, expr)
       make_let _ defns _ expr = (ELet False defns expr)

pCase = pThen4 make_case (pLit "case") pExpr (pLit "of") pAlters
        where
          make_case _ expr _ alters = ECase expr alters
          pAlters = pOneOrMoreWithSep pAlter (pLit ";")
          pAlter = pThen4 make_alter pTag (pLit "-") (pLit ">") pExpr
          pTag = pThen3 (\x y z -> y) (pLit "<") pNum (pLit ">")
          make_alter num _ _ expr = (num, [], expr)

pExpr :: Parser CoreExpr
pExpr = ((pEVar `pAlt` pENum) `pAlt` pLet) `pAlt` pCase

-- Testing: parse "f x y = 3; g x y = h;"
-- parse "f=3; g x y = let z = x in z; h x = case (let y = x in y) of <1> -> 2 ; <2> -> 5"


-- Understand how all the empty parses get combined?

-- TODO: letrec, case statement bindings?
-- TODO: More requirement for terminating ';' for single let binding (same problem with ;)
-- TODO: Exercise 1.22, Left Recursion, ...