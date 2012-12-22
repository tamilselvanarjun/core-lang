

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