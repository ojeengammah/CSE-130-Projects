module Language.Nano.Eval
  ( execFile, execString, execExpr
  , eval, lookupId, prelude
  , parse
  , env0
  )
  where

import Control.Exception (throw, catch)
import Language.Nano.Types
import Language.Nano.Parser
import Foreign.C (e2BIG)

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (readFile f >>= execString) `catch` exitError

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) `catch` exitError

--------------------------------------------------------------------------------
-- | `parse s` returns the Expr representation of the String s
--
-- >>> parse "True"
-- EBool True
--
-- >>> parse "False"
-- EBool False
--
-- >>> parse "123"
-- EInt 123
--
-- >>> parse "foo"
-- EVar "foo"
--
-- >>> parse "x + y"
-- EBin Plus (EVar "x") (EVar "y")
--
-- >>> parse "if x <= 4 then a || b else a && b"
-- EIf (EBin Le (EVar "x") (EInt 4)) (EBin Or (EVar "a") (EVar "b")) (EBin And (EVar "a") (EVar "b"))
--
-- >>> parse "if 4 <= z then 1 - z else 4 * z"
-- EIf (EBin Le (EInt 4) (EVar "z")) (EBin Minus (EInt 1) (EVar "z")) (EBin Mul (EInt 4) (EVar "z"))
--
-- >>> parse "let a = 6 * 2 in a /= 11"
-- ELet "a" (EBin Mul (EInt 6) (EInt 2)) (EBin Ne (EVar "a") (EInt 11))
--
-- >>> parseTokens "() (  )"
-- Right [LPAREN (AlexPn 0 1 1),RPAREN (AlexPn 1 1 2),LPAREN (AlexPn 3 1 4),RPAREN (AlexPn 6 1 7)]
--
-- >>> parse "f x"
-- EApp (EVar "f") (EVar "x")
--
-- >>> parse "(\\ x -> x + x) (3 * 3)"
-- EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EBin Mul (EInt 3) (EInt 3))
--
-- >>> parse "(((add3 (x)) y) z)"
-- EApp (EApp (EApp (EVar "add3") (EVar "x")) (EVar "y")) (EVar "z")
--
-- >>> parse <$> readFile "tests/input/t1.hs"
-- EBin Mul (EBin Plus (EInt 2) (EInt 3)) (EBin Plus (EInt 4) (EInt 5))
--
-- >>> parse <$> readFile "tests/input/t2.hs"
-- ELet "z" (EInt 3) (ELet "y" (EInt 2) (ELet "x" (EInt 1) (ELet "z1" (EInt 0) (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1"))))))
--
-- >>> parse "1-2-3"
-- EBin Minus (EBin Minus (EInt 1) (EInt 2)) (EInt 3)
-- >>> parse "1+a&&b||c+d*e-f-g x"
-- EBin Or (EBin And (EBin Plus (EInt 1) (EVar "a")) (EVar "b")) (EBin Minus (EBin Minus (EBin Plus (EVar "c") (EBin Mul (EVar "d") (EVar "e"))) (EVar "f")) (EApp (EVar "g") (EVar "x")))
--
-- >>> parse "1:3:5:[]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))
--
-- >>> parse "[1,3,5]"
-- EBin Cons (EInt 1) (EBin Cons (EInt 3) (EBin Cons (EInt 5) ENil))

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseExpr

exitError :: Error -> IO Value
exitError (Error msg) = return (VErr msg)

--------------------------------------------------------------------------------
-- | `eval env e` evaluates the Nano expression `e` in the environment `env`
--   (i.e. uses `env` for the values of the **free variables** in `e`),
--   and throws an `Error "unbound variable"` if the expression contains
--   a free variable that is **not bound** in `env`.
--
-- part (a)
--
-- >>> eval env0 (EBin Minus (EBin Plus (EVar "x") (EVar "y")) (EBin Plus (EVar "z") (EVar "z1")))
-- 0
--
-- >>> eval env0 (EVar "p")
-- *** Exception: Error {errMsg = "unbound variable: p"}
--
-- part (b)
--
-- >>> eval []  (EBin Le (EInt 2) (EInt 3))
-- True
--
-- >>> eval []  (EBin Eq (EInt 2) (EInt 3))
-- False
--
-- >>> eval []  (EBin Eq (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> eval []  (EBin Lt (EInt 2) (EBool True))
-- *** Exception: Error {errMsg = "type error: binop"}
--
-- >>> let e1 = EIf (EBin Lt (EVar "z1") (EVar "x")) (EBin Ne (EVar "y") (EVar "z")) (EBool False)
-- >>> eval env0 e1
-- True
--
-- >>> let e2 = EIf (EBin Eq (EVar "z1") (EVar "x")) (EBin Le (EVar "y") (EVar "z")) (EBin Le (EVar "z") (EVar "y"))
-- >>> eval env0 e2
-- False
--
-- part (c)
--
-- >>> let e1 = EBin Plus (EVar "x") (EVar "y")
-- >>> let e2 = ELet "x" (EInt 1) (ELet "y" (EInt 2) e1)
-- >>> eval [] e2
-- 3
--
-- part (d)
--
-- >>> eval [] (EApp (ELam "x" (EBin Plus (EVar "x") (EVar "x"))) (EInt 3))
-- 6
--
-- >>> let e3 = ELet "h" (ELam "y" (EBin Plus (EVar "x") (EVar "y"))) (EApp (EVar "f") (EVar "h"))
-- >>> let e2 = ELet "x" (EInt 100) e3
-- >>> let e1 = ELet "f" (ELam "g" (ELet "x" (EInt 0) (EApp (EVar "g") (EInt 2)))) e2
-- >>> eval [] e1
-- 102
--
-- part (e)
-- |
-- >>> :{
-- eval [] (ELet "fac" (ELam "n" (EIf (EBin Eq (EVar "n") (EInt 0))
--                                  (EInt 1)
--                                  (EBin Mul (EVar "n") (EApp (EVar "fac") (EBin Minus (EVar "n") (EInt 1))))))
--             (EApp (EVar "fac") (EInt 10)))
-- :}
-- 3628800
--
-- part (f)
--
-- >>> let el = EBin Cons (EInt 1) (EBin Cons (EInt 2) ENil)
-- >>> execExpr el
-- (1 : (2 : []))
-- >>> execExpr (EApp (EVar "head") el)
-- 1
-- >>> execExpr (EApp (EVar "tail") el)
-- (2 : [])
--------------------------------------------------------------------------------
eval :: Env -> Expr -> Value
--------------------------------------------------------------------------------
-- eval = error "TBD:eval"
eval _ (EInt n)        = VInt n
eval _ (ENil)          = VNil
eval _ (EBool b)       = VBool b
eval env (EVar x)        = lookupId x env
eval env (EBin op e1 e2) = evalOp op v1 v2
  where
    v1 = eval env e1
    v2 = eval env e2

eval env (EIf p t f)  = eval env (if getBool (eval env p) then t else f)
  where
    getBool :: Value -> Bool
    getBool (VBool bool) = bool
    getBool _ = throw (Error "type error: predicate not a boolean")

eval env (ELet id e1 e2) = eval ((id, eval env e1):env) e2
    
eval env (ELam x y) = VClos env x y

eval env (EApp e1 e2) = evApp (getId e1) (eval env e1) (eval env e2)
  where
    getId :: Expr -> Id
    getId (EVar id) = id
    getId _ = ""

evApp :: Id -> Value -> Value -> Value
evApp id (VClos env' x body) value = eval env'' body
  where
    env'' = (x, value) : (id, VClos env' x body) : env'
evApp _ (VPrim (func)) value = func value
evApp _ _ _ = throw (Error "not a function")



--------------------------------------------------------------------------------
evalOp :: Binop -> Value -> Value -> Value
--------------------------------------------------------------------------------
evalOp Plus (VInt a) (VInt b) = VInt (a + b)
evalOp Minus (VInt a) (VInt b) = VInt (a - b)
evalOp Mul (VInt a) (VInt b) = VInt (a * b)
evalOp Eq (VInt a) ( VInt b) = (VBool (a == b))
evalOp Eq (VBool a) (VBool b) = VBool (a == b) 
evalOp Eq (VNil) (VNil) = VBool(True)
evalOp Eq a b = checkEq a b
evalOp Ne (VInt a) (VInt b) = (VBool (a /= b))
evalOp Ne (VBool a) (VBool b) = (VBool (a /= b))
evalOp Lt (VInt a) (VInt b) = VBool (a < b)
evalOp Le (VInt a) (VInt b) = VBool (a <= b)
evalOp And (VBool a) (VBool b) = VBool (a && b)
evalOp Or (VBool a) (VBool b) = VBool (a || b)
evalOp Cons a b = VCons a b
evalOp binop _ _ = throw (Error "type error : binop")

checkEq :: Value -> Value -> Value
checkEq (VCons _ _) (VNil) = VBool(False)
checkEq (VNil) (VCons _ _) = VBool(True)
checkEq (VCons m ms) (VCons m2 m2s) =  VBool ( vEqual && vsEqual)
  where 
    (VBool vEqual) = (checkEq m m2)
    (VBool vsEqual) = (checkEq ms m2s)
checkEq _ _ = throw (Error ("type error: invalid equality"))

--------------------------------------------------------------------------------
-- | `lookupId x env` returns the most recent
--   binding for the variable `x` (i.e. the first
--   from the left) in the list representing the
--   environment, and throws an `Error` otherwise.
--
-- >>> lookupId "z1" env0
-- 0
-- >>> lookupId "x" env0
-- 1
-- >>> lookupId "y" env0
-- 2
-- >>> lookupId "mickey" env0
-- *** Exception: Error {errMsg = "unbound variable: mickey"}
--------------------------------------------------------------------------------
lookupId :: Id -> Env -> Value
--------------------------------------------------------------------------------
lookupId x env
  | null env = throw (Error ("unbound variable: " ++ x))
  | x == fst(head env) = snd(head env)
  | otherwise = lookupId x (tail env)

prelude :: Env
prelude =
  [ -- HINT: you may extend this "built-in" environment
    -- with some "operators" that you find useful...
    ("head", VPrim ( h) ),
    ("tail", VPrim ( t ))
  ]

h :: Value -> Value
h (VNil) = throw (Error "no head in empty list")
h (VCons v _) = v
h _ = throw (Error "type error : head requires a list")

t:: Value -> Value
t (VNil) = throw (Error "no tail in empty list")
t (VCons _ vs) = vs
t _ = throw (Error "type error: tail requires a list")

env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]

--------------------------------------------------------------------------------
