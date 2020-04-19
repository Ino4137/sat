{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Sat where

import Control.Recursion
import Data.Bits
import Data.Char (ord, chr)
import Data.Functor.Identity
import Control.Monad
import Data.List

import Text.Parsec.Char
import Text.Parsec
import Text.Parsec.Expr

data ExprF f =
    Not f
  | And f f
  | Or  f f
  | Impl f f
  | Equiv f f
  | Atom Char
  deriving Functor

notE f = Fix (Not f)
andE f g = Fix (And f g)
orE  f g = Fix (Or f g)
implE f g = Fix (Impl f g)
equivE f g = Fix (Equiv f g)
atomE f = Fix (Atom f)

type Expr = Fix ExprF
newtype Env = Env Int
-- read the bits left to right
--  a b c
-- [1,0,1]
showEnv k (Env n) = show . map (`rem` 2) . take k . iterate (`div` 2) $ n

-- too lazy, for now wroking under the invariant that all the var names are lowercase
value :: Char -> Env -> Bool
value (ord -> n) (Env env) = testBit env (n - ord 'a')

-- too lazy to implement proper De Brujin indices, it all just werks
eval :: Env -> Expr -> Bool
eval env = cata alg
  where
    impl a b = not a || b
    alg (Atom c)  = value c env
    alg (Not f)   = not f
    alg (And f g) = f && g
    alg (Or f g)  = f || g
    alg (Impl f g) = impl f g
    alg (Equiv f g) = impl f g && impl g f
    
tree :: Expr -> String
tree = cata alg
  where
    alg (Atom c) = [c]
    alg (Not f)  = "(not " ++ f ++ ")"
    alg (And f g) = "(" ++ f ++ " and " ++ g ++ ")"
    alg (Or f g) = "(" ++ f ++ " or " ++ g ++ ")"
    alg (Impl f g) = "(" ++ f ++ " impl " ++ g ++ ")"
    alg (Equiv f g) = "(" ++ f ++ " eqiv " ++ g ++ ")"

variables :: Expr -> Int
variables = cata alg
  where
    alg (Atom (ord -> n)) = setBit 0 (n - ord 'a')
    alg (Not f) = f
    alg (Or f g) = max f g
    alg (And f g) = max f g
    alg (Impl f g) = max f g
    alg (Equiv f g) = max f g

falsify :: Expr -> ([Env], Int)
falsify exp = (,succ . round . logBase 2 $ fromIntegral n) . filter (flip ((not.).eval) exp) . fmap Env $ enumFromTo 0 n
  where n = variables exp 

pprintEnvs :: ([Env], Int) -> String
pprintEnvs (e,n) =
  let header = printVars $ pure . chr . (+ ord 'a') <$> enumFromTo 0 (n-1)
  in header ++ intercalate "\n" (showEnv n <$> e)
  where
    printVars (intercalate "," -> e) = '[' : e ++ "]\n" 

-- Parsing

parseExpr = parse expr "" . filter (not.(`elem` " \t"))

expr :: Parsec String u Expr
expr = buildExpressionParser table term

term = between (char '(') (char ')') expr
  <|> (atomE <$> lower)  

-- only one that actually needs AssocRight is ->, rest could be AssocLeft
table :: OperatorTable String u Identity Expr
table = [
    [prefix "~" notE],
    [binary "/\\" andE AssocRight], 
    [binary "\\/" orE AssocRight],
    [binary "->" implE AssocRight, binary "<->" equivE AssocRight] 
  ]
  where
    prefix :: String -> (Expr -> Expr) -> Operator String u Identity Expr
    prefix n f   = Prefix (string n >> pure f)
    binary :: String -> (Expr -> Expr -> Expr) -> Assoc -> Operator String u Identity Expr
    binary n f a = Infix  (string n >> pure f) a
