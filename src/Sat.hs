{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

module Sat where

import Control.Recursion
import qualified Data.IntMap as IM
import Data.Char (ord, chr)
import Control.Applicative

import Text.Parsec.Char
import Text.Parsec

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
type Env = IM.IntMap Bool
type EnvR = IM.IntMap 
value :: Char -> Env -> Maybe Bool
value (ord -> n) = IM.lookup n 

eval :: Env -> Expr -> Maybe Bool
eval env = cata alg
  where
    impl a b = not a || b
    alg (Atom c)  = value c env
    alg (Not f)   = not <$> f
    alg (And f g) = liftA2 (&&) f g
    alg (Or f g)  = liftA2 (||) f g
    alg (Impl f g) = liftA2 impl f g
    alg (Equiv f g) = (&&) <$> (liftA2 impl f g) <*> (liftA2 impl g f)

parseExp :: String -> Expr
parseExp = parse expP ""
  where
    expP = do
      
