module Stmt where

import qualified Control.Monad as CM
import qualified Data.Bifunctor as B

newtype Identifier = I {name :: String} deriving (Eq)
type Nat           = Integer

instance Show Identifier where
  show (I n) = n

data Val = NatVal Nat
         | IdVal Identifier
         | BoolVal Bool deriving (Eq, Show)

data Expr = Val :+: Val
          | Val :-: Val
          | Val :==: Val
          | Val :<: Val
          | ValExpr Val
          deriving (Eq, Show)

data Stmt = Identifier :<-: Expr
          | Skip
          | Twice Stmt
          | Compound [Stmt]
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Return Expr
          deriving (Eq, Show)
