{-# LANGUAGE RankNTypes, KindSignatures, BangPatterns, GADTs #-}

{-|
Module      : Language.LambdaBase.Core
Description : The core dataTypes of the language
-}
module Language.LambdaBase.Core where

import Data.List

-- | Evaluation scheme, Lazy or Strict
data EVS = Lazy | Strict deriving (Show, Eq)

-- | Lambda argument
data Arg = Arg !String !EVS deriving (Show, Eq)

{-|
Infix / Prefix / LateInfix

The LateInfix is a trick used is the eval function.
-}
data Fix = Infix | Prefix | LateInfix deriving (Eq,Show)

{-|
A name is a 'to be' literal
In a programme, foo can be a name ( if it is not a lambda argument ). "foo" is also a name but is delimited by double quotes.
-}
data NameType =
    Naked
  | Delimited String String
  deriving (Eq,Show)

{-|
An expression of host language a.
-}
data Expr a =
    Lambda !Arg !(Expr a) !Fix
  | Expr ![Expr a] !Fix
  | Name !String !NameType !Fix
  | Lit !a !Fix

{-|
Crappy show instance ;(
-}
instance (Show a) => Show (Expr a) where
    show (Lambda (Arg a _) e _)       = "(\\"++a++" -> \n"++(show e)++")"
    show (Expr l Prefix)              = "(" ++ ( concat ( intersperse " \n " . map (show) $ l ) ) ++ ")"
    show (Expr l Infix)               = "`(" ++ ( concat ( intersperse " \n " . map (show) $ l ) ) ++ ")`"
    show (Name x Naked _)             = x
    show (Name x (Delimited d1 d2) _) = d1 ++ x ++ d2
    show (Lit a f)                    = show a
{-|
The Lit class is for the language contained inside lambdaBase.

toLit sould take a Name and transform it to a literal of its language.

apply is for function application.

A programme

> add 123 321

will be evaluated by transforming add to a literal.

Transforming 123 to a literal.

Partially apply add and 123 with the apply function.

Transforming 321 to a literal.

Apply the "add123" literal and the 321 literal.

And if the Lit instance is correct, the result should be a 444 literal.

See lambdaLit for an example of a Lit instance.
-}
class Lit a where
    apply :: a -> a -> Expr a
    fromExpr :: Expr a -> a
    toLit :: Expr a -> a
    getEVS :: a -> EVS
