{-# LANGUAGE RankNTypes, KindSignatures, BangPatterns, GADTs #-}

{-|
Module      : Language.LambdaBase.Eval
Description : Evaluation function for lambdaBase

This module is for evaluation of lambdaBase programmes after parsing (see the eval function).

The language is simple. There is 2 'thing' : lambda and literals.
Lambda are in the form :

> \x -> x y z ...

Lambda have only one argument. If you want a multi argument function you can nest lambdas like this :

> \x -> \y -> x + y

This mean partial application on lambda is possible.

In a programme like "add 123 321" lambdaBase cant execute the add function since it doesn't know what it is.
In this case, the toLit function of the Lit class is called.
In the presence of "add 123 321" the first 2 tokens ( add and 123 ) will be converted to member of the Lit class an reduced with the apply function.

To step to reducing "add 123 321" will be:

> add 123 321
> (toLit add) 123 321
> (litAdd) (toLit 123) 321
> (apply lit lit123) 321
> litAdd123 321
> litAdd123_321
> lit444

For an example of an emplementation of the Lit class, see the LambdaLit packaged.


By default lambdas are strict in it argument. If you want a lazy one ( to define an if function for example ) you can using a ~ like this :

> (\x ~> x) -- lazy
> (\x -> x) -- strict

An if function (lazy on everything but the condition) would look like this :

> (\if -> \True -> \False -> if True 123 321) (\c -> \b1 ~> \b2 ~> c b1 b2) (\b1 ~> \b2 ~> b1) (\b1 ~> \b2 ~> b2)

-}
module Language.LambdaBase.Eval (eval,setFix,substituteInExpr) where

import Language.LambdaBase.Parser (fixityOf)
import Language.LambdaBase.Core

toExList :: forall a. Expr a -> Expr a
toExList (Expr a f) = Expr a f
toExList a = Expr [a] Prefix

toListOfEx :: forall t. Expr t -> [Expr t]
toListOfEx (Expr a _) = a
toListOfEx a = toListOfEx $ toExList a

isName :: forall t. Expr t -> Bool
isName (Name _ _ _) = True
isName _            = False

evalLambda :: (Lit a, Show a, Eq a) => Expr a -> Expr a -> Expr a
evalLambda l@(Lambda (Arg n _) _ _) to = case substituteLambda l (setFix to (fixityOf n)) of
    Lambda _ e _ -> e
evalLambda _ _ = error "Can't call evalLambda on non lambda expression"

{-|
Set Prefix/Infix/LateFix on an expression
-}
setFix :: Expr a -> Fix -> Expr a
setFix (Name x y _)   f = Name x y f
setFix (Expr l _)     f = Expr l f
setFix (Lambda x y _) f = Lambda x y f
setFix (Lit a _)      f = Lit a f

getFix :: Expr a -> Fix
getFix (Lambda _ _ f) = f
getFix (Expr _ f)     = f
getFix (Name _ _ f)   = f
getFix (Lit _ f)      = f

substituteLambda :: (Lit a, Show a, Eq a) => Expr a -> Expr a -> Expr a
substituteLambda (Lambda (Arg n s) ex lfix) to =
    Lambda (Arg n s) ( substituteInExpr ex (Name n Naked (fixityOf n)) to ) lfix
substituteLambda _ _ = error "Can't call substituteLambda on non lambda expression"

nameEq :: Expr a -> Expr a -> Bool
nameEq (Name a b c) (Name a2 b2 c2) = (a == a2) && (b == b2) && (c == c2)
nameEq _ _ = False

nameNotEq :: Expr a -> Expr a -> Bool
nameNotEq a b = not $ nameEq a b

{-|
Take en expression, a from and a to and replace all from by to and return the new expression.
-}
substituteInExpr :: (Lit a, Show a, Eq a) => Expr a -> Expr a -> Expr a -> Expr a
substituteInExpr !e !from !to =
    Expr (map (helper from to) (toListOfEx e)) (getFix e)
  where
    helper !f !t !e = case e of
        l@(Lambda (Arg nn ss) lex llfix) -> if nameNotEq (Name nn Naked (fixityOf nn)) f then Lambda (Arg nn ss) (substituteInExpr lex from to) llfix else l
        Expr ne efix            -> Expr (map (helper f t) ne) efix
        ne                      -> if nameEq e f then t else ne


{-|
Reduce an expression. Can return an expression if it can't be reduced more. Ex :

> (\x -> x) (\x -> x)

This will reduce to :

> (\x -> x)

Since it can't be reduced more.
-}
eval :: (Lit a, Show a, Eq a) => Expr a -> Expr a

-- Swap infix element
eval !(Expr ( x : inf : xs ) fix) | getFix inf == Infix =
    eval $ Expr ( (setFix inf Prefix) : (setFix x Prefix) : xs ) fix

-- NAME TO LIT
-- Turn a name to a literal
eval !n@(Name _ _ _) =
    Lit ( toLit n ) Prefix

-- Convert the name at the head in a literal
eval !(Expr ( n@(Name x t fi) : xs ) fix) =
    eval $ Expr ( (Lit (toLit n) fi) : xs ) fix


-- CLEANING UNWRAPING
-- Simple unwraping
eval !(Expr ([a]) fix) =
    eval $ setFix a fix

-- If the programme starts with parentesis delete theme, it can't do anything ^.^'
eval !(Expr ((Expr e fix2):xs) fix) =
    eval $ Expr ( e ++ xs ) fix


-- LAMBDA STUF
-- LateFix Lambda
eval !(Expr ( l@(Lambda (Arg _ Strict) _ _) : a : xs ) fix) | getFix a /= LateInfix =
    eval $ Expr ( a : (setFix l LateInfix) : xs ) fix
eval !(Expr ( l@(Lambda (Arg _ Lazy) _ _) : a : xs ) fix) | getFix a /= LateInfix =
    eval $ Expr ( (evalLambda l (setFix a Prefix)) : xs ) fix
-- Feed lambda. Here, I can't wait for the lateInfix swap since it would reTrigger the LateFix Lambda swap
eval !(Expr ( a : (l@(Lambda _ _ _)) : xs ) fix) | getFix l == LateInfix =
    eval $ Expr ( (evalLambda l (setFix a Prefix)) : xs ) fix


-- Setting the Expr after the Lit to be evaluated befor being passed to the Lit with apply
eval !(Expr ( a1@(Lit lit _) : a2@(Expr _ _) : xs ) fix) | getEVS lit == Strict =
    eval ( Expr ( a2 : (setFix a1 LateInfix) : xs ) fix )


-- LATE THING
-- Swap lateInfix element
eval !(Expr ( x : inf : xs ) fix) | getFix inf == LateInfix =
    eval $ Expr ( (setFix inf Prefix) : (setFix x Prefix) : xs ) fix


-- If we get here, there realy is nothing to do but consume literals or end.
-- LIT APPLY -> LIT NAME, LIT LIT, LIT EXPR
-- Fuse litterals with a to be literal with toLit
eval !(Expr ( (Lit x _) : (Name y t fi) : xs ) fix) =
    eval ( Expr (( apply x ( toLit (Name y t fi) )) : xs) fix )

-- Apply literals
eval !(Expr ( (Lit !x _) : (Lit !y _) : xs ) fix) =
    eval ( Expr ((apply x y) : xs) fix )
 
-- Feed a real Expr to a literal, like passing a lambda to a literal when the lambda can't be reduced
eval !(Expr ( (Lit x _) : y : xs ) fix) =
    eval $ Expr ((apply x (fromExpr y)) : xs) fix


-- END
eval !x = x
