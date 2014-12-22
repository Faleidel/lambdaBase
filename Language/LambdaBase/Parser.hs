{-# LANGUAGE RankNTypes, KindSignatures, BangPatterns #-}

{-|
All you need to parse lambdaBase.
-}
module Language.LambdaBase.Parser (parseExpr, name, operatorChars, fixityOf) where

import Text.ParserCombinators.Parsec
import Language.LambdaBase.Core

{-|
Parse a valid name
-}
name = many1 $ oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_*+-!@#$%?&=<>^|/.:"

{-|
What is a valid char for an operator.
-}
operatorChars = "_*+-!@#$%?&=<>^|/.:"

{-|
Easy function to parse a string.
-}
parseExpr :: String -> Either ParseError (Expr a)
parseExpr s = parse expr "" s

exprSep = do
    spaces
    optional $ do
        comment
        spaces
    return ()

comment = do
    choice [
        try inlineComment ,
        try lineComment
      ]

lineComment = do
    string "--"
    n <- many $ noneOf "\n"
    return ()

inlineComment = do
    string "{-"
    n <- many $ do
        choice [
            try (string "-" >> (notFollowedBy $ string "}") >> (return 'a'))
            , noneOf "-"
          ]
    string "-}"
    return ()

parenthesis = do
    string "("
    optional spaces
    n <- expr
    optional spaces
    string ")"
    return n

isOperator :: String -> Bool
isOperator n = and . map (\x -> any (==x) operatorChars) $ n

{-|
Will be infix if the string is an operator
-}
fixityOf :: String -> Fix
fixityOf n = if isOperator n then Infix else Prefix

nameNaked = do
    n <- name
    return $ Name n Naked $ fixityOf n

infixName = do
    (Name s d f) <- notNakedName "`" "`"
    return $ case fixityOf s of
        Infix  -> Name s d Prefix
        Prefix -> Name s d Infix

nameExpr = do
    choice [
        try nameNaked ,
        try infixName ,
        try $ notNakedName "{"  "}" ,
        try $ notNakedName ","  "," ,
        try $ notNakedName "\"" "\"" ,
        try $ notNakedName "'"  "'" ,
        try $ notNakedName "~"  "~" ,
        try $ notNakedName "["  "]"
      ]

notNakedName o c = do
    string o
    content <- many $ noneOf c
    string c
    return $ Name content (Delimited o c) Prefix

lambda = do
    string "\\"
    spaces
    n <- name
    spaces
    evsS <- choice [string "->", string "~>"]
    let evs = case evsS of
                   "->" -> Strict
                   "~>" -> Lazy
    exprSep
    content <- expr
    return $ Lambda (Arg n evs) content Prefix

exprSimple = do
    choice [
        try parenthesis ,
        try nameExpr ,
        try lambda
      ]

expr = do
    optional spaces
    exprs <- sepEndBy1 exprSimple exprSep
    optional spaces
    return $ Expr exprs Prefix
