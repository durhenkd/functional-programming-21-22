module L12 where

import Parser
import Result


{-
    E 12.1.1 Create a new parser using satisfies , char :: Char -> Parser Char that generates a
    parser that parses a given character (received as first argument).
-}

charS :: Char -> Parser Char
charS c = satisfies (\x -> x == c) "Unexpected Character"

{-
    E 12.1.2 Create a new parser using digit :: Parser Char that parses a digit.
-}

digit :: Parser Char
digit = satisfies (\c -> elem c ['0' .. '9']) "digit parser"

{-
    E 12.2.1 Create a new parser for strings, string’ , that only uses the combinators defined so far
    ( andThen , orElse and pMap ), without using inner functions.
-}

string' :: String -> Parser String
string' "" = succeed []
string' (subc:subs) = (\(x, xs) -> x:xs) `pMap` (char subc `andThen` string' subs)

{-
    E 12.2.2 Create a new parser number :: Parser Int that parses a number.
-}

number :: Parser Int
number =  (\x -> read x :: Int) `pMap` (some digit)

{-
    E 12.3.1 Create a new combinator pThen :: Parser a -> Parser b -> Parser b . First, it runs the
    first parser, pa . If pa succeeds, it discards the result and runs pb on the remaining
    input from pa . If pa fails, the error is returned.
-}

-- | Chain two parses, discarding the value of the first parser
pThen :: Parser a -> Parser b -> Parser b
pThen pa pb = parser inner
  where
    inner input =
      case runParser pa input of
        Success (a, rest) ->
          case runParser pb rest of
            Success (b, remaining) -> success b remaining
            Error err -> Error err
        Error err -> Error err

{-
    E 12.3.2 Create a parser sepBy sep p with the signature sepBy :: Parser a -> Parser b -> Parser [b]
-}

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep p = parser inner
    where
        inner "" =  success [] ""
        inner input = 
            case runParser p input of
                Success (r, rest) ->
                -- `many` always succeeds
                    case runParser sep rest of
                        Success (rs, remaining) -> 
                            case inner remaining of
                                Success (rsc, rem) -> success (r:rsc) rem
                        Error _ -> case inner rest of
                            Success (rsc, rem) -> success (r:rsc) rem
                Error _ -> success [] input

{-  
    E 12.3.3 Create a new combinator between :: Parser a -> Parser b -> Parser c -> Parser c
        which takes as arguments 3 parsers: between pHd pTl p . It runs them in the order pHd ,
        p and pTl , passing the remaining input from the one to the other. If all 3 parsers
        succeed, it discards the results from pHd and pTl and returns the result of p . If any
        parser fails the error is returned.
-}

-- | Run a parser between two other parsers, discarding the result of the enclosing parsers
--
-- >>> runParser (between (char '"') (char '"') (many letter)) "\"Hello\""
-- Success ("Hello","")
-- >>> runParser (between (char '[') (char ']') number) "[1]"
-- Success (1,"")
--
-- >>> runParser (between (char '[') (char ']') number) "[1|"
-- Error (UnexpectedInput {gotInput = "|", expectedInput = "character ']'"})

between :: Parser a -> Parser b -> Parser c -> Parser c
between pHd pTl p = parser inner
    where
        inner "" = Error UnexpectedEndOfInput
        inner input = 
            case runParser ((pHd `andThen` p) `andThen` pTl) input of
                Success (r, rem) -> 
                    case r of 
                        ((_, result), _) -> success result rem
                Error err -> Error err
