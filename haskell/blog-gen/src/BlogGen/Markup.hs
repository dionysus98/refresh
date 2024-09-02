module BlogGen.Markup
  ( Document,
    Structure (..),
    parse,
  )
where

import Data.Maybe (maybeToList)
import Numeric.Natural

type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show)

trim :: String -> String
trim = unwords . words

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
-- Empty; Done case
parseLines context [] = maybeToList context
-- Heading 1
parseLines context (('*' : ' ' : line) : rest) =
  maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
-- Unordered List
parseLines context (('-' : ' ' : line) : rest) =
  case context of
    Just (UnorderedList unList) ->
      parseLines (Just (UnorderedList (unList <> [trim line]))) rest
    _ ->
      maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
-- Ordered List
parseLines context (('#' : ' ' : line) : rest) =
  case context of
    Just (OrderedList oList) ->
      parseLines (Just (OrderedList (oList <> [trim line]))) rest
    _ ->
      maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)
-- Ordered List
parseLines context (('>' : ' ' : line) : rest) =
  case context of
    Just (CodeBlock code) ->
      parseLines (Just (CodeBlock (code <> [line]))) rest
    _ ->
      maybe id (:) context (parseLines (Just (CodeBlock [line])) rest)
-- Paragraph case
parseLines context (curLine : rest) =
  let --
      line = trim curLine
   in --
      if line == ""
        then
          maybe id (:) context $ parseLines Nothing rest
        else --
          case context of
            Just (Paragraph para) ->
              parseLines (Just (Paragraph (unwords [para, line]))) rest
            _ ->
              maybe id (:) context $ parseLines (Just $ Paragraph line) rest
