module Markup
  ( Document,
    Structure (..),
  )
where

import Numeric.Natural

type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show)

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines curPara ipTxts =
  let para = Paragraph $ unlines $ reverse curPara
   in case ipTxts of
        [] -> [para]
        curLine : rest ->
          if trim curLine == ""
            then
              para : parseLines [] rest
            else
              parseLines (curLine : curPara) rest

trim :: String -> String
trim = unwords . words