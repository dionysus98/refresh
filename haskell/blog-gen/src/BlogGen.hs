{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module BlogGen where

import Control.Monad (when)
import BlogGen.Convert (convert)
import qualified BlogGen.Html as Html
import qualified BlogGen.Markup as Markup
import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      content <- getContents
      putStrLn (process "Empty title" content)
    [input, output] -> do   
      content <- readFile input
      exists <- doesFileExist output
      let writeResult = writeFile output (process input content)
      if exists
        then whenIO confirm writeResult
        else writeResult
    _ ->
      putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"

process :: Html.Title -> String -> String
process title =
  Html.render . convert title . Markup.parse

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ -> do
      putStrLn "Invalid response. use y or n"
      confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  do
    result <- cond
    when result action
