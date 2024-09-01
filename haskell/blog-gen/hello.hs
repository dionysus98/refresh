import Html

main :: IO ()
main = putStrLn $ render myhtml

myhtml :: Html
myhtml =
  html_
    "basic haskell"
    $ append_
      (h1_ "Creative Heading")
    $ append_
      (p_ "para #1")
      (p_ "para #2")