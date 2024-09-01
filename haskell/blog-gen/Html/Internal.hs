module Html.Internal where

-- Types

newtype Html = Html String

newtype Structure = Structure String

type Title = String

-- EDSL

html_ :: Title -> Structure -> Html
html_ title (Structure content) =
  Html $
    el "html" $
      el "head" (el "title" $ escape title)
        <> el "body" content

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

li_ :: Structure -> Structure
li_ (Structure s) = Structure $ el "li" s

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructStr)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructStr)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

-- render

render :: Html -> String
render (Html html) = html

-- utils

getStructStr :: Structure -> String
getStructStr (Structure s) = s

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> escape content <> "</" <> tag <> ">"

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar