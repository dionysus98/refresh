module BlogGen.Convert (convert) where

import qualified BlogGen.Html as Html
import qualified BlogGen.Markup as Markup

convertStruct :: Markup.Structure -> Html.Structure
convertStruct struct =
  case struct of
    Markup.Heading lvl txt ->
      Html.h_ lvl txt
    Markup.Paragraph p ->
      Html.p_ p
    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list
    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list
    Markup.CodeBlock list ->
      Html.code_ (unlines list)

convert :: (Foldable t) => Html.Title -> t Markup.Structure -> Html.Html
convert title = Html.html_ title . foldMap convertStruct
