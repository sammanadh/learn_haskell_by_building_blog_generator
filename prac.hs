-- EDSL implementation

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

newtype Structure = Structure String
newtype Html = Html String

p_ :: String -> Structure
h1_ :: String -> Structure
title_ :: String -> Structure

p_ = Structure . el "p"
h1_ = Structure . el "h1"
title_ = Structure . el "title"

html_ :: Html
html_ (Structure str) = Html str 

append_ :: Structure -> Structure -> Structure
append_ (Structure str1) (Structure str2) = Structure (str1 <> str2)

getStructureString (Structure str1) = str1

myhtml :: Html
myhtml = html_ 
  (append_ 
    ( el "head" (getStructureString ( title_ "my title")) )
    ( el "body" (getStructureString ( h1_ "this is the content")))
  )

render :: Html -> String
render (Html str) = str

main :: IO()
main = putStrLn (render myhtml)