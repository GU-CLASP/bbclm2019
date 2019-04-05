module ParseLib where

import Data.Char

lowerCase :: [Char] -> [Char]
lowerCase = map toLower

isTag :: [Char] -> Bool
isTag ('[':_) = True
isTag _ = False


cleanPunkt :: [Char] -> [Char]
cleanPunkt = filter (not . (`elem` ",."))

cleanArrows :: [Char] -> [Char]
cleanArrows line = case reverse line of
  '>':'-':rest -> trim (reverse rest)
  _ -> line

separateDigits :: String -> String
separateDigits (m:n:xs) | isDigit m && isDigit n = m : " &+ " ++ separateDigits (n:xs)
separateDigits (x:xs) = x : separateDigits xs
separateDigits [] = []

isBlank :: Char -> Bool
isBlank = isSpace

halfTrim :: [Char] -> [Char]
halfTrim = dropWhile isBlank . reverse

trim :: [Char] -> [Char]
trim = halfTrim . halfTrim 



cleanExampleNumber :: [Char] -> [Char]
cleanExampleNumber line =
  case break (== '.') line of
    (left,'.':right) | all isDigit left -> trim right
    _ -> line

-- >>> cleanExampleNumber "123.  bla blah"
-- "  bla blah"

-- >>> cleanExampleNumber "123.  45% of blan"
-- "45% of blan"


-- >>> cleanExampleNumber "Take a load of this: 123.  45% of blan"
-- "Take a load of this: 123.  45% of blan"
-- >>> cleanExampleNumber "123.  45% of blan"
-- "45% of blan"

preParse :: String -> [String]
preParse = filter (not . isTag)  . map (separateDigits . lowerCase . cleanPunkt . cleanArrows . cleanExampleNumber . trim) . lines
