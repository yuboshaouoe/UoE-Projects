module Tutorial4 where

import Data.List (nub)
import Data.List
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://homepages.inf.ed.ac.uk/wadler/testpage.html"

testHTML :: String
testHTML =    "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\n"
           ++ "<html>\n"
           ++ "<head>\n"
           ++ "<title>FP: Tutorial 4</title>\n"
           ++ "</head>\n"
           ++ "<body>\n"
           ++ "<h1>A Boring test page</h1>\n"
           ++ "<h2>for tutorial 4</h2>\n"
           ++ "<a href=\"https://course.inf.ed.ac.uk/inf1a\">Inf1A Learn</a><br>\n"
           ++ "<b>Lecturer:</b> <a href=\"mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br>\n"
           ++ "<b>TA:</b> <a href=\"mailto:cchirita@exseed.ed.ac.uk\">Claudia-Elena Chirita</a>\n"
           ++ "</body>\n"
           ++ "</html>\n\n"

testLinks :: [Link]
testLinks =  ["https://course.inf.ed.ac.uk/inf1a\">Inf1A Learn",
              "mailto:wadler@inf.ed.ac.uk\">Philip Wadler",
              "mailto:cchirita@exseed.ed.ac.uk\">Claudia-Elena Chirita"]

testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Philip Wadler","wadler@inf.ed.ac.uk")
               , ("Claudia-Elena Chirita","cchirita@exseed.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.

sameString :: String -> String -> Bool
sameString str1 str2 = map toLower str1 == map toLower str2


-- 2.

prefix :: String -> String -> Bool
prefix str1 str2 = isPrefixOf (map toLower str1) (map toLower str2)

prop_prefix_pos :: String -> Int -> Property
prop_prefix_pos str n = n >= 0 ==> prefix substr (map toUpper str)
  where
    substr = take n str

prop_prefix_neg :: String -> Int -> Property
prop_prefix_neg str n = 0 <= n && n < length str ==> (not $ prefix str substr)
  where
    substr = take n str

--If n given in prop_prefix_pos is larger than length(str), it would return an error.
        
-- 3.

contains :: String -> String -> Bool
contains str1 str2 = not (null [k | k <- [0..length str1], prefix str2 (drop k str1)])

prop_contains :: String -> Int -> Int -> Bool
prop_contains str m n = contains (map toUpper str) substr && contains (map toLower str) substr
  where
    substr = drop m (take n str)


-- 4.

takeUntil :: String -> String -> String
takeUntil str1 str2
  | not(null [k | k <- [0..length str2], prefix str1 (drop k str2)]) = take (head([k | k <- [0..length str2], prefix str1 (drop k str2)])) str2
  | otherwise = str2

dropUntil :: String -> String -> String
dropUntil str1 str2
  | null [k | k <- [0..length str2], prefix str1 (drop k str2)] == False = drop (length(str1) + (head([k | k <- [0..length str2], prefix str1 (drop k str2)]))) str2
  | otherwise = ""

-- 5.

split :: String -> String -> [String]
split "" str2 = error "split_indicating_str must not be empty"
split str1 "" = [""]
split str1 str2
  | contains str2 str1 == False = [str2]
  | otherwise = takeUntil str1 str2 : (split str1 (dropUntil str1 str2))

reconstruct :: String -> [String] -> String
reconstruct str1 [strs]
  | str1 == "" = error "split_indicating_str must not be empty"
  | null [strs] = []
  | length([strs]) == 1 = head([strs])
  | otherwise = concat([str ++ str1 | str <- init([strs]), str /= ""])

prop_split :: String -> String -> Property
prop_split sep str = not (null sep) ==> reconstruct sep (split sep str) `sameString` str

-- 6.

linksFromHTML :: HTML -> [Link]
linksFromHTML html = [takeUntil "</a>" (dropUntil "<a href=\"" link) | link <- (split "\n" html), contains link "https:" || (contains link "@" && contains link "mailto:")]

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.

takeEmails :: [Link] -> [Link]
takeEmails xs = [link | link <- xs, contains link "@" && contains link "mailto:"]


-- 8.

link2pair :: Link -> (Name, Email)
link2pair link = ((takeUntil "\"" (dropUntil "\">" link)), (takeUntil "\">" (dropUntil "mailto:" link)))

-- 9.

emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html = [link2pair email | email <- takeEmails(linksFromHTML html)]

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- Optional Material

-- 10.

findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name xs = [x | x <- xs, contains (fst(x)) name]


-- 11.

emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html name = findEmail name (emailsFromHTML html)


ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
