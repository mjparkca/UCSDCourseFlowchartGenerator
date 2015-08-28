import Data.Char
import Data.List
import Network.HTTP
import Network.URI (parseURI)
import System.IO
import System.Cmd
import Text.Regex.Posix
import Text.XML.HXT.Core

import Course

output_name = "course_flowchart"

-- reads from stdin subject codes separated by spaces
-- followed by url to course descriptions
main = do
    hSetBuffering stdout NoBuffering

    putStr "Enter subject code(s): "
    maj <- getLine
    let expr = toRegEx maj

    putStr "Enter URL to course descriptions: "
    url <- getLine
    html <- get url

    let doc = readString [withParseHTML yes, withWarnings no] html
    courses <- runX $ doc //> (hasCourseName <+> hasCourseDesc) //> getText
    course_name <- runX $ doc //> hasCourseName //> getText

    let courseList = toCourseList
                         (map (attachSubjectCode maj) (map trim courses))
                         (map (attachSubjectCode maj) (map trim course_name))
                         expr
    writeFile (output_name ++ ".gv") . toGV . processCourseList $ courseList

    layoutPath <- readFile "Layout_Manager.txt"
    rawSystem (trim layoutPath) ["-Tpng", output_name ++ ".gv", "-o",
                                   output_name ++ ".png"]

  where hasCourseName = hasAttrValue "class" (== "course-name")
        hasCourseDesc = hasAttrValue "class" (== "course-descriptions")
        toRegEx str = "(" ++ (intercalate "|" . words $ str) ++ ") [0-9]+[A-Z]*"
        get url = simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody
          where uri = case parseURI url of
                          Nothing -> error $ "Invalid URI: " ++ url
                          Just u -> u
        trim = unwords . words
        attachSubjectCode _ [] = []
        attachSubjectCode maj str@(x:xs)
            | isDigit x = maj ++ (' ' : str)
            | otherwise = str

--------------------------------------------------------------------------------

toCourseList :: [String] -> [String] -> String -> [Course]
toCourseList courses name expr = map (`fromNameDesc` expr) (zip name desc)
    where desc = tail . map concat . chunkDesc courses $ name

chunkDesc :: [String] -> [String] -> [[String]]
chunkDesc [] _ = []
chunkDesc s cs = cons (case break (`elem` cs) s of
                                (l, s') -> (l, case s' of
                                                   [] -> []
                                                   _:s'' -> chunkDesc s'' cs))
    where cons ~(h, t) = h : t

fromNameDesc :: (String, String) -> String  -> Course
fromNameDesc (name, desc) expr = Course (fetchID name expr)
                                        (fetchPrereq desc expr)
                                        (fetchCoreq desc expr)

-- hard-coded rules for parsing course descriptions
expr_prereq = "Prerequisites:"
expr_coreq = "must be taken concurrently|should be taken concurrently"
expr_coreq2 = "concurrent enrollment|corequisite"

fetchID :: String -> String  -> String
fetchID desc expr = desc =~ expr :: String

fetchPrereq :: String -> String  -> [String]
fetchPrereq desc expr
    | desc =~ expr_coreq :: Bool = init prq
    | otherwise = prq
    where (_, _, ed) = desc =~ expr_prereq :: (String, String, String)
          (beg, _, _) = ed =~ expr_coreq2 :: (String, String, String)
          prq = getAllTextMatches $ beg =~ expr :: [String]

fetchCoreq :: String -> String  -> [String]
fetchCoreq desc expr
    | desc =~ expr_coreq :: Bool = [last x]
    | otherwise = crq
    where (_, _, ed) = desc =~ expr_prereq :: (String, String, String)
          (beg, _, ed') = ed =~ expr_coreq2 :: (String, String, String)
          crq = getAllTextMatches $ ed' =~ expr :: [String]
          x = getAllTextMatches $ beg =~ expr :: [String]

--------------------------------------------------------------------------------

processCourseList :: [Course] -> [Course]
processCourseList = pr3 . pr2 . pr1 . pr0

-- filter courses with no names. Why am I getting these?
pr0 :: [Course] -> [Course]
pr0 cs = [c | c <- cs, not . null . courseID $ c]

-- filter graduate courses
pr1 :: [Course] -> [Course]
pr1 = filter (\ x -> courseNum x < 200)

-- remove graduate courses in prereq and coreq
pr2 :: [Course] -> [Course]
pr2 = map pr2'

pr2' :: Course -> Course
pr2' x = Course (courseID x)
                (filter isUndergrad (coursePrereq x))
                (filter isUndergrad (courseCoreq x))
    where isUndergrad y = courseNum (Course y [] []) < 200

-- filter self-reference in prereq and coreq
pr3 :: [Course] -> [Course]
pr3 = map pr3'

pr3' :: Course -> Course
pr3' x = Course (courseID x)
                (filter (\ y -> courseID x /= y) (coursePrereq x))
                (filter (\ y -> courseID x /= y) (courseCoreq x))

-- filter duplicate prereq forming triangle
-- doesn't work because some courses form a loop
pr4 :: [Course] -> [Course]
pr4 cs = pr4' cs cs

pr4' :: [Course] -> [Course] -> [Course]
pr4' [] _ = []
pr4' (c:cs) cs0 = (Course (courseID c) (filter (`notElem` (map courseID (prereqOfCourses (prereqOf c cs0) cs0))) (coursePrereq c)) (courseCoreq c)) : pr4' cs cs0

prereqOf :: Course -> [Course] -> [Course]
prereqOf c cs = [course | course <- cs, (courseID course) `elem` (coursePrereq c)]

prereqOfCourses :: [Course] -> [Course] -> [Course]
prereqOfCourses cs1 cs2 = foldr union [] (map (\ x -> prereqOf x cs2) cs1)

--------------------------------------------------------------------------------

-- convert to .dot format used by Graphviz
toGV :: [Course] -> String
toGV c = unlines ["digraph TechTree {",
                  "\tgraph [rankdir=LR];",
                  concatMap ('\t':) (toGV' c c),
                  "}"]

toGV' :: [Course] -> [Course] -> [String]
toGV' [] _ = []
toGV' (c:cs) courses = (map (\ x -> (map spaceTo_ (courseID c)) ++ " -> " ++ (map spaceTo_ (courseID x)) ++ ";") (filter (c `isPrereqOf`) courses)) ++ (toGV' cs courses)
  where spaceTo_ ' ' = '_'
        spaceTo_ c = c
