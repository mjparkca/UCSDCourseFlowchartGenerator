module Course (
    Course(..),
    isPrereqOf,
    isCoreqOf,
    courseNum
) where

import Data.Char
import Data.List

type CourseID = String
type CoursePrereq = [CourseID]
type CourseCoreq = [CourseID]

data Course = Course {
    courseID     :: CourseID,
    coursePrereq :: CoursePrereq,
    courseCoreq  :: CourseCoreq
} deriving (Eq)

isPrereqOf :: Course -> Course -> Bool
isPrereqOf course1 course2 = (courseID course1) `elem` (coursePrereq course2)

isCoreqOf :: Course -> Course -> Bool
isCoreqOf course1 course2 = (courseID course1) `elem` (courseCoreq course2)

courseNum :: Course -> Int
courseNum c = read . filter isDigit . courseID $ c :: Int

instance Show Course where
    show (Course code prq crq) = "Course ID: " ++ code ++ ". Prerequisites: " ++ prereqFormat ++ coreqFormat
     where prereqFormat
             | null prq = "none."
             | otherwise = intercalate ", " prq ++ "."
           coreqFormat
             | null crq = ""
             | otherwise = " Corequisites: " ++ (intercalate ", " crq) ++ "."