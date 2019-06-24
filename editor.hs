--allows use of head, last, append(++) and makes lists easier to understand
import Data.List
import System.IO
import System.IO.Unsafe

--declare new data type named TextEditor
--has a constructor naed Line that takes 4 character lists as parameters
--deriving (Show) allows output of custom data type as a character string
data TextEditor = Line [Char] [Char] [Char] [Char] deriving (Show)

create :: TextEditor
create = Line [] [] [] []

init :: TextEditor -> [Char] -> TextEditor
init(Line x s y c) input = Line input [] [] []

--can't delete a var so just changed into empty list
destroy :: TextEditor -> [a]
destroy(Line x s y c) = []

moveLeft :: TextEditor -> TextEditor
moveLeft(Line x s y c) = Line (reverse (tail (reverse x))) [] ([head (reverse x)]++s++y) c

moveRight :: TextEditor -> TextEditor
moveRight(Line x s y c) = Line (x++s++[head y]) [] (tail y) c

wordLeft :: TextEditor -> TextEditor
wordLeft(Line [] s y c) = Line [] s y c
wordLeft(Line x s y c)
    | (head (reverse x)) == ' ' = moveLeft(Line x s y c)
    | otherwise = wordLeft(moveLeft(Line x s y c))

wordRight :: TextEditor -> TextEditor
wordRight(Line x s [] c) = Line x s [] c
wordRight(Line x s y c)
    | (head y) == ' ' = moveRight(Line x s y c)
    | otherwise = wordRight(moveRight(Line x s y c))

home :: TextEditor -> TextEditor
home(Line x s y c) = Line [] [] (x++s++y) c

end :: TextEditor -> TextEditor
end(Line x s y c) = Line (x++s++c) [] [] c

write :: TextEditor -> [Char] -> TextEditor
write(Line x s y c) text = Line (x++text) [] y c

selectLeft :: TextEditor -> TextEditor
selectLeft(Line x s y c) = Line (reverse (tail (reverse x))) ([head (reverse x)]++s) y c

selectRight :: TextEditor -> TextEditor
selectRight(Line x s y c) = Line x (s++[head y]) (tail y) c

selectWordLeft :: TextEditor -> TextEditor
selectWordLeft(Line [] s y c) = Line [] s y c
selectWordLeft(Line x s y c)
    | (head (reverse x)) == ' ' = selectLeft(Line x s y c)
    | otherwise = selectWordLeft(selectLeft(Line x s y c))

selectWordRight :: TextEditor -> TextEditor
selectWordRight(Line x s [] c) = Line x s [] c
selectWordRight(Line x s y c)
    | (head y) == ' ' = selectRight(Line x s y c)
    | otherwise = selectWordRight(selectRight(Line x s y c))

selectAllLeft :: TextEditor -> TextEditor
selectAllLeft(Line x s y c) = Line [] (x++s) y c

selectAllRight :: TextEditor -> TextEditor
selectAllRight(Line x s y c) = Line x (s++y) [] c

copy :: TextEditor -> TextEditor
copy(Line x s y c) = Line x s y s

cut :: TextEditor -> TextEditor
cut(Line x s y c) = Line x [] y s

paste :: TextEditor -> TextEditor
paste(Line x s y c) = Line (x++c) [] y c

backspace :: TextEditor -> TextEditor
backspace(Line x s y c)
    | s == [] = Line (reverse (tail (reverse x))) [] y c
    | otherwise = Line x [] y c

delete :: TextEditor -> TextEditor
delete(Line x s y c)
    | s == [] = Line x [] (tail y) c
    | otherwise = Line x [] y c

save :: TextEditor -> [Char] -> IO()
save(Line x s y c) file = writeFile file (x++s++y)

--unsafePeformIO converts IO string to string
open :: [Char] -> TextEditor
open file = Line (unsafePerformIO(readFile file)) [] [] []

--filter accepts lambda expression which evaluates to false for spaces so that they are filtered out
concatenate :: TextEditor -> TextEditor
concatenate(Line x s y c) = Line (filter (\z -> z /= ' ') (x++s++y)) [] [] c

--------------------------------------------------
-- ******************************************** --
--------------------------------------------------
filter2 :: (a -> Bool) -> [a] -> [a]
filter2 p []    = []
filter2 p x
  | p (head x) == True = (head x) : filter2 p (tail x)
  | otherwise     = filter2 p (tail x)