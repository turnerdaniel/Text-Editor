import Data.List -- Used for head, last and append(++)
import System.IO -- Used for reading and writing to file
import System.IO.Unsafe -- Used to convert IO [Char] to normal [Char]

-- Declare a new data type called TextEditor
-- Constructor: 4 char lists as parameters
-- Derives from Show to allow character string output on screen
data TextEditor = Line [Char] [Char] [Char] [Char] deriving (Show)

-- Create a new TextEditor
create :: TextEditor
create = Line [] [] [] []

-- Initialise the TextEditor with text
initialise :: TextEditor -> [Char] -> TextEditor
initialise(Line x s y c) input = Line input [] [] []

-- Delete the TextEditor by replacing with empty list
destroy :: TextEditor -> [a]
destroy(Line x s y c) = []

-- Move the cursor one position to the left 
  -- if text highlighted, move to start of selection
moveLeft :: TextEditor -> TextEditor
moveLeft(Line [] s y c) = Line [] s y c
moveLeft(Line x s y c) = Line (reverse (tail (reverse x))) [] ([head (reverse x)]++s++y) c

-- Move the cursor one position to the right
  -- if text highlighted, move to end of selection
moveRight :: TextEditor -> TextEditor
moveRight(Line x s [] c) = Line x s [] c
moveRight(Line x s y c) = Line (x++s++[head y]) [] (tail y) c

-- Move the cursor left to the nearest word (Tail Recursion)
wordLeft :: TextEditor -> TextEditor
wordLeft(Line [] s y c) = Line [] s y c
wordLeft(Line x s y c)
    | (head (reverse x)) == ' ' = moveLeft(Line x s y c)
    | otherwise = wordLeft(moveLeft(Line x s y c))

-- Move the cursor right to the nearest word (Tail Recursion)
wordRight :: TextEditor -> TextEditor
wordRight(Line x s [] c) = Line x s [] c
wordRight(Line x s y c)
    | (head y) == ' ' = moveRight(Line x s y c)
    | otherwise = wordRight(moveRight(Line x s y c))

-- Move the cursor to the start of the line
home :: TextEditor -> TextEditor
home(Line x s y c) = Line [] [] (x++s++y) c

-- Move the cursor to the end of the line
end :: TextEditor -> TextEditor
end(Line x s y c) = Line (x++s++y) [] [] c

-- Write text at the current cursor position
  -- if text highlighted, replace selection
write :: TextEditor -> [Char] -> TextEditor
write(Line x s y c) input = Line (x++input) [] y c

-- Highlight text one letter to the left of the cursor
selectLeft :: TextEditor -> TextEditor
selectLeft(Line [] s y c) = Line [] s y c
selectLeft(Line x s y c) = Line (reverse (tail (reverse x))) ([head (reverse x)]++s) y c

-- Highlight text one letter to the right of the cursor
selectRight :: TextEditor -> TextEditor
selectRight(Line x s [] c) = Line x s [] c
selectRight(Line x s y c) = Line x (s++[head y]) (tail y) c

-- Highlight the word to the left of the cursor (Tail Recursion)
selectWordLeft :: TextEditor -> TextEditor
selectWordLeft(Line [] s y c) = Line [] s y c
selectWordLeft(Line x s y c)
    | (head (reverse x)) == ' ' = selectLeft(Line x s y c)
    | otherwise = selectWordLeft(selectLeft(Line x s y c))

-- Highlight the word to the right of the cursor (Tail Recursion)
selectWordRight :: TextEditor -> TextEditor
selectWordRight(Line x s [] c) = Line x s [] c
selectWordRight(Line x s y c)
    | (head y) == ' ' = selectRight(Line x s y c)
    | otherwise = selectWordRight(selectRight(Line x s y c))

-- Highlight all text to the left of the cursor
selectAllLeft :: TextEditor -> TextEditor
selectAllLeft(Line x s y c) = Line [] (x++s) y c

-- Highlight all text to the right of the cursor
selectAllRight :: TextEditor -> TextEditor
selectAllRight(Line x s y c) = Line x (s++y) [] c

-- Highlight all text
selectAll :: TextEditor -> TextEditor
selectAll(Line x s y c) = Line [] (x++s++y) [] c

-- Copy the highlighted text to the clipboard
copy :: TextEditor -> TextEditor
copy(Line x s y c) = Line x s y s

-- Delete and copy the highlighted text to the clipboard
cut :: TextEditor -> TextEditor
cut(Line x s y c) = Line x [] y s

-- Insert text from the clipboard at the cursor position 
  -- If text highlighted, replace selection
paste :: TextEditor -> TextEditor
paste(Line x s y c) = Line (x++c) [] y c

-- Delete character preceding the cursor
  -- if text highlighted, delete selection
backspace :: TextEditor -> TextEditor
backspace(Line [] [] y c) = Line [] [] y c
backspace(Line x s y c)
    | s == [] = Line (reverse (tail (reverse x))) [] y c
    | otherwise = Line x [] y c

-- Delete character following the cursor
  -- if text highlighted, delete selection
delete :: TextEditor -> TextEditor
delete(Line x [] [] c) = Line x [] [] c
delete(Line x s y c)
    | s == [] = Line x [] (tail y) c
    | otherwise = Line x [] y c

-- Saves the text to the specified file path (Impure Function - side effects from file operation)
save :: TextEditor -> [Char] -> IO()
save(Line x s y c) file = writeFile file (x++s++y)

-- Opens the file in the text editor (Impure Function -side effects from file operation)
open :: [Char] -> TextEditor
open [] = Line [] [] [] []
open file = Line (unsafePerformIO (readFile file)) [] [] []

-- Concatenate all text into a single word (Tail Recursion)
concatenate :: TextEditor -> TextEditor
concatenate(Line x s y c) = Line (removeSpace (x++s++y)) [] [] c

-- Remove spaces from character string (Tail Recursion & Helper Function)
removeSpace :: [Char] -> [Char]
removeSpace [] = []
removeSpace text
    | (head text) == ' ' = removeSpace(tail text)
    | otherwise = (head text) : removeSpace (tail text)

-- Display help information
help = putStrLn "Commands available for the Text Editor:\n\
                \\n\
                \   create                            Create a new Text Editor.\n\
                \   initialise <TextEditor> <Text>    Initialise the Text Editor with some text.\n\
                \   destroy <TextEditor>              Delete the Text Editor.\n\
                \   moveLeft <TextEditor>             Move the cursor one position to the left.\n\
                \   moveRight <TextEditor>            Move the cursor one position to the right.\n\
                \   wordLeft <TextEditor>             Move the cursor left to the nearest word.\n\
                \   wordRight <TextEditor>            Move the cursor right to the nearest word.\n\
                \   home <TextEditor>                 Move the cursor to the start of the line.\n\
                \   end <TextEditor>                  Move the cursor to the end of the line.\n\
                \   write <TextEditor>                Insert text at the current cursor position.\n\
                \   selectLeft <TextEditor>           Highlight text one letter to the left of the cursor.\n\
                \   selectRight <TextEditor>          Highlight text one letter to the right of the cursor.\n\
                \   selectWordLeft <TextEditor>       Highlight the word to the left of the cursor.\n\
                \   selectWordRight <TextEditor>      Highlight the word to the right of the cursor.\n\
                \   selectAllLeft <TextEditor>        Highlight all text to the left of the cursor.\n\
                \   selectAllRight <TextEditor>       Highlight all text to the right of the cursor.\n\
                \   selectAll <TextEditor>            Highlight all text.\n\
                \   copy <TextEditor>                 Copy the highlighted text to the clipboard.\n\
                \   cut <TextEditor>                  Remove and copy the highlighted text to the clipboard.\n\
                \   paste <TextEditor>                Insert text from the clipboard at the cursor position.\n\
                \   backspace <TextEditor>            Remove character preceding the cursor.\n\
                \   delete <TextEditor>               Remove character following the cursor.\n\
                \   save <TextEditor> <File Path>     Save the text to the specified file path.\n\
                \   open <File Path>                  Open the file in the text editor.\n\
                \   concatenate <TextEditor>          Merge all text into a single word.\n\
                \\n\
                \With the parameters:\n\
                \\n\
                \   <TextEditor>                      A variable holding a TextEditor instance assigned by the \'create\' command.\n\
                \   <Text>                            A sequence of alphanumeric characters.\n\
                \   <File Path>                       A path to a text file (.txt).\n\
                \\n\
                \Example Usage:\n\
                \\n\
                \   a = create\n\
                \   b = initialise a \"example text\"\n\
                \   b\n\
                \   > Line \"example text\" \"\" \"\" \"\"\n\
                \   c = selectWordLeft b\n\
                \   c\n\
                \   > Line \"example\" \" text\" \"\" \"\"\n\
                \   d = write c \" usage.\"\n\
                \   > Line \"example usage.\" \"\" \"\" \"\"\n\
                \" 