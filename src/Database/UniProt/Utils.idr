-----------------------------------------------------------------------------------------[ Module ]
--{1
--                                                                              (\_/)
--                                                                              (o.O)
--                                                                              (> <)
--                                                                             #######
--                                                                           KILLER BUNNY
--                                                                             APPROVED

module Database.UniProt.Utils

import Lightyear
import Lightyear.Char
import Lightyear.Strings
import Lightyear.StringFile

%default partial
%access export

-- %flag C "-O3"
-- %flag C "-g"

--}

------------------------------------------------------------------------------------------[ Utils ]
--{1

fromJust : a -> Maybe a -> a
fromJust def x with (x)
  | Nothing = def
  | Just  v = v

trimBy : (Char -> Bool) -> String -> String
trimBy p = pack . (dropWhile p) . reverse . (dropWhile p) . reverse . unpack

||| Execute a string parser on a file.
parseFile' : Parser a -> String -> Eff (Either String a) [FILE ()]
parseFile' p fname = do
    Result src <- readFile fname
      | FError err => pure (Left $ "File Error! - " ++ show err)
    case parse p src of
      Left  err => pure (Left $ "Parse Error! -  " ++ show err)
      Right res => pure (Right res)

whitespace : Parser ()
whitespace = skip $ many (oneOf " \t")

whitespaces : Parser ()
whitespaces = skip $ some (oneOf " \t")

-- override lightyear 'lexeme' as newlines are significant
tok : Parser a -> Parser a
tok p = p <* whitespace

-- match evidence token... and trash it
optEvidence : Parser ()
optEvidence = whitespace *> opt (char '{' *> many (noneOf "}") *> char '}') *> whitespace

||| Multiline string with end token:
|||   @pre : the uniprot field prefix
|||   @end : the terminating char
mlstr : (pre : String) -> (end : Char) -> Parser String
mlstr pre end = let str = with List (pack <$> some (noneOf (pack [end,'\n'])))
                in (\x,y => concat (intersperse " " (x::y)))
              <$> ((opt (endOfLine *> string pre)) *> whitespace *> str)
              <*> (many (endOfLine *> string pre *> whitespace *> str))

||| Multiline string without end token:
|||   @pre : the uniprot field prefix
mlstr' : (pre : String) -> Parser String
mlstr' pre = let str = pack <$> some (noneOf "\n")
             in (\x,y => concat (intersperse " " (x::y)))
           <$> ((opt (endOfLine *> string pre)) *> whitespace *> str)
           <*> (many (endOfLine *> string pre *> whitespace *> str))

||| Multiline list:
|||   @pre : the UniProt field prefix
|||   @sep : the seperator char
|||   @end : the terminating string
mllst : (pre : String) -> (sep : Char) -> (end : Char) -> Parser (List String)
mllst pre sep end = let str = with List (pack <$> many (noneOf $ pack [sep, end, '\n']))
                    in (\x,y => (x::y))
                  <$> (string pre *> whitespace *> tok str)
                  <*> many ((char sep) *> opt (endOfLine *> string pre) *> whitespace *> str)
                      <* char end

||| Parse a complete block from uniprot file into string
|||   @pre : the uniprot field prefix
chunk : (pre : String) -> Parser String
chunk pre = let str = pack <$> some (noneOf "\n")
            in (\x,y => concat (intersperse " " (x::y)))
          <$> (string pre *> whitespace *> str)
          <*> (many (endOfLine *> string pre *> whitespace *> str))

--}

