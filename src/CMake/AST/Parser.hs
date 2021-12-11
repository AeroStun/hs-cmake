-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2021 AeroStun
-- License     :  Apache-2.0
--
-- Maintainer  :  AeroStun <24841307+AeroStun@users.noreply.github.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- CMake grammar productions parsers
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module CMake.AST.Parser (
  file,
  statement,
  statements,
  commandInvocation,
  identifier,
  arguments,
  argument,
  escapeSequence,
  variableReference
  ) where
import           CMake.AST.Defs
import           Control.Applicative   (many, optional, some, (<|>))
import           Control.Monad         (void)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isSpace, toLower, toUpper)
import           Data.Functor          (($>))
import           Data.Maybe            (catMaybes, maybeToList)
import           Text.Parser.LookAhead (lookAhead)
import           Text.Trifecta         (CharParsing, Parser, alphaNum, anyChar,
                                        between, char, eof, manyTill, newline,
                                        noneOf, notChar, notFollowedBy, oneOf,
                                        position, satisfy, satisfyRange,
                                        skipOptional, space, spaces, string,
                                        surroundedBy, try, (<?>))
import           Text.Trifecta.Delta   (Delta (..))

(~~) :: CharParsing m => Char -> Char -> m Char
(~~) = satisfyRange

fromDelta :: Delta -> SourceLocation
fromDelta (Columns c b)         = SourceLocation "<unknown>" 0 c b
fromDelta (Tab _ _ b)           = SourceLocation "<unknown>" 0 0 b
fromDelta (Lines l c b _)       = SourceLocation "<unknown>" l c b
fromDelta (Directed fn l c b _) = SourceLocation fn l c b

-- | Match the lowercase or uppercase form of 'c'
caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

-- | Match the string 's', accepting either lowercase or uppercase form of each character
caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> "\"" ++ s ++ "\""

spaceNonLF :: Parser ()
spaceNonLF = void $ satisfy $ \c -> isSpace c && c /= '\n'

lineEnding :: Parser ()
lineEnding = skipOptional lineComment >> void newline

lookCommandAhead :: String -> Parser ()
lookCommandAhead name = void $ lookAhead (many spaceNonLF *> caseInsensitiveString name)


file :: Parser File
file = statements <* endStale <* eof -- FIXME skip opt leading BOM
  where
    endStale :: Parser ()
    endStale = many (try bracketComment <|> spaceNonLF) *> skipOptional lineComment *> eof

staleElement :: Parser ()
staleElement = many (try bracketComment <|> spaceNonLF) *> lineEnding

statements :: Parser [Statement]
statements = catMaybes <$> many fileElement
  where
    fileElement :: Parser (Maybe Statement)
    fileElement = try staleElement *> many spaceNonLF $> Nothing
              <|> Just <$> (try (spaces <* void (notFollowedBy $ char '#')) *> statement)



statement :: Parser Statement
statement =  conditionalStatement
         <|> MacroStatement <$> scopeBlock "macro"
         <|> FunctionStatement <$> scopeBlock "function"
         <|> ForeachStatement <$> scopeBlock "foreach"
         <|> condLoopStatement
         <|> invocationStatement <?> "invocation"

invocationStatement :: Parser Statement
invocationStatement = notFollowedBy blockTerminators
                   *> (InvocationStatement <$> commandInvocation)
  where
    blockTerminators :: Parser ()
    blockTerminators = foldl1 (<|>) $ lookCommandAhead <$> ["elseif", "else", "endif", "endmacro", "endfunction", "endforeach", "endwhile"]

conditionalStatement :: Parser Statement
conditionalStatement = ConditionalStatement <$> (ConditionalChain <$> if_ <*> torso <*> foot)
  where
    torso :: Parser [ConditionalBlock]
    torso = many elif_ >>= (\c -> (c++) <$> waist)
    waist :: Parser [ConditionalBlock]
    waist = maybeToList <$> optional else_
    foot :: Parser CommandInvocation
    foot = lookCommandAhead "endif" *> commandInvocation
    if_,elif_,else_ :: Parser ConditionalBlock
    if_ = conditionalBlock "if" ordering
    elif_ = conditionalBlock "elseif" ordering
    else_ = conditionalBlock "else" ordering
    ordering  :: [String]
    ordering = ["if", "elseif", "else", "endif"]

condLoopStatement :: Parser Statement
condLoopStatement = WhileStatement <$> bust <*> foot
  where
    bust = conditionalBlock "while" ["while", "endwhile"]
    foot = lookCommandAhead "endwhile" *> commandInvocation

conditionalBlock :: String -> [String] -> Parser ConditionalBlock
conditionalBlock entry ordering = lookCommandAhead entry *> (ConditionalBlock <$> commandInvocation <*> statements) <* delimiter
  where
    delimiter :: Parser ()
    delimiter = foldl1 (<|>) $ lookCommandAhead <$> tail (dropWhile (/=entry) ordering)



scopeBlock :: String -> Parser ScopeBlock
scopeBlock entry =  lookAhead (caseInsensitiveString entry) *> do
    intro <- commandInvocation
    body <- statements
    outro <- lookCommandAhead ("end" ++ entry) *> commandInvocation
    -- FIXME error on mismatching args for intro and outro
    return $ ScopeBlock intro body outro

commandInvocation :: Parser CommandInvocation
commandInvocation = do
    pos <- position
    funcId <- identifier
    void $ many spaceNonLF
    args <- between (char '(') (char ')') arguments
    void $ many space
    return (CommandInvocation funcId args (fromDelta pos)) <?> "command"


identifier :: Parser Identifier
identifier = Identifier <$> some ('A' ~~ 'Z' <|> 'a' ~~ 'z' <|> char '_' <|> '0' ~~ '9') <?> "identifier"

arguments :: Parser Arguments
arguments = do
    arg <- maybeToList <$> optional argument
    rest <- concat <$> many separatedArguments
    return (arg <> rest) <?> "arguments"

separatedArguments :: Parser Arguments
separatedArguments = maybeToList <$> try (some separation *> optional argument)
                 <|> parenthesize <$> (many separation *> between (char '(') (char ')') arguments)
  where
    parenthesize :: Arguments -> Arguments
    parenthesize = ((,BracketArgument) "(" :) . (++ [(")", BracketArgument)])

separation :: Parser ()
separation = void space <|> lineEnding


argument :: Parser Argument
argument = bracketArgument <|> quotedArgument <|> unquotedArgument

bracketArgument :: Parser Argument
bracketArgument = do
    opening <- char '[' *> many (char '=') <* char '['
    let closing = string $ "]" <> opening <> "]"
    (,BracketArgument) <$> anyChar `manyTill` try closing


quotedArgument :: Parser Argument
quotedArgument = (,QuotedArgument) . catMaybes <$> many quotedElement `surroundedBy` char '"'
  where
    quotedElement :: Parser (Maybe Char)
    quotedElement = Just <$> (noneOf "\"" <|> try escapeSequence)
                <|> (char '\\' *> newline $> Nothing)

unquotedArgument :: Parser Argument
unquotedArgument = (,UnquotedArgument) <$> some (satisfy unElem <|> escapeSequence)
  where
    unElem :: Char -> Bool
    unElem c = c `BS.notElem` "()#\"" && not (isSpace c)


escapeSequence :: Parser Char
escapeSequence = char '\\' >> (oneOf "()#\" \\$@^;" <|> escapeEncoded)

escapeEncoded :: Parser Char
escapeEncoded = char 't' $> '\t'
            <|> char 'r' $> '\r'
            <|> char 'n' $> '\n'


bracketComment :: Parser ()
bracketComment = char '#' >> void bracketArgument  <?> "bracket comment"

lineComment :: Parser ()
lineComment = char '#' >> void (many $ notChar '\n') <?> "line comment"



variableReference :: Parser VariableReference
variableReference = char '$' *> (VariableReference <$> braces (many variableReferenceSection))
  where
    braces :: Parser m -> Parser m
    braces = between (char '{') (char '}')

variableReferenceSection :: Parser VariableReferenceSection
variableReferenceSection = IdentifierSection <$> try (some (alphaNum <|> satisfy (`BS.elem` "/_.+-") <|> escapeSequence))
                       <|> NestedReference <$> try variableReference
                       <|> IdentifierSection <$> some (alphaNum <|> satisfy (`BS.elem` "/_.+-$") <|> escapeSequence)
