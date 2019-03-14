{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{- | The Parser module contains a monadic, depth tracking XML parser that
provides a nicer interface around the @xml-conduit@ package as well as
parsing functions for reading xsd-specific types from an Element's
contents.

-}
module Parser
    ( -- * Running Parsers
      Parser
    , runParser
    , ParserContext(..)
    , runParserContext
    , ParsingError(..)
    , parseError
    , liftEither
      -- * Querying Parsing Context
    , getContext
    , getElement
    , getHierarchy
      -- * Conditional Parsing
    , tryParser
    , oneOf
    , optional
      -- * Selecting Elements
    , matchName
    , descend
    , find
    , findAll
    , at
      -- * Reading Content
    , parseRead
    , parseBool
    , parseInteger
    , parseDecimal
    , parseDate
    , parseDatetime
    , parseContent
    , parseContentWith
      -- * Utilities
    , withNamespace
    )
where

import           Control.Applicative            ( (<|>) )
import           Control.Monad.Catch.Pure       ( Exception )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                , ask
                                                , asks
                                                , local
                                                , lift
                                                )
import           Control.Monad.Except           ( ExceptT(..)
                                                , runExceptT
                                                , throwError
                                                )
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Functor.Identity          ( Identity(..)
                                                , runIdentity
                                                )
import           Data.Maybe                     ( mapMaybe
                                                , listToMaybe
                                                )
import           Data.Text                      ( Text
                                                , intercalate
                                                , unpack
                                                , pack
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time                      ( UTCTime
                                                , Day
                                                )
import           Data.Time.Format               ( ParseTime
                                                , parseTimeM
                                                , defaultTimeLocale
                                                )
import           Numeric                        ( readFloat
                                                , readSigned
                                                )
import           Text.Read                      ( readEither )
import           Text.XML                       ( Node(..)
                                                , Element(..)
                                                , Name(..)
                                                , parseLBS
                                                , def
                                                , documentRoot
                                                )

-- RUNNING

-- | The Parser Monad is used for parsing XML elements into datatypes.
--
-- It tracks the current Element in a cursor-like fashion, where you can
-- descend into child Elements to parse nested XML, as well as the Names of
-- elements that have been descended into already.
--
-- Running a Parser returns the result or a ParsingError.
newtype Parser a
    = Parser { fromParser :: ReaderT ParserContext (ExceptT ParsingError Identity) a }
    deriving (Functor, Applicative, Monad)

-- | The context a Parser holds during parsing.
data ParserContext
    = ParserContext
        { cursor :: Element
        -- ^ The currently selected Element
        , hierarchy :: [Name]
        -- ^ The hierarchy of Element Name's we've descended into - in
        -- reverse order.
        }
    deriving (Show)

-- | An error that has occured during XML parsing.
data ParsingError
    = ParsingError
        { errorMessage :: Text
        , parentNodes :: [Name]
        }

instance Exception ParsingError

-- | Show the parent node hierarchy as names separated by @ > @ signs
-- followed by the error message enclosed in braces.
instance Show ParsingError where
    show err =
        unpack $
            intercalate " > " (map nameLocalName $ parentNodes err)
            <> " { "
            <> errorMessage err
            <> " }"

-- | Run a Parser on an Element, returning an error or the parsed value.
runParser :: Element -> Parser a -> Either ParsingError a
runParser el = runParserContext $ ParserContext el [elementName el]

-- | Run a Parser with the given ParserContext.
runParserContext :: ParserContext -> Parser a -> Either ParsingError a
runParserContext ctx parser =
    runIdentity $ runExceptT $ runReaderT (fromParser parser) ctx

-- | Signal a parsing error with the given message.
parseError :: Text -> Parser a
parseError msg = do
    parents <- getHierarchy
    Parser $ lift $ ExceptT $ Identity $ Left $ ParsingError
        { errorMessage = msg
        , parentNodes  = parents
        }

-- | Lift an Either into a Parser by throwing a ParsingError if necessary.
liftEither :: Either String a -> Parser a
liftEither = either (parseError . pack) return



-- QUERYING

-- | Get the Element at the cursor.
getElement :: Parser Element
getElement = Parser $ asks cursor

-- | Get the current hierarchy of Element Names, ordered from the top-most
-- to the current Element.
getHierarchy :: Parser [Name]
getHierarchy = Parser $ asks $ reverse . hierarchy

-- | Get the entire Parsing Context.
getContext :: Parser ParserContext
getContext = Parser ask



-- CONDITIONALS

-- | Attempt parsing the current Element, returning the ParsingError
-- instead of aborting.
tryParser :: Parser a -> Parser (Either ParsingError a)
tryParser parser = do
    ctx <- getContext
    return $ runParserContext ctx parser

-- | Attempt parsing with multiple Parsers, returning the first valid
-- result. If all parsers fail, rethrow the 'ParsingError' with the longest
-- 'parentNodes' list.
oneOf :: [Parser a] -> Parser a
oneOf ps = untilSucceeds (ps, []) >>= \case
    Right val  -> return val
    Left  errs -> case findLongest errs of
        Nothing  -> parseError "No parsers to attempt"
        Just err -> Parser (throwError err)
  where
    -- Attempt parsers until one succeeds or all have been tried,
    -- collecting all the errors in a list.
    untilSucceeds
        :: ([Parser a], [ParsingError]) -> Parser (Either [ParsingError] a)
    untilSucceeds (ps_, es) = case ps_ of
        []       -> return $ Left $ reverse es
        p : rest -> tryParser p >>= \case
            Right val -> return $ Right val
            Left  err -> untilSucceeds (rest, err : es)
    -- Return the error with the longest 'parentNodes' list.
    findLongest :: [ParsingError] -> Maybe ParsingError
    findLongest es = case es of
        []  -> Nothing
        [e] -> Just e
        this : next : rest ->
            if length (parentNodes this) > length (parentNodes next)
                then findLongest $ this : rest
                else findLongest $ next : rest

-- | Try a parser, returning a Maybe to handle failure instead of throwing
-- a ParsingError.
optional :: Parser a -> Parser (Maybe a)
optional parser = do
    ctx <- getContext
    case runParserContext ctx parser of
        Left  _   -> return Nothing
        Right val -> return $ Just val



-- SELECTING


-- | Ensure the 'Element' 'Name' matches the given name, then apply the
-- parser.
--
-- Throws an error if the name does not match.
matchName :: Name -> Parser a -> Parser a
matchName name parser = do
    el <- getElement
    if elementName el == name
        then parser
        else parseError $ "Expected Element with Name: " <> nameLocalName name

-- | Descend into an Element & parse it. The 'Name' of the element we
-- descend into is prepended to the 'hierarchy' of the given Parser's
-- 'ParserContext'.
descend :: Parser a -> Element -> Parser a
descend parser el = Parser $ local
    (\ctx -> ctx { cursor = el, hierarchy = elementName el : hierarchy ctx })
    (fromParser parser)

-- | Run the Parser on a nested element. The first matching name is
-- descended into at each level. No backtracking or retrying is done if the
-- 'Name' path is unparseable.
--
-- Throws an error if passed an empty list of names or the 'Name' path is
-- unreachable.
at :: [Name] -> Parser a -> Parser a
at n p = case n of
    []            -> parseError "No name to descend into"
    [    name]    -> find name p
    name :   rest -> find name (at rest p)

-- | Descend into the first matching 'Element' and run the parser.
--
-- Throws an error if no matching children are found.
find :: Name -> Parser a -> Parser a
find name parser = getElementsByName name >>= \case
    []     -> parseError $ "Could Not Find Element: " <> nameLocalName name
    el : _ -> descend parser el

-- | Parse all children with matching names using the given 'Parser'.
--
-- Throws an error if the parser fails on any of the children.
findAll :: Name -> Parser a -> Parser [a]
findAll name parser = getElementsByName name >>= mapM (descend parser)


-- | Return all children of the current element with the matching 'Name'.
getElementsByName :: Name -> Parser [Element]
getElementsByName name = mapMaybe getByName . elementNodes <$> getElement
  where
    getByName = \case
        NodeElement e -> if elementName e == name then Just e else Nothing
        _             -> Nothing


-- READING

-- | Expect a single 'NodeContent' child & parse it from a Read instance.
parseRead :: Read a => Parser a
parseRead = parseContent >>= liftEither . readEither . unpack

-- | Parse an @xsd:bool@ from the Element contents.
--
-- Valid values are @1@, @0@, @true@, and @false@.
parseBool :: Parser Bool
parseBool = parseContent >>= \case
    "true"  -> return True
    "1"     -> return True
    "false" -> return False
    "0"     -> return False
    s       -> parseError $ "Expected an xsd:bool, got: " <> s

-- | Pare an @xsd:integer@ from the Element contents.
parseInteger :: Parser Integer
parseInteger = unpack <$> parseContent >>= liftEither . readEither . \case
    '+' : rest -> rest
    str        -> str

-- | Parse an @xsd:decimal@ from the Element contents.
parseDecimal :: Parser Rational
parseDecimal = do
    str <- dropWhile (== '+') . unpack <$> parseContent
    case listToMaybe (readSigned readFloat str) of
        Nothing -> parseError $ "Expected an xsd:decimal, got: " <> pack str
        Just (val, _) -> return val

-- | Parse an @xsd:date@ from the Element contents.
--
-- Supported formats are @YYYY-MM-DD±HH:MM@ for zoned dates, and
-- @YYYY-MM-DDZ@ or @YYYY-MM-DD@ for UTC dates.
parseDate :: Parser Day
parseDate = do
    text <- unpack <$> parseContent
    let parsed =
            readDate "%F%z" text <|> readDate "%FZ" text <|> readDate "%F" text
    case parsed of
        Just t  -> return t
        Nothing -> parseError $ "Expected an xsd:date type, got: " <> pack text

-- | Parse an @xsd:dateTime@ from the Element's contents.
--
-- Supported formats are @YYYY-MM-DDTHH:MM:SS±HH:MM@ for zoned date times,
-- and @YYYY-MM-DDTHH:MM:SSZ@ or @YYYY-MM-DDTHH:MM:SS@ for UTC date times.
parseDatetime :: Parser UTCTime
parseDatetime = do
    text <- unpack <$> parseContent
    let parsed =
            readDate "%FT%T%z" text
                <|> readDate "%FT%TZ" text
                <|> readDate "%FT%T"  text
    case parsed of
        Just t -> return t
        Nothing ->
            parseError $ "Expected an xsd:datetime type, got: " <> pack text

readDate :: ParseTime a => String -> String -> Maybe a
readDate = parseTimeM True defaultTimeLocale

-- | Parse the Element's content, throwing an error if any other child
-- Nodes are present.
parseContent :: Parser Text
parseContent = do
    el <- getElement
    case elementNodes el of
        [NodeContent t] -> return t
        _ ->
            parseError
                $  "Expected NodeContent Singleton in Element: "
                <> nameLocalName (elementName el)

-- | Get the Element's content, parse it into an XML 'Text.XML.Document',
-- then run the given Parser on the 'documentRoot'.
--
-- Useful when you have a complete XML document nested inside an XML
-- Element.
--
-- Throws an error when the text content is not a valid XML document.
parseContentWith :: Parser a -> Parser a
parseContentWith p = do
    text <- parseContent
    case parseLBS def $ LBS.fromStrict $ encodeUtf8 text of
        Left err ->
            parseError
                $  "Expected NodeContent Containing Valid XML Document, got: "
                <> pack (show err)
        Right xmlDoc -> descend p $ documentRoot xmlDoc


-- UTILITIES


-- | Convenience function for building a @Text -> Name@ function with
-- a predefined namespace.
--
-- > soapName :: Text -> Name
-- > soapName = withNamespace "http://schemas.xmlsoap.org/soap/envelope/"
withNamespace :: Text -> (Text -> Name)
withNamespace namespace name = Name
    { nameLocalName = name
    , nameNamespace = Just namespace
    , namePrefix    = Nothing
    }
