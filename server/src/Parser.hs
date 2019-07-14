{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{- | The Parser module contains a monadic, depth tracking XML parser that
provides a nicer interface around the @xml-conduit@ package as well as
parsing functions for reading xsd-specific types from an Element's
contents.

-}
module Parser
    ( -- * Typeclass
      FromXML(..)
    , parseDocumentRoot
      -- * Running Parsers
    , Parser
    , runParser
    , ParserContext(..)
    , runParserContext
      -- * Parsing Errors
    , ParsingError(..)
    , ParsingErrorType(..)
    , prettyParsingError
    , parseError
    , throwParsingError
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
import           Data.Ratio                     ( Ratio )
import           Data.Text                      ( Text
                                                , intercalate
                                                , unpack
                                                , pack
                                                )
import qualified Data.Text                     as T
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
import           Text.XML                       ( Document(..)
                                                , Node(..)
                                                , Element(..)
                                                , Name(..)
                                                , parseLBS
                                                , def
                                                , documentRoot
                                                )

-- CLASS

-- | Parse XML into a type using the `xml-conduits` package.
class FromXML a where
    fromXML :: Parser a

-- | Parse the root elment of an XML Document.
parseDocumentRoot :: FromXML a => Document -> Either ParsingError a
parseDocumentRoot doc = runParser (documentRoot doc) fromXML


-- DEFAULT INSTANCES

-- | Parse an @xsd:bool@ from the Element contents.
--
-- Valid values are @1@, @0@, @true@, and @false@.
instance FromXML Bool where
    fromXML = parseContent >>= \case
        "true"  -> return True
        "1"     -> return True
        "false" -> return False
        "0"     -> return False
        s       -> throwParsingError $ ContentParsingError "xsd:bool" s

-- | Parse an @xsd:integer@ from the Element contents. Leading @+@ signs
-- are allowed.
instance FromXML Integer where
    fromXML = unpack <$> parseContent >>= liftEither . readEither . \case
        '+' : rest -> rest
        str        -> str

-- | Parse an @xsd:decimal@ from the Element contents. Leading @+@ signs
-- are allowed.
instance FromXML (Ratio Integer) where
    fromXML = do
        text <- parseContent
        let str = T.dropWhile (== '+') text
        case listToMaybe (readSigned readFloat $ unpack str) of
            Nothing ->
                throwParsingError $ ContentParsingError "xsd:decimal" str
            Just (val, _) -> return val


-- | Parse an @xsd:date@ from the Element contents.
--
-- Supported formats are @YYYY-MM-DD±HH:MM@ for zoned dates, and
-- @YYYY-MM-DDZ@ or @YYYY-MM-DD@ for UTC dates.
instance FromXML Day where
    fromXML = do
        text <- unpack <$> parseContent
        let
            parsed =
                readDate "%F%z" text
                    <|> readDate "%FZ" text
                    <|> readDate "%F"  text
        case parsed of
            Just t -> return t
            Nothing ->
                throwParsingError $ ContentParsingError "xsd:date" $ pack text


-- | Parse an @xsd:dateTime@ from the Element's contents.
--
-- Supported formats are @YYYY-MM-DDTHH:MM:SS±HH:MM@ for zoned date times,
-- and @YYYY-MM-DDTHH:MM:SSZ@ or @YYYY-MM-DDTHH:MM:SS@ for UTC date times.
instance FromXML UTCTime where
    fromXML = do
        text <- unpack <$> parseContent
        let parsed =
                readDate "%FT%T%z" text
                    <|> readDate "%FT%TZ" text
                    <|> readDate "%FT%T"  text
        case parsed of
            Just t -> return t
            Nothing ->
                throwParsingError $ ContentParsingError "xsd:datetime" $ pack
                    text

readDate :: ParseTime a => String -> String -> Maybe a
readDate = parseTimeM True defaultTimeLocale


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
        { errorType :: ParsingErrorType
        , errorCursor :: Element
        , parentNodes :: [Name]
        }
    deriving (Show)

instance Exception ParsingError

-- | Specific errors that can occur.
data ParsingErrorType
    = -- | Just attach some message text to the error.
      GenericParsingError Text
      -- | The current element's name does not match the expected name.
    | NameMismatch Name
      -- | Could not find the element with the given name.
    | ElementNotFound Name
      -- | 'oneOf' was passed an empty list of 'Parser's.
    | NoParsersGiven
      -- | 'at' was passed an empty list of 'Name's.
    | NoNamesGiven
      -- | Could not parse the text content of the current element.
    | ContentParsingError
        Text
        -- ^ Expected format
        Text
        -- ^ Actual content

    -- | Could not parse the text content of the curent element as XML.
    | XMLContentParsingError
        Text
        -- ^ XML Parsing Error
        Text
        -- ^ Actual Content

    -- | Element contained more than just a single NodeContent value.
    | ExpectedOnlyText
    -- | Tried to match a specific list of child node types.
    | UnexpectedChildNodes Text
    deriving (Show, Eq)

-- | Show the parent node hierarchy as names separated by @ > @ signs
-- followed by a descriptive error message enclosed in braces.
prettyParsingError :: ParsingError -> Text
prettyParsingError err = wrapper $ case errorType err of
    GenericParsingError message -> [message]
    NameMismatch expected ->
        [ "Element name does not match expected name of:"
        , "\t" <> prettyName expected
        ]
    ElementNotFound desiredName ->
        [ "Expected child element with name: " <> prettyName desiredName
            , "Actual children:"
            ]
            <> map (\n -> "\t" <> prettyName n) childNames
    NoParsersGiven ->
        ["No parsers to attempt: `oneOf` was passed an empty list of parsers"]
    NoNamesGiven ->
        ["No names to descend into: `at` was passed an empty list of names"]
    ContentParsingError expectedFormat actual ->
        [ "Expected contents with format: " <> expectedFormat
        , "Actual contents was: " <> actual
        ]
    XMLContentParsingError xmlError actual ->
        [ "Expected text contents with escaped XML document"
        , "XML Parsing Error: " <> xmlError
        , "Actual contents: " <> actual
        ]
    ExpectedOnlyText ->
        "Expected only text content, but other nodes were present."
            : map ("\t" <>) describeAllChildren
    UnexpectedChildNodes expectedFormat ->
        "Expected child node(s) to be: "
            <> expectedFormat
            :  "But got:"
            :  map ("\t" <>) describeAllChildren
  where
    wrapper :: [Text] -> Text
    wrapper body =
        parentHierarchy <> " {\n" <> T.unlines (map ("\t" <>) body) <> "\n}"
    parentHierarchy :: Text
    parentHierarchy = intercalate " > " (map nameLocalName $ parentNodes err)
    prettyName :: Name -> Text
    prettyName n =
        maybe "" (\ns -> "{" <> ns <> "}") (nameNamespace n) <> nameLocalName n
    childNames :: [Name]
    childNames = foldr
        (\n acc -> case n of
            NodeElement el -> elementName el : acc
            _              -> acc
        )
        []
        (elementNodes $ errorCursor err)
    describeAllChildren :: [Text]
    describeAllChildren =
        map
                (\case
                    NodeContent     t -> "Text: " <> t
                    NodeComment     c -> "Comment: " <> c
                    NodeInstruction i -> "Instruction: " <> pack (show i)
                    NodeElement e -> "Element: " <> prettyName (elementName e)
                )
            $ elementNodes
            $ errorCursor err

-- | Run a Parser on an Element, returning an error or the parsed value.
runParser :: Element -> Parser a -> Either ParsingError a
runParser el = runParserContext $ ParserContext el [elementName el]

-- | Run a Parser with the given ParserContext.
runParserContext :: ParserContext -> Parser a -> Either ParsingError a
runParserContext ctx parser =
    runIdentity $ runExceptT $ runReaderT (fromParser parser) ctx

-- | Signal a generic parsing error with the given message.
parseError :: Text -> Parser a
parseError = throwParsingError . GenericParsingError

-- | Throw a specific 'ParsingErrorType'.
throwParsingError :: ParsingErrorType -> Parser a
throwParsingError errType = do
    element <- getElement
    parents <- getHierarchy
    Parser $ lift $ ExceptT $ Identity $ Left $ ParsingError
        { errorType   = errType
        , errorCursor = element
        , parentNodes = parents
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
        Nothing  -> throwParsingError NoParsersGiven
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
        else throwParsingError $ NameMismatch name

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
    []            -> throwParsingError NoNamesGiven
    [    name]    -> find name p
    name :   rest -> find name (at rest p)

-- | Descend into the first matching 'Element' and run the parser.
--
-- Throws an error if no matching children are found.
find :: Name -> Parser a -> Parser a
find name parser = getElementsByName name >>= \case
    []     -> throwParsingError $ ElementNotFound name
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

-- | Parse the Element's content, throwing an error if any other child
-- Nodes are present.
parseContent :: Parser Text
parseContent = do
    el <- getElement
    case elementNodes el of
        [NodeContent t] -> return t
        _               -> throwParsingError ExpectedOnlyText

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
            throwParsingError $ XMLContentParsingError (pack $ show err) text
        Right xmlDoc -> descend p $ documentRoot xmlDoc


-- UTILITIES


-- | Convenience function for building a @Text -> Name@ function with
-- a predefined namespace.
--
-- > soapName :: Text -> Name
-- > soapName = withNamespace "http://schemas.xmlsoap.org/soap/envelope/"
withNamespace :: Text -> (Text -> Name)
withNamespace namespace name = Name { nameLocalName = name
                                    , nameNamespace = Just namespace
                                    , namePrefix    = Nothing
                                    }
