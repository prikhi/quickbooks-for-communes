{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{- | The Parser module contains a monadic, depth tracking XML parser that
provides a nicer interface around the @xml-conduit@ package as well as
parsing functions for reading QuickBooks-specific types.

-}
module Parser
    ( -- * Running Parsers
      Parser
    , runParser
    , ParsingError
    , parseError
    , liftEither
     -- * Selecting Elements
    , matchName
    , find
    , findAll
    , at
     -- * Reading Content
    , parseRead
    , parseBool
    , parseDate
    , parseContent
    , parseContentWith
    )
where

import           Control.Applicative            ( (<|>) )
import           Control.Monad.Catch.Pure       ( Exception )
import           Control.Monad.Reader           ( ReaderT
                                                , MonadReader
                                                , runReaderT
                                                , asks
                                                , local
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , runExceptT
                                                , throwError
                                                )
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Functor.Identity          ( Identity
                                                , runIdentity
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text
                                                , intercalate
                                                , unpack
                                                , pack
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time                      ( UTCTime )
import           Data.Time.Format               ( parseTimeM
                                                , defaultTimeLocale
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
--
-- TODO: Don't use a MonadReader or MonadError, just custom functions that
-- maintain Context & properly construct errors?
newtype Parser a
    = Parser { fromParser :: ReaderT ParserContext (ExceptT ParsingError Identity) a }
    deriving (Functor, Applicative, Monad, MonadReader ParserContext, MonadError ParsingError)

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
runParser el parser =
    runIdentity $ runExceptT $ runReaderT (fromParser parser) $ ParserContext
        el
        [elementName el]

-- | Signal a parsing error with the given message.
parseError :: Text -> Parser a
parseError msg = do
    parents <- asks $ reverse . hierarchy
    throwError $ ParsingError {errorMessage = msg, parentNodes = parents}

-- | Lift an Either into a Parser by throwing a ParsingError if necessary.
liftEither :: Either String a -> Parser a
liftEither = either (parseError . pack) return



-- SELECTING

-- | Ensure the 'Element' 'Name' matches the given name, then apply the
-- parser.
--
-- Throws an error if the name does not match.
matchName :: Name -> Parser a -> Parser a
matchName name parser = do
    el <- asks cursor
    if elementName el == name
        then parser
        else parseError $ "Expected Element with Name: " <> nameLocalName name

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
findAll :: Name -> Parser a -> Parser [a]
findAll name parser = getElementsByName name >>= mapM (descend parser)

-- | Descend into an Element & parse it.
descend :: Parser a -> Element -> Parser a
descend parser el = local
    (\ctx -> ctx { cursor = el, hierarchy = elementName el : hierarchy ctx })
    parser

-- | Return all children of the current element with the matching 'Name'.
getElementsByName :: Name -> Parser [Element]
getElementsByName name = asks $ mapMaybe getByName . elementNodes . cursor
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

-- | Parse an @xsd:date@ from the Element contents.
--
-- Supported formats are @YYYY-MM-DD±HH:MM@ for zoned times, @YYYY-MM-DDZ@
-- for UTC time, & @YYYY-MM-DD@.
parseDate :: Parser UTCTime
parseDate = do
    text <- unpack <$> parseContent
    let parsed =
            readDate "%F%eZ" text <|> readDate "%FZ" text <|> readDate "%F" text
    case parsed of
        Just t  -> return t
        Nothing -> parseError $ "Expected an xsd:date type, got: " <> pack text
    where readDate = parseTimeM True defaultTimeLocale

-- | Parse the Element's content, throwing an error if any other child
-- Nodes are present.
parseContent :: Parser Text
parseContent = do
    el <- asks cursor
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
