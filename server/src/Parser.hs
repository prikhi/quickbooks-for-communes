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
    , throwEither
     -- * Selecting Elements
    , matchName
    , find
    , findAll
    , at
     -- * Reading Content
    , parseRead
    , parseBool
    , parseContent
    , parseContentWith
    )
where

import           Control.Monad.Catch.Pure       ( MonadThrow(..)
                                                , SomeException
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , MonadReader
                                                , runReaderT
                                                , ask
                                                , asks
                                                , local
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError
                                                , runExceptT
                                                , throwError
                                                , liftEither
                                                )
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Functor.Identity          ( Identity
                                                , runIdentity
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Text.Read                      ( readEither )
import           Text.XML                       ( Node(..)
                                                , Element(..)
                                                , Name(..)
                                                , parseLBS_
                                                , def
                                                , documentRoot
                                                )

-- RUNNING

-- | The Parser Monad is used for parsing XML elements into datatypes. It
-- tracks the current Element in a cursor-like fashion, where you can
-- descend into child Elements to parse nested XML. Running a Parser
-- returns the result or a String error.
newtype Parser a
    = Parser { fromParser :: ReaderT Element (ExceptT String Identity) a }
    deriving (Functor, Applicative, Monad, MonadReader Element, MonadError String)

-- | Run a Parser on an Element, returning an error or the parsed value.
runParser :: Element -> Parser a -> Either String a
runParser el parser =
    runIdentity $ runExceptT $ runReaderT (fromParser parser) el

-- | Lift an Either to a MonadThrow instance by throwing Left values as
-- SomeException.
throwEither :: MonadThrow m => Either String a -> m a
throwEither = \case
    Right x       -> return x
    Left  errText -> throwM (error errText :: SomeException)


-- SELECTING

-- | Ensure the 'Element' 'Name' matches the given name, then apply the
-- parser.
--
-- Throws an error if the name does not match.
matchName :: Name -> Parser a -> Parser a
matchName name parser = do
    el <- ask
    if elementName el == name
        then parser
        else throwError $ "Expected Element with Name: " <> show
            (nameLocalName name)

-- | Run the Parser on a nested element. The first matching name is
-- descended into at each level. No backtracking or retrying is done if the
-- 'Name' path is unparseable.
--
-- Throws an error if passed an empty list of names or the 'Name' path is
-- unreachable.
at :: [Name] -> Parser a -> Parser a
at n p = case n of
    []            -> throwError "No name to descend into"
    [    name]    -> find name p
    name :   rest -> find name (at rest p)

-- | Descend into the first matching 'Element' and run the parser.
--
-- Throws an error if no matching children are found.
find :: Name -> Parser a -> Parser a
find name parser = getElementsByName name >>= \case
    [] ->
        throwError $ "Could Not Find Element: " <> unpack (nameLocalName name)
    el : _ -> descend parser el

-- | Parse all children with matching names using the given 'Parser'.
findAll :: Name -> Parser a -> Parser [a]
findAll name parser = getElementsByName name >>= mapM (descend parser)

-- | Descend into an Element & parse it.
descend :: Parser a -> Element -> Parser a
descend parser el = const el `local` parser

-- | Return all children of the current element with the matching 'Name'.
getElementsByName :: Name -> Parser [Element]
getElementsByName name = asks $ mapMaybe getByName . elementNodes
  where
    getByName = \case
        NodeElement e -> if elementName e == name then Just e else Nothing
        _             -> Nothing


-- READING

-- | Expect a single 'NodeContent' child & parse it from a Read instance.
parseRead :: Read a => Parser a
parseRead = parseContent >>= liftEither . readEither . unpack

-- | Expect a single 'NodeContent' child & parse it into a Bool, expecting
-- the text values for the QuickBooks IQBBoolType.
parseBool :: Parser Bool
parseBool = parseContent >>= \case
    "true"  -> return True
    "false" -> return False
    s       -> throwError $ "Could not parse IQBBoolType, got: " <> unpack s

-- | Parse the Element's content, throwing an error if any other child
-- Nodes are present.
parseContent :: Parser Text
parseContent = do
    el <- ask
    case elementNodes el of
        [NodeContent t] -> return t
        _ ->
            throwError $ "Expected NodeContent Singleton in Element: " <> unpack
                (nameLocalName $ elementName el)

-- | Get the Element's content, parse it into an XML 'Text.XML.Document',
-- then run the given Parser on the 'documentRoot'.
parseContentWith :: Parser a -> Parser a
parseContentWith p = do
    text <- parseContent
    let xmlContent = parseLBS_ def $ LBS.fromStrict $ encodeUtf8 text
    descend p $ documentRoot xmlContent
