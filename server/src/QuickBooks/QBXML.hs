{-# LANGUAGE OverloadedStrings #-}
module QuickBooks.QBXML
    ( Request(..)
    , qbxmlDoc
    , buildRequest
    , Callback(..)
    , CallbackResponse(..)
    )
where

import           Control.Applicative            ( (<|>) )
import           Control.Monad.Catch.Pure       ( MonadThrow(..)
                                                , SomeException
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )
import           Text.XML.Generator
import           Text.XML                       ( Node(..)
                                                , Element(..)
                                                )
import           XML                            ( FromXML(..)
                                                , ToXML(..)
                                                )

data Request
    = AccountQuery
    deriving (Show)

buildRequest :: Request -> ByteString
buildRequest r = wrapper $ case r of
    AccountQuery -> xelemEmpty "AccountQueryRq"
  where
    wrapper :: Xml Elem -> ByteString
    wrapper el = xrender $ qbxmlDoc $ xelem "QBXML" $ xelem
        "QBXMLMsgsRq"
        (xattr "onError" "stopOnError", el)

qbxmlDoc :: Xml Elem -> Xml Doc
qbxmlDoc content =
    let qbxmlDocInfo =
            defaultDocInfo { docInfo_docType = Just qbxmlDeclaration }
        qbxmlDeclaration = "<?qbxml version=\"13.0\"?>"
    in  doc qbxmlDocInfo content

data Callback
    = ServerVersion
    | ClientVersion Text
    deriving (Show)

instance FromXML Callback where
    fromXML e = parseServerVersion e
            <|> parseClientVersion e

parseServerVersion :: MonadThrow m => Element -> m Callback
parseServerVersion el =
    if elementName el == "{http://developer.intuit.com/}serverVersion"
        then return ServerVersion
        else parsingError "ServerVersion parse failure"

parseClientVersion :: MonadThrow m => Element -> m Callback
parseClientVersion el =
    if elementName el == "{http://developer.intuit.com/}clientVersion"
        then case elementNodes el of
            [NodeElement strVersion] -> parseVersion strVersion
            e -> parsingError $ "Invalid clientVersion Body" ++ show e
        else parsingError "ClientVersion parse failure"
  where
    parseVersion vEl =
        if elementName vEl == "{http://developer.intuit.com/}strVersion"
            then case elementNodes vEl of
                [NodeContent v] -> return $ ClientVersion v
                _               -> parsingError "Invalid clientVersion Content"
            else parsingError $ "Invalid clientVersion Body" ++ show vEl

parsingError :: MonadThrow m => String -> m a
parsingError s = throwM (error s :: SomeException)

data CallbackResponse
    = ServerVersionResp Text
    | ClientVersionResp Text
    deriving (Show)

instance ToXML CallbackResponse where
    toXML r = case r of
        ServerVersionResp v ->
            xelemQ qbNamespace "serverVersionResponse" $
                xelemQ qbNamespace "serverVersionResult" v
        ClientVersionResp v ->
            xelemQ qbNamespace "clientVersionResponse" $
                xelemQ qbNamespace "clientVersionResult" v


qbNamespace :: Namespace
qbNamespace = namespace "" "http://developer.intuit.com/"
