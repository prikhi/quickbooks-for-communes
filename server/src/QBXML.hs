{-# LANGUAGE OverloadedStrings #-}
module QBXML
    ( Request(..)
    , qbxmlDoc
    , buildRequest
    , Callback(..)
    , CallbackResponse(..)
    )
where

import           Control.Monad.Catch.Pure       ( MonadThrow(..)
                                                , SomeException
                                                )
import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )
import           Text.XML.Generator
import           Text.XML                       ( Element(..) )
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
    deriving (Show)

instance FromXML Callback where
    fromXML = parseServerVersion

parseServerVersion :: MonadThrow m => Element -> m Callback
parseServerVersion el =
    if elementName el == "{http://developer.intuit.com/}serverVersion"
        then return ServerVersion
        else throwM (error "ServerVersion parse failure" :: SomeException)

data CallbackResponse
    = ServerVersionResp Text
    deriving (Show)

instance ToXML CallbackResponse where
    toXML r = case r of
        ServerVersionResp v ->
            xelemQ qbNamespace "serverVersionResponse" $
                xelemQ qbNamespace "serverVersionResult" v

qbNamespace :: Namespace
qbNamespace = namespace "" "http://developer.intuit.com/"
