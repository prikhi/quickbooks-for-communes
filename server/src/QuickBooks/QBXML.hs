{-# LANGUAGE OverloadedStrings #-}
module QuickBooks.QBXML
    ( Request(..)
    , qbxmlDoc
    , buildRequest
    )
where

import           Data.ByteString                ( ByteString )
import           Text.XML.Generator             ( Xml
                                                , Elem
                                                , Doc
                                                , DocInfo(..)
                                                , doc
                                                , defaultDocInfo
                                                , xrender
                                                , xelem
                                                , xelemEmpty
                                                , xattr
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
