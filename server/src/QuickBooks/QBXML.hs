{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module QuickBooks.QBXML
    ( -- * qbXML Requests
      Request(..)
    , buildRequest
      -- * qbXML Responses
    , Response(..)
    , HostData(..)
    , parseHostData
    , CompanyData(..)
    , PreferenceData(..)
      -- ** Basic Types
    , QBFileMode(..)
    )
where

import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )
import           Parser                         ( Parser
                                                , throwEither
                                                , runParser
                                                , matchName
                                                , find
                                                , findAll
                                                , parseContent
                                                , parseRead
                                                , parseBool
                                                )
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
import           XML                            ( FromXML(..) )


-- Requests

data Request
    = AccountQuery
    deriving (Show)

-- | Build the XML ByteString for a Request.
buildRequest :: Request -> ByteString
buildRequest r = wrapper $ case r of
    AccountQuery -> xelemEmpty "AccountQueryRq"
  where
    wrapper :: Xml Elem -> ByteString
    wrapper el = xrender $ qbxmlDoc $ xelem "QBXML" $ xelem
        "QBXMLMsgsRq"
        (xattr "onError" "stopOnError", el)

-- | Build a qbXML v13.0 document from a root element.
qbxmlDoc :: Xml Elem -> Xml Doc
qbxmlDoc content =
    let qbxmlDocInfo =
            defaultDocInfo { docInfo_docType = Just qbxmlDeclaration }
        qbxmlDeclaration = "<?qbxml version=\"13.0\"?>"
    in  doc qbxmlDocInfo content


-- Responses

-- | Possible qbXML Responses we can parse.
data Response
    = HostResponse HostData
    | CompanyResponse CompanyData
    | PreferencesResponse PreferenceData


-- | Information about the QuickBooks product & version we are
-- communicating with.
data HostData
    = HostData
        { productName :: Text
        -- ^ The Name of the QuickBooks Version on the Host.
        , majorVersion :: Integer
        -- ^ The numeric Major Version of the QuickBooks Host.
        , minorVersion :: Integer
        -- ^ The numeric Minor Version of the QuickBooks Host.
        , country :: Text
        -- ^ The Country Code the edition of QuickBooks was designed for.
        -- TODO: Parse this into ISO Country Codes?
        , supportedQbxmlVersions :: [Text]
        -- ^ The qbXML the QuickBooks Host Supports.
        -- TODO: Should we parse this into 'Data.Version.Version'?
        , isAutomaticLogin :: Bool
        -- ^ Does the Data File Allow Automatic Logins?
        , fileMode :: QBFileMode
        -- ^ Whether QuickBooks is open in 'SingleUser' or 'MultiUser' mode.
        } deriving (Show, Read)

-- | Parse HostData from an 'Element' containing a @HostQueryRs@ child.
instance FromXML HostData where
    fromXML el =
        throwEither $ runParser el $ find "HostQueryRs" $ find "HostRet" parseHostData

-- | Parse a 'HostData' value from a @HostRet@ 'Element'.
parseHostData :: Parser HostData
parseHostData = matchName "HostRet" $ do
    productName            <- find "ProductName" parseContent
    majorVersion           <- find "MajorVersion" parseRead
    minorVersion           <- find "MinorVersion" parseRead
    country                <- find "Country" parseContent
    supportedQbxmlVersions <- findAll "SupportedQBXMLVersion" parseContent
    isAutomaticLogin       <- find "IsAutomaticLogin" parseBool
    fileMode               <- find "QBFileMode" parseRead
    return HostData {..}


data CompanyData
    = CompanyData
        {
        } deriving (Show, Read)

data PreferenceData
    = PreferenceData
        {
        } deriving (Show, Read)


-- Response Helpers

-- | How many users the QuickBooks Company File supports.
data QBFileMode
    = SingleUser
    | MultiUser
    deriving (Show, Read, Eq)
