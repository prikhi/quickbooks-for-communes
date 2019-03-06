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
    , parseCompanyData
    , PreferenceData(..)
      -- ** Basic Types
    , QBFileMode(..)
    , Address(..)
    , TaxForm(..)
    , Month(..)
    , Service(..)
    , ServiceStatus(..)
    , AccountantCopy(..)
    )
where

import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Parser                         ( Parser
                                                , matchName
                                                , find
                                                , findAll
                                                , optional
                                                , parseContent
                                                , parseRead
                                                , parseBool
                                                , parseDate
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
        , countryVersion :: Text
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

-- | Parse a 'HostData' value from a @HostRet@ 'Element'.
parseHostData :: Parser HostData
parseHostData = matchName "HostRet" $ do
    productName            <- find "ProductName" parseContent
    majorVersion           <- find "MajorVersion" parseRead
    minorVersion           <- find "MinorVersion" parseRead
    countryVersion         <- find "Country" parseContent
    supportedQbxmlVersions <- findAll "SupportedQBXMLVersion" parseContent
    isAutomaticLogin       <- find "IsAutomaticLogin" parseBool
    fileMode               <- find "QBFileMode" parseRead
    return HostData {..}


-- | Information about the QuickBooks Company File.
data CompanyData
    = CompanyData
        { isSampleCompany :: Bool
        -- ^ Is the Company File one of the sample files provided by
        -- QuickBooks?
        , legalName :: Maybe Text
        -- ^ The name of the Company, for tax forms & pay stubs.
        , legalAddress :: Maybe Address
        -- ^ The address of the Company, for tax forms & pay stubs.
        , name :: Maybe Text
        -- ^ The common name of the Company, used in invoices, checks, and
        -- other forms.
        , address :: Maybe Address
        -- ^ The common address of the Company, used in checks and other
        -- forms.
        , customerAddress :: Maybe Address
        -- ^ The address the Company uses to receive mail from its
        -- customers.
        , phone :: Maybe Text
        -- ^ The company's contact phone number.
        , fax :: Maybe Text
        -- ^ The company's contact fax number.
        , email :: Maybe Text
        -- ^ The company's contact email.
        , website :: Maybe Text
        -- ^ The company's website.
        , firstFiscalYearMonth :: Maybe Month
        -- ^ The first month of the Company's 12-period fiscal year.
        , firstIncomeTaxYearMonth :: Maybe Month
        -- ^ The first month used for the default date range in income tax
        -- summary/detail reports.
        , companyType :: Maybe Text
        -- ^ The company type selected by the user when creating the
        -- company file.
        , ein :: Maybe Text
        -- ^ Employer Identification Number
        , ssn :: Maybe Text
        -- ^ Social Security Number
        , taxForm :: Maybe TaxForm
        -- ^ The tax form the company expects to file for it's taxes.
        -- Any value other than 'OtherOrNone' allows the Company to
        -- associate each account with a tax form line.
        , subscribedServices :: [Service]
        -- ^ Intuit services the company is currently subscribed to.
        , accountantCopy :: Maybe AccountantCopy
        -- ^ Information about the Accountant's Copy of the company file,
        -- if one has been made.
        } deriving (Show, Read)

-- | Parse a 'CompanyData value from a @CompanyRet@ 'Element'.
parseCompanyData :: Parser CompanyData
parseCompanyData = matchName "CompanyRet" $ do
    isSampleCompany         <- find "IsSampleCompany" parseBool
    legalName               <- optionalText "LegalCompanyName"
    legalAddress            <- findOptional "LegalAddress" parseAddress
    name                    <- optionalText "CompanyName"
    address                 <- findOptional "Address" parseAddress
    customerAddress <- findOptional "CompanyAddressForCustomer" parseAddress
    phone                   <- optionalText "Phone"
    fax                     <- optionalText "Fax"
    email                   <- optionalText "Email"
    website                 <- optionalText "CompanyWebSite"
    firstFiscalYearMonth    <- findOptional "FirstMonthFiscalYear" parseMonth
    firstIncomeTaxYearMonth <- findOptional "FirstMonthIncomTaxYear" parseMonth
    taxForm                 <- findOptional "TaxForm" parseTaxForm
    companyType             <- optionalText "CompanyType"
    ein                     <- optionalText "EIN"
    ssn                     <- optionalText "SSN"
    subscribedServices      <- find "SubscribedServices"
        $ findAll "Service" parseService
    accountantCopy <- findOptional "AccountantCopy" parseAccountantCopy
    return CompanyData {..}
  where
    optionalText n = optional $ find n parseContent
    findOptional n = optional . find n


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


-- | A Complete QuickBooks Address.
data Address
    = Address
        { lineOne :: Maybe Text
        , lineTwo :: Maybe Text
        , lineThree :: Maybe Text
        , lineFour :: Maybe Text
        , lineFive :: Maybe Text
        , city :: Maybe Text
        , state :: Maybe Text
        , postalCode :: Maybe Text
        , country :: Maybe Text
        , note :: Maybe Text
        } deriving (Show, Read)

-- | Parse an Address from the current element.
parseAddress :: Parser Address
parseAddress = do
    lineOne    <- optional $ find "Addr1" parseContent
    lineTwo    <- optional $ find "Addr2" parseContent
    lineThree  <- optional $ find "Addr3" parseContent
    lineFour   <- optional $ find "Addr4" parseContent
    lineFive   <- optional $ find "Addr5" parseContent
    city       <- optional $ find "City" parseContent
    state      <- optional $ find "State" parseContent
    postalCode <- optional $ find "PostalCode" parseContent
    country    <- optional $ find "Country" parseContent
    note       <- optional $ find "Note" parseContent
    return Address {..}


-- | The Form the Company Expects to File For Their Taxes.
data TaxForm
    = Form1040
    | Form1065
    | Form1120
    | Form1120S
    | Form990
    | Form990PF
    | Form990T
    | OtherOrNone
    deriving (Show, Read)

-- | Parse a TaxForm from it's 'Read' instance.
parseTaxForm :: Parser TaxForm
parseTaxForm = parseRead


-- | A month as defined by QuickBooks.
data Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
    deriving (Show, Read, Eq)

-- | Parse a Month from it's 'Read' instance.
parseMonth :: Parser Month
parseMonth = parseRead


-- | An Intuit Service the Company may subscribe to.
data Service
    = Service
        { serviceName :: Text
        , serviceDomain :: Text
        -- ^ The provider of the service.
        , serviceStatus :: ServiceStatus
        }
    deriving (Show, Read, Eq)

-- | The status of a service the company is or has been subscribed to.
data ServiceStatus
    = Active
    | Expired
    | Never
    | Pending
    | Suspended
    | Terminated
    | Trial
    deriving (Show, Read, Eq)

-- | Parse a Service from the current Element.
parseService :: Parser Service
parseService = do
    serviceName   <- find "Name" parseContent
    serviceDomain <- find "Domain" parseContent
    serviceStatus <- find "ServiceStatus" parseRead
    return Service {..}


-- | Describe's the Accountant's Copy of the company file. An
-- Accountant Copy is a version of the company file that allows an
-- Accountant to make separate changes to the data while you do normal
-- operations. The Accountant's copy can be merged back into your orignal
-- company file.
data AccountantCopy
    = AccountantCopy
        { accountantCopyExists :: Bool
        -- ^ Has a copy been made?
        , dividingDate :: Maybe UTCTime
        -- ^ The fiscal period the accountant is working on. You cannot
        -- modify or create transactions before this date.
        }
    deriving (Show, Read)

-- | Parse a AccountantCopy from the current Element.
parseAccountantCopy :: Parser AccountantCopy
parseAccountantCopy = do
    accountantCopyExists <- find "AccountantCopyExists" parseBool
    dividingDate         <- optional $ find "DividingDate" parseDate
    return AccountantCopy {..}
