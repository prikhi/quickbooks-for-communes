{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{- | This module contains types describing the qbXML requests and their
responses, as well as XML generation & parsing function.

It is very incomplete - there are many request endpoints and we've really
only implemented what we need. If you'd like support for additional
requests, feel free to open up an issue on github.

-}
module QuickBooks.QBXML
    ( -- * qbXML Requests
      Request(..)
    , AccountQueryFilters(..)
    , buildRequest

      -- * qbXML Responses
    , Response(..)

      -- ** HostQuery
    , HostData(..)
    , QBFileMode(..)

      -- ** CompanyQuery
    , CompanyData(..)
    , TaxForm(..)
    , Service(..)
    , AccountantCopy(..)

      -- ** PreferencesQuery
    , PreferencesData(..)
    , AccountingPreferences(..)
    , AssignClass(..)
    , FinanceChargePreferences(..)
    , CalculateFinanceChargesFrom(..)
    , JobsAndEstimatesPreferences(..)
    , MultiCurrencyPreferences(..)
    , MultiLocationInventoryPreferences(..)
    , PurchasesAndVendorsPreferences(..)
    , ReportsPreferences(..)
    , AgingReportBasis(..)
    , SummaryReportBasis(..)
    , SalesAndCustomersPreferences(..)
    , PriceLevelPreferences(..)
    , SalesTaxPreferences(..)
    , SalesTaxFrequency(..)
    , TimeTrackingPreferences(..)
    , CurrentAppAccessRights(..)
    , ItemsAndInventoryPreferences(..)

      -- ** AccountQuery
    , AccountData(..)
    , AccountType(..)
    , SpecialAccountType(..)
    , TaxLineData(..)
    , CashFlowClassification(..)

      -- ** Basic Types
    , Percentage(..)
    , ListReference(..)
    , OptionalListReference(..)
    , Address(..)
    , Month(..)
    , DayOfWeek(..)
    )
where

import           Data.Hashable                  ( Hashable )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding             ( decodeUtf8 )
import           Data.Time                      ( Day
                                                , UTCTime
                                                , formatTime
                                                , defaultTimeLocale
                                                )
import           GHC.Generics                   ( Generic )
import           Parser                         ( parseError
                                                , matchName
                                                , oneOf
                                                , find
                                                , findAll
                                                , optional
                                                , parseContent
                                                , parseRead
                                                , parseBool
                                                , parseInteger
                                                , parseDecimal
                                                , parseDate
                                                , parseDatetime
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
                                                , xelemWithText
                                                , xattr
                                                )
import           XML                            ( FromXML(..) )


-- Requests

newtype Request
    = AccountQuery (Maybe AccountQueryFilters)
    deriving (Show, Read)

newtype AccountQueryFilters
    = AccountQueryFilters
        { aqFromModifiedDate :: UTCTime
        -- ^ Note: Times in QuickBooks are not adjusted for DST!
        }
    deriving (Show, Read)

-- | Build the Request's XML into a Text value.
--
-- Newlines are stripped out because the QuickBooks SDK's XML parser did
-- not like the output format of the 'xrender' function.
buildRequest :: Request -> Text
buildRequest r = wrapper $ case r of
    AccountQuery mFilters -> case mFilters of
        Nothing -> xelemEmpty "AccountQueryRq"
        Just filters ->
            xelem "AccountQueryRq"
                $ xelemWithText "FromModifiedDate"
                $ T.pack
                $ formatTime defaultTimeLocale "%FT%T-00:00"
                $ aqFromModifiedDate filters
  where
    wrapper :: Xml Elem -> Text
    wrapper el =
        T.filter (/= '\n')
            . decodeUtf8
            . xrender
            $ qbxmlDoc
            $ xelem "QBXML"
            $ xelem "QBXMLMsgsRq" (xattr "onError" "stopOnError", el)

-- | Build a qbXML v13.0 document from a root element.
qbxmlDoc :: Xml Elem -> Xml Doc
qbxmlDoc content =
    let qbxmlDocInfo = defaultDocInfo { docInfo_docType = Just qbxmlDeclaration
                                      , docInfo_standalone = False
                                      }
        qbxmlDeclaration = "<?qbxml version=\"13.0\"?>"
    in  doc qbxmlDocInfo content



-- Responses

-- | Possible qbXML Responses we can parse.
--
-- TODO: Wrapper type w/ request id, status code/severity/message, & return
-- count?
data Response
    = HostResponse HostData
    | CompanyResponse CompanyData
    | PreferencesResponse PreferencesData
    | AccountQueryResponse [AccountData]
    deriving (Show, Read)

instance FromXML Response where
    fromXML = oneOf
        [ HostResponse <$> find "HostQueryRs" fromXML
        , CompanyResponse <$> find "CompanyQueryRs" fromXML
        , PreferencesResponse <$> find "PreferencesQueryRs" fromXML
        , AccountQueryResponse <$> find "AccountQueryRs" (findAll "AccountRet" fromXML)
        ]


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

-- | Parse a 'HostData' value from a @HostRet@ 'Text.XML.Element'.
instance FromXML HostData where
    fromXML = matchName "HostRet" $ do
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

-- | Parse a 'CompanyData' value from a @CompanyRet@ 'Text.XML.Element'.
instance FromXML CompanyData where
    fromXML = matchName "CompanyRet" $ do
        isSampleCompany         <- find "IsSampleCompany" parseBool
        legalName               <- optionalText "LegalCompanyName"
        legalAddress            <- findOptional "LegalAddress" fromXML
        name                    <- optionalText "CompanyName"
        address                 <- findOptional "Address" fromXML
        customerAddress <- findOptional "CompanyAddressForCustomer" fromXML
        phone                   <- optionalText "Phone"
        fax                     <- optionalText "Fax"
        email                   <- optionalText "Email"
        website                 <- optionalText "CompanyWebSite"
        firstFiscalYearMonth    <- findOptional "FirstMonthFiscalYear" fromXML
        firstIncomeTaxYearMonth <- findOptional "FirstMonthIncomTaxYear" fromXML
        taxForm                 <- findOptional "TaxForm" fromXML
        companyType             <- optionalText "CompanyType"
        ein                     <- optionalText "EIN"
        ssn                     <- optionalText "SSN"
        subscribedServices      <- find "SubscribedServices"
            $ findAll "Service" fromXML
        accountantCopy          <- findOptional "AccountantCopy" fromXML
        return CompanyData {..}
      where
        optionalText n = optional $ find n parseContent
        findOptional n = optional . find n


-- | The preferences the QuickBooks user has set in their company file.
-- These can only be modified directly through the QuickBooks UI.
data PreferencesData
    = PreferencesData
        { accountingPreferences :: AccountingPreferences
        , financeChargePreferences :: FinanceChargePreferences
        , jobsAndEstimatesPreferences :: JobsAndEstimatesPreferences
        , multiCurrencyPreferences  :: Maybe MultiCurrencyPreferences
        , multiLocationInventoryPreferences :: Maybe MultiLocationInventoryPreferences
        , purchasesAndVendorsPreferences :: PurchasesAndVendorsPreferences
        , reportsPreferences :: ReportsPreferences
        , salesAndCustomersPreferences :: SalesAndCustomersPreferences
        , salesTaxPreferences :: Maybe SalesTaxPreferences
        , timeTrackingPreferences :: Maybe TimeTrackingPreferences
        , currentAppAccessRights :: CurrentAppAccessRights
        , itemsAndInventoryPreferences :: Maybe ItemsAndInventoryPreferences
        } deriving (Show, Read)

-- | Parse a PreferencesData' value from a @PreferencesRet@ 'Text.XML.Element'.
instance FromXML PreferencesData where
    fromXML = matchName "PreferencesRet" $ do
        accountingPreferences <- find "AccountingPreferences"
                                    fromXML
        financeChargePreferences <- find "FinanceChargePreferences"
                                        fromXML
        jobsAndEstimatesPreferences <- find "JobsAndEstimatesPreferences"
                                            fromXML
        multiCurrencyPreferences <- optional
            $ find "MultiCurrencyPreferences" fromXML
        multiLocationInventoryPreferences <- optional $ find
            "MultiLocationInventoryPreferences"
            fromXML
        purchasesAndVendorsPreferences <- find
            "PurchasesAndVendorsPreferences"
            fromXML
        reportsPreferences <- find "ReportsPreferences" fromXML
        salesAndCustomersPreferences <- find "SalesAndCustomersPreferences"
                                             fromXML
        salesTaxPreferences <- optional
            $ find "SalesTaxPreferences" fromXML
        timeTrackingPreferences <- optional
            $ find "TimeTrackingPreferences" fromXML
        currentAppAccessRights <- find "CurrentAppAccessRights"
                                    fromXML
        itemsAndInventoryPreferences <- optional $ find
            "ItemsAndInventoryPreferences"
            fromXML
        return PreferencesData {..}


data AccountData
    = AccountData
        { accountReference :: ListReference
        -- ^ The account's List ID & Full Name.
        , accountName :: Text
        -- ^ The name of the account, not including any ancestor accounts.
        , accountEditSequence :: Text
        -- ^ A modification counter for an object. When updating an
        -- account, you must include this value. QuickBooks will compare
        -- the submitted EditSequence value with it's EditSequence value to
        -- ensure there have been no changes to the account since we've
        -- fetched the data.
        , isActive :: Bool
        -- ^ Is the account enabled for use in QuickBooks?
        , parentAccount :: Maybe OptionalListReference
        -- ^ The parent account of the account.
        , subLevel :: Integer
        -- ^ The number of ancestors the account has.
        , accountType :: AccountType
        -- ^ The type of QuickBooks account this represents. Note that you
        -- cannot create or modify a 'NonPosting' account through the SDK
        -- since those are managed exclusively by QuickBooks.
        , specialAccountType :: Maybe SpecialAccountType
        -- ^ Accounts with a SpecialAccountType were automatically created
        -- by QuickBooks for some purpose. Some of these are not overriden
        -- or modifiable by the SDK.
        , isTaxAccount :: Maybe Bool
        -- ^ Is the account used for tax purposes?
        , accountNumber :: Maybe Text
        -- ^ The account number for the Chart of Accounts. Numbers are only
        -- visible in the QuickBooks UI if the 'usingAccountNumbers'
        -- preference is 'True'.
        , bankNumber :: Maybe Text
        -- ^ The bank account number for the account. Only returned if your
        -- application has been granted access to sensitive data.
        , description :: Maybe Text
        -- ^ Some descriptive text about the account.
        , balance :: Maybe Rational
        -- ^ The current balance of the account. May not be present for
        -- some income & balance sheet accounts.
        , totalBalance :: Maybe Rational
        -- ^ The total balance for the account & all it's sub-accounts.
        , salesTaxCode :: Maybe OptionalListReference
        -- ^ The sales-tax code indicating whether the account has taxable
        -- or non-taxable transactions.
        , taxLineData :: Maybe TaxLineData
        -- ^ The tax line this account is associated with for the company
        -- file's 'TaxForm'.
        , cashFlowClassification :: Maybe CashFlowClassification
        -- ^ How is the account classified for cash flow reporting?
        , currency :: Maybe OptionalListReference
        -- ^ The default currency for the account.
        , createdAt :: UTCTime
        -- ^ The time the account was created.
        , modifiedAt :: UTCTime
        -- ^ The time the account was last modified.
        } deriving (Show, Read)

instance FromXML AccountData where
    fromXML = matchName "AccountRet" $ do
        accountReference <- fromXML
        accountName <- find "Name" parseContent
        accountEditSequence <- find "EditSequence" parseContent
        isActive <- find "IsActive" parseBool
        parentAccount <- optional $ find "ParentRef" fromXML
        subLevel <- find "Sublevel" parseInteger
        accountType <- find "AccountType" fromXML
        specialAccountType <- optional $ find "SpecialAccountType" fromXML
        isTaxAccount <- optional $ find "IsTaxAccount" parseBool
        accountNumber <- optional $ find "AccountNumber" parseContent
        bankNumber <- optional $ find "BankNumber" parseContent
        description <- optional $ find "Descr" parseContent
        balance <- optional $ find "Balance" parseDecimal
        totalBalance <- optional $ find "TotalBalance" parseDecimal
        salesTaxCode <- optional $ find "SalesTaxCodeRef" fromXML
        taxLineData <- optional $ find "TaxLineInfoRet" fromXML
        cashFlowClassification <- optional $ find "CashFlowClassification" fromXML
        currency <- optional $ find "CurrencyRef" fromXML
        createdAt <- find "TimeCreated" parseDatetime
        modifiedAt <- find "TimeModified" parseDatetime
        return AccountData {..}


-- Response Helpers

-- HOST

-- | How many users the QuickBooks Company File supports.
data QBFileMode
    = SingleUser
    | MultiUser
    deriving (Show, Read, Eq)


-- COMPANY

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
instance FromXML TaxForm where
    fromXML = parseRead


-- | An Intuit Service the Company may subscribe to.
data Service
    = Service
        { serviceName :: Text
        , serviceDomain :: Text
        -- ^ The provider of the service.
        , serviceStatus :: ServiceStatus
        }
    deriving (Show, Read, Eq)

-- | Parse the Service fields from the current Element.
instance FromXML Service where
    fromXML = do
        serviceName   <- find "Name" parseContent
        serviceDomain <- find "Domain" parseContent
        serviceStatus <- find "ServiceStatus" parseRead
        return Service {..}

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

-- | Parse the status using it's 'Read' instance.
instance FromXML ServiceStatus where
    fromXML = parseRead


-- | Describes the Accountant's Copy of the company file. An Accountant
-- Copy is a version of the company file that allows an Accountant to make
-- separate changes to the data while you do normal operations. The
-- Accountant's copy can be merged back into your orignal company file.
data AccountantCopy
    = AccountantCopy
        { accountantCopyExists :: Bool
        -- ^ Has a copy been made?
        , dividingDate :: Maybe Day
        -- ^ The fiscal period the accountant is working on. You cannot
        -- modify or create transactions before this date.
        }
    deriving (Show, Read)

-- | Parse a AccountantCopy from the current Element.
instance FromXML AccountantCopy where
    fromXML = do
        accountantCopyExists <- find "AccountantCopyExists" parseBool
        dividingDate         <- optional $ find "DividingDate" parseDate
        return AccountantCopy {..}


-- PREFERENCES

-- | Accounting Preferences Set By the User.
data AccountingPreferences
    = AccountingPreferences
        { usingAccountNumbers :: Bool
        -- ^ Can we set the account number when adding accounts?
        , requiresAccounts :: Bool
        -- ^ Do transactions require accounts to be recorded?
        , usingClassTracking :: Bool
        -- ^ Do transactions have @Class@ fields?
        , assignClassesTo :: Maybe AssignClass
        -- ^ This is undocumented in the QB Desktop API Reference...
        , usingAuditTrail :: Bool
        -- ^ Logging all transactions in the audit trail report?
        , assigningJournalEntryNumbers :: Bool
        -- ^ Are numbers automatically assigned to journal entries?
        , closingDate :: Maybe Day
        -- ^ The company's closing date. Transaction access before this
        -- point may be restricted.
        } deriving (Show, Read)

-- | Parse an AccountingPreferences from the current Element.
instance FromXML AccountingPreferences where
    fromXML = do
        usingAccountNumbers          <- find "IsUsingAccountNumbers" parseBool
        requiresAccounts             <- find "IsRequiringAccounts" parseBool
        usingClassTracking           <- find "IsUsingClassTracking" parseBool
        assignClassesTo <- optional $ find "AssignClassesTo" fromXML
        usingAuditTrail              <- find "IsUsingAuditTrail" parseBool
        assigningJournalEntryNumbers <- find "IsAssigningJournalEntryNumbers"
                                            parseBool
        closingDate <- optional $ find "ClosingDate" parseDate
        return AccountingPreferences {..}


-- | This is undocumented in the QB Desktop API Reference & I haven't
-- bothered to look up what it means...
data AssignClass
    = AssignNone
    | Accounts
    | Items
    | Names
    deriving (Show, Read, Eq)

-- | Parse an AssignClass from the text of the current Element.
instance FromXML AssignClass where
    fromXML = parseContent >>= \case
        "None"     -> return AssignNone
        "Accounts" -> return Accounts
        "Items"    -> return Items
        "Names"    -> return Names
        s          -> parseError $ "Expected AssignClass, got: " <> s


-- | How finance charges are assessed against customers for late payments.
data FinanceChargePreferences
    = FinanceChargePreferences
        { annualInterestRate :: Maybe Percentage
        -- ^ The interest rate to calculate finance charges with.
        , minimumFinanceCharge :: Maybe Rational
        -- ^ The minimum charge to apply for any amount overdue.
        , gracePeriod :: Integer
        -- ^ The number of days before applying finance charges to an
        -- overdue invoice.
        , financeChargeAccount :: Maybe OptionalListReference
        -- ^ The account that tracks finance charges that customers pay.
        , assessingForOverdueCharges :: Bool
        -- ^ Do we charge finance charges on overdue finance charges?
        , calculateChargesFrom :: CalculateFinanceChargesFrom
        -- ^ The type of date to calculate finance charges from.
        , markedToBePrinted :: Bool
        -- ^ Are newly created finance-charge invoices marked as needing
        -- printing?
        } deriving (Show, Read)

-- | Parse a FinanceChargePreferences from the current Element.
instance FromXML FinanceChargePreferences where
    fromXML = do
        annualInterestRate   <- optional $ find "AnnualInterestRate" fromXML
        minimumFinanceCharge <- optional $ find "MinFinanceCharge" parseDecimal
        gracePeriod          <- find "GracePeriod" parseInteger
        financeChargeAccount <- optional
            $ find "FinanceChargeAccountRef" fromXML
        assessingForOverdueCharges <- find "IsAssessingForOverdueCharges" parseBool
        calculateChargesFrom       <- find "CalculateChargesFrom"
                                        fromXML
        markedToBePrinted <- find "IsMarkedToBePrinted" parseBool
        return FinanceChargePreferences {..}

-- | Should we calculate finance charges starting from the invoice date or
-- payment due date?
data CalculateFinanceChargesFrom
    = DueDate
    | InvoiceOrBilledDate
    deriving (Show, Read, Eq)

-- | Parse the CalculateFinanceChargesFrom type using it's 'Read' instance.
instance FromXML CalculateFinanceChargesFrom where
    fromXML = parseRead


-- | How to handle jobs & estimates.
data JobsAndEstimatesPreferences
    = JobsAndEstimatesPreferences
        { usingEstimates :: Bool
        -- ^ The company is set up to create estimates for jobs.
        , usingProgressInvoicing :: Bool
        -- ^ Support for creating invoices for portions of an estimate.
        , printingItemsWithZeroAmounts :: Bool
        -- ^ Print line items with zero amounts on progress invoices.
        } deriving (Show, Read)

-- | Parse a JobsAndEstimatesPreferences from the current Element.
instance FromXML JobsAndEstimatesPreferences where
    fromXML = do
        usingEstimates               <- find "IsUsingEstimates" parseBool
        usingProgressInvoicing       <- find "IsUsingProgressInvoicing" parseBool
        printingItemsWithZeroAmounts <- find "IsPrintingItemsWithZeroAmounts"
                                            parseBool
        return JobsAndEstimatesPreferences {..}


-- | Current multi-currency settings for the company file.
data MultiCurrencyPreferences
    = MultiCurrencyPreferences
        { multiCurrencyIsOn :: Maybe Bool
        -- ^ Is the multicurrency feature turned on?
        , homeCurrency :: Maybe OptionalListReference
        -- ^ The currency of the country where the business is physically
        -- located.
        } deriving (Read, Show)

-- | Parse the MultiCurrency Settings from the curent Element.
instance FromXML MultiCurrencyPreferences where
    fromXML = do
        multiCurrencyIsOn <- optional $ find "IsMultiCurrencyOn" parseBool
        homeCurrency      <- optional $ find "HomeCurrencyRef" fromXML
        return MultiCurrencyPreferences {..}


-- | The QuickBooks Desktop API Reference doesn't have documentation on
-- this...
data MultiLocationInventoryPreferences
    = MultiLocationInventoryPreferences
        { multiLocationInventoryIsAvailable :: Maybe Bool
        , multiLocationInventoryIsEnabled :: Maybe Bool
        } deriving (Read, Show)

-- | Parse the MultiLocationInventoryPreferences from the current element.
instance FromXML MultiLocationInventoryPreferences where
    fromXML = do
        multiLocationInventoryIsAvailable <- optional
            $ find "IsMultiLocationInventoryAvailable" parseBool
        multiLocationInventoryIsEnabled <- optional
            $ find "IsMultiLocationInventoryEnabled" parseBool
        return MultiLocationInventoryPreferences {..}


-- | How to handle purchases & vendors.
data PurchasesAndVendorsPreferences
    = PurchasesAndVendorsPreferences
        { usingInventory :: Bool
        -- ^ Are inventory-related features of QuickBooks available?
        , daysBillsAreDue :: Integer
        -- ^ How many days are bills due after receipt?
        , automaticallyUsingDiscounts :: Bool
        -- ^ Are vendor discounts & credits automatically applied to bills
        -- being paid?
        , defaultDiscountAccount :: Maybe OptionalListReference
        -- ^ The account where vendor discounts are tracked.
        } deriving (Show, Read)

instance FromXML PurchasesAndVendorsPreferences where
    fromXML = do
        usingInventory              <- find "IsUsingInventory" parseBool
        daysBillsAreDue             <- find "DaysBillsAreDue" parseInteger
        automaticallyUsingDiscounts <- find "IsAutomaticallyUsingDiscounts"
                                            parseBool
        defaultDiscountAccount <- optional
            $ find "DefaultDiscountAccountRef" fromXML
        return PurchasesAndVendorsPreferences {..}


-- | Settings for Reporting.
data ReportsPreferences
    = ReportsPreferences
        { agingReportBasis :: AgingReportBasis
        -- ^ When do overdue days in aging reports begin?
        , summaryReportBasis :: SummaryReportBasis
        -- ^ Do summary reports use cash-basis or accrual-basis
        -- bookkeeping?
        } deriving (Show, Read)

instance FromXML ReportsPreferences where
    fromXML = do
        agingReportBasis   <- find "AgingReportBasis" fromXML
        summaryReportBasis <- find "SummaryReportBasis" fromXML
        return ReportsPreferences {..}

-- | The start date for overdue days in aging reports.
data AgingReportBasis
    = AgeFromDueDate
    -- ^ Overdue days begin with the due date on the invoice.
    | AgeFromTransactionDate
    -- ^ Overdue days begin with the date the transaction was created.
    deriving (Show, Read, Eq)

-- | Parse an AgingReportBasis using it's 'Read' instance.
instance FromXML AgingReportBasis where
    fromXML = parseRead

-- | The basis for the summary reports.
data SummaryReportBasis
    = AccrualBasis
    | CashBasis
    deriving (Show, Read, Eq)

instance FromXML SummaryReportBasis where
    fromXML = parseContent >>= \case
        "Accrual" -> return AccrualBasis
        "Cash"    -> return CashBasis
        s         -> parseError $ "Expected SummaryReportBasis, got: " <> s


-- | Settings for sales & customers.
data SalesAndCustomersPreferences
    = SalesAndCustomersPreferences
        { defaultShippingMethod :: Maybe OptionalListReference
        -- ^ The default value in all @Ship Via@ fields. Refers to an item
        -- in the @ShipMethod@ list.
        , defaultShipingSite :: Maybe Text
        -- ^ The FOB - where invoiced products are shipped from.
        , defaultMarkup :: Maybe Percentage
        -- ^ Default percentage to mark up an inventory item from it's
        -- cost.
        , trackingReimbursedExpensesAsIncome :: Bool
        -- ^ Are expenses and reimbursements for expenses tracked in
        -- separate accounts?
        , autoApplyingPayments :: Bool
        -- ^ Are customer's payments automatically applied to outstanding
        -- invoices? The oldest invoice will be prioritized.
        , priceLevels :: Maybe PriceLevelPreferences
        -- ^ Settings for custom price levels for specific customers.
        } deriving (Show, Read)

instance FromXML SalesAndCustomersPreferences where
    fromXML = do
        defaultShippingMethod <- optional
            $ find "DefaultShipMethodRef" fromXML
        defaultShipingSite <- optional $ find "DefaultFOB" parseContent
        defaultMarkup <- optional $ find "DefaultMarkup" fromXML
        trackingReimbursedExpensesAsIncome <- find
            "IsTrackingReimbursedExpensesAsIncome"
            parseBool
        autoApplyingPayments <- find "IsAutoApplyingPayments" parseBool
        priceLevels <- optional $ find "PriceLevels" fromXML
        return SalesAndCustomersPreferences {..}

-- | Settings for custom price levels for specific customers.
data PriceLevelPreferences
    = PriceLevelPreferences
        { usingPriceLevels :: Bool
        -- ^ Are custom price levels enabled for the company file?
        , roundingSalesPriceUp :: Maybe Bool
        -- ^ Are amounts rounded up to the nearest whole dollar for fixed
        -- percentage price levels?
        } deriving (Show, Read)

instance FromXML PriceLevelPreferences where
    fromXML = do
        usingPriceLevels     <- find "IsUsingPriceLevels" parseBool
        roundingSalesPriceUp <- optional $ find "IsRoundingSalesPriceUp" parseBool
        return PriceLevelPreferences {..}


-- | Sales Tax settings for the company file.
data SalesTaxPreferences
    = SalesTaxPreferences
        { defaultSalesTaxCode :: OptionalListReference
        -- ^ The default tax code for sales. Refers to an item in the
        -- SalesTaxCode list.
        , salesTaxPaymentFrequency :: SalesTaxFrequency
        -- ^ Frequency of sales tax reports.
        , defaultTaxableSalesTaxCode :: OptionalListReference
        -- ^ The default tx code for taxable sales.
        , defaultNonTaxableSalesTaxCode :: OptionalListReference
        -- ^ The default tx code for non-taxable sales.
        , usingVendorTaxCode :: Maybe Bool
        -- ^ Undocumented in QuickBooks Desktop API Reference.
        , usingCustomerTaxCode :: Maybe Bool
        -- ^ Undocumented in QuickBooks Desktop API Reference.
        , usingAmountsIncludeTax :: Maybe Bool
        -- ^ Undocumented in QuickBooks Desktop API Reference.
        } deriving (Show, Read)

instance FromXML SalesTaxPreferences where
    fromXML = do
        defaultSalesTaxCode <- find "DefaultItemSalesTaxRef" fromXML
        salesTaxPaymentFrequency <- find "PaySalesTax" fromXML
        defaultTaxableSalesTaxCode <- find "DefaultTaxableSalesTaxCodeRef"
                                        fromXML
        defaultNonTaxableSalesTaxCode <- find "DefaultNonTaxableSalesTaxCodeRef"
                                            fromXML
        usingVendorTaxCode     <- optional $ find "IsUsingVendorTaxCode" parseBool
        usingCustomerTaxCode   <- optional $ find "IsUsingCustomerTaxCode" parseBool
        usingAmountsIncludeTax <- optional
            $ find "IsUsingAmountsIncludeTax" parseBool
        return SalesTaxPreferences {..}

-- | The frequency of sales tax reports.
data SalesTaxFrequency
    = MonthlyTaxReport
    | QuarerlyTaxReport
    | AnnualTaxReport
    deriving (Show, Read, Eq)

instance FromXML SalesTaxFrequency where
    fromXML = parseContent >>= \case
        "Monthly"   -> return MonthlyTaxReport
        "Quarterly" -> return QuarerlyTaxReport
        "Annually"  -> return AnnualTaxReport
        s           -> parseError $ "Expected SalesTaxFrequency, got: " <> s


-- | Time-tracking settings for the company file.
newtype TimeTrackingPreferences
    = TimeTrackingPreferences
        { firstDayOfTheWeek :: DayOfWeek
        -- ^ The first day of a weekly timesheet period.
        } deriving (Show, Read)

instance FromXML TimeTrackingPreferences where
    fromXML =
        TimeTrackingPreferences <$> find "FirstDayOfWeek" fromXML


-- | The company file's access restrictions for our application.
data CurrentAppAccessRights
    = CurrentAppAccessRights
        { automaticLoginAllowed :: Bool
        -- ^ Can we automatically log in to the company file?
        , automaticLoginUserName :: Maybe Text
        -- ^ The username allowed to automatically log in.
        , personalDataAccessAllowed :: Bool
        -- ^ Can we access sensitive personal data in the file?
        } deriving (Show, Read)

instance FromXML CurrentAppAccessRights where
    fromXML = do
        automaticLoginAllowed  <- find "IsAutomaticLoginAllowed" parseBool
        automaticLoginUserName <- optional
            $ find "AutomaticLoginUserName" parseContent
        personalDataAccessAllowed <- find "IsPersonalDataAccessAllowed" parseBool
        return CurrentAppAccessRights {..}


-- | This entire datatype is undocumented in the QuickBooks Desktop API
-- Reference.
data ItemsAndInventoryPreferences
    = ItemsAndInventoryPreferences
        { enhancedInventoryReceivingEnabled :: Maybe Bool
        , trackingSerialOrLotNumber :: Maybe Bool
        , trackingOnSalesTransactions :: Maybe Bool
        , trackingOnPurchaseTransactions :: Maybe Bool
        , trackingOnInventoryAdjustment :: Maybe Bool
        , trackingOnBuildAssembly :: Maybe Bool
        , fifoEnabled :: Maybe Bool
        , fifoEffectiveDate :: Maybe Day
        , rsbEnabled :: Maybe Bool
        , barcodeEnabled :: Maybe Bool
        } deriving (Read, Show)

instance FromXML ItemsAndInventoryPreferences where
    fromXML = do
        enhancedInventoryReceivingEnabled <- optionalBool
            "EnhancedInventoryReceivingEnabled"
        trackingSerialOrLotNumber   <- optionalBool "IsTrackingSerialOrLotNumber"
        trackingOnSalesTransactions <- optionalBool
            "IsTrackingOnSalesTransactionsEnabled"
        trackingOnPurchaseTransactions <- optionalBool
            "IsTrackingOnPurchaseTransactionsEnabled"
        trackingOnInventoryAdjustment <- optionalBool
            "IsTrackingOnInventoryAdjustmentEnabled"
        trackingOnBuildAssembly <- optionalBool "IsTrackingOnBuildAssemblyEnabled"
        fifoEnabled             <- optionalBool "FIFOEnabled"
        fifoEffectiveDate       <- optionalFind "FIFOEffectiveDate" parseDate
        rsbEnabled              <- optionalBool "IsRSBEnabled"
        barcodeEnabled          <- optionalBool "IsBarcodeEnabled"
        return ItemsAndInventoryPreferences {..}
      where
        optionalBool n = optionalFind n parseBool
        optionalFind n = optional . find n


-- ACCOUNTS

-- | The types of accounts a user can create in QuickBooks.
--
-- Note: You cannot create or modify a 'NonPosting' account since they are
-- managed exclusively by QuickBooks.
data AccountType
    = AccountsPayable
    | AccountsReceivable
    | Bank
    | CostOfGoodsSold
    | CreditCard
    | Equity
    | Expense
    | FixedAsset
    | Income
    | LongTermLiability
    | NonPosting
    | OtherAsset
    | OtherCurrentAsset
    | OtherCurrentLiability
    | OtherExpense
    | OtherIncome
    deriving (Show, Read, Eq)

-- | Use the 'Read' instance to parse an AccountType.
instance FromXML AccountType where
    fromXML = parseRead


-- | Special account types label accounts that have been automatically
-- created by QuickBooks when needed.
--
-- Note: Some special accounts cannot be overridden, but the QuickBooks
-- documentation does not specify exactly which types.
data SpecialAccountType
    = SpecialAccountsPayable
    | SpecialAccountsReceivable
    | CondenseItemAdjustmentExpenses
    | SpecialCostOfGoodsSold
    | DirectDepositLiabilities
    | Estimates
    | ExchangeGainLoss
    | InventoryAssets
    | ItemReceiptAccount
    | OpeningBalanceEquity
    | PayrollExpenses
    | PayrollLiabilities
    | PettyCash
    | PurchaseOrders
    | ReconciliationDifferences
    | RetainedEarnings
    | SalesOrders
    | SalesTaxPayable
    | UncategorizedExpenses
    | UncategorizedIncome
    | UndepositedFunds
    deriving (Show, Read, Eq)

-- | Parse using the 'Read' instance except for the values prefixed with
-- @Special@. I.e., 'SpecialAccountsPayable', 'SpecialAccountsReceivable',
-- and 'SpecialCostOfGoodsSold'.
instance FromXML SpecialAccountType where
    fromXML = parseContent >>= \case
        "AccountsPayable" -> return SpecialAccountsPayable
        "AccountsReceivable" -> return SpecialAccountsReceivable
        "CostOfGoodsSold" -> return SpecialCostOfGoodsSold
        _ -> parseRead


-- | The line on a 'TaxForm' an Account is associated with. Only relevant
-- if the company file has a 'TaxForm' specified.
data TaxLineData
    = TaxLineData
        { taxLineID :: Integer
        , taxLineName :: Maybe Text
        } deriving (Show, Read)

-- | Parse a TaxLineData from the current element.
instance FromXML TaxLineData where
    fromXML =
        TaxLineData
            <$> find "TaxLineID" parseInteger
            <*> optional (find "TaxLineName" parseContent)


-- | Account classifications for cash flow reporting.
data CashFlowClassification
    = None
    -- ^ The Account has not yet been classified.
    | Operating
    | Investing
    | Financing
    | NotApplicable
    -- ^ The Account does not qualify to be classified.
    --
    -- E.g., a bank account that tracks cash transactions.
    deriving (Show, Read)

-- | Parse a CashFlowClassification from it's 'Read' instance.
instance FromXML CashFlowClassification where
    fromXML = parseRead



-- BASICS

-- | A percentage is a decimal number representing percents like interest
-- rates. E.g., 10.5% == Percentage 10.5
newtype Percentage
    = Percentage { fromPercentage :: Rational } deriving (Show, Read, Eq, Num)

-- | Parse a Percentage from the text of the current Element.
instance FromXML Percentage where
    fromXML = Percentage <$> parseDecimal


-- | A reference to an item in a QuickBooks list, e.g. an Account, TaxCode,
-- or Currency.
--
-- TODO: Maybe list-specific newtype wrappers, e.g. AccountRef, ItemRef,
-- TaxCodeRef.
data ListReference
    = ListReference
        { listID :: Text
        , fullName :: Text
        } deriving (Show, Read, Eq, Generic)

instance Hashable ListReference

-- | Parse a ListReference from the current element's ListID & FullName
-- child elements.
instance FromXML ListReference where
    fromXML = do
        listID   <- find "ListID" parseContent
        fullName <- find "FullName" parseContent
        return ListReference {..}


-- | A potential reference to an item in a QuickBooks list.
--
-- Unlike the 'ListReference' type, these are optional fields in query
-- responses, so there may not be a ListID or FullName.
data OptionalListReference
    = OptionalListReference
        { optionalListID :: Maybe Text
        , optionalFullName :: Maybe Text
        } deriving (Show, Read, Eq)

-- | Parse an OptionalListReference from the current Element.
instance FromXML OptionalListReference where
    fromXML = do
        optionalListID   <- optional $ find "ListID" parseContent
        optionalFullName <- optional $ find "FullName" parseContent
        return OptionalListReference {..}


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
instance FromXML Address where
    fromXML = do
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
instance FromXML Month where
    fromXML = parseRead


-- | The days of the week.
data DayOfWeek
    = Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
    deriving (Read, Show, Eq)

-- | Parse a DayOfWeek using it's 'Read' instance.
instance FromXML DayOfWeek where
    fromXML = parseRead
