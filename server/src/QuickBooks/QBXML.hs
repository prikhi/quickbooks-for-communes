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
    , buildRequest

      -- * qbXML Responses
    , Response(..)

      -- ** HostQuery
    , HostData(..)
    , parseHostData
    , QBFileMode(..)

      -- ** CompanyQuery
    , CompanyData(..)
    , parseCompanyData
    , TaxForm(..)
    , Service(..)
    , AccountantCopy(..)

      -- ** PreferencesQuery
    , PreferencesData(..)
    , parsePreferencesData
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

      -- ** Basic Types
    , Percentage(..)
    , parsePercentage
    , ListReference(..)
    , parseListReference
    , Address(..)
    , parseAddress
    , Month(..)
    , parseMonth
    , DayOfWeek(..)
    , parseDayOfWeek
    )
where

import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Parser                         ( Parser
                                                , parseError
                                                , matchName
                                                , find
                                                , findAll
                                                , optional
                                                , parseContent
                                                , parseRead
                                                , parseBool
                                                , parseInteger
                                                , parseDecimal
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
--
-- TODO: Wrapper type w/ request id, status code/severity/message, & return
-- count?
data Response
    = HostResponse HostData
    | CompanyResponse CompanyData
    | PreferencesResponse PreferencesData


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

-- | Parse a 'CompanyData' value from a @CompanyRet@ 'Text.XML.Element'.
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
parsePreferencesData :: Parser PreferencesData
parsePreferencesData = matchName "PreferencesRet" $ do
    accountingPreferences <- find "AccountingPreferences"
                                  parseAccountingPreferences
    financeChargePreferences <- find "FinanceChargePreferences"
                                     parseFinanceChargePreferences
    jobsAndEstimatesPreferences <- find "JobsAndEstimatesPreferences"
                                        parseJobsAndEstimatesPreferences
    multiCurrencyPreferences <- optional
        $ find "MultiCurrencyPreferences" parseMultiCurrencyPreferences
    multiLocationInventoryPreferences <- optional $ find
        "MultiLocationInventoryPreferences"
        parseMultiLocationInventoryPreferences
    purchasesAndVendorsPreferences <- find
        "PurchasesAndVendorsPreferences"
        parsePurchasesAndVendorsPreferences
    reportsPreferences <- find "ReportsPreferences" parseReportsPreferences
    salesAndCustomersPreferences <- find "SalesAndCustomersPreferences"
                                         parseSalesAndCustomersPreferences
    salesTaxPreferences <- optional
        $ find "SalesTaxPreferences" parseSalesTaxPreferences
    timeTrackingPreferences <- optional
        $ find "TimeTrackingPreferences" parseTimeTrackingPreferences
    currentAppAccessRights <- find "CurrentAppAccessRights"
                                   parseCurrentAppAccessRights
    itemsAndInventoryPreferences <- optional $ find
        "ItemsAndInventoryPreferences"
        parseItemsAndInventoryPreferences
    return PreferencesData {..}



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
parseTaxForm :: Parser TaxForm
parseTaxForm = parseRead


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


-- | Describes the Accountant's Copy of the company file. An Accountant
-- Copy is a version of the company file that allows an Accountant to make
-- separate changes to the data while you do normal operations. The
-- Accountant's copy can be merged back into your orignal company file.
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
        , closingDate :: Maybe UTCTime
        -- ^ The company's closing date. Transaction access before this
        -- point may be restricted.
        } deriving (Show, Read)

-- | Parse an AccountingPreferences from the current Element.
parseAccountingPreferences :: Parser AccountingPreferences
parseAccountingPreferences = do
    usingAccountNumbers          <- find "IsUsingAccountNumbers" parseBool
    requiresAccounts             <- find "IsRequiringAccounts" parseBool
    usingClassTracking           <- find "IsUsingClassTracking" parseBool
    assignClassesTo <- optional $ find "AssignClassesTo" parseAssignClass
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
parseAssignClass :: Parser AssignClass
parseAssignClass = parseContent >>= \case
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
        , financeChargeAccount :: Maybe ListReference
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
parseFinanceChargePreferences :: Parser FinanceChargePreferences
parseFinanceChargePreferences = do
    annualInterestRate   <- optional $ find "AnnualInterestRate" parsePercentage
    minimumFinanceCharge <- optional $ find "MinFinanceCharge" parseDecimal
    gracePeriod          <- find "GracePeriod" parseInteger
    financeChargeAccount <- optional
        $ find "FinanceChargeAccountRef" parseListReference
    assessingForOverdueCharges <- find "IsAssessingForOverdueCharges" parseBool
    calculateChargesFrom       <- find "CalculateChargesFrom"
                                       parseCalculateFinanceChargesFrom
    markedToBePrinted <- find "IsMarkedToBePrinted" parseBool
    return FinanceChargePreferences {..}

-- | Should we calculate finance charges starting from the invoice date or
-- payment due date?
data CalculateFinanceChargesFrom
    = DueDate
    | InvoiceOrBilledDate
    deriving (Show, Read, Eq)

-- | Parse the CalculateFinanceChargesFrom type using it's 'Read' instance.
parseCalculateFinanceChargesFrom :: Parser CalculateFinanceChargesFrom
parseCalculateFinanceChargesFrom = parseRead


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
parseJobsAndEstimatesPreferences :: Parser JobsAndEstimatesPreferences
parseJobsAndEstimatesPreferences = do
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
        , homeCurrency :: Maybe ListReference
        -- ^ The currency of the country where the business is physically
        -- located.
        } deriving (Read, Show)

-- | Parse the MultiCurrency Settings from the curent Element.
parseMultiCurrencyPreferences :: Parser MultiCurrencyPreferences
parseMultiCurrencyPreferences = do
    multiCurrencyIsOn <- optional $ find "IsMultiCurrencyOn" parseBool
    homeCurrency      <- optional $ find "HomeCurrencyRef" parseListReference
    return MultiCurrencyPreferences {..}


-- | The QuickBooks Desktop API Reference doesn't have documentation on
-- this...
data MultiLocationInventoryPreferences
    = MultiLocationInventoryPreferences
        { multiLocationInventoryIsAvailable :: Maybe Bool
        , multiLocationInventoryIsEnabled :: Maybe Bool
        } deriving (Read, Show)

-- | Parse the MultiLocationInventoryPreferences from the current element.
parseMultiLocationInventoryPreferences
    :: Parser MultiLocationInventoryPreferences
parseMultiLocationInventoryPreferences = do
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
        , defaultDiscountAccount :: Maybe ListReference
        -- ^ The account where vendor discounts are tracked.
        } deriving (Show, Read)

parsePurchasesAndVendorsPreferences :: Parser PurchasesAndVendorsPreferences
parsePurchasesAndVendorsPreferences = do
    usingInventory              <- find "IsUsingInventory" parseBool
    daysBillsAreDue             <- find "DaysBillsAreDue" parseInteger
    automaticallyUsingDiscounts <- find "IsAutomaticallyUsingDiscounts"
                                        parseBool
    defaultDiscountAccount <- optional
        $ find "DefaultDiscountAccountRef" parseListReference
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

parseReportsPreferences :: Parser ReportsPreferences
parseReportsPreferences = do
    agingReportBasis   <- find "AgingReportBasis" parseAgingReportBasis
    summaryReportBasis <- find "SummaryReportBasis" parseSummaryReportBasis
    return ReportsPreferences {..}

-- | The start date for overdue days in aging reports.
data AgingReportBasis
    = AgeFromDueDate
    -- ^ Overdue days begin with the due date on the invoice.
    | AgeFromTransactionDate
    -- ^ Overdue days begin with the date the transaction was created.
    deriving (Show, Read, Eq)

-- | Parse an AgingReportBasis using it's 'Read' instance.
parseAgingReportBasis :: Parser AgingReportBasis
parseAgingReportBasis = parseRead

-- | The basis for the summary reports.
data SummaryReportBasis
    = AccrualBasis
    | CashBasis
    deriving (Show, Read, Eq)

parseSummaryReportBasis :: Parser SummaryReportBasis
parseSummaryReportBasis = parseContent >>= \case
    "Accrual" -> return AccrualBasis
    "Cash"    -> return CashBasis
    s         -> parseError $ "Expected SummaryReportBasis, got: " <> s


-- | Settings for sales & customers.
data SalesAndCustomersPreferences
    = SalesAndCustomersPreferences
        { defaultShippingMethod :: Maybe ListReference
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

parseSalesAndCustomersPreferences :: Parser SalesAndCustomersPreferences
parseSalesAndCustomersPreferences = do
    defaultShippingMethod <- optional
        $ find "DefaultShipMethodRef" parseListReference
    defaultShipingSite <- optional $ find "DefaultFOB" parseContent
    defaultMarkup <- optional $ find "DefaultMarkup" parsePercentage
    trackingReimbursedExpensesAsIncome <- find
        "IsTrackingReimbursedExpensesAsIncome"
        parseBool
    autoApplyingPayments <- find "IsAutoApplyingPayments" parseBool
    priceLevels <- optional $ find "PriceLevels" parsePriceLevelPreferences
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

parsePriceLevelPreferences :: Parser PriceLevelPreferences
parsePriceLevelPreferences = do
    usingPriceLevels     <- find "IsUsingPriceLevels" parseBool
    roundingSalesPriceUp <- optional $ find "IsRoundingSalesPriceUp" parseBool
    return PriceLevelPreferences {..}


-- | Sales Tax settings for the company file.
data SalesTaxPreferences
    = SalesTaxPreferences
        { defaultSalesTaxCode :: ListReference
        -- ^ The default tax code for sales. Refers to an item in the
        -- SalesTaxCode list.
        , salesTaxPaymentFrequency :: SalesTaxFrequency
        -- ^ Frequency of sales tax reports.
        , defaultTaxableSalesTaxCode :: ListReference
        -- ^ The default tx code for taxable sales.
        , defaultNonTaxableSalesTaxCode :: ListReference
        -- ^ The default tx code for non-taxable sales.
        , usingVendorTaxCode :: Maybe Bool
        -- ^ Undocumented in QuickBooks Desktop API Reference.
        , usingCustomerTaxCode :: Maybe Bool
        -- ^ Undocumented in QuickBooks Desktop API Reference.
        , usingAmountsIncludeTax :: Maybe Bool
        -- ^ Undocumented in QuickBooks Desktop API Reference.
        } deriving (Show, Read)

parseSalesTaxPreferences :: Parser SalesTaxPreferences
parseSalesTaxPreferences = do
    defaultSalesTaxCode <- find "DefaultItemSalesTaxRef" parseListReference
    salesTaxPaymentFrequency <- find "PaySalesTax" parseSalesTaxFrequency
    defaultTaxableSalesTaxCode <- find "DefaultTaxableSalesTaxCodeRef"
                                       parseListReference
    defaultNonTaxableSalesTaxCode <- find "DefaultNonTaxableSalesTaxCodeRef"
                                          parseListReference
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

parseSalesTaxFrequency :: Parser SalesTaxFrequency
parseSalesTaxFrequency = parseContent >>= \case
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

parseTimeTrackingPreferences :: Parser TimeTrackingPreferences
parseTimeTrackingPreferences =
    TimeTrackingPreferences <$> find "FirstDayOfWeek" parseDayOfWeek


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

parseCurrentAppAccessRights :: Parser CurrentAppAccessRights
parseCurrentAppAccessRights = do
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
        , fifoEffectiveDate :: Maybe UTCTime
        , rsbEnabled :: Maybe Bool
        , barcodeEnabled :: Maybe Bool
        } deriving (Read, Show)

parseItemsAndInventoryPreferences :: Parser ItemsAndInventoryPreferences
parseItemsAndInventoryPreferences = do
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



-- BASICS

-- | A percentage is a decimal number representing percents like interest
-- rates. E.g., 10.5% == Percentage 10.5
newtype Percentage
    = Percentage { fromPercentage :: Rational } deriving (Show, Read, Eq, Num)

-- | Parse a Percentage from the text of the current Element.
parsePercentage :: Parser Percentage
parsePercentage = Percentage <$> parseDecimal


-- | A reference to an item in a QuickBooks list. E.g., an Account or Tax
-- Code.
--
-- TODO: Maybe list-specific newtype wrappers, e.g. AccountRef, ItemRef,
-- TaxCodeRef.
data ListReference
    = ListReference
        { listID :: Maybe Text
        , fullName :: Maybe Text
        } deriving (Show, Read)

-- | Parse a ListReference from the current Element.
parseListReference :: Parser ListReference
parseListReference = do
    listID   <- optional $ find "ListID" parseContent
    fullName <- optional $ find "FullName" parseContent
    return ListReference {..}


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
parseDayOfWeek :: Parser DayOfWeek
parseDayOfWeek = parseRead
