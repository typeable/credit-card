{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ||" #-}

module Data.CreditCard
  ( -- * Types
    CreditCardDate(..)
  , ccdYear
  , ccdMonth
  , cardExpirationDay
  , CreditCardNumber(..)
  , _CreditCardNumber
  , CreditCardPAN(..)
  , mkCreditCardPan
  , panTrailingDigits
  , CreditCardSecurity(..)
  , parseCreditCardSecurity
  , CreditCard(..)
  , ccNumber
  , ccHolderName
  , ccSecurity
  , ccExpirationDate
  , ccZipCode
  , ZipCode(..)
  , _ZipCode
  , CreditCardType(..)
  , _Visa
  , _MasterCard
  , _AmericanExpress
  , _DiscoverCard
  , _DinersClub
  , _JCB
  , _UnionPay
  , CreditCardError(..)
  , _UnknownCardNumber
  , _InvalidCardNumber
  -- * functions
  , parseCardNumber
  , renderCardNumber
  , apiCreditCardNumber
  , formatExpiryDate
  , parseExpirationDate
  , creditCardType
  , guessCreditCardType
  , guessPANCardType
  , getLuhnDigit
  , checkLuhnDigit
  , isValidCCNumber
  ) where

import Control.Lens hiding (elements)

import Control.Monad (guard, (<=<), replicateM)
import Data.Aeson as Aeson
import Data.Aeson.Inflections (defaultFieldLabelModifier)
import Data.Binary (Binary)
import Data.Char (isDigit)
import Data.Digit
import Data.Either (isRight)
import Data.Foldable as F
import Data.Ix
import Data.List as L
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
#ifdef BACKEND
import Data.Proxy
import Data.OpenApi as OpenApi
  ( ToSchema(..), SchemaOptions(..), defaultSchemaOptions
  , genericDeclareNamedSchema)
#ifdef USE_POSTGRES
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
#endif
#endif
import GHC.Generics (Generic)
import Numeric.Natural
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
#ifdef USE_STORE
import qualified Data.Store
import qualified Data.Store.Internal
#endif


newtype ZipCode = ZipCode
  { unZipCode :: Text }
  deriving stock (Show, Eq, Generic)
#ifdef BACKEND
  deriving newtype (ToJSON, FromJSON, ToSchema)
#else
  deriving newtype (ToJSON, FromJSON)
#endif
#ifdef USE_STORE
  deriving anyclass Data.Store.Store
#endif

makePrisms ''ZipCode

data CreditCardDate = CreditCardDate
  { _ccdYear  :: Int
    -- ^ Full year with century, e.g. 2000, 2017, 2020 ...
  , _ccdMonth :: Int
    -- ^ Month number in range [1..12]
  } deriving (Eq, Show, Generic)
#ifdef USE_STORE
  deriving anyclass Data.Store.Store
#endif

makeLenses ''CreditCardDate

formatExpiryDate :: CreditCardDate -> Text
formatExpiryDate ccd = s2 (ccd ^. ccdMonth) <> s2 (ccd ^. ccdYear - 2000)
  where
    s2 = T.justifyRight 2 '0' . T.pack . show

-- NOTE: This function is supposed to be called 'parseExpiryDate',
-- but we already have a function with the same name in b2b/api
-- that works slightly differently, i.e. it parses the date in
-- MM/YY format, not MMYY. Therefore _expiration_, not _expiry_ for now.
parseExpirationDate :: Text -> Maybe CreditCardDate
parseExpirationDate txt = do
  guard (T.length txt == 4)
  [monthTxt, yearTxt] <- case T.chunksOf 2 txt of
    [m,y] -> Just [m,y]
    _ -> Nothing
  month <- preview (_DigitsText . to digitsToNum) monthTxt
  guard (inRange (1,12) month)
  year <- (+2000) <$> preview (_DigitsText . to digitsToNum) yearTxt
  -- Look, I know this code might very well survive after 2100.
  -- But I'll probably be dead by then, so I don't care.
  guard (inRange (2000,2099) year)
  pure (CreditCardDate year month)

instance FromJSON CreditCardDate where
  parseJSON = withText "CreditCardDate" $ \s ->
    case parseExpirationDate s of
      Just d -> pure d
      Nothing -> fail "Cannot parse Credit Card Date"

instance ToJSON CreditCardDate where
  toJSON = toJSON . formatExpiryDate

instance Arbitrary CreditCardDate where
  arbitrary = CreditCardDate <$> year <*> month
    where
      month = choose (1,12)
      year = choose (2000,2099)

-- | Takes exact day when CC expires (the last day of expiration
-- month)
cardExpirationDay :: CreditCardDate -> Day
cardExpirationDay ccd = fromGregorian year month day
  where
    year = ccd ^. ccdYear . to fromIntegral
    month = ccd ^. ccdMonth
    day = gregorianMonthLength year month

newtype CreditCardNumber = CreditCardNumber
  { unCreditCardNumber :: NonEmpty Digit }
  deriving (Eq, Generic)

makePrisms ''CreditCardNumber

renderCardNumber :: CreditCardNumber -> Text
renderCardNumber = view (_CreditCardNumber . re _DigitsText)

parseCardNumber :: Text -> Maybe CreditCardNumber
parseCardNumber = preview (_DigitsText . from _CreditCardNumber)

apiCreditCardNumber :: Prism' Text CreditCardNumber
apiCreditCardNumber = prism' renderCardNumber parseCardNumber

#ifdef USE_STORE
instance Data.Store.Store CreditCardNumber where
  poke = Data.Store.poke . renderCardNumber
  peek = do
    maybeNumber <- parseCardNumber <$> Data.Store.peek
    maybe (fail "invalid stored credit card number") pure maybeNumber
  size = Data.Store.VarSize (Data.Store.Internal.getSize . renderCardNumber)
#endif

instance FromJSON CreditCardNumber where
  parseJSON = withText "CreditCardNumber" $ \s ->
    case parseCardNumber s of
      Just c -> pure c
      Nothing -> fail "Cannot parse Credit Card Number"

instance ToJSON CreditCardNumber where
  toJSON = toJSON . renderCardNumber

-- | This instance specifically hides CC number replacing it with PAN.
--   For example, 1234567890123456 becomes 1234********3456.
instance Show CreditCardNumber where
  show = show . mkCreditCardPan

instance Arbitrary CreditCardNumber where
  arbitrary = CreditCardNumber . NE.fromList <$> listOf1 arbitrary

-- | Credit card with '*' character instead of digits except first and
-- last 4 digits. It is safe to store this in DB and in logs
newtype CreditCardPAN = CreditCardPAN
  { unCreditCardPAN :: Text
  } deriving (Eq, Ord)
    deriving newtype (Show, FromJSON, ToJSON)

panTrailingDigits :: CreditCardPAN -> Text
panTrailingDigits (CreditCardPAN txt) = T.takeEnd 4 txt

-- | Create a bogus card number from PAN.
--   This may not seem terribly useful on its own,
--   but this allows to reuse some functions working
--   with CC numbers.
panToBogusNumber :: CreditCardPAN -> Maybe CreditCardNumber
panToBogusNumber (CreditCardPAN txt) = do
  leading <- preview _DigitsText $ T.take 4 txt
  trailing <- preview _DigitsText $ T.takeEnd 4 txt
  let middle = NE.fromList $ replicate (T.length txt - 8) D0
  return $ CreditCardNumber $ sconcat $ NE.fromList [leading, middle, trailing]


mkCreditCardPan :: CreditCardNumber -> CreditCardPAN
mkCreditCardPan n = CreditCardPAN $ firstDigits <> stars <> lastDigits
  where
    ntext = renderCardNumber n
    firstDigits = T.take 4 ntext
    lastDigits = T.reverse $ T.take 4 $ T.reverse ntext
    stars = T.pack $ L.replicate (T.length ntext - 8) '*'

newtype CreditCardSecurity = CreditCardSecurity
  { unCCSecurity :: Text }
  deriving (Eq, Ord, Generic)
#ifdef BACKEND
  deriving newtype (ToJSON, FromJSON, ToSchema)
#else
  deriving newtype (ToJSON, FromJSON)
#endif
#ifdef USE_STORE
  deriving anyclass Data.Store.Store
#endif

instance Arbitrary CreditCardSecurity where
  arbitrary = CreditCardSecurity . T.pack . concatMap (show :: Int -> String)
    <$> replicateM 3 (choose (0, 9))

-- | Show instance for 'CreditCardSecurity' doesn't actually show
--   anything. It replaces all characters with '*'.
instance Show CreditCardSecurity where
  show (CreditCardSecurity t) = show $ T.map (const '*') t

makePrisms ''CreditCardSecurity

-- | Parse and validate credit card security code.
--   The code must be a three-digit sequence.
parseCreditCardSecurity :: Text -> Maybe CreditCardSecurity
parseCreditCardSecurity txt
  | isValid = pure $ CreditCardSecurity txt
  | otherwise = Nothing
  where
    isValid = (3, 4) `inRange` T.length txt && T.all isDigit txt

data CreditCard = CreditCard
  { _ccNumber         :: !CreditCardNumber
  , _ccHolderName     :: !Text
  , _ccSecurity       :: !(Maybe CreditCardSecurity)
  , _ccExpirationDate :: !CreditCardDate
  , _ccZipCode        :: !(Maybe ZipCode)
  } deriving (Eq, Generic, Show)
#ifdef USE_STORE
  deriving anyclass Data.Store.Store
#endif

instance FromJSON CreditCard where
  parseJSON =
    genericParseJSON defaultOptions
      { Aeson.fieldLabelModifier = defaultFieldLabelModifier }
instance ToJSON CreditCard where
  toJSON =
    genericToJSON defaultOptions
      { Aeson.fieldLabelModifier = defaultFieldLabelModifier }

instance Arbitrary CreditCard where
  arbitrary =
    CreditCard <$> arbitrary <*> cardholder <*> security <*> arbitrary <*> zipc
    where
      cardholder = elements ["Mr Cardholder", "Mrs Cardholder"]
      security = elements [Nothing, pure $ CreditCardSecurity "1337"]
      zipc = elements [Nothing, pure $ ZipCode "12345"]

makeLenses ''CreditCard

data CreditCardType
  = Visa
  | MasterCard
  | AmericanExpress
  | DiscoverCard
  | DinersClub
  | JCB
  | UnionPay
  deriving (Eq, Show, Generic, Enum, Bounded)

instance Binary CreditCardType

makePrisms ''CreditCardType

instance Arbitrary CreditCardType where
  arbitrary = elements [minBound..maxBound]

instance FromJSON CreditCardType where
  parseJSON =
    genericParseJSON defaultOptions

instance ToJSON CreditCardType where
  toJSON =
    genericToJSON defaultOptions

data CreditCardError
  = UnknownCardNumber
  | InvalidCardNumber
  deriving (Eq, Show)

makePrisms ''CreditCardError

getDigits :: Natural -> [Natural]
getDigits = F.toList . fmap digitToNum . unDigits . natToDigits

-- | Calculate luhn checksum digit
getLuhnDigit :: [Digit] -> Digit
getLuhnDigit n = digitFromNumMod10 (total * 9)
  where
    total = sum $ L.zipWith multEven [0..] $ L.reverse n
    multEven (i :: Int) (digitToNum -> a)
      | even i    = sum $ getDigits (a * 2)
      | otherwise = a

-- | Checks luhn checksum of card number
checkLuhnDigit :: CreditCardNumber -> Bool
checkLuhnDigit (CreditCardNumber n) =
  NE.last n == getLuhnDigit (NE.init n)

-- | Determines credit card by it's number
guessCreditCardType :: CreditCardNumber -> Maybe CreditCardType
guessCreditCardType n
  | isMasterCard   = Just MasterCard
  | isAmEx         = Just AmericanExpress
  | isDiscoverCard = Just DiscoverCard
  | isDinersClub   = Just DinersClub
  | isJCB          = Just JCB
  | isVisa         = Just Visa  --  NOTE: starts with just 4? realy?
  | isUnionPay     = Just UnionPay
  | otherwise      = Nothing
  where
    isVisa         = (4 == firstDigits 1) && (digitsLength `elem` [13,16,19])
    isMasterCard   = or
      [ inRange (51, 55) (firstDigits 2) && (digitsLength == 16)
      , inRange (2221, 2720) (firstDigits 4) && (digitsLength == 16)]
    isAmEx         = (firstDigits 2 `elem` [34, 37]) && (digitsLength == 15)
    isUnionPay     = (firstDigits 2 == 62) && (digitsLength `elem` [16,17,18,19])
    isDiscoverCard = or
      [ firstDigits 4 == 6011
      , inRange (644, 649) (firstDigits 3)
      , inRange (622126, 622925) (firstDigits 6)
      , firstDigits 2 == 65]
      && (digitsLength `elem` [16, 19])
    isDinersClub = or
      [ inRange (300, 305) (firstDigits 3) && digitsLength == 14
      , firstDigits 3 == 309 && digitsLength == 14
      , firstDigits 2 == 36 && digitsLength == 14
      , inRange (38, 39) (firstDigits 2) && digitsLength == 14
      , firstDigits 2 `elem` [54, 55] && digitsLength == 16]
    isJCB = or
      [ inRange (3528, 3589) (firstDigits 4) && digitsLength `elem` [15, 16]
      , firstDigits 4 `elem` [1800, 2131] && digitsLength == 15
      , firstDigits 6 == 357266 && digitsLength == 19]
    digitsLength   = L.length $ n ^. _CreditCardNumber
    firstDigits x  = poly10 @Integer $ L.take x
      $ n ^.. _CreditCardNumber . folded . to digitToNum

creditCardType :: CreditCardNumber -> Either CreditCardError CreditCardType
creditCardType n
  | checkLuhnDigit n = case guessCreditCardType n of
    Nothing -> Left UnknownCardNumber
    Just t  -> Right t
  | otherwise = Left InvalidCardNumber

isValidCCNumber :: CreditCardNumber -> Bool
isValidCCNumber = isRight . creditCardType

guessPANCardType :: CreditCardPAN -> Maybe CreditCardType
guessPANCardType = guessCreditCardType <=< panToBogusNumber

#ifdef BACKEND
#ifdef USE_POSTGRES
deriving instance FromField CreditCardPAN
deriving instance ToField CreditCardPAN
instance ToField CreditCardType where
  toField cardType = toField $ case cardType of
    Visa           -> "Visa"
    MasterCard     -> "MasterCard"
    AmericanExpress-> "AmericanExpress"
    DiscoverCard   -> "DiscoverCard"
    DinersClub     -> "DinersClub"
    JCB            -> "JCB"
    UnionPay       -> "UnionPay" :: Text

instance FromField CreditCardType where
  fromField f mbs = do
    txt <- fromField f mbs
    case T.unpack txt of
        "Visa"           -> return Visa
        "MasterCard"     -> return MasterCard
        "AmericanExpress"-> return AmericanExpress
        "DiscoverCard"   -> return DiscoverCard
        "DinersClub"     -> return DinersClub
        "JCB"            -> return JCB
        "UnionPay"       -> return UnionPay
        _ -> returnError ConversionFailed f ("Invalid credit card type: " ++ T.unpack txt)
#endif
#endif

#ifdef BACKEND
instance ToSchema CreditCardDate where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToSchema CreditCardNumber where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)

instance ToSchema CreditCard where
  declareNamedSchema = genericDeclareNamedSchema opts
    where
      opts = defaultSchemaOptions
        { OpenApi.fieldLabelModifier = defaultFieldLabelModifier }

instance ToSchema CreditCardType where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions
#endif
