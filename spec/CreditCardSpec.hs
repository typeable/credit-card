module CreditCardSpec (spec) where

import Data.Aeson
import Data.CreditCard
import Data.Foldable
import Data.Maybe
import Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck


mcNumbers :: [Text]
mcNumbers =
  [ "5555555555554444"
  , "5105105105105100"
  , "5500005555555559"
  , "5555555555555557"
  , "5454545454545454"
  , "5555515555555551"
  , "5115915115915118" ]

visaNumbers :: [Text]
visaNumbers =
  [ "4007000000027"
  , "4012888888881881"
  , "4061724061724061"
  , "4110144110144115"
  , "4111111111111111"
  , "4114360123456785"
  , "4222222222222"
  , "4444414444444441"
  , "4444424444444440"
  , "4444444444444448" ]

aMeXNumbers :: [Text]
aMeXNumbers =
  [ "371449635398431"
  , "343434343434343"
  , "371144371144376"
  , "341134113411347" ]

discoverNumbers :: [Text]
discoverNumbers =
  [ "6011016011016011"
  , "6011111111111117"
  , "6011000000000004"
  , "6011000990139424" ]

dinersNumbers :: [Text]
dinersNumbers =
  [ "38520000023237"
  , "30000000000004" ]

jcbNumbers :: [Text]
jcbNumbers =
  [ "3530111333300000"
  , "3566002020360505"
  , "3566003566003566" ]

wrongNumbers :: [Text]
wrongNumbers =
  [ "5555455555554444"
  , "5105205105105100"
  , "4004000000027"
  , "4013888888881881"
  , "371549635398431"
  , "343534343434343"
  , "6012016011016011"
  , "6061111111111117"
  , "38522000023237"
  , "30000000000005"
  , "353011133330000"
  , "3566002028360505" ]

checkNumbers :: [Text] -> CreditCardType -> Spec
checkNumbers ns t = describe (show t) $ do
  for_ ns $ \n -> it (T.unpack n) $ do
    creditCardType (fromJust $ parseCardNumber n) `shouldBe` Right t

spec :: Spec
spec = describe "Credit card number validation" $ do
  checkNumbers mcNumbers MasterCard
  checkNumbers visaNumbers Visa
  checkNumbers aMeXNumbers AmericanExpress
  checkNumbers discoverNumbers DiscoverCard
  checkNumbers dinersNumbers DinersClub
  checkNumbers jcbNumbers JCB
  it "Wrong numbers" $ for_ wrongNumbers $ \n -> do
    creditCardType (fromJust $ parseCardNumber n) `shouldBe`
      Left InvalidCardNumber
  prop "Card reader" $ \(cn :: CreditCardNumber) ->
    (parseCardNumber . renderCardNumber) cn == Just cn
  prop "Expiry date" $ \(d :: CreditCardDate) ->
    (parseExpirationDate . formatExpiryDate) d == Just d
  prop "JSON instances isomorphism" $ \(cc :: CreditCard) ->
    (decode . encode) cc == Just cc
