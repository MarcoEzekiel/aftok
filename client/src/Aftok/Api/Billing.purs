module Aftok.Api.Billing where

import Prelude
import Control.Alternative ((<|>))
-- import Control.Monad.Error.Class (throwError)
-- import Control.Monad.Except.Trans (runExceptT, except, withExceptT)
-- import Control.Monad.Except.Trans (ExceptT, runExceptT, except, withExceptT)
-- import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError(..), (.:), (.:?))
import Data.Argonaut.Encode (encodeJson)
import Data.BigInt (toNumber, fromNumber) as BigInt
import Data.DateTime (DateTime)
-- import Data.DateTime.Instant (Instant, toDateTime)
import Data.Either (Either(..), note)
import Foreign.Object (Object)
-- import Data.Foldable (class Foldable, foldr, foldl, foldMapDefaultR)
-- import Data.Functor.Compose (Compose(..))
-- import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
-- import Data.Ratio (Ratio, (%))
import Data.Time.Duration (Hours(..), Days(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Data.UUID (UUID, parseUUID)
-- import Effect (Effect)
import Effect.Aff (Aff)
-- import Effect.Class as EC
-- import Foreign.Object (Object)
import Affjax (post, get)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
-- import Affjax.StatusCode (StatusCode(..))
import Aftok.Types
  ( ProjectId, pidStr )
import Aftok.Zcash
  ( Zatoshi(..) )
import Aftok.Api.Types
  (APIError(..))
import Aftok.Api.Json
  ( parseResponse
  )

newtype BillableId
  = BillableId UUID

derive instance billableIdEq :: Eq BillableId

derive instance billableIdOrd :: Ord BillableId

derive instance billableIdNewtype :: Newtype BillableId _

parseBillableIdJSON :: String -> Either JsonDecodeError BillableId
parseBillableIdJSON uuidStr = 
    BillableId <$> note (TypeMismatch"Failed to decode billable UUID") (parseUUID uuidStr)

instance billableIdDecodeJson :: DecodeJson BillableId where
  decodeJson json = do
    obj <- decodeJson json
    parseBillableIdJSON =<< obj .: "billableId"

data Recurrence
  = Annually
  | Monthly Int
  | Weekly Int
  | OneTime

recurrenceJSON :: Recurrence -> Json
recurrenceJSON = case _ of
  Annually -> encodeJson $ { annually: {} }
  Monthly i -> encodeJson $ { monthly: i }
  Weekly i -> encodeJson $ { weekly: i }
  OneTime -> encodeJson $ { onetime: {} }

type Billable = 
  { name :: String
  , description :: String
  , message :: String
  , recurrence :: Recurrence
  , amount :: Zatoshi
  , gracePeriod :: Days
  , expiryPeriod :: Hours
  }

billableJSON :: Billable -> Json
billableJSON b = encodeJson $
  { schemaVersion: "1.0"
  , name: b.name
  , description: b.description
  , message: b.message
  , recurrence: recurrenceJSON b.recurrence
  , currency: "ZEC"
  , amount: BigInt.toNumber (unwrap b.amount)
  -- API requires grace period as days
  , gracePeriod: unwrap b.gracePeriod
  -- API requires expiry period as seconds
  , requestExpiryPeriod: unwrap b.expiryPeriod * 60.0 * 60.0
  }

parseRecurrence :: Json -> Either JsonDecodeError Recurrence
parseRecurrence json = do
  obj <- decodeJson json
  let annually = traverse (map \(_ :: Unit) -> Annually) (obj .:? "annually")
      monthly =  traverse (map Monthly) (obj .:? "monthly")
      weekly =   traverse (map Weekly) (obj .:? "weekly")
      onetime =  traverse (map \(_ :: Unit) -> OneTime) (obj .:? "onetime")
  join $ note (UnexpectedValue json) (annually <|> monthly <|> weekly <|> onetime)
      


parseBillableJSON :: Object Json -> Either JsonDecodeError (Tuple BillableId Billable)
parseBillableJSON obj = do
  billableId <- parseBillableIdJSON =<< obj .: "billableId"
  bobj <- obj .: "billable"
  name :: String <- bobj .: "name"
  description :: String <- bobj .: "description"
  let message = ""
  recurrence :: Recurrence <- parseRecurrence =<< bobj .: "recurrence"
  amount <- 
    map Zatoshi 
      $   (note (TypeMismatch "Failed to decode as Zatoshi") <<< BigInt.fromNumber) 
      =<< (_ .: "zatoshi")
      =<< (bobj .: "amount")
  gracePeriod <- Days <$> bobj .: "gracePeriod"
  expiryPeriod <- Hours <$> bobj .: "gracePeriod"
  pure $ Tuple billableId {name, description, message, recurrence, amount, gracePeriod, expiryPeriod }


newtype PaymentRequestId
  = PaymentRequestId UUID

derive instance paymentRequestIdEq :: Eq PaymentRequestId

derive instance paymentRequestIdOrd :: Ord PaymentRequestId

derive instance paymentRequestIdNewtype :: Newtype PaymentRequestId _

instance paymentRequestIdDecodeJson :: DecodeJson PaymentRequestId where
  decodeJson json = do
    uuidStr <- decodeJson json
    PaymentRequestId <$> note (TypeMismatch "Failed to decode paymentRequest UUID") (parseUUID uuidStr)

newtype PaymentRequest' t = PaymentRequest
  {
  }

type PaymentRequest = PaymentRequest' DateTime

createBillable :: ProjectId -> Billable -> Aff (Either APIError BillableId)
createBillable pid billable = do
  let body = RB.json $ billableJSON billable
  response <- post RF.json ("/api/projects/" <> pidStr pid <> "/billables") (Just body)
  parseResponse decodeJson response

listProjectBillables :: ProjectId -> Aff (Either APIError (Array (Tuple BillableId Billable)))
listProjectBillables pid = do
  response <- get RF.json ("/api/projects/" <> pidStr pid <> "/billables")
  parseResponse (traverse parseBillableJSON <=< decodeJson) response

listUnpaidPaymentRequests :: BillableId -> Aff (Either APIError (Array (Tuple PaymentRequestId PaymentRequest)))
listUnpaidPaymentRequests billId = pure $ Left Forbidden
