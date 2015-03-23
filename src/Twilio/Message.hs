{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}

module Twilio.Message
  ( -- * Resource
    Message(..)
  , get
  , get'
    -- * Types
  , MessageDirection(..)
  , MessageStatus(..)
  ) where

import Twilio.Types hiding (CallStatus(..), CallDirection(..))

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Time.Clock (UTCTime)
import Network.URI (URI, parseRelativeReference)

{- Resource -}

data Message = Message
  { sid         :: !MessageSID
  , dateCreated :: !UTCTime
  , dateUpdated :: !UTCTime
  , dateSent    :: !(Maybe UTCTime) -- TODO: this causes parsing problems
  , accountSID  :: !AccountSID
  , to          :: !String
  , from        :: !String
  , body        :: !String
  , status      :: !MessageStatus
--  , numSegments :: !Integer
  , direction   :: !MessageDirection
--  , price       :: !Double
  , priceUnit   :: !PriceUnit
  , apiVersion  :: !APIVersion
  , uri         :: !URI
  } deriving (Show, Eq)

instance FromJSON Message where
  parseJSON (Object v) = Message
    <$>  v .: "sid"
    <*> (v .: "date_created" >>= parseDateTime)
    <*> (v .: "date_updated" >>= parseDateTime)
    <*> (v .: "date_sent"    >>= parseMaybeDateTime)
    <*>  v .: "account_sid"
    <*>  v .: "to"
    <*>  v .: "from"
    <*>  v .: "body"
    <*>  v .: "status"
--     <*> (v .: "num_segments" <&> fmap safeRead
--                              >>= maybeReturn)
    <*>  v .: "direction"
--    <*> (v .: "price"        <&> fmap safeRead
--                             >>= maybeReturn')
    <*>  v .: "price_unit"
    <*>  v .: "api_version"
    <*> (v .: "uri"          <&> parseRelativeReference
                             >>= maybeReturn)
  where
    parseMaybeDateTime :: a -> Parser (Maybe UTCTime)
    parseMaybeDateTime (String a) = parseDateTime a
    parseMaybeDateTime _ -> return Nothing

  parseJSON _ = mzero

-- | Get a 'Message' by 'MessageSID'.
get :: (MonadThrow m, MonadIO m) => MessageSID -> TwilioT m Message
get messageSID
  = requestForAccount $ "/Messages/" ++ getSID messageSID ++ ".json"

-- | Get an account's 'Message' by 'MessageSID'.
get' :: (MonadThrow m, MonadIO m)
     => AccountSID
     -> MessageSID
     -> TwilioT m Message
get' accountSID messageSID = forAccount accountSID $ get messageSID

{- Types -}

data MessageDirection
  = Inbound
  | OutboundAPI
  | OutboundCall
  | OutboundReply
  deriving Eq

instance Show MessageDirection where
  show Inbound       = "inbound"
  show OutboundAPI   = "outbound-api"
  show OutboundCall  = "outbound-call"
  show OutboundReply = "outbound-reply"

instance FromJSON MessageDirection where
  parseJSON (String "inbound")        = return Inbound
  parseJSON (String "outbound-api")   = return OutboundAPI
  parseJSON (String "outbound-call")  = return OutboundCall
  parseJSON (String "outbound-reply") = return OutboundReply
  parseJSON _ = mzero

data MessageStatus
  = Queued
  | Sending
  | Sent
  | Failed
  | Received
  | Delivered
  deriving Eq

instance Show MessageStatus where
  show Queued    = "queued"
  show Sending   = "sending"
  show Sent      = "sent"
  show Failed    = "failed"
  show Received  = "received"
  show Delivered = "delivered"

instance FromJSON MessageStatus where
  parseJSON (String "queued")    = return Queued
  parseJSON (String "sending")   = return Sending
  parseJSON (String "sent")      = return Sent
  parseJSON (String "failed")    = return Failed
  parseJSON (String "received")  = return Received
  parseJSON (String "delivered") = return Delivered
  parseJSON _ = mzero
