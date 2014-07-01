{- LANGUAGE DeriveFunctor #-}
{-#LANGUAGE DeriveDataTypeable #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE PatternGuards #-}
{-#LANGUAGE Rank2Types #-}
{-#LANGUAGE ScopedTypeVariables #-}

module Twilio.Types
  ( -- * Twilio Monad
    Twilio
  , runTwilio
  , runTwilio'
    -- * Twilio Monad Transformer
  , TwilioT
  , runTwilioT
  , runTwilioT'
    -- * Twilio Exceptions
  , TwilioException(..)
    -- * Credentials
  , Credentials
  , parseCredentials
    -- ** Authentication Token
  , AuthToken
  , getAuthToken
  , parseAuthToken
    -- * System Identifiers (SIDs)
  , SID(getSID, parseSID)
  , AccountSID
  , ApplicationSID
  , CallSID
  , ConnectAppSID
  , MessageSID
  , PhoneNumberSID
    -- * List Resources
  , List(..)
  , PagingInformation(..)
  , AnsweredBy(..)
  , APIVersion(..)
  , CallDirection(..)
  , CallStatus(..)
  , PriceUnit(..)
  , Wrapper
  , wrap
  , (<&>)
  , filterEmpty
  , parseDateTime
  , safeRead
  , maybeReturn
  , maybeReturn'
  , maybeReturn''
  , request
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad (MonadPlus, liftM2, mzero)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isLower, isNumber)
import Data.Maybe (fromJust)
import Data.Text (pack, unpack)
import Data.Time.Format (parseTime)
import Data.Time.Clock (UTCTime)
import Debug.Trace (trace)
import Data.Typeable
import Network.HTTP.Client (Request, applyBasicAuth, brConsume, newManager,
  parseUrl, responseBody, withResponse)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.URI (URI, parseRelativeReference)
import System.Locale (defaultTimeLocale)

-- | The set of 'Exception's that may be thrown when attempting to make
-- requests against Twilio's REST API.
data TwilioException
  = InvalidAccountSID  !String
  | InvalidAuthToken   !String
  | InvalidCredentials !Credentials
  deriving (Show, Eq, Typeable)

instance Exception TwilioException

{- Twilio Monad and Monad Transformer -}

-- | This monad allows you to make authenticated REST API requests to Twilio
-- using your 'AccountSID' and 'AuthToken'.
type Twilio = TwilioT IO

-- | Run zero or more REST API requests to Twilio.
runTwilio :: Credentials -> Twilio a -> IO a
runTwilio = runTwilioT

-- | Parse an account SID and authentication token before running zero or more
-- REST API requests to Twilio.
runTwilio' :: IO String  -- ^ Account SID
           -> IO String  -- ^ Authentication Token
           -> Twilio a
           -> IO a
runTwilio' = runTwilioT'

-- | This monad transformer allows you to make authenticated REST API requests
-- to Twilio using your 'AccountSID' and 'AuthToken'.
newtype TwilioT m a
  = TwilioT { _runTwilioT :: (MonadThrow m, MonadIO m) => Credentials -> m a }

-- | Run zero or more REST API requests to Twilio, unwrapping the inner monad
-- @m@.
runTwilioT :: (MonadThrow m, MonadIO m) => Credentials -> TwilioT m a -> m a
runTwilioT = flip _runTwilioT

-- | Parse an account SID and authentication token before running zero or more
-- REST API requests to Twilio, unwrapping the inner monad @m@.
runTwilioT' :: (MonadThrow m, MonadIO m)
            => m String     -- ^ Account SID
            -> m String     -- ^ Authentication Token
            -> TwilioT m a
            -> m a
runTwilioT' getAccountSID getAuthToken twilio = do
  accountSID <- getAccountSID
  authToken  <- getAuthToken
  case parseCredentials accountSID authToken of
    Left  exception   -> throw exception
    Right credentials -> runTwilioT credentials twilio

instance Functor (TwilioT m) where
  fmap f ma = TwilioT $ \credentials -> do
    a <- _runTwilioT ma credentials
    return $ f a

liftTwilioT :: m a -> TwilioT m a
liftTwilioT m = TwilioT (const m)

instance Applicative m => Applicative (TwilioT m) where
  pure = liftTwilioT . pure
  f <*> v = TwilioT $ \r -> _runTwilioT f r <*> _runTwilioT v r

instance Alternative m => Alternative (TwilioT m) where
  empty = liftTwilioT empty
  m <|> n = TwilioT $ \r -> _runTwilioT m r <|> _runTwilioT n r

instance Monad m => Monad (TwilioT m) where
  return a = TwilioT (return . const a)
  m >>= k = TwilioT $ \client -> do
    a <- _runTwilioT m client
    _runTwilioT (k a) client

instance Monad m => MonadReader Credentials (TwilioT m) where
  ask = TwilioT return
  local f m = TwilioT $ _runTwilioT m . f

instance MonadTrans TwilioT where
  lift m = TwilioT $ const m

instance MonadIO m => MonadIO (TwilioT m) where
  liftIO = lift . liftIO

{- Credentials -}

-- | Your 'AccountSID' and 'AuthToken' are used to make authenticated REST API
-- requests to Twilio.
type Credentials = (AccountSID, AuthToken)

-- | Parse an account SID and authentication token to 'Credential's.
parseCredentials
  :: String             -- ^ Account SID
  -> String             -- ^ Authentication Token
  -> Either TwilioException Credentials
parseCredentials accountSID authToken = uncurry (liftM2 (,))
  ( parseSID accountSID :: Either TwilioException AccountSID
  , parseAuthToken authToken )

-- | Your authentication token is used to make authenticated REST API requests
-- to your Twilio account.
newtype AuthToken = AuthToken { getAuthToken' :: String }
  deriving (Show, Eq, Ord)

-- | Get the 'String' representation of an 'AuthToken'.
getAuthToken :: AuthToken -> String
getAuthToken = getAuthToken'

-- | Parse a 'String' to an 'AuthToken'.
parseAuthToken :: String -> Either TwilioException AuthToken
parseAuthToken token
  | length token == 32
  , all (\x -> isLower x || isNumber x) token
  = Right $ AuthToken token
  | otherwise
  = Left $ InvalidAuthToken token

maybeReturnE (Left _) = mzero
maybeReturnE (Right a) = return a

instance FromJSON AuthToken where
  parseJSON (String v) = maybeReturnE . parseAuthToken $ unpack v
  parseJSON _ = mzero

{- System Identifiers (SIDs) -}

-- | 'SID's are 34 characters long and begin with two capital letters.
class SID a where

  -- | Get the 'wrap'ped constructor of the 'SID'.
  getSIDWrapper :: Wrapper (String -> a)

  -- | Get the two capital letters that prefix the 'SID'.
  getPrefix :: Const (Char, Char) a

  -- | Get the 'String' representation of the 'SID'.
  getSID :: a -> String

  -- | Parse a 'String' to an instance of the 'SID'.
  parseSID :: String -> Either TwilioException a
  parseSID sid@(a:b:xs)
    | (a, b) == getConst (getPrefix :: Const (Char, Char) a)
    , length xs == 32
    , all (\x -> isLower x || isNumber x) xs
    = Right $ unwrap (getSIDWrapper :: Wrapper (String -> a)) sid
    | otherwise
    = Left $ InvalidAccountSID sid
  parseSID str = Left $ InvalidAccountSID str

  -- | Parse a 'JSON' 'Value' to an instance of the 'SID'.
  parseJSONToSID :: Value -> Parser a
  parseJSONToSID (String v) = case parseSID (unpack v) of
    Right sid -> return sid
    Left  _   -> mzero
  parseJSONToSID _ = mzero

-- | Account 'SID's begin with \"AC\".
newtype AccountSID = AccountSID { getAccountSID :: String }
  deriving (Show, Eq, Ord)

instance SID AccountSID where
  getSIDWrapper = wrap AccountSID
  getPrefix = Const ('A', 'C')
  getSID = getAccountSID

instance FromJSON AccountSID where
  parseJSON = parseJSONToSID

-- | Application 'SID's begin with \"AP\".
newtype ApplicationSID = ApplicationSID { getApplicationSID :: String }
  deriving (Show, Eq, Ord)

instance SID ApplicationSID where
  getSIDWrapper = wrap ApplicationSID
  getPrefix = Const ('A', 'P')
  getSID = getApplicationSID

instance FromJSON ApplicationSID where
  parseJSON = parseJSONToSID

-- | Call 'SID's begin with \"CA\".
newtype CallSID = CallSID { getCallSID :: String }
  deriving (Show, Eq, Ord)

instance SID CallSID where
  getSIDWrapper = wrap CallSID
  getPrefix = Const ('C', 'A')
  getSID = getCallSID

instance FromJSON CallSID where
  parseJSON = parseJSONToSID

-- | Connect App 'SID's begin with \"CN\".
newtype ConnectAppSID = ConnectAppSID { getConnectAppSID :: String }
  deriving (Show, Eq, Ord)

instance SID ConnectAppSID where
  getSIDWrapper = wrap ConnectAppSID
  getPrefix = Const ('C', 'N')
  getSID = getConnectAppSID

instance FromJSON ConnectAppSID where
  parseJSON = parseJSONToSID

-- | Message 'SID's begin with \"MM\".
newtype MessageSID = MessageSID { getMessageSID :: String }
  deriving (Show, Eq, Ord)

instance SID MessageSID where
  getSIDWrapper = wrap MessageSID
  getPrefix = Const ('S', 'M')
  getSID = getMessageSID

instance FromJSON MessageSID where
  parseJSON = parseJSONToSID

-- | Phone number 'SID's begin with \"PN\".
newtype PhoneNumberSID = PhoneNumberSID { getPhoneNumberSID :: String }
  deriving (Show, Eq, Ord)

instance SID PhoneNumberSID where
  getSIDWrapper = wrap PhoneNumberSID
  getPrefix = Const ('P', 'N')
  getSID = getPhoneNumberSID

instance FromJSON PhoneNumberSID where
  parseJSON = parseJSONToSID

{- List Resources -}

class FromJSON b => List a b | a -> b where

  -- | Get the 'wrap'-ed constructor of the 'List'.
  getListWrapper :: Wrapper (Maybe PagingInformation -> [b] -> a)

  {--- | The 'PagingInformation' for the 'List'.
  -- getPagingInformation :: a -> PagingInformation -}

  -- | The items in the 'List'.
  getList :: a -> [b]

  -- | The plural name for the items in the 'List'.
  getPlural :: Const String (a, b)

  -- | Parse a 'JSON' 'Value' to an instance of the 'List'.
  parseJSONToList :: Value -> Parser a
  parseJSONToList o@(Object v)
      =  unwrap (getListWrapper :: Wrapper (Maybe PagingInformation -> [b] -> a))
     <$> maybePagingInformation
     <*> (v .: pack (getConst (getPlural :: Const String (a, b))) :: Parser [b])
    where
      maybePagingInformation = case fromJSON o of
        Success pagingInformation -> return $ Just pagingInformation
        _ -> return Nothing
  parseJSONToList v = trace (show v) mzero

data PagingInformation = PagingInformation
  { -- | The current page number. Zero-indexed, so the first page is 0.
    pageNumber :: !Integer
    -- | The total number of pages.
  , numberOfPages :: !Integer
    -- | How many items are in each page.
  , pageSize :: !Integer
    -- | The total number of items in the list.
  , total :: !Integer
    -- | The position in the overall list of the first item in this page.
  , start :: !Integer
    -- | The position in the overall list of the last item in this page.
  , end :: !Integer
    -- | The 'URI' of the current page.
  , pageURI :: !URI
    -- | The 'URI' for the first page of this list.
  , firstPageURI :: !(Maybe URI)
    -- | The 'URI' for the next page of this list.
  , nextPageURI :: !(Maybe URI)
    -- | The 'URI' for the previous page of this list.
  , previousPageURI :: !(Maybe URI)
    -- | The 'URI' for the last page of this list.
  , lastPageURI :: !(Maybe URI)
  } deriving (Show, Eq, Ord)

instance FromJSON PagingInformation where
  parseJSON (Object v)
    =  PagingInformation
   <$>  v .: "page"
   <*>  v .: "num_pages"
   <*>  v .: "page_size"
   <*>  v .: "total"
   <*>  v .: "start"
   <*>  v .: "end"
   <*> (v .: "uri"               <&> parseRelativeReference
                                 >>= maybeReturn)
   <*> (v .: "first_page_uri"    <&> fmap parseRelativeReference
                                 >>= maybeReturn')
   <*> (v .: "next_page_uri"     <&> fmap parseRelativeReference
                                 >>= maybeReturn')
   <*> (v .: "previous_page_uri" <&> fmap parseRelativeReference
                                 >>= maybeReturn')
   <*> (v .: "last_page_uri"     <&> fmap parseRelativeReference
                                 >>= maybeReturn')
  parseJSON _ = mzero

maybeReturn' :: Maybe (Maybe a) -> Parser (Maybe a)
maybeReturn' Nothing = return Nothing
maybeReturn' (Just Nothing) = mzero
maybeReturn' (Just ma) = return ma

maybeReturn'' :: Maybe (Maybe (Maybe a)) -> Parser (Maybe a)
maybeReturn'' Nothing = return Nothing
maybeReturn'' (Just Nothing) = return Nothing
maybeReturn'' (Just (Just Nothing)) = mzero
maybeReturn'' (Just (Just ma)) = return ma

-- NOTE(markandrus): Types used by Calls. If any of these end up being Calls-
-- specific, move them. APIVersion probably is not.

data PriceUnit
  = USD
  | EUR
  | JPY
  | OtherPriceUnit !String
  deriving Eq

-- NOTE(markandrus): This should be all ISO 4217 currencies. Maybe make a
-- separate library for these.
instance Show PriceUnit where
  show USD = "USD"
  show EUR = "EUR"
  show JPY = "JPY"
  show (OtherPriceUnit pu) = pu

instance FromJSON PriceUnit where
  parseJSON (String "USD") = return USD
  parseJSON (String "EUR") = return EUR
  parseJSON (String "JPY") = return JPY
  parseJSON (String t) = return . OtherPriceUnit $ unpack t
  parseJSON _ = mzero

data APIVersion
  = API20100401
  | API20080801
  deriving Eq

instance Show APIVersion where
  show API20100401 = "2010-04-01"
  show API20080801 = "2008-08-01"

instance FromJSON APIVersion where
  parseJSON (String "2010-04-01") = return API20100401
  parseJSON (String "2008-08-01") = return API20080801
  parseJSON _ = mzero

data CallStatus
  = Queued
  | Ringing
  | InProgress
  | Canceled
  | Completed
  | Failed
  | Busy
  | NoAnswer
  deriving Eq

instance Show CallStatus where
  show Queued     = "queued"
  show Ringing    = "ringing"
  show InProgress = "in-progress"
  show Canceled   = "canceled"
  show Completed  = "completed"
  show Failed     = "failed"
  show Busy       = "busy"
  show NoAnswer   = "no-answer"

instance FromJSON CallStatus where
  parseJSON (String "queued")      = return Queued
  parseJSON (String "ringing")     = return Ringing
  parseJSON (String "in-progress") = return InProgress
  parseJSON (String "canceled")    = return Canceled
  parseJSON (String "completed")   = return Completed
  parseJSON (String "failed")      = return Failed
  parseJSON (String "busy")        = return Busy
  parseJSON (String "no-answer")   = return NoAnswer
  parseJSON _ = mzero

data CallDirection
  = Inbound
  | OutboundAPI
  | OutboundDial
  deriving Eq

instance Show CallDirection where
  show Inbound      = "inbound"
  show OutboundAPI  = "outbound-api"
  show OutboundDial = "outbound-dial"

instance FromJSON CallDirection where
  parseJSON (String "inbound")       = return Inbound
  parseJSON (String "outbound-api")  = return OutboundAPI
  parseJSON (String "outbound-dial") = return OutboundDial
  parseJSON _ = mzero

data AnsweredBy
  = Human
  | Machine
  deriving Eq

instance Show AnsweredBy where
  show Human   = "human"
  show Machine = "machine"

instance FromJSON AnsweredBy where
  parseJSON (String "human")   = return Human
  parseJSON (String "machine") = return Machine
  parseJSON _ = mzero

newtype Wrapper a = Wrapper { unwrap :: a }

-- | 'wrap's a value so as not to break encapsulation.
wrap :: a -> Wrapper a
wrap = Wrapper

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

filterEmpty :: String -> Maybe String
filterEmpty "" = Nothing
filterEmpty s = Just s

parseDateTime :: (Monad m, MonadPlus m) => String -> m UTCTime
parseDateTime s =
  case parseTime defaultTimeLocale "%a, %d %b %Y %T %z" s of
    Just dateTime -> return dateTime
    Nothing       -> mzero

safeRead :: (Monad m, MonadPlus m, Read a) => String -> m a
safeRead s = case reads s of
  [(a, "")] -> return a
  _         -> mzero

maybeReturn :: (Monad m, MonadPlus m) => Maybe a -> m a
maybeReturn (Just a) = return a
maybeReturn Nothing  = mzero

request :: (MonadIO m, FromJSON a) => String -> TwilioT m a
request resourceURL = do
    manager <- liftIO (newManager tlsManagerSettings)
    (user, pass) <- ask
    let request = fromJust $ do
        req <- parseUrl resourceURL
        return $ applyBasicAuth (C.pack $ getSID user)
                                (C.pack $ getAuthToken pass) req
    liftIO $ withResponse request manager $ \response -> do
      bs <- LBS.fromChunks <$> brConsume (responseBody response)
      putStrLn . C.unpack $ LBS.toStrict bs
      return . fromJust $ decode bs
