module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Twilio.Messages as TM
import Twilio.Message as Message
import Twilio.Types

main :: IO ()
main = do
  x <- runTwilio' (getEnv "ACCOUNT_SID")
                  (getEnv "AUTH_TOKEN")
         $ TM.post msg
  print (Message.sid x)

  where
    msg = TM.SendMessage
      { TM.sendTo = "+19192131776"
      , TM.sendFrom = "+13304003807"
      , TM.sendBody = "ian's test message"
      }
