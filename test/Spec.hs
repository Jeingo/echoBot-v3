import Test.Hspec

import App.Handle.Request as Request
import Data.Functor.Identity (Identity)

main :: IO ()
main = hspec test

handle :: Request.Handle Identity
handle = Request.Handle 
  { sendHelpText = \helpText chatId token -> return () ,
    sendKeyboard = \repeatText keyB chatId token -> return () ,
    sendMessages = \resp token numOfRepeat -> return () ,
    sendStikers = \resp token numOfRepeat -> return () }

test :: Spec
test = do
  describe "Test : Choose road after request (help , repeat , message ..)" $ do
    it "if get /help :" $ 1 + 1 `shouldBe` 2
