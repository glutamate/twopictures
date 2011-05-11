{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Network.CGI as C
--import Text.XHtml
import System.Directory 
import Control.Monad.Trans
import System.Random

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.String 
import Prelude as P
import System.Time

import Data.String

page  = 
     body $ do h1 $ "Welcome"
               "Press "
               a "here " ! href "hello.cgi"
               " to begin"
 
writeOrAppendFile nm s 
  = do e <- doesFileExist nm
       if e then appendFile nm s
            else writeFile nm s


writeResp inps n = 
      lift $ do 
        writeOrAppendFile "../userids" $ show (n,inps) ++"\n"

cgiMain :: CGI CGIResult
cgiMain = do 
--        pics <- fmap show $ lift (getDirectoryContents "pictures")
        imps <- getInputs 
        rndn <- lift $ randomRIO (0::Int, 999999)
        writeResp imps rndn
        let cook = newCookie "twopicsid" $ show rndn
        setCookie cook
        C.output $ renderHtml $ page 
 
main :: IO ()
main = runCGI $ handleErrors cgiMain