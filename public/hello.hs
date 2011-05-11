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

least3 xs = length xs > 3

pick2 :: [a] -> IO (a,a)
pick2 xs = do
      let nxs = length xs
      n1 <- randomRIO (0, length xs - 1)
      n2 <- randomRIO (1, length xs - 1)
      return (xs!!n1, xs !!((n1+n2) `mod` nxs))

--page :: Html 
pimg im = img ! src (fromString $ "pictures/"++im) ! width "300" ! height "200"

pinp vl = input ! type_ "radio" ! name "resp" ! value (fromString vl) >> fromString vl
pinpHidden n im = input ! type_ "hidden" 
                        ! name (fromString $ "image"++show n)
                        ! value (fromString im)
page pic1 pic2 = 
     body $ do h6 $ "hello"
               table $ tr $ do td $ pimg pic1
                               td $ pimg pic2
               H.form ! method "post" ! action "hello.cgi" $ do 
                   p "How similar are these actions?"
                   mapM_ (pinp . show) [1..5]
                   pinpHidden 1 pic1
                   pinpHidden 2 pic2
                   input ! type_ "submit" ! value "Go!"
 
writeOrAppendFile nm s 
  = do e <- doesFileExist nm
       if e then appendFile nm s
            else writeFile nm s

writeResp r im1 im2 uid = 
      lift $ do 
        tm <- toCalendarTime =<< getClockTime
        let conts =  uid ++","++r++","++im1++","++im2++","++ calendarTimeToString tm++"\n"
        writeOrAppendFile "../responses" $ conts

cgiMain :: CGI CGIResult
cgiMain = do 
--        pics <- fmap show $ lift (getDirectoryContents "pictures")
        prevResp <- getInput "resp"
        prevIm1 <- getInput "image1"
        prevIm2 <- getInput "image2"
        uid <- getCookie "twopicsid"
        case (prevResp, prevIm1, prevIm2, uid) of
          (Just r, Just im1, Just im2, Just uid) -> writeResp r im1 im2 uid
          _ -> return ()
        (pic1,pic2) <- lift $ pick2 =<< fmap (filter least3) (getDirectoryContents "pictures")
        C.output $ renderHtml $ page pic1 pic2
 
main :: IO ()
main = runCGI $ handleErrors cgiMain