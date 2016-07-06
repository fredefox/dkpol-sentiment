{-# language OverloadedStrings #-}
module Twitter
    ( getTwitterPosts
    ) where

import Control.Exception
import Web.Twitter.Conduit
--import Web.Twitter.Types.Lens
--import Data.Conduit
--import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
--import Control.Monad.IO.Class
import Control.Lens
import qualified Data.ByteString as B
import Web.Authenticate.OAuth as OA
import qualified Data.ByteString.Char8 as S8
import System.IO

getTwitterPosts nfo s = do
    mgr <- newManager tlsManagerSettings
    call nfo mgr . search . T.pack $ s
