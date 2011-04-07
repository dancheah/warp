{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Blaze.ByteString.Builder
import Data.Monoid
import Data.Enumerator (run_, enumList, ($$))
import Network.HTTP.Types
import Blaze.ByteString.Builder.Char.Utf8
import qualified Data.ByteString as B

main = run 3000 app

app1 req = return $ 
            case rawPathInfo req of 
                "/file/nolen" -> fileNoLen
                _ -> index $ rawPathInfo req

app req = return $ 
   case rawPathInfo req of
       "/builder/withlen" -> builderWithLen
       "/builder/nolen" -> builderNoLen
       "/enum/withlen" -> enumWithLen
       "/enum/nolen" -> enumNoLen
       "/file/withlen" -> fileWithLen
       "/file/nolen" -> fileNoLen
       _ -> index $ rawPathInfo req

builderWithLen = ResponseBuilder
    status200
    [ ("Content-Type", "text/plain")
    , ("Content-Length", "4")
    ]
    $ copyByteString "PONG"

builderNoLen = ResponseBuilder
    status200
    [ ("Content-Type", "text/plain")
    ]
    $ copyByteString "PONG"

fileWithLen = ResponseFile
    status200
    [ ("Content-Type", "text/plain")
    , ("Content-Length", "4")
    ]
    "pong.txt" Nothing

fileNoLen = ResponseFile
    status200
    [ ("Content-Type", "text/plain")
    ]
    "pong.txt" Nothing

enumWithLen = ResponseEnumerator $ \f ->
    run_ $ (enumList 1 $ map copyByteString ["P", "O", "NG"]) $$ f
        status200
        [ ("Content-Type", "text/plain")
        , ("Content-Length", "4")
        ]

enumNoLen = ResponseEnumerator $ \f ->
    run_ $ (enumList 1 $ map copyByteString ["P", "O", "NG"]) $$ f
        status200
        [ ("Content-Type", "text/plain")
        ]

index :: B.ByteString -> Response
index p = ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p><a href='/builder/withlen'>builder withlen</a></p>\n"
    , "<p><a href='/builder/nolen'>builder nolen</a></p>\n"
    , "<p><a href='/enum/withlen'>enum withlen</a></p>\n"
    , "<p><a href='/enum/nolen'>enum nolen</a></p>\n"
    , "<p><a href='/file/withlen'>file withlen</a></p>\n"
    , "<p><a href='/file/nolen'>file nolen</a></p>\n"
    , p
    ]
