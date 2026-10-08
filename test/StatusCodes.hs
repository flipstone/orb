module StatusCodes
  ( testGroup
  ) where

import Data.Foldable (traverse_)
import GHC.TypeLits (fromSSymbol)
import Hedgehog ((===))
import Hedgehog qualified as HH
import Network.HTTP.Types qualified as HTTP
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as TastyHH

import Orb qualified

testGroup :: Tasty.TestTree
testGroup =
  Tasty.testGroup
    "StatusCodes"
    [ TastyHH.testProperty "status code constants match http-types" prop_constantsMatchHttpTypes
    ]

prop_constantsMatchHttpTypes :: HH.Property
prop_constantsMatchHttpTypes =
  HH.withTests 1 . HH.property $
    traverse_ checkStatusCode statusCodes

data StatusCodeCheck = StatusCodeCheck
  { checkSymbol :: String
  , checkHTTPStatus :: HTTP.Status
  , expectedHTTPStatus :: HTTP.Status
  }

statusCodeCheck :: Orb.StatusCode code -> HTTP.Status -> StatusCodeCheck
statusCodeCheck statusCode =
  StatusCodeCheck
    (fromSSymbol (Orb.statusCodeSymbol statusCode))
    (Orb.statusCodeHTTPStatus statusCode)

checkStatusCode :: HH.MonadTest m => StatusCodeCheck -> m ()
checkStatusCode check = do
  checkSymbol check === show (HTTP.statusCode (expectedHTTPStatus check))
  HTTP.statusCode (checkHTTPStatus check) === HTTP.statusCode (expectedHTTPStatus check)
  HTTP.statusMessage (checkHTTPStatus check) === HTTP.statusMessage (expectedHTTPStatus check)

statusCodes :: [StatusCodeCheck]
statusCodes =
  [ statusCodeCheck Orb.status100 HTTP.status100
  , statusCodeCheck Orb.status101 HTTP.status101
  , statusCodeCheck Orb.status200 HTTP.status200
  , statusCodeCheck Orb.status201 HTTP.status201
  , statusCodeCheck Orb.status202 HTTP.status202
  , statusCodeCheck Orb.status203 HTTP.status203
  , statusCodeCheck Orb.status204 HTTP.status204
  , statusCodeCheck Orb.status205 HTTP.status205
  , statusCodeCheck Orb.status206 HTTP.status206
  , statusCodeCheck Orb.status300 HTTP.status300
  , statusCodeCheck Orb.status301 HTTP.status301
  , statusCodeCheck Orb.status302 HTTP.status302
  , statusCodeCheck Orb.status303 HTTP.status303
  , statusCodeCheck Orb.status304 HTTP.status304
  , statusCodeCheck Orb.status305 HTTP.status305
  , statusCodeCheck Orb.status307 HTTP.status307
  , statusCodeCheck Orb.status308 HTTP.status308
  , statusCodeCheck Orb.status400 HTTP.status400
  , statusCodeCheck Orb.status401 HTTP.status401
  , statusCodeCheck Orb.status402 HTTP.status402
  , statusCodeCheck Orb.status403 HTTP.status403
  , statusCodeCheck Orb.status404 HTTP.status404
  , statusCodeCheck Orb.status405 HTTP.status405
  , statusCodeCheck Orb.status406 HTTP.status406
  , statusCodeCheck Orb.status407 HTTP.status407
  , statusCodeCheck Orb.status408 HTTP.status408
  , statusCodeCheck Orb.status409 HTTP.status409
  , statusCodeCheck Orb.status410 HTTP.status410
  , statusCodeCheck Orb.status411 HTTP.status411
  , statusCodeCheck Orb.status412 HTTP.status412
  , statusCodeCheck Orb.status413 HTTP.status413
  , statusCodeCheck Orb.status414 HTTP.status414
  , statusCodeCheck Orb.status415 HTTP.status415
  , statusCodeCheck Orb.status416 HTTP.status416
  , statusCodeCheck Orb.status417 HTTP.status417
  , statusCodeCheck Orb.status418 HTTP.status418
  , statusCodeCheck Orb.status422 HTTP.status422
  , statusCodeCheck Orb.status428 HTTP.status428
  , statusCodeCheck Orb.status429 HTTP.status429
  , statusCodeCheck Orb.status431 HTTP.status431
  , statusCodeCheck Orb.status500 HTTP.status500
  , statusCodeCheck Orb.status501 HTTP.status501
  , statusCodeCheck Orb.status502 HTTP.status502
  , statusCodeCheck Orb.status503 HTTP.status503
  , statusCodeCheck Orb.status504 HTTP.status504
  , statusCodeCheck Orb.status505 HTTP.status505
  , statusCodeCheck Orb.status511 HTTP.status511
  ]
