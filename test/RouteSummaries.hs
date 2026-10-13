{-# LANGUAGE OverloadedStrings #-}

module RouteSummaries
  ( testGroup
  ) where

import Data.Set qualified as Set
import Hedgehog ((===))
import Hedgehog qualified as HH
import Network.HTTP.Types qualified as HTTPTypes
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as TastyHH

import Fixtures qualified
import Orb qualified

testGroup :: Tasty.TestTree
testGroup =
  Tasty.testGroup
    "RouteSummaries"
    [ test_routeSummaries
    , test_renderRouteSummaries
    ]

test_routeSummaries :: Tasty.TestTree
test_routeSummaries =
  TastyHH.testProperty "summarizes every route in declaration order" . HH.withTests 1 . HH.property $
    Orb.routeSummaries Fixtures.routeSummariesRouter
      === [ Orb.RouteSummary
              { Orb.routeSummaryMethod = HTTPTypes.GET
              , Orb.routeSummaryPath = "/"
              , Orb.routeSummaryRouteType = Just "Fixtures.RouteSummaries.Wrapped ()"
              , Orb.routeSummaryLabels = Set.empty
              }
          , Orb.RouteSummary
              { Orb.routeSummaryMethod = HTTPTypes.GET
              , Orb.routeSummaryPath = "/simple"
              , Orb.routeSummaryRouteType = Just "Fixtures.SimpleGet.SimpleGet"
              , Orb.routeSummaryLabels = Set.empty
              }
          , Orb.RouteSummary
              { Orb.routeSummaryMethod = HTTPTypes.POST
              , Orb.routeSummaryPath = "/simple"
              , Orb.routeSummaryRouteType = Just "Fixtures.SimplePost.SimplePost"
              , Orb.routeSummaryLabels = Set.singleton "subset"
              }
          , Orb.RouteSummary
              { Orb.routeSummaryMethod = HTTPTypes.GET
              , Orb.routeSummaryPath = "/nested/{nestedId}/inner"
              , Orb.routeSummaryRouteType = Just "Fixtures.SimpleGet.SimpleGet"
              , Orb.routeSummaryLabels = Set.singleton "nested"
              }
          , Orb.RouteSummary
              { Orb.routeSummaryMethod = HTTPTypes.GET
              , Orb.routeSummaryPath = "/wrapped"
              , Orb.routeSummaryRouteType = Just "Fixtures.RouteSummaries.Wrapped (Maybe SimpleGet)"
              , Orb.routeSummaryLabels = Set.empty
              }
          , Orb.RouteSummary
              { Orb.routeSummaryMethod = HTTPTypes.GET
              , Orb.routeSummaryPath = "/beeline"
              , Orb.routeSummaryRouteType = Nothing
              , Orb.routeSummaryLabels = Set.empty
              }
          ]

test_renderRouteSummaries :: Tasty.TestTree
test_renderRouteSummaries =
  TastyHH.testProperty "renders each route on two lines" . HH.withTests 1 . HH.property $
    lines (Orb.renderRouteSummaries (Orb.routeSummaries (Orb.provideOpenApi "outer" Fixtures.routeSummariesRouter)))
      === [ "GET     /"
          , "          Fixtures.RouteSummaries.Wrapped ()  [outer]"
          , "GET     /simple"
          , "          Fixtures.SimpleGet.SimpleGet  [outer]"
          , "POST    /simple"
          , "          Fixtures.SimplePost.SimplePost  [outer, subset]"
          , "GET     /nested/{nestedId}/inner"
          , "          Fixtures.SimpleGet.SimpleGet  [nested, outer]"
          , "GET     /wrapped"
          , "          Fixtures.RouteSummaries.Wrapped (Maybe SimpleGet)  [outer]"
          , "GET     /beeline"
          , "          <beeline route>  [outer]"
          ]
