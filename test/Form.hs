module Form
  ( testGroup
  ) where

import Data.List.NonEmpty qualified as NEL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Hedgehog ((===))
import Hedgehog qualified as HH
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as TastyHH

import Orb qualified

testGroup :: Tasty.TestTree
testGroup =
  Tasty.testGroup
    "Form"
    [ TastyHH.testProperty "preserves the order of a repeated param" prop_repeatedParamOrderPreserved
    ]

prop_repeatedParamOrderPreserved :: HH.Property
prop_repeatedParamOrderPreserved = HH.property $ do
  vals <-
    HH.forAll
      . Gen.nonEmpty (Range.linear 1 50)
      $ Gen.text (Range.linear 1 12) Gen.alphaNum

  let
    keyText = T.pack "item"
    key = TE.encodeUtf8 keyText
    params =
      [ (key, TE.encodeUtf8 v)
      | v <- NEL.toList vals
      ]

  form <- HH.evalEither $ Orb.getForm (params, [])
  field <-
    maybe
      (fail "Expected key 'item' to exist in the form.")
      pure
      (Map.lookup keyText form)

  case field of
    Orb.ParamField ps ->
      NEL.toList ps === NEL.toList vals
    Orb.FileField _fs ->
      fail "Expected ParamField for 'item', but got FileField."
