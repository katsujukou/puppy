module Test.Puppy.Docs.Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Puppy.Docs.UI.Route (Route(..), docRoute, route, routePath)
import Routing.Duplex as RD
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

-- Representative route values the app can navigate to. Docs pages are data
-- driven, so they are modelled by `Doc` carrying the path segments.
allRoutes :: Array Route
allRoutes =
  [ Home
  , Search { q: "effect monad" }
  , docRoute "/getting-started"
  , docRoute "/getting-started/ffi-and-js-interop"
  , docRoute "/dev"
  , docRoute "/dev/optimizations"
  ]

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  describe "Route" do
    it "round-trips every route through print/parse" do
      for_ allRoutes \r ->
        RD.parse route (RD.print route r) `shouldEqual` Right r

    it "prints the expected paths" do
      RD.print route Home `shouldEqual` "/"
      RD.print route (docRoute "/dev") `shouldEqual` "/dev"
      RD.print route (docRoute "/dev/optimizations") `shouldEqual` "/dev/optimizations"
      RD.print route (docRoute "/getting-started/ffi-and-js-interop") `shouldEqual` "/getting-started/ffi-and-js-interop"
      RD.print route (Search { q: "x y" }) `shouldEqual` "/search?q=x%20y"

    it "keeps Home distinct from a docs page and parses paths into Doc" do
      RD.parse route "/" `shouldEqual` Right Home
      RD.parse route "/dev" `shouldEqual` Right (Doc [ "dev" ])
      RD.parse route "/dev/optimizations" `shouldEqual` Right (Doc [ "dev", "optimizations" ])

    it "routePath reconstructs a Doc route's manifest path" do
      routePath (docRoute "/dev/optimizations") `shouldEqual` "/dev/optimizations"
      routePath (docRoute "/getting-started") `shouldEqual` "/getting-started"
