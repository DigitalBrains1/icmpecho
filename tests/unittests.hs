import Prelude

import Test.Tasty

import qualified Tests.IcmpEcho.Basic

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.IcmpEcho.Basic.tests
  ]
