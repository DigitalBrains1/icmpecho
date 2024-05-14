module IcmpEcho where

import Clash.Prelude hiding (someNatVal)
import Data.Proxy (Proxy(..))
import GHC.TypeNats (someNatVal)

import qualified Data.List as L

{-
 - One's complement addition.
 -}
(~+~) :: KnownNat n
      => Unsigned n
      -> Unsigned n
      -> Unsigned n
a ~+~ b = truncateB summed + resize (bitCoerce carry)
    where
        summed = a `add` b
        carry = msb summed

f ::
  Unsigned 4 ->
  Unsigned 4
f = decodeGray
{-# ANN f (defSyn "f") #-}

g ::
  forall n .
  SNat n ->
  Bool
g SNat =
  let
    is = [minBound :: Unsigned (n+1) .. maxBound]
  in
    is == L.map (decodeGray . encodeGray) is

h ::
  Natural ->
  Bool
h n =
  case someNatVal n of
    SomeNat (Proxy @n) -> g (SNat @n)

encodeGray ::
  forall n .
  KnownNat n =>
  Unsigned n ->
  Unsigned n
encodeGray n = (n `shiftR` 1) `xor` n

decodeGray ::
  forall n .
  KnownNat n =>
  Unsigned (n+1) ->
  Unsigned (n+1)
decodeGray = unpack . go . pack
 where
  go ::
    forall m .
    KnownNat m =>
    BitVector (m+1) ->
    BitVector (m+1)
  go m =
    case compareSNat (SNat @m) d0 of
      SNatLE ->
        m
      SNatGT ->
        let (mhead, mtail) = split @_ @1 m
        in mhead ++# (mtail `xor` go @(m-1) (truncateB $ m `shiftR` 1))
