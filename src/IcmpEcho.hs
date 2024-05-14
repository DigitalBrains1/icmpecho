module IcmpEcho where

import Clash.Prelude

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
decodeGray = go
 where
  go ::
    forall m .
    KnownNat m =>
    Unsigned (m+1) ->
    Unsigned (m+1)
  go m =
    case compareSNat (SNat @m) d0 of
      SNatLE -> m
      SNatGT -> m `xor` (extend $ go @(m-1) $ truncateB $ m `shiftR` 1)
