module Tests.IcmpEcho.Basic where

import Control.Monad.Extra (replicateM)
import Data.Bits (complement)
import Data.List (foldl', intersperse)
import Numeric (showHex)
import Prelude

import Clash.Hedgehog.Sized.Index
import Clash.Hedgehog.Sized.Unsigned
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import IcmpEcho

import qualified Clash.Prelude as C
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data AlterPacket
  = AlterMac
    -- ^ Alter the destination MAC address
  | AlterL2Proto
    -- ^ Alter the EtherType, the IPv4 version field or the Internet Header
    -- Length field
  | AlterL3Proto
    -- ^ Alter the level 3 protocol
  | AlterIpType
    -- ^ Alter the destination IP address or the ICMP type
  deriving (Eq, Enum, Show)

genPacket ::
  H.MonadGen m =>
  Maybe AlterPacket ->
  -- | (ICMP echo request packet, ICMP echo reply packet)
  m ([C.Unsigned 8], [C.Unsigned 8])
genPacket alter = do
  destMac1 <-
    if alter == Just AlterMac then do
      pos <- genIndex @_ @6 Range.constantBounded
      byte <- genUnsigned Range.constantBounded
      pure (replaceByte pos byte destMac)
    else do
      pure destMac
  l2proto1 <-
    if alter == Just AlterL2Proto then do
      pos <- genIndex @_ @3 Range.constantBounded
      byte <- genUnsigned Range.constantBounded
      pure (replaceByte pos byte l2proto)
    else do
      pure l2proto
  l3proto1 <-
    if alter == Just AlterL3Proto
    then genUnsigned Range.constantBounded
    else pure l3proto
  ipType1 <-
    if alter == Just AlterIpType then do
      pos <- genIndex @_ @4 Range.constantBounded
      byte <- genUnsigned Range.constantBounded
      pure (replaceByte pos byte ipType)
    else do
      pure ipType
  srcMac <- replicateM 6 $ genUnsigned Range.constantBounded
  random2 <- replicateM 8 $ genUnsigned Range.constantBounded
  srcIp <- replicateM 4 $ genUnsigned Range.constantBounded
  extraLen <- Gen.integral (Range.constant 0 64)
  random4 <- replicateM (extraLen + 7) $ genUnsigned Range.constantBounded
  let
    request cksum = destMac1 ++ srcMac ++ l2proto1 ++ random2 ++ [l3proto1] ++ cksum ++
      srcIp ++ ipType1 ++ random4
    request1 = request $ u16toU8s $ checksumPacket $ request [0,0]
    reply cksum =
      srcMac ++ destMac ++ l2proto ++ random2 ++ [l3proto] ++ cksum ++ take 4 ipType ++ srcIp ++ [0x01] ++ random4
    reply1 = reply $ u16toU8s $ checksumPacket $ reply [0,0]
    reply2
      | destMac1 == destMac
      , l2proto1 == l2proto
      , l3proto1 == l3proto
      , ipType1 == ipType
      = reply1
      | otherwise = []
  pure (request1, reply2)
 where
  destMac = [ 0x52, 0x54, 0x00, 0xeb, 0x9b, 0xd0 ]
  l2proto = [ 0x08, 0x00, 0x45 ]
  l3proto = 0x01
  ipType = [ 0x0a, 0x00, 0x00, 0x02, 0x08 ]

  replaceByte pos byte xs =
    let (xs1, xs2) = splitAt (fromIntegral pos) xs
    in xs1 ++ (byte:tail xs2)


checksumPacket ::
  [C.Unsigned 8] ->
  C.Unsigned 16
checksumPacket xs = complement $ foldl' (~+~) 0 $ map to16 $ parts $ take 20 $ drop 14 xs
 where
  parts []  = []
  parts xs1 = let (xs2, xs3) = splitAt 2 xs1 in xs2:parts xs3
  to16 :: [C.Unsigned 8] -> C.Unsigned 16
  to16 xs1 =
    let
      hi = C.resize $ head xs1
      lo = C.resize $ head (tail xs1)
    in 256 * hi + lo

u16toU8s ::
  C.Unsigned 16 ->
  [C.Unsigned 8]
u16toU8s n = [ C.resize (n `C.shiftR` 8), C.resize n ]

pPrintPacket ::
  [C.Unsigned 8] ->
  String
pPrintPacket packet =
  ("MAC: " ++) . showHexList mac . ("\nEtherType: " ++) .
  showHexList etherType .
  ("\nIP ver/len: " ++) . showHex8 verLen . ("\nL3 proto: " ++) . showHex8 l3Proto .
  ("\nDest IP: " ++) . showHexList destIp . ("\nICMP Type: " ++) . showHex8 icmpType .
  ("\nPayload length: " ++) . shows payLen . ("\nComputed checksum: " ++) . showHex16 checksum . ("\nHex dump:\n" ++) $ showDump packet ""
 where
  (mac, packet1) = splitAt 6 packet
  (etherType, packet2) = splitAt 2 $ drop 6 packet1
  verLen = head packet2
  packet3 = drop 9 packet2
  l3Proto = head packet3
  packet4 = tail packet3
  (destIp, packet5) = splitAt 4 $ drop 6 packet4
  icmpType = head packet5
  payLen = length packet5 - 8
  checksum = checksumPacket packet

  showHexAlign p n =
    let cs = showHex n ""
    in ((replicate (p - length cs) '0' ++ cs) ++)

  showHex8 = showHexAlign 2
  showHex16 = showHexAlign 4

  showHexList ns =
    foldr (.) id $ intersperse (' ':) $ map showHex8 ns

  showDump ns =
    foldr (.) id $ intersperse ('\n':) $ map showHexList $ parts ns
   where
    parts [] = []
    parts ns1 = let (ns2, ns3) = splitAt 8 ns1 in ns2:parts ns3
