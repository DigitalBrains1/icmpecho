{-# LANGUAGE OverloadedStrings #-}

module Tests.IcmpEcho.Basic where

import Control.Monad
import Data.Bits (complement)
import Data.List (foldl', intersperse)
import Data.Maybe (isJust, fromJust)
import Data.Proxy (Proxy(..))
import GHC.TypeNats (SomeNat(..), someNatVal, type (+))
import Numeric (showHex)
import Numeric.Natural (Natural)
import Prelude

import Clash.Hedgehog.Sized.Index
import Clash.Hedgehog.Sized.Unsigned
import Test.Tasty
import Test.Tasty.Hedgehog

import IcmpEcho

import Tests.IcmpEcho.PacketFifo (responderStream')

import qualified Data.List as L
import qualified Clash.Prelude as C
import qualified Clash.Explicit.Prelude as E

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Test.Tasty.HUnit as U

tests :: TestTree
tests =
  testGroup "IcmpEcho"
    [ testGroup "PacketFifo"
      [ testPropertyNamed "Basic" "basicPacketFifoProp" basicPacketFifoProp
      ]
    , testGroup "IcmpEcho"
      [ U.testCase "Gray coding identity up to 17 bits" $ assertGrayIdentity 17
      ]
    ]

basicPacketFifoProp ::
  H.Property
basicPacketFifoProp = H.property $ do
  alters <- H.forAll $ Gen.list (Range.linear 0 10) (Gen.maybe Gen.enumBounded)
  (inPkts, expectedPkts) <- mapAndUnzipM (H.forAllWith pPrintPair . genPacket) alters
  let outPkts = runPacketFifo inPkts
  H.footnote $ pPrintPacketSets (zip inPkts expectedPkts) outPkts ""
  H.assert (outPkts == expectedPkts)
 where
  pPrintPair (inp, ex) =
    ("Input packet:\n" ++) . pPrintPacket inp . ("\nExpected packet:\n" ++) $
    pPrintPacket ex "\n"

assertGrayIdentity ::
  Natural ->
  U.Assertion
assertGrayIdentity n = forM_ [1 .. n] (\m -> U.assertBool (show m) $ idFor m)
 where
  idFor m =
    case someNatVal (m-1) of
      SomeNat (Proxy @j) ->
        let is = [0 :: C.Unsigned (j+1) .. maxBound]
        in L.map (decodeGray . encodeGray) is == is

runPacketFifo ::
  [[C.Unsigned 8]] ->
  [[C.Unsigned 8]]
runPacketFifo inPkts = outPkts
 where
  (outPkts, streamIn) = go inPkts streamOut
  streamOut =
    C.sample $ snd $
    responderStream' C.systemClockGen E.noReset C.enableGen
      (C.fromList streamIn) (pure True)
  go [] _ = ([], [])
  go (inPkt:inPkts0) streamOut0 = (outPkt:outPkts0, streamIn0 ++ streamIn1)
   where
    (outPkts0, streamIn1) = go inPkts0 streamOut2
    streamIn0 =
      (map (Just . (True,)) $ init inPkt) ++
      [Just (False, last inPkt)] ++ L.replicate 3 Nothing ++
      map (const Nothing) outPkt
    (streamOut1, streamOut2) = span isJust $ drop (length inPkt + 3) streamOut0
    outPkt = map (snd . fromJust) streamOut1

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
  deriving (Eq, Enum, Bounded, Show)

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
  random3 <- replicateM 2 $ genUnsigned Range.constantBounded
  random4 <- genUnsigned Range.constantBounded
  extraLen <- Gen.integral (Range.linear 0 64)
  random5 <- replicateM (extraLen + 4) $ genUnsigned Range.constantBounded
  let
    request cksum =
      destMac1 ++ srcMac ++ l2proto1 ++ random2 ++ [l3proto1] ++ random3 ++
      srcIp ++ ipType1 ++ [random4] ++ cksum ++ random5
    request1 = request $ u16toU8s $ checksumIcmp $ request [0,0]
    reply cksum =
      srcMac ++ destMac ++ l2proto ++ random2 ++ [l3proto] ++ random3 ++
      take 4 ipType ++ srcIp ++ [0x00] ++ [random4] ++ cksum ++ random5

    reply1 = reply $ u16toU8s $ checksumIcmp $ reply [0,0]
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


checksumIcmp ::
  [C.Unsigned 8] ->
  C.Unsigned 16
checksumIcmp xs =
  complement $ foldl' (~+~) 0 $ map to16 $ parts $ take 8 $ drop 34 xs
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
  ShowS
pPrintPacket [] = ("No packet\n" ++)
pPrintPacket packet
  | length packet < 42
  = ("Runt packet\nDump:\n" ++) . showDump packet . ('\n':)
  | otherwise
  = ("Src MAC: " ++) . showHexList srcMac . ("\nDst MAC: " ++) .
    showHexList dstMac . ("\nEtherType: " ++) . showHexList etherType .
    ("\nIP ver/len: " ++) . showHex8 verLen . ("\nL3 proto: " ++) .
    showHex8 l3Proto . ("\nSrc IP: " ++) . showHexList srcIp .
    ("\nDst IP: " ++) . showHexList destIp . ("\nICMP Type: " ++) .
    showHex8 icmpType . ("\nPayload length: " ++) . shows payLen .
    ("\nComputed checksum: " ++) . showHex16 checksum . ("\nHex dump:\n" ++) .
    showDump packet . ('\n':)
 where
  (dstMac, packet1) = splitAt 6 packet
  (srcMac, packet2) = splitAt 6 packet1
  (etherType, packet3) = splitAt 2 packet2
  verLen = head packet3
  packet4 = drop 9 packet3
  l3Proto = head packet4
  packet5 = tail packet4
  (srcIp, packet6) = splitAt 4 $ drop 2 packet5
  (destIp, packet7) = splitAt 4 packet6
  icmpType = head packet7
  payLen = length packet7 - 8
  checksum = checksumIcmp packet

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


pPrintPackets ::
  [[C.Unsigned 8]] ->
  ShowS
pPrintPackets = go (0 :: Int)
 where
  go _ [] = id
  go n (pkt:pkts) =
    ("Packet " ++) . shows n . (":\n" ++) . pPrintPacket pkt . ('\n':) .
    go (n+1) pkts

pPrintPacketSets ::
  [ ( [C.Unsigned 8]
    , [C.Unsigned 8]
    )] ->
  [[C.Unsigned 8]] ->
  ShowS
pPrintPacketSets inpExps outs = go (0 :: Int) inpExps (map Just outs)
 where
  go _ [] [] = id
  go _ [] (Nothing:_) = id
  go n [] ((Just out):outs0) =
    fmt "Extraneous output packet" n out . go (n+1) [] outs0
  go n inpExps0@(_:_) [] = go n inpExps0 (replicate (length inpExps0) Nothing)
  go n ((inp, ex):inpExps0) (out:outs0) =
    fmt "Input packet" n inp . fmt "Expected packet" n ex . fmtOut out .
    go (n+1) inpExps0 outs0
   where
    fmtOut Nothing = ("Missing output packet\n" ++)
    fmtOut (Just pkt) = fmt "Output packet" n pkt

  fmt hdr n pkt =
    ((hdr ++ " ") ++) . shows n . (":\n" ++) . pPrintPacket pkt . ('\n':)
