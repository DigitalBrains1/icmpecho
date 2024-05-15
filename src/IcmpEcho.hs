{-# LANGUAGE RecordWildCards #-}

module IcmpEcho where

import Clash.Prelude
import Data.Maybe (isJust)

import qualified Clash.Explicit.Prelude as E
import qualified Data.List as L


type FifoAddrSize = 8

sampledResponder ::
  [Maybe (Unsigned 8)] ->
  [(IcmpResponderState, Maybe (Bool, Unsigned 8))]
sampledResponder is = L.zip (sample ss) (sample os)
 where
  (ss, errs, os) =
    responderStream systemClockGen systemClockGen resetGen resetGen
    (pure False) $ fromList is

responderStream ::
  KnownDomain miiRx =>
  KnownDomain miiTx =>
  Clock miiRx ->
  Clock miiTx ->
  Reset miiRx ->
  Reset miiTx ->
  "rxErr" ::: Signal miiRx Bool ->
  "dIn" ::: Signal miiRx (Maybe (Unsigned 8)) ->
  ( "s" ::: Signal miiTx IcmpResponderState
  , "txErr" ::: Signal miiTx Bool
  , "dOut" :::
      Signal miiTx
        ( Maybe
          ( "last" ::: Bool
          , "dOutByte" ::: Unsigned 8
          )
        )
  )
responderStream rxClk txClk rxRst txRst rxErr dIn = (s, txErr, dOut)
 where
  fifoMemOut =
    E.asyncRamPow2 rxClk txClk enableGen fifoAddr fifoWrite

  writePtr0 = mux (isJust <$> dIn) (writePtr1 + 1) writePtr1
  writePtr1 = E.register rxClk rxRst enableGen 0 writePtr0
  fifoWrite = liftA2 (\a d -> (a,) <$> d) writePtr1 dIn

  writePtrSync = counterSynchroniser rxClk txClk txRst writePtr0

  (s, fifoAddr, txErr, dOut) =
    withClock txClk (withReset txRst icmpResponder) writePtrSync
    fifoMemOut

counterSynchroniser ::
  forall src dst n .
  KnownDomain src =>
  KnownDomain dst =>
  KnownNat n =>
  1 <= n =>
  Clock src ->
  Clock dst ->
  Reset dst ->
  Signal src (Unsigned n) ->
  Signal dst (Unsigned n)
counterSynchroniser sClk dClk rst inp = out
 where
  dffSync =
    setName @"dffSync" $
      E.dualFlipFlopSynchronizer sClk dClk rst enableGen 0 .
      E.register sClk E.noReset enableGen 0

  out = fmap decodeGray $ dffSync $ fmap encodeGray inp

icmpResponder ::
  forall dom .
  HiddenClock dom =>
  HiddenReset dom =>
  "writePtr" ::: Signal dom (Unsigned FifoAddrSize) ->
  "fifoMemOut" ::: Signal dom (Unsigned 8) ->
  ( "s" ::: Signal dom IcmpResponderState
  , "fifoAddr" :::  Signal dom (Unsigned FifoAddrSize)
  , "txErr" ::: Signal dom Bool
  , "dOut" :::
      Signal dom
        ( Maybe
          ( "last" ::: Bool
          , "dOutByte" ::: Unsigned 8
          )
        )
  )
icmpResponder writePtr fifoMemOut =
  unbundle $ withEnable enableGen mealy icmpResponderT icmpResponderIS $
  bundle (writePtr, fifoMemOut)

data Fsm
  = Idle
  | SrcMac
  | DstMac
  | IPv4Hdr  -- Actually includes EtherType
  | SrcIP
  | DstIP
  | IcmpTC
  | IcmpCheck
  | Trailer
  deriving (Show, Enum, Generic, NFDataX, ShowX)

data IcmpResponderState = IcmpResponderState
  { fsm :: Fsm
  , readPtr :: Unsigned FifoAddrSize
  , stateCnt :: Unsigned 8
  }
  deriving (Show, Generic, NFDataX, ShowX)

icmpResponderIS :: IcmpResponderState
icmpResponderIS =
  IcmpResponderState
    { fsm=Idle
    , readPtr=0
    , stateCnt=undefined
    }

icmpResponderT ::
  "s" ::: IcmpResponderState ->
  ( "writePtr" ::: Unsigned FifoAddrSize
  , "fifoMemOut" ::: Unsigned 8) ->
  ( IcmpResponderState
  , ( IcmpResponderState
    , "fifoAddr" ::: Unsigned FifoAddrSize
    , "txErr" ::: Bool
    , "dOut" :::
       ( Maybe
          ( "last" ::: Bool
          , "dOutByte" ::: Unsigned 8
          )
       )
    )
  )
icmpResponderT IcmpResponderState{..} (writePtr, fifoMemOut)
  = (s1, (s1, readPtr, txErr, dOut))
 where
  s1 = IcmpResponderState
         { fsm=fsm1
         , readPtr=readPtr1
         , stateCnt=stateCnt1
         }

  dOut =
    case fsm of
      Idle -> Nothing
      _    -> Just (lastB, fifoMemOut)

  lastB =
    case fsm of
      Trailer -> fifoOcc == 1
      _       -> False

  nextFsm =
    case fsm of
      Trailer -> Idle
      _ -> succ fsm

  trans =
    case fsm of
      Idle -> fifoOcc == 8
      Trailer -> fifoOcc == 1
      _ -> stateCnt == 0

  fsm1 = if trans then nextFsm else fsm

  stateCnt1 =
    if trans then
      case nextFsm of
        SrcMac -> snatToUnsigned d5
        DstMac -> snatToUnsigned d5
        IPv4Hdr -> snatToUnsigned d13
        SrcIP -> snatToUnsigned d3
        DstIP -> snatToUnsigned d3
        IcmpTC -> snatToUnsigned d1
        IcmpCheck -> snatToUnsigned d1
        _ -> undefined
    else
      stateCnt - 1

  readPtr1 =
    if trans then
      case nextFsm of
        SrcMac -> readPtr + 6
        DstMac -> readPtr - 11
        IPv4Hdr -> readPtr + 7
        SrcIP -> readPtr + 5
        DstIP -> readPtr - 7
        IcmpTC -> readPtr + 5
        _ -> readPtrInc
    else
      case fsm of
        Idle -> readPtr
        _ -> readPtrInc
   where
    readPtrInc = readPtr + 1

  -- FIFO occupancy
  fifoOcc = writePtr - readPtr

  txErr = False

snatToUnsigned ::
  forall m n .
  KnownNat n =>
  CLog 2 (m+1) <= n =>
  SNat m ->
  Unsigned n
snatToUnsigned = snatToNum

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

encodeGray ::
  forall n .
  KnownNat n =>
  Unsigned n ->
  Unsigned n
encodeGray n = (n `shiftR` 1) `xor` n

decodeGray ::
  forall n .
  KnownNat n =>
  1 <= n =>
  Unsigned n ->
  Unsigned n
decodeGray = unpack . leToPlusKN @1 @n go . pack
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
