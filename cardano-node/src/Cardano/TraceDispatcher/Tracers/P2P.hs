{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.Tracers.P2P
  (
    namesForLocalRootPeers
  , severityLocalRootPeers
  , docLocalRootPeers

  , namesForPublicRootPeers
  , severityPublicRootPeers
  , docPublicRootPeers

  , namesForPeerSelection
  , severityPeerSelection
  , docPeerSelection

  , namesForDebugPeerSelection
  , severityDebugPeerSelection
  , docDebugPeerSelection

  , namesForPeerSelectionCounters
  , severityPeerSelectionCounters
  , docPeerSelectionCounters

  , namesForPeerSelectionActions
  , severityPeerSelectionActions
  , docPeerSelectionActions

  , namesForConnectionManager
  , severityConnectionManager
  , docConnectionManager

  , namesForServer
  , severityServer
  , docServer

  , namesForInboundGovernor
  , severityInboundGovernor
  , docInboundGovernor

  ) where

import           Cardano.Logging
import           Cardano.Prelude hiding (group, show)
import           Data.Aeson (ToJSON, ToJSONKey, Value (..), object, toJSON,
                     toJSONList, (.=))
import           Data.Aeson.Types (listValue)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text (pack)
import           Network.Socket (SockAddr)
import           Prelude (id, show)

import           Cardano.Node.Configuration.Topology ()
import           Cardano.Node.Configuration.TopologyP2P ()
import           Cardano.Tracing.OrphanInstances.Network ()

import           Cardano.TraceDispatcher.Tracers.NodeToNode ()
import           Cardano.TraceDispatcher.Tracers.NonP2P ()

import           Ouroboros.Network.ConnectionHandler
                     (ConnectionHandlerTrace (..))
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types
                     (ConnectionManagerCounters (..),
                     ConnectionManagerTrace (..))
import qualified Ouroboros.Network.ConnectionManager.Types as ConnectionManager
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace (..))
import qualified Ouroboros.Network.InboundGovernor as InboundGovernor
import           Ouroboros.Network.InboundGovernor.State
                     (InboundGovernorCounters (..))
import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.PeerSelection.Governor
                     (DebugPeerSelection (..), PeerSelectionCounters (..),
                     PeerSelectionState (..), PeerSelectionTargets (..),
                     TracePeerSelection (..))
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.PeerStateActions
                     (PeerSelectionActionsTrace (..))
import           Ouroboros.Network.PeerSelection.RelayAccessPoint
                     (RelayAccessPoint)
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                     (TraceLocalRootPeers (..), TracePublicRootPeers (..))
import           Ouroboros.Network.PeerSelection.Types ()
import           Ouroboros.Network.RethrowPolicy (ErrorCommand (..))
import           Ouroboros.Network.Server2 (ServerTrace (..))

--------------------------------------------------------------------------------
-- LocalRootPeers Tracer
--------------------------------------------------------------------------------

namesForLocalRootPeers :: TraceLocalRootPeers ntnAddr resolverError -> [Text]
namesForLocalRootPeers TraceLocalRootDomains {} = ["LocalRootDomains"]
namesForLocalRootPeers TraceLocalRootWaiting {} = ["LocalRootWaiting"]
namesForLocalRootPeers TraceLocalRootResult {}  = ["LocalRootResult"]
namesForLocalRootPeers TraceLocalRootGroups {}  = ["LocalRootGroups"]
namesForLocalRootPeers TraceLocalRootFailure {} = ["LocalRootFailure"]
namesForLocalRootPeers TraceLocalRootError {}   = ["LocalRootError"]

severityLocalRootPeers :: TraceLocalRootPeers ntnAddr resolverError -> SeverityS
severityLocalRootPeers _ = Info

instance (ToJSONKey ntnAddr, ToJSONKey RelayAccessPoint, Show ntnAddr, Show exception) =>
    LogFormatting (TraceLocalRootPeers ntnAddr exception) where
  forMachine _dtal (TraceLocalRootDomains groups) =
    mkObject [ "kind" .= String "LocalRootDomains"
             , "localRootDomains" .= toJSON groups
             ]
  forMachine _dtal (TraceLocalRootWaiting d dt) =
    mkObject [ "kind" .= String "LocalRootWaiting"
             , "domainAddress" .= toJSON d
             , "diffTime" .= show dt
             ]
  forMachine _dtal (TraceLocalRootResult d res) =
    mkObject [ "kind" .= String "LocalRootResult"
             , "domainAddress" .= toJSON d
             , "result" .= toJSONList res
             ]
  forMachine _dtal (TraceLocalRootGroups groups) =
    mkObject [ "kind" .= String "LocalRootGroups"
             , "localRootGroups" .= toJSON groups
             ]
  forMachine _dtal (TraceLocalRootFailure d exception) =
    mkObject [ "kind" .= String "LocalRootFailure"
             , "domainAddress" .= toJSON d
             , "reason" .= show exception
             ]
  forMachine _dtal (TraceLocalRootError d exception) =
    mkObject [ "kind" .= String "LocalRootError"
             , "domainAddress" .= toJSON d
             , "reason" .= show exception
             ]
  forHuman = pack . show

docLocalRootPeers :: Documented (TraceLocalRootPeers ntnAddr resolverError)
docLocalRootPeers = Documented [
    DocMsg
      (TraceLocalRootDomains anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootWaiting anyProto anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootResult anyProto anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootGroups anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootFailure anyProto anyProto)
      []
      ""
  , DocMsg
      (TraceLocalRootError anyProto anyProto)
      []
      ""
  ]

--------------------------------------------------------------------------------
-- PublicRootPeers Tracer
--------------------------------------------------------------------------------

namesForPublicRootPeers :: TracePublicRootPeers -> [Text]
namesForPublicRootPeers TracePublicRootRelayAccessPoint {} = ["PublicRootRelayAccessPoint"]
namesForPublicRootPeers TracePublicRootDomains {} = ["PublicRootDomains"]
namesForPublicRootPeers TracePublicRootResult {}  = ["PublicRootResult"]
namesForPublicRootPeers TracePublicRootFailure {}  = ["PublicRootFailure"]

severityPublicRootPeers :: TracePublicRootPeers -> SeverityS
severityPublicRootPeers _ = Info

instance LogFormatting TracePublicRootPeers where
  forMachine _dtal (TracePublicRootRelayAccessPoint relays) =
    mkObject [ "kind" .= String "PublicRootRelayAddresses"
             , "relayAddresses" .= toJSONList relays
             ]
  forMachine _dtal (TracePublicRootDomains domains) =
    mkObject [ "kind" .= String "PublicRootDomains"
             , "domainAddresses" .= toJSONList domains
             ]
  forMachine _dtal (TracePublicRootResult b res) =
    mkObject [ "kind" .= String "PublicRootResult"
             , "domain" .= show b
             , "result" .= toJSONList res
             ]
  forMachine _dtal (TracePublicRootFailure b d) =
    mkObject [ "kind" .= String "PublicRootFailure"
             , "domain" .= show b
             , "reason" .= show d
             ]
  forHuman = pack . show

docPublicRootPeers :: Documented TracePublicRootPeers
docPublicRootPeers = Documented [
    DocMsg
      (TracePublicRootRelayAccessPoint anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootDomains anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootResult anyProto anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootFailure anyProto anyProto)
      []
      ""
  ]

--------------------------------------------------------------------------------
-- PeerSelection Tracer
--------------------------------------------------------------------------------

namesForPeerSelection :: TracePeerSelection peeraddr -> [Text]
namesForPeerSelection TraceLocalRootPeersChanged {} = ["LocalRootPeersChanged"]
namesForPeerSelection TraceTargetsChanged {}        = ["TargetsChanged"]
namesForPeerSelection TracePublicRootsRequest {}    = ["ublicRootsRequest"]
namesForPeerSelection TracePublicRootsResults {}    = ["PublicRootsResults"]
namesForPeerSelection TracePublicRootsFailure {}    = ["PublicRootsFailure"]
namesForPeerSelection TraceGossipRequests {}        = ["GossipRequests"]
namesForPeerSelection TraceGossipResults {}         = ["GossipResults"]
namesForPeerSelection TraceForgetColdPeers {}       = ["ForgetColdPeers"]
namesForPeerSelection TracePromoteColdPeers {}      = ["PromoteColdPeers"]
namesForPeerSelection TracePromoteColdLocalPeers {} = ["PromoteColdLocalPeers"]
namesForPeerSelection TracePromoteColdFailed {}     = ["PromoteColdFailed"]
namesForPeerSelection TracePromoteColdDone {}       = ["PromoteColdDone"]
namesForPeerSelection TracePromoteWarmPeers {}      = ["PromoteWarmPeers"]
namesForPeerSelection TracePromoteWarmLocalPeers {} = ["PromoteWarmLocalPeers"]
namesForPeerSelection TracePromoteWarmFailed {}     = ["PromoteWarmFailed"]
namesForPeerSelection TracePromoteWarmDone {}       = ["PromoteWarmDone"]
namesForPeerSelection TraceDemoteWarmPeers {}       = ["DemoteWarmPeers"]
namesForPeerSelection TraceDemoteWarmFailed {}      = ["DemoteWarmFailed"]
namesForPeerSelection TraceDemoteWarmDone {}        = ["DemoteWarmDone"]
namesForPeerSelection TraceDemoteHotPeers {}        = ["DemoteHotPeers"]
namesForPeerSelection TraceDemoteLocalHotPeers {}   = ["DemoteLocalHotPeers"]
namesForPeerSelection TraceDemoteHotFailed {}       = ["DemoteHotFailed"]
namesForPeerSelection TraceDemoteHotDone {}         = ["DemoteHotDone"]
namesForPeerSelection TraceDemoteAsynchronous {}    = ["DemoteAsynchronous"]
namesForPeerSelection TraceGovernorWakeup {}        = ["GovernorWakeup"]
namesForPeerSelection TraceChurnWait {}             = ["ChurnWait"]
namesForPeerSelection TraceChurnMode {}             = ["ChurnMode"]


severityPeerSelection :: TracePeerSelection peeraddr -> SeverityS
severityPeerSelection TraceLocalRootPeersChanged {} = Notice
severityPeerSelection TraceTargetsChanged        {} = Notice
severityPeerSelection TracePublicRootsRequest    {} = Info
severityPeerSelection TracePublicRootsResults    {} = Info
severityPeerSelection TracePublicRootsFailure    {} = Error
severityPeerSelection TraceGossipRequests        {} = Debug
severityPeerSelection TraceGossipResults         {} = Debug
severityPeerSelection TraceForgetColdPeers       {} = Info
severityPeerSelection TracePromoteColdPeers      {} = Info
severityPeerSelection TracePromoteColdLocalPeers {} = Info
severityPeerSelection TracePromoteColdFailed     {} = Info
severityPeerSelection TracePromoteColdDone       {} = Info
severityPeerSelection TracePromoteWarmPeers      {} = Info
severityPeerSelection TracePromoteWarmLocalPeers {} = Info
severityPeerSelection TracePromoteWarmFailed     {} = Info
severityPeerSelection TracePromoteWarmDone       {} = Info
severityPeerSelection TraceDemoteWarmPeers       {} = Info
severityPeerSelection TraceDemoteWarmFailed      {} = Info
severityPeerSelection TraceDemoteWarmDone        {} = Info
severityPeerSelection TraceDemoteHotPeers        {} = Info
severityPeerSelection TraceDemoteLocalHotPeers   {} = Info
severityPeerSelection TraceDemoteHotFailed       {} = Info
severityPeerSelection TraceDemoteHotDone         {} = Info
severityPeerSelection TraceDemoteAsynchronous    {} = Info
severityPeerSelection TraceGovernorWakeup        {} = Info
severityPeerSelection TraceChurnWait             {} = Info
severityPeerSelection TraceChurnMode             {} = Info

instance LogFormatting (TracePeerSelection SockAddr) where
  forMachine _dtal (TraceLocalRootPeersChanged lrp lrp') =
    mkObject [ "kind" .= String "LocalRootPeersChanged"
             , "previous" .= toJSON lrp
             , "current" .= toJSON lrp'
             ]
  forMachine _dtal (TraceTargetsChanged pst pst') =
    mkObject [ "kind" .= String "TargetsChanged"
             , "previous" .= toJSON pst
             , "current" .= toJSON pst'
             ]
  forMachine _dtal (TracePublicRootsRequest tRootPeers nRootPeers) =
    mkObject [ "kind" .= String "PublicRootsRequest"
             , "targetNumberOfRootPeers" .= tRootPeers
             , "numberOfRootPeers" .= nRootPeers
             ]
  forMachine _dtal (TracePublicRootsResults res group dt) =
    mkObject [ "kind" .= String "PublicRootsResults"
             , "result" .= toJSONList (toList res)
             , "group" .= group
             , "diffTime" .= dt
             ]
  forMachine _dtal (TracePublicRootsFailure err group dt) =
    mkObject [ "kind" .= String "PublicRootsFailure"
             , "reason" .= show err
             , "group" .= group
             , "diffTime" .= dt
             ]
  forMachine _dtal (TraceGossipRequests targetKnown actualKnown aps sps) =
    mkObject [ "kind" .= String "GossipRequests"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "availablePeers" .= toJSONList (toList aps)
             , "selectedPeers" .= toJSONList (toList sps)
             ]
  forMachine _dtal (TraceGossipResults res) =
    mkObject [ "kind" .= String "GossipResults"
             , "result" .= toJSONList (map ( bimap show id <$> ) res)
             ]
  forMachine _dtal (TraceForgetColdPeers targetKnown actualKnown sp) =
    mkObject [ "kind" .= String "ForgeColdPeers"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteColdPeers targetKnown actualKnown sp) =
    mkObject [ "kind" .= String "PromoteColdPeers"
             , "targetEstablished" .= targetKnown
             , "actualEstablished" .= actualKnown
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteColdLocalPeers tLocalEst aLocalEst sp) =
    mkObject [ "kind" .= String "PromoteColdLocalPeers"
             , "targetLocalEstablished" .= tLocalEst
             , "actualLocalEstablished" .= aLocalEst
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteColdFailed tEst aEst p d err) =
    mkObject [ "kind" .= String "PromoteColdFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "delay" .= toJSON d
             , "reason" .= show err
             ]
  forMachine _dtal (TracePromoteColdDone tEst aEst p) =
    mkObject [ "kind" .= String "PromoteColdDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TracePromoteWarmPeers tActive aActive sp) =
    mkObject [ "kind" .= String "PromoteWarmPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteWarmLocalPeers taa sp) =
    mkObject [ "kind" .= String "PromoteWarmLocalPeers"
             , "targetActualActive" .= toJSONList taa
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TracePromoteWarmFailed tActive aActive p err) =
    mkObject [ "kind" .= String "PromoteWarmFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TracePromoteWarmDone tActive aActive p) =
    mkObject [ "kind" .= String "PromoteWarmDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteWarmPeers tEst aEst sp) =
    mkObject [ "kind" .= String "DemoteWarmPeers"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteWarmFailed tEst aEst p err) =
    mkObject [ "kind" .= String "DemoteWarmFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TraceDemoteWarmDone tEst aEst p) =
    mkObject [ "kind" .= String "DemoteWarmDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteHotPeers tActive aActive sp) =
    mkObject [ "kind" .= String "DemoteHotPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteLocalHotPeers taa sp) =
    mkObject [ "kind" .= String "DemoteLocalHotPeers"
             , "targetActualActive" .= toJSONList taa
             , "selectedPeers" .= toJSONList (toList sp)
             ]
  forMachine _dtal (TraceDemoteHotFailed tActive aActive p err) =
    mkObject [ "kind" .= String "DemoteHotFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "reason" .= show err
             ]
  forMachine _dtal (TraceDemoteHotDone tActive aActive p) =
    mkObject [ "kind" .= String "DemoteHotDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  forMachine _dtal (TraceDemoteAsynchronous msp) =
    mkObject [ "kind" .= String "DemoteAsynchronous"
             , "state" .= toJSON msp
             ]
  forMachine _dtal TraceGovernorWakeup =
    mkObject [ "kind" .= String "GovernorWakeup"
             ]
  forMachine _dtal (TraceChurnWait dt) =
    mkObject [ "kind" .= String "ChurnWait"
             , "diffTime" .= toJSON dt
             ]
  forMachine _dtal (TraceChurnMode c) =
    mkObject [ "kind" .= String "ChurnMode"
             , "event" .= show c ]
  forHuman = pack . show

docPeerSelection :: Documented (TracePeerSelection SockAddr)
docPeerSelection = Documented [
    DocMsg
      (TraceLocalRootPeersChanged anyProto anyProto)
      []
      ""
  , DocMsg
      (TraceTargetsChanged anyProto anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootsRequest 1 1)
      []
      ""
  , DocMsg
      (TracePublicRootsResults anyProto 1 anyProto)
      []
      ""
  , DocMsg
      (TracePublicRootsResults anyProto 1 anyProto)
      []
      ""
  , DocMsg
      (TraceGossipRequests 1 1 anyProto anyProto)
      []
      "target known peers, actual known peers, peers available for gossip,\
      \ peers selected for gossip"
  , DocMsg
      (TraceGossipResults [])
      []
      ""
  , DocMsg
      (TraceForgetColdPeers 1 1 anyProto)
      []
      "target known peers, actual known peers, selected peers"
  , DocMsg
      (TracePromoteColdPeers 1 1 anyProto)
      []
      "target established, actual established, selected peers"
  , DocMsg
      (TracePromoteColdLocalPeers 1 1 anyProto)
      []
      "target local established, actual local established, selected peers"
  , DocMsg
      (TracePromoteColdFailed 1 1 anyProto anyProto anyProto)
      []
      "target established, actual established, peer, delay until next\
      \ promotion, reason"
  , DocMsg
      (TracePromoteColdDone 1 1 anyProto)
      []
      "target active, actual active, selected peers"
  , DocMsg
      (TracePromoteWarmPeers 1 1 anyProto)
      []
      "target active, actual active, selected peers"
  , DocMsg
      (TracePromoteWarmLocalPeers [] anyProto)
      []
      "local per-group (target active, actual active), selected peers"
  , DocMsg
      (TracePromoteWarmFailed 1 1 anyProto anyProto)
      []
      "target active, actual active, peer, reason"
  , DocMsg
      (TracePromoteWarmDone 1 1 anyProto)
      []
      "target active, actual active, peer"
  , DocMsg
      (TraceDemoteWarmPeers 1 1 anyProto)
      []
      "target established, actual established, selected peers"
  , DocMsg
      (TraceDemoteWarmFailed 1 1 anyProto anyProto)
      []
      "target established, actual established, peer, reason"
  , DocMsg
      (TraceDemoteWarmDone 1 1 anyProto)
      []
      "target established, actual established, peer"
  , DocMsg
      (TraceDemoteHotPeers 1 1 anyProto)
      []
      "target active, actual active, selected peers"
  , DocMsg
      (TraceDemoteLocalHotPeers [] anyProto)
      []
      "local per-group (target active, actual active), selected peers"
  , DocMsg
      (TraceDemoteHotFailed 1 1 anyProto anyProto)
      []
      "target active, actual active, peer, reason"
  , DocMsg
      (TraceDemoteHotFailed 1 1 anyProto anyProto)
      []
      "target active, actual active, peer, reason"
  , DocMsg
      (TraceDemoteHotDone 1 1 anyProto )
      []
      "target active, actual active, peer"
  , DocMsg
      (TraceDemoteAsynchronous anyProto )
      []
      ""
  , DocMsg
      TraceGovernorWakeup
      []
      ""
  , DocMsg
      (TraceChurnWait anyProto)
      []
      ""
  , DocMsg
      (TraceChurnMode anyProto)
      []
      ""
  ]

peerSelectionTargetsToObject :: PeerSelectionTargets -> Value
peerSelectionTargetsToObject
  PeerSelectionTargets { targetNumberOfRootPeers,
                         targetNumberOfKnownPeers,
                         targetNumberOfEstablishedPeers,
                         targetNumberOfActivePeers } =
    Object $
      mkObject [ "roots" .= targetNumberOfRootPeers
               , "knownPeers" .= targetNumberOfKnownPeers
               , "established" .= targetNumberOfEstablishedPeers
               , "active" .= targetNumberOfActivePeers
               ]

--------------------------------------------------------------------------------
-- DebugPeerSelection Tracer
--------------------------------------------------------------------------------

namesForDebugPeerSelection :: DebugPeerSelection SockAddr peerConn -> [Text]
namesForDebugPeerSelection _ = ["GovernorState"]

severityDebugPeerSelection :: DebugPeerSelection SockAddr peerConn -> SeverityS
severityDebugPeerSelection _ = Debug

instance Show peerConn => LogFormatting (DebugPeerSelection SockAddr peerConn) where
  forMachine DNormal (TraceGovernorState blockedAt wakeupAfter
                   PeerSelectionState { targets, knownPeers, establishedPeers, activePeers }) =
    mkObject [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "targets" .= peerSelectionTargetsToObject targets
             , "numberOfPeers" .=
                 Object (mkObject [ "known" .= KnownPeers.size knownPeers
                                  , "established" .= EstablishedPeers.size establishedPeers
                                  , "active" .= Set.size activePeers
                                  ])
             ]
  forMachine _ (TraceGovernorState blockedAt wakeupAfter ev) =
    mkObject [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "peerSelectionState" .= String (pack $ show ev)
             ]
  forHuman = pack . show

docDebugPeerSelection :: Documented (DebugPeerSelection SockAddr peerConn)
docDebugPeerSelection = Documented
  [  DocMsg
      (TraceGovernorState anyProto anyProto anyProto)
      []
      ""
  ]

namesForPeerSelectionCounters :: PeerSelectionCounters -> [Text]
namesForPeerSelectionCounters _ = []

severityPeerSelectionCounters :: PeerSelectionCounters -> SeverityS
severityPeerSelectionCounters _ = Info

instance LogFormatting PeerSelectionCounters where
  forMachine _dtal ev =
    mkObject [ "kind" .= String "PeerSelectionCounters"
             , "coldPeers" .= coldPeers ev
             , "warmPeers" .= warmPeers ev
             , "hotPeers" .= hotPeers ev
             ]
  forHuman = pack . show
  asMetrics PeerSelectionCounters {..} =
    [ IntM
        "cardano.node.peerSelection.cold"
        (fromIntegral coldPeers)
    , IntM
        "cardano.node.peerSelection.warm"
        (fromIntegral warmPeers)
    , IntM
        "cardano.node.peerSelection.hot"
        (fromIntegral hotPeers)
      ]

docPeerSelectionCounters :: Documented PeerSelectionCounters
docPeerSelectionCounters = Documented
  [  DocMsg
      (PeerSelectionCounters 1 1 1)
      [ ("cardano.node.peerSelection.cold", "Number of cold peers")
      , ("cardano.node.peerSelection.warm", "Number of warm peers")
      , ("cardano.node.peerSelection.hot", "Number of hot peers") ]
      "Counters for cold, warm and hot peers"
  ]

--------------------------------------------------------------------------------
-- PeerSelectionActions Tracer
--------------------------------------------------------------------------------

namesForPeerSelectionActions :: PeerSelectionActionsTrace ntnAddr -> [Text]
namesForPeerSelectionActions PeerStatusChanged   {}     = ["StatusChanged"]
namesForPeerSelectionActions PeerStatusChangeFailure {} = ["StatusChangeFailure"]
namesForPeerSelectionActions PeerMonitoringError {}     = ["MonitoringError"]
namesForPeerSelectionActions PeerMonitoringResult {}    = ["MonitoringResult"]

severityPeerSelectionActions :: PeerSelectionActionsTrace ntnAddr -> SeverityS
severityPeerSelectionActions PeerStatusChanged {}       = Info
severityPeerSelectionActions PeerStatusChangeFailure {} = Error
severityPeerSelectionActions PeerMonitoringError {}     = Error
severityPeerSelectionActions PeerMonitoringResult {}    = Debug

-- TODO: Write PeerStatusChangeType ToJSON at ouroboros-network
-- For that an export is needed at ouroboros-network
instance LogFormatting (PeerSelectionActionsTrace SockAddr) where
  forMachine _dtal (PeerStatusChanged ps) =
    mkObject [ "kind" .= String "PeerStatusChanged"
             , "peerStatusChangeType" .= show ps
             ]
  forMachine _dtal (PeerStatusChangeFailure ps f) =
    mkObject [ "kind" .= String "PeerStatusChangeFailure"
             , "peerStatusChangeType" .= show ps
             , "reason" .= show f
             ]
  forMachine _dtal (PeerMonitoringError connId s) =
    mkObject [ "kind" .= String "PeerMonitoridngError"
             , "connectionId" .= toJSON connId
             , "reason" .= show s
             ]
  forMachine _dtal (PeerMonitoringResult connId wf) =
    mkObject [ "kind" .= String "PeerMonitoringResult"
             , "connectionId" .= toJSON connId
             , "withProtocolTemp" .= show wf
             ]
  forHuman = pack . show

docPeerSelectionActions :: Documented (PeerSelectionActionsTrace ntnAddr)
docPeerSelectionActions = Documented
  [  DocMsg
      (PeerStatusChanged anyProto)
      []
      ""
  ,  DocMsg
      (PeerStatusChangeFailure anyProto anyProto)
      []
      ""
  ,  DocMsg
      (PeerMonitoringError anyProto anyProto)
      []
      ""
  ,  DocMsg
      (PeerMonitoringResult anyProto anyProto)
      []
      ""
  ]

--------------------------------------------------------------------------------
-- Connection Manager Tracer
--------------------------------------------------------------------------------

namesForConnectionManager :: ConnectionManagerTrace ntnAddr cht -> [Text]
namesForConnectionManager TrIncludeConnection {}  = ["IncludeConnection"]
namesForConnectionManager TrUnregisterConnection {} = ["UnregisterConnection"]
namesForConnectionManager TrConnect {}  = ["Connect"]
namesForConnectionManager TrConnectError {} = ["ConnectError"]
namesForConnectionManager TrTerminatingConnection {} = ["TerminatingConnection"]
namesForConnectionManager TrTerminatedConnection {} = ["TerminatedConnection"]
namesForConnectionManager TrConnectionHandler {} = ["ConnectionHandler"]
namesForConnectionManager TrShutdown {} = ["Shutdown"]
namesForConnectionManager TrConnectionExists {} = ["ConnectionExists"]
namesForConnectionManager TrForbiddenConnection {} = ["ForbiddenConnection"]
namesForConnectionManager TrImpossibleConnection {} = ["ImpossibleConnection"]
namesForConnectionManager TrConnectionFailure {} = ["ConnectionFailure"]
namesForConnectionManager TrConnectionNotFound {} = ["ConnectionNotFound"]
namesForConnectionManager TrForbiddenOperation {} = ["ForbiddenOperation"]
namesForConnectionManager TrPruneConnections {}  = ["PruneConnections"]
namesForConnectionManager TrConnectionCleanup {} = ["ConnectionCleanup"]
namesForConnectionManager TrConnectionTimeWait {} = ["ConnectionTimeWait"]
namesForConnectionManager TrConnectionTimeWaitDone {} = ["ConnectionTimeWaitDone"]
namesForConnectionManager TrConnectionManagerCounters {} = ["ConnectionManagerCounters"]
namesForConnectionManager TrState {} = ["State"]
namesForConnectionManager ConnectionManager.TrUnexpectedlyFalseAssertion {} =
                            ["UnexpectedlyFalseAssertion"]

severityConnectionManager ::
  ConnectionManagerTrace addr
    (ConnectionHandlerTrace versionNumber agreedOptions) -> SeverityS
severityConnectionManager TrIncludeConnection {}                  = Debug
severityConnectionManager TrUnregisterConnection {}               = Debug
severityConnectionManager TrConnect {}                            = Debug
severityConnectionManager TrConnectError {}                       = Info
severityConnectionManager TrTerminatingConnection {}              = Debug
severityConnectionManager TrTerminatedConnection {}               = Debug
severityConnectionManager (TrConnectionHandler _ ev')             =
        case ev' of
          TrHandshakeSuccess {}     -> Info
          TrHandshakeClientError {} -> Notice
          TrHandshakeServerError {} -> Info
          TrError _ _ ShutdownNode  -> Critical
          TrError _ _ ShutdownPeer  -> Info

severityConnectionManager TrShutdown                              = Info
severityConnectionManager TrConnectionExists {}                   = Info
severityConnectionManager TrForbiddenConnection {}                = Info
severityConnectionManager TrImpossibleConnection {}               = Info
severityConnectionManager TrConnectionFailure {}                  = Info
severityConnectionManager TrConnectionNotFound {}                 = Debug
severityConnectionManager TrForbiddenOperation {}                 = Info

severityConnectionManager TrPruneConnections {}                   = Notice
severityConnectionManager TrConnectionCleanup {}                  = Debug
severityConnectionManager TrConnectionTimeWait {}                 = Debug
severityConnectionManager TrConnectionTimeWaitDone {}             = Debug
severityConnectionManager TrConnectionManagerCounters {}          = Info
severityConnectionManager TrState {}                              = Info
severityConnectionManager ConnectionManager.TrUnexpectedlyFalseAssertion {} =
                            Error

instance (Show addr, Show versionNumber, Show agreedOptions, LogFormatting addr,
          ToJSON addr, ToJSON versionNumber, ToJSON agreedOptions)
      => LogFormatting (ConnectionManagerTrace addr (ConnectionHandlerTrace versionNumber agreedOptions)) where
    forMachine dtal (TrIncludeConnection prov peerAddr) =
        mkObject $ reverse
          [ "kind" .= String "IncludeConnection"
          , "remoteAddress" .= forMachine dtal peerAddr
          , "provenance" .= String (pack . show $ prov)
          ]
    forMachine dtal (TrUnregisterConnection prov peerAddr) =
        mkObject $ reverse
          [ "kind" .= String "UnregisterConnection"
          , "remoteAddress" .= forMachine dtal peerAddr
          , "provenance" .= String (pack . show $ prov)
          ]
    forMachine _dtal (TrConnect (Just localAddress) remoteAddress) =
        mkObject
          [ "kind" .= String "ConnectTo"
          , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
          ]
    forMachine dtal (TrConnect Nothing remoteAddress) =
        mkObject
          [ "kind" .= String "ConnectTo"
          , "remoteAddress" .= forMachine dtal remoteAddress
          ]
    forMachine _dtal (TrConnectError (Just localAddress) remoteAddress err) =
        mkObject
          [ "kind" .= String "ConnectError"
          , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
          , "reason" .= String (pack . show $ err)
          ]
    forMachine dtal (TrConnectError Nothing remoteAddress err) =
        mkObject
          [ "kind" .= String "ConnectError"
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "reason" .= String (pack . show $ err)
          ]
    forMachine _dtal (TrTerminatingConnection prov connId) =
        mkObject
          [ "kind" .= String "TerminatingConnection"
          , "provenance" .= String (pack . show $ prov)
          , "connectionId" .= toJSON connId
          ]
    forMachine dtal (TrTerminatedConnection prov remoteAddress) =
        mkObject
          [ "kind" .= String "TerminatedConnection"
          , "provenance" .= String (pack . show $ prov)
          , "remoteAddress" .= forMachine dtal remoteAddress
          ]
    forMachine dtal (TrConnectionHandler connId a) =
        mkObject
          [ "kind" .= String "ConnectionHandler"
          , "connectionId" .= toJSON connId
          , "connectionHandler" .= forMachine dtal a
          ]
    forMachine _dtal TrShutdown =
        mkObject
          [ "kind" .= String "Shutdown"
          ]
    forMachine dtal (TrConnectionExists prov remoteAddress inState) =
        mkObject
          [ "kind" .= String "ConnectionExists"
          , "provenance" .= String (pack . show $ prov)
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "state" .= toJSON inState
          ]
    forMachine _dtal (TrForbiddenConnection connId) =
        mkObject
          [ "kind" .= String "ForbiddenConnection"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrImpossibleConnection connId) =
        mkObject
          [ "kind" .= String "ImpossibleConnection"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrConnectionFailure connId) =
        mkObject
          [ "kind" .= String "ConnectionFailure"
          , "connectionId" .= toJSON connId
          ]
    forMachine dtal (TrConnectionNotFound prov remoteAddress) =
        mkObject
          [ "kind" .= String "ConnectionNotFound"
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "provenance" .= String (pack . show $ prov)
          ]
    forMachine dtal (TrForbiddenOperation remoteAddress connState) =
        mkObject
          [ "kind" .= String "ForbiddenOperation"
          , "remoteAddress" .= forMachine dtal remoteAddress
          , "connectionState" .= toJSON connState
          ]
    forMachine dtal (TrPruneConnections peers) =
        mkObject
          [ "kind" .= String "PruneConnections"
          , "peers" .= toJSON (forMachine dtal `map` peers)
          ]
    forMachine _dtal (TrConnectionCleanup connId) =
        mkObject
          [ "kind" .= String "ConnectionCleanup"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrConnectionTimeWait connId) =
        mkObject
          [ "kind" .= String "ConnectionTimeWait"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrConnectionTimeWaitDone connId) =
        mkObject
          [ "kind" .= String "ConnectionTimeWaitDone"
          , "connectionId" .= toJSON connId
          ]
    forMachine _dtal (TrConnectionManagerCounters cmCounters) =
        mkObject
          [ "kind"  .= String "ConnectionManagerCounters"
          , "state" .= toJSON cmCounters
          ]
    forMachine _dtal (TrState cmState) =
        mkObject
          [ "kind"  .= String "ConnectionManagerState"
          , "state" .= listValue (\(addr, connState) ->
                                         object
                                           [ "remoteAddress"   .= toJSON addr
                                           , "connectionState" .= toJSON connState
                                           ])
                                       (Map.toList cmState)
          ]
    forMachine _dtal (ConnectionManager.TrUnexpectedlyFalseAssertion info) =
        mkObject
          [ "kind" .= String "UnexpectedlyFalseAssertion"
          , "info" .= String (pack . show $ info)
          ]
    forHuman = pack . show
    asMetrics (TrConnectionManagerCounters (ConnectionManagerCounters {..})) =
          [ IntM
              "cardano.node.connectionManager.prunableConns"
              (fromIntegral prunableConns)
          , IntM
              "cardano.node.connectionManager.duplexConns"
              (fromIntegral duplexConns)
          , IntM
              "cardano.node.connectionManager.unidirectionalConns"
              (fromIntegral uniConns)
          , IntM
              "cardano.node.connectionManager.incomingConns"
              (fromIntegral incomingConns)
          , IntM
              "cardano.node.connectionManager.outgoingConns"
              (fromIntegral outgoingConns)
            ]
    asMetrics _ = []

instance (Show versionNumber, ToJSON versionNumber, ToJSON agreedOptions)
  => LogFormatting (ConnectionHandlerTrace versionNumber agreedOptions) where
  forMachine _dtal (TrHandshakeSuccess versionNumber agreedOptions) =
    mkObject
      [ "kind" .= String "HandshakeSuccess"
      , "versionNumber" .= toJSON versionNumber
      , "agreedOptions" .= toJSON agreedOptions
      ]
  forMachine _dtal (TrHandshakeClientError err) =
    mkObject
      [ "kind" .= String "HandshakeClientError"
      , "reason" .= toJSON err
      ]
  forMachine _dtal (TrHandshakeServerError err) =
    mkObject
      [ "kind" .= String "HandshakeServerError"
      , "reason" .= toJSON err
      ]
  forMachine _dtal (TrError e err cerr) =
    mkObject
      [ "kind" .= String "Error"
      , "context" .= show e
      , "reason" .= show err
      , "command" .= show cerr
      ]


docConnectionManager :: Documented
  (ConnectionManagerTrace
    ntnAddr
    (ConnectionHandlerTrace
      ntnVersion
      ntnVersionData))
docConnectionManager = Documented
  [  DocMsg
      (TrIncludeConnection anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrUnregisterConnection anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrConnect Nothing anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectError Nothing anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrTerminatingConnection anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrTerminatedConnection anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionHandler anyProto anyProto)
      []
      ""
  ,  DocMsg
      TrShutdown
      []
      ""
  ,  DocMsg
      (TrConnectionExists anyProto anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrForbiddenConnection anyProto)
      []
      ""
  ,  DocMsg
      (TrImpossibleConnection anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionFailure anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionNotFound anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrForbiddenOperation anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrPruneConnections [])
      []
      ""
  ,  DocMsg
      (TrConnectionCleanup anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionTimeWait anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionTimeWaitDone anyProto)
      []
      ""
  ,  DocMsg
      (TrConnectionManagerCounters anyProto)
      [("cardano.node.connectionManager.prunableConns","")
      ,("cardano.node.connectionManager.duplexConns","")
      ,("cardano.node.connectionManager.unidirectionalConns","")
      ,("cardano.node.connectionManager.incomingConns","")
      ,("cardano.node.connectionManager.outgoingConns","")
      ]
      ""
  ,  DocMsg
      (TrState anyProto)
      []
      ""
  ,  DocMsg
      (ConnectionManager.TrUnexpectedlyFalseAssertion anyProto)
      []
      ""
  ]

--------------------------------------------------------------------------------
-- Server Tracer
--------------------------------------------------------------------------------

namesForServer :: ServerTrace ntnAddr -> [Text]
namesForServer TrAcceptConnection {}  = ["AcceptConnection"]
namesForServer TrAcceptError {}       = ["AcceptError"]
namesForServer TrAcceptPolicyTrace {} = ["AcceptPolicy"]
namesForServer TrServerStarted {}     = ["Started"]
namesForServer TrServerStopped {}     = ["Stopped"]
namesForServer TrServerError {}       = ["Error"]

severityServer ::  ServerTrace ntnAddr -> SeverityS
severityServer TrAcceptConnection {}  = Debug
severityServer TrAcceptError {}       = Error
severityServer TrAcceptPolicyTrace {} = Notice
severityServer TrServerStarted {}     = Notice
severityServer TrServerStopped {}     = Notice
severityServer TrServerError {}       = Critical

instance (Show addr, LogFormatting addr, ToJSON addr)
      => LogFormatting (ServerTrace addr) where
  forMachine dtal (TrAcceptConnection peerAddr)     =
    mkObject [ "kind" .= String "AcceptConnection"
             , "address" .= forMachine dtal peerAddr
             ]
  forMachine _dtal (TrAcceptError exception)         =
    mkObject [ "kind" .= String "AcceptErroor"
             , "reason" .= show exception
             ]
  forMachine dtal (TrAcceptPolicyTrace policyTrace) =
    mkObject [ "kind" .= String "AcceptPolicyTrace"
             , "policy" .= forMachine dtal policyTrace
             ]
  forMachine dtal (TrServerStarted peerAddrs)       =
    mkObject [ "kind" .= String "AcceptPolicyTrace"
             , "addresses" .= toJSON (forMachine dtal `map` peerAddrs)
             ]
  forMachine _dtal TrServerStopped                   =
    mkObject [ "kind" .= String "ServerStopped"
             ]
  forMachine _dtal (TrServerError exception)         =
    mkObject [ "kind" .= String "ServerError"
             , "reason" .= show exception
             ]
  forHuman = pack . show

docServer :: Documented (ServerTrace ntnAddr)
docServer = Documented
  [  DocMsg
      (TrAcceptConnection anyProto)
      []
      ""
  ,  DocMsg
      (TrAcceptError anyProto)
      []
      ""
  ,  DocMsg
      (TrAcceptPolicyTrace anyProto)
      []
      ""
  ,  DocMsg
      (TrServerStarted anyProto)
      []
      ""
  ,  DocMsg
      TrServerStopped
      []
      ""
  ,  DocMsg
      (TrServerError anyProto)
      []
      ""
  ]

--------------------------------------------------------------------------------
-- InboundGovernor Tracer
--------------------------------------------------------------------------------

namesForInboundGovernor :: InboundGovernorTrace peerAddr -> [Text]
namesForInboundGovernor TrNewConnection {}         = ["NewConnection"]
namesForInboundGovernor TrResponderRestarted {}    = ["ResponderRestarted"]
namesForInboundGovernor TrResponderStartFailure {} = ["ResponderStartFailure"]
namesForInboundGovernor TrResponderErrored {}      = ["ResponderErrored"]
namesForInboundGovernor TrResponderStarted {}      = ["ResponderStarted"]
namesForInboundGovernor TrResponderTerminated {}   = ["ResponderTerminated"]
namesForInboundGovernor TrPromotedToWarmRemote {}  = ["PromotedToWarmRemote"]
namesForInboundGovernor TrPromotedToHotRemote {}   = ["PromotedToHotRemote"]
namesForInboundGovernor TrDemotedToColdRemote {}   = ["DemotedToColdRemote"]
namesForInboundGovernor TrWaitIdleRemote {}        = ["WaitIdleRemote"]
namesForInboundGovernor TrMuxCleanExit {}          = ["MuxCleanExit"]
namesForInboundGovernor TrMuxErrored {}            = ["MuxErrored"]
namesForInboundGovernor TrInboundGovernorCounters {} = ["InboundGovernorCounters"]
namesForInboundGovernor TrRemoteState {}           = ["RemoteState"]
namesForInboundGovernor InboundGovernor.TrUnexpectedlyFalseAssertion {} =
                            ["UnexpectedlyFalseAssertion"]

severityInboundGovernor :: InboundGovernorTrace peerAddr -> SeverityS
severityInboundGovernor TrNewConnection {}           = Debug
severityInboundGovernor TrResponderRestarted {}      = Debug
severityInboundGovernor TrResponderStartFailure {}   = Error
severityInboundGovernor TrResponderErrored {}        = Info
severityInboundGovernor TrResponderStarted {}        = Debug
severityInboundGovernor TrResponderTerminated {}     = Debug
severityInboundGovernor TrPromotedToWarmRemote {}    = Info
severityInboundGovernor TrPromotedToHotRemote {}     = Info
severityInboundGovernor TrDemotedToColdRemote {}     = Info
severityInboundGovernor TrWaitIdleRemote {}          = Debug
severityInboundGovernor TrMuxCleanExit {}            = Debug
severityInboundGovernor TrMuxErrored {}              = Info
severityInboundGovernor TrInboundGovernorCounters {} = Info
severityInboundGovernor TrRemoteState {}             = Debug
severityInboundGovernor InboundGovernor.TrUnexpectedlyFalseAssertion {}
                                                     = Error

instance (ToJSON addr, Show addr)
      => LogFormatting (InboundGovernorTrace addr) where
  forMachine _dtal (TrNewConnection p connId)            =
    mkObject [ "kind" .= String "NewConnection"
             , "provenance" .= show p
             , "connectionId" .= toJSON connId
             ]
  forMachine _dtal (TrResponderRestarted connId m)       =
    mkObject [ "kind" .= String "ResponderStarted"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  forMachine _dtal (TrResponderStartFailure connId m s)  =
    mkObject [ "kind" .= String "ResponderStartFailure"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             , "reason" .= show s
             ]
  forMachine _dtal (TrResponderErrored connId m s)       =
    mkObject [ "kind" .= String "ResponderErrored"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             , "reason" .= show s
             ]
  forMachine _dtal (TrResponderStarted connId m)         =
    mkObject [ "kind" .= String "ResponderStarted"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  forMachine _dtal (TrResponderTerminated connId m)      =
    mkObject [ "kind" .= String "ResponderTerminated"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  forMachine _dtal (TrPromotedToWarmRemote connId opRes) =
    mkObject [ "kind" .= String "PromotedToWarmRemote"
             , "connectionId" .= toJSON connId
             , "result" .= toJSON opRes
             ]
  forMachine _dtal (TrPromotedToHotRemote connId)        =
    mkObject [ "kind" .= String "PromotedToHotRemote"
             , "connectionId" .= toJSON connId
             ]
  forMachine _dtal (TrDemotedToColdRemote connId od)     =
    mkObject [ "kind" .= String "DemotedToColdRemote"
             , "connectionId" .= toJSON connId
             , "result" .= show od
             ]
  forMachine _dtal (TrWaitIdleRemote connId opRes) =
    mkObject [ "kind" .= String "WaitIdleRemote"
             , "connectionId" .= toJSON connId
             , "result" .= toJSON opRes
             ]
  forMachine _dtal (TrMuxCleanExit connId)               =
    mkObject [ "kind" .= String "MuxCleanExit"
             , "connectionId" .= toJSON connId
             ]
  forMachine _dtal (TrMuxErrored connId s)               =
    mkObject [ "kind" .= String "MuxErrored"
             , "connectionId" .= toJSON connId
             , "reason" .= show s
             ]
  forMachine _dtal (TrInboundGovernorCounters counters) =
    mkObject [ "kind" .= String "InboundGovernorCounters"
             , "warmPeers" .= warmPeersRemote counters
             , "hotPeers" .= hotPeersRemote counters
             ]
  forMachine _dtal (TrRemoteState st) =
    mkObject [ "kind" .= String "RemoteState"
             , "remoteSt" .= toJSON st
             ]
  forMachine _dtal (InboundGovernor.TrUnexpectedlyFalseAssertion info) =
    mkObject [ "kind" .= String "UnexpectedlyFalseAssertion"
             , "remoteSt" .= String (pack . show $ info)
             ]
  forHuman = pack . show
  asMetrics (TrInboundGovernorCounters InboundGovernorCounters {..}) =
            [ IntM
                "cardano.node.inbound-governor.warm"
                (fromIntegral warmPeersRemote)
            , IntM
                "cardano.node.inbound-governor.hot"
                (fromIntegral hotPeersRemote)
              ]
  asMetrics _ = []

docInboundGovernor :: Documented (InboundGovernorTrace peerAddr)
docInboundGovernor = Documented
  [  DocMsg
      (TrResponderRestarted anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrResponderStartFailure anyProto anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrResponderErrored anyProto anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrResponderStarted anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrResponderTerminated anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrPromotedToWarmRemote anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrPromotedToHotRemote anyProto)
      []
      ""
  ,  DocMsg
      (TrDemotedToColdRemote anyProto anyProto)
      []
      "All mini-protocols terminated.  The boolean is true if this connection\
      \ was not used by p2p-governor, and thus the connection will be terminated."
  ,  DocMsg
      (TrWaitIdleRemote anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrMuxCleanExit anyProto)
      []
      ""
  ,  DocMsg
      (TrMuxErrored anyProto anyProto)
      []
      ""
  ,  DocMsg
      (TrInboundGovernorCounters anyProto)
      []
      ""
  ,  DocMsg
      (TrRemoteState anyProto)
      []
      ""
  ,  DocMsg
      (InboundGovernor.TrUnexpectedlyFalseAssertion anyProto)
      []
      ""
  ]
