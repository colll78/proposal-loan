{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module CrowdProposal.NodeSpending (pNodeSpending) where 

import Plutarch.Prelude
import Plutarch.LedgerApi.V3.Contexts (PScriptContext, PAddress, PCredential, PProposalProcedure, PGovernanceAction)
import qualified Plutarch.Monadic as P

pNodeSpending ::
  Term s (PCurrencySymbol :--> PScriptContext :--> PUnit)
pNodeSpending  = plam $ \ctx -> P.do
  ctxF <- pletFields @["txtxInfoF", "redeemer", "scriptInfo"] ctx
  txInfoF <- pletFields @["mint"] ctxF.txInfo

  pure $ pconstant ()


pNodeMinting ::
  Term s (PScriptContext :--> PUnit)
pNodeMinting  = plam $ \ctx -> P.do
  ctxF <- pletFields @["txtxInfoF", "redeemer", "scriptInfo"] ctx
  txInfoF <- pletFields @["mint"] ctxF.txInfo

  pure $ pconstant ()