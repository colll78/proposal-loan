{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ProposalLend (pProposalLoanSpending) where 

import CrowdProposal.Utils (pexactlyOneOwnInput, plovelaceValueOf, pmustFind, pmustExist)
import Plutarch.Prelude
import Plutarch.LedgerApi.V3 
import qualified Plutarch.Monadic as P

newtype PProposalLoanDatum (s :: S)
  = PProposalLoanDatum
      ( Term
          s
          ( PDataRecord
              '[ "lenderRewardAccount" ':= PCredential
               , "fee" ':= PInteger
               ]
          )
      )
  deriving anyclass
    ( PlutusType
    , PIsData
    , PDataFields
    )

instance DerivePlutusType PProposalLoanDatum where 
  type DPTStrat _ = PlutusTypeData 

newtype PProposalLoanAction (s :: S)
  = PProposalLoanAction
      ( Term
          s
          ( PDataRecord
              '[ "lenderRewardAccount" ':= PCredential
               , "lenderAddress" ':= PAddress
               , "fee" ':= PInteger
               ]
          )
      )
  deriving anyclass
    ( PlutusType
    , PIsData
    , PDataFields
    )

instance DerivePlutusType PProposalLoanAction where 
  type DPTStrat _ = PlutusTypeData 

pProposalLoanSpending ::
  Term s (PScriptContext :--> PUnit)
pProposalLoanSpending  = plam $ \ctx -> P.do 
  ctxF <- pletFields @["txtxInfoF", "redeemer", "scriptInfo"] ctx
  txInfoF <- pletFields @["inputs", "outputs", "proposalProcedures"] ctxF.txInfo
  txInputs <- plet $ txInfoF.inputs
  scriptInfoF <- pletFields @["_1"] ctxF.scriptInfo 
  let rdmr = punsafeCoerce @PGovernanceAction (pto ctxF.redeemer)
  ownInputRef <- plet $ scriptInfoF._0
  ownInputCred <- plet $ pfield @"credential" #$
    pfield @"address" #$ 
      pfield @"resolved" #$
        pmustFind (\txIn -> pfield @"outRef" # txIn #== ownInputRef) txInputs

  PDJust ownInputDatum <- pmatch scriptInfoF._1
  ownInputDatumF <- pletFields @["lenderRewardAccount", "lenderAddress", "fee"] (pto ownInputDatum)
  let expectedProposal = 
        pforgetData $ pdata $ pcon $ PProposalProcedure $
          pdcons @"deposit" # (pdata 100_000_000_000) #$
          pdcons @"returnAddr" # ownInputDatumF.lenderRewardAccount #$
          pdcons @"governanceAction" # rdmr #$
          pdnil
      actualProposal = phead # txInfoF.proposalProcedures
      hasFeeOutput = 
        pmustExist @PBuiltinList 
          # plam(\out -> 
              (pfield @"address" # out) #== ownInputDatumF.lenderAddress
                #&& (plovelaceValueOf out #== ownInputDatumF.fee)
            )   
          # txInfoF.outputs  
      mintChecks = 
        pand'List
          [ hasFeeOutput
          , pexactlyOneOwnInput # ownInputCred # txInputs
          , actualProposal #== expectedProposal 
          ] 
  pure $
    pif ( mintChecks ) (pconstant ()) perror 


