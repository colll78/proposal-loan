{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module AlwaysFails (pProposalLoanSpending) where 

import Plutarch.Prelude
import Plutarch.LedgerApi.V3.Contexts (PScriptContext, PAddress, PCredential, PProposalProcedure, PGovernanceAction)
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
  ctxF <- pletFields @["txInfo", "redeemer", "scriptInfo"] ctx
  scriptInfoF <- pletFields @["_0", "_1"] ctxF.scriptInfo 
  let rdmr = punsafeCoerce @PGovernanceAction (pto ctxF.redeemer)
  let ownInputRef = scriptInfoF._0 
  PDJust ownInputDatum <- pmatch scriptInfoF._1
  ownInputDatumF <- pletFields @["lenderRewardAccount", "lenderAddress", "fee"] (pto ownInputDatum)
  let expectedProposal = 
        pforgetData $ pdata $ pcon $ PProposalProcedure $
          pdcons @"deposit" # (pdata 100_000_000_000) #$
          pdcons @"returnAddr" # ownInputDatumF.lenderRewardAccount #$
          pdcons @"governanceAction" # rdmr #$
          pdnil
      actualProposal = phead # ctxF.proposalProcedures
      hasFeeOutput = 
        pmustExist @PBuiltinList 
          # plam(\out -> 
              (pfield @"address" # out) #== ownInputDatumF.lenderAddress
                #&& (plovelaceValueOf out #== ownInputDatumF.fee)
            )   
          # info.outputs  
      mintChecks = 
        pand'List
          [ hasFeeOutput
          , actualProposal #== expectedProposal 
          ] 
    pure $
      pif ( mintChecks ) (pconstant ()) perror 

plovelaceValueOf ::
  forall (anyKey :: AssocMap.KeyGuarantees) (anyAmount :: AmountGuarantees) (s :: S).
  Term s (PValue anyKey anyAmount) -> PInteger
plovelaceValueOf txoVal = 
  let adaCSEntry = phead # (pto $ pto txoVal)
  in (psndBuiltin #$ phead #$ pto $ pfromData $ psndBuiltin # adaCSEntry) 