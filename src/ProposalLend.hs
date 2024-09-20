{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module ProposalLend (pProposalLoanSpending) where 

import CrowdProposal.Utils (pexactlyOneOwnInput, plovelaceValueOf, pmustFind, pmustExist, pand'List)
import Plutarch.Prelude
import Plutarch.LedgerApi.V3 
import qualified Plutarch.Monadic as P
import Plutarch.DataRepr hiding (ptoFields, PFields)
import Plutarch.Builtin
import Plutarch.Unsafe
import Plutarch.DataRepr.Internal.Field  

data PProposalLoanDatum (s :: S)
  = PProposalLoanDatum
      ( Term
          s
          ( PDataRecord
              '[ "lenderRewardAccount" ':= PCredential
               , "lenderAddress" ':= PAddress
               , "fee" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass
    ( PlutusType
    , PIsData
    , PDataFields
    )

instance DerivePlutusType PProposalLoanDatum where 
  type DPTStrat _ = PlutusTypeData 

data PProposalLoanAction (s :: S)
  = PAcceptLoan (Term s (PDataRecord '[ "govAct" ':= PGovernanceAction ]))
  | PCancelLoan (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass
    ( PlutusType
    , PIsData
    )

instance DerivePlutusType PProposalLoanAction where 
  type DPTStrat _ = PlutusTypeData 

pProposalLoanSpending ::
  Term s (PScriptContext :--> PUnit)
pProposalLoanSpending  = plam $ \ctx -> P.do 
  ctxF <- pletFields @'["txInfo", "redeemer", "scriptInfo"] ctx
  txInfoF <- pletFields @'["inputs", "outputs", "proposalProcedures"] ctxF.txInfo
  txInputs <- plet $ punsafeCoerce txInfoF.inputs
  -- let isSpending = pfstBuiltin # scriptInfoConstrPair #== 1

  --PSpendingScript sinf' <- pmatch ctxF.scriptInfo 
  scriptInfoF <- pletFieldsSpending (pforgetData ctxF.scriptInfo)
  ownInputRef <- plet $ scriptInfoF._0

  let rdmr = punsafeCoerce @_ @_ @(PAsData PGovernanceAction) (pto ctxF.redeemer)
  ownInputCred <- plet $ pfield @"credential" #$
                    pfield @"address" #$ 
                      pfield @"resolved" #$
                        pmustFind @PBuiltinList # plam (\txIn -> (pfield @"outRef" # txIn) #== ownInputRef) # txInputs
  PDJust ((pfield @"_0" #) -> ownInDat) <- pmatch scriptInfoF._1
  ownInputDatumF <- pletFields @'["lenderRewardAccount", "lenderAddress", "fee"] (punsafeCoerce @_ @_ @PProposalLoanDatum (pto ownInDat))
  let expectedProposal = 
        pdata $ pcon $ PProposalProcedure $
          pdcons @"deposit" # pconstantData 100_000_000_000 #$
          pdcons @"returnAddr" # ownInputDatumF.lenderRewardAccount #$
          pdcons @"governanceAction" # rdmr #$
          pdnil
      actualProposal = phead # txInfoF.proposalProcedures
      hasFeeOutput = 
        pmustExist @PBuiltinList 
          # plam (\out -> 
              (pfield @"address" # out) #== ownInputDatumF.lenderAddress #&& 
              ownInputDatumF.fee #<= plovelaceValueOf (pfield @"value" # out)
            )  
          # txInfoF.outputs  
      mintChecks = 
        pand'List
          [ hasFeeOutput
          , pexactlyOneOwnInput # ownInputCred # txInputs
          , actualProposal #== expectedProposal 
          ] 
  pif ( mintChecks ) (pconstant ()) perror 

-- pletFieldsSpending :: Term s PData -> PScriptInfoHRec s
-- pletFieldsSpending term =
--   let constrPair = pasConstr # term
--       fields = psndBuiltin # constrPair
--       checkedFields = 
--         pif ((pfstBuiltin # constrPair) #== 1)
--           fields
--           perror
--       outRef = punsafeCoerce @_ @_ @(PAsData PTxOutRef) $ phead # checkedFields
--       datum = punsafeCoerce @_ @_ @(PAsData (PMaybeData PDatum)) $ phead # (ptail # checkedFields)
--   in HCons (Labeled @"_0" outRef) (HCons (Labeled @"_1" datum) HNil)
-- pletFieldsSpending :: Term s PData -> PScriptInfoHRec s
-- pletFieldsSpending term =
--   plet (pasConstr # term) $ \constrPair ->
--     plet (psndBuiltin # constrPair) $ \fields ->
--       plet (pif ((pfstBuiltin # constrPair) #== 1) fields perror) $ \checkedFields ->
--         let outRef = punsafeCoerce @_ @_ @(PAsData PTxOutRef) $ phead # checkedFields
--             datum = punsafeCoerce @_ @_ @(PAsData (PMaybeData PDatum)) $ phead # (ptail # checkedFields)
--         in HCons (Labeled @"_0" outRef) (HCons (Labeled @"_1" datum) HNil)

type PScriptInfoHRec (s :: S) =
  HRec
    '[ '("_0", Term s (PAsData PTxOutRef))
     , '("_1", Term s (PAsData (PMaybeData PDatum)))
     ]

pletFieldsSpending ::  forall {s :: S} {r :: PType}. Term s PData -> (PScriptInfoHRec s -> Term s r) -> Term s r 
pletFieldsSpending term = runTermCont $ do
  constrPair <- tcont $ plet $ pasConstr # term
  fields <- tcont $ plet $ psndBuiltin # constrPair
  checkedFields <- tcont $ plet $ pif ((pfstBuiltin # constrPair) #== 1) fields perror
  let outRef = punsafeCoerce @_ @_ @(PAsData PTxOutRef) $ phead # checkedFields
      datum = punsafeCoerce @_ @_ @(PAsData (PMaybeData PDatum)) $ phead # (ptail # checkedFields)
  tcont $ \f -> f $ HCons (Labeled @"_0" outRef) (HCons (Labeled @"_1" datum) HNil)

-- pletFieldsSpending :: Term s PData -> PScriptInfoHRec s
-- pletFieldsSpending term =
--   plet (pasConstr # term) $ \constrPair -> 
--     pif ((pfstBuiltin # constrPair) #== 1)
--       (
--         plet (psndBuiltin # constrPair) $ \fields -> 
--           let outRef = punsafeCoerce @_ @_ @(PAsData PTxOutRef) $ phead # fields
--               datum = punsafeCoerce @_ @_ @(PAsData (PMaybeData PDatum)) $ phead # (ptail # fields)
--           in HCons (Labeled @"_0" outRef) (HCons (Labeled @"_1" datum) HNil)
--       )
--       perror 

-- pletFieldsSpending :: Term s PData -> PScriptInfoHRec s
-- pletFieldsSpending term = P.do 
--   constrPair <- plet $ pasConstr # term
--   fields <- plet $ psndBuiltin # constrPair
  
--   let outRef = punsafeCoerce @_ @_ @(PAsData PTxOutRef) $ phead # fields
--       datum = punsafeCoerce @_ @_ @(PAsData (PMaybeData PDatum)) $ phead # (ptail # fields)
  
--   pif ((pfstBuiltin # constrPair) #== 1)
--       (HCons (Labeled @"_0" outRef) (HCons (Labeled @"_1" datum) HNil))
--       perror
      


-- pletFieldsConstr ::
--   forall fs a s b ps bs.
--   ( PDataFields a
--   , ps ~ PFields a
--   , bs ~ Bindings ps fs
--   , BindFields ps bs
--   , PIsData a
--   ) =>
--   Integer ->
--   Term s a ->
--   (HRecOf a fs s -> Term s b) ->
--   Term s b
-- pletFieldsConstr idx term =
--   runTermCont $ do
--     constrPair <- tcont $ plet $ pasConstr #$ pforgetData $ pdata term
--     fields <- tcont $ plet $ psndBuiltin # constrPair
--     checkedFields <- tcont $ plet $
--       pif ((pfstBuiltin # constrPair) #== pconstant idx)
--         fields
--         perror
--     bindFields @ps @bs (Proxy @bs) (punsafeCoerce @_ @_ @(PDataRecord ps) checkedFields)