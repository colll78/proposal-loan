{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module CrowdProposal.Utils (
  ptryLookupValue
  , pisScriptCredential
  , pexactlyOneOwnInput
  , plovelaceValueOf
  , pmustFind
  , pmustExist
  , pheadSingleton
  , pcond
  , pand'List
) where

import Plutarch.Prelude
import Plutarch.LedgerApi.V3
import Plutarch.Builtin
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Bool (pand')

ptryLookupValue ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PValue keys amounts
        :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
    )
ptryLookupValue = phoistAcyclic $
  plam $ \policyId val ->
    pmatch val $ \(PValue val') ->
      precList
        ( \self x xs ->
            pif
              (pfstBuiltin # x #== policyId)
              ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                  tokens
              )
              (self # xs)
        )
        (const perror)
        # pto val'

pexactlyOneOwnInput :: Term s (PCredential :--> PBuiltinList PTxInInfo :--> PBool)
pexactlyOneOwnInput =
  phoistAcyclic $ plam $ \ownCred ->
    let 
        go = pfix #$ plam $ \self -> 
              pelimList 
                (\x xs -> 
                  plet (pfield @"credential" # (pfield @"address" # (pfield @"resolved" # x))) $ \cred -> 
                    pif ( pisScriptCredential cred )
                        ( pif (cred #== ownCred) 
                              ( go2 # xs )
                              ( self # xs )
                        )  
                        ( self # xs )
                )
                (pconstant False)

    
        go2 = pfix #$ plam $ \self -> 
                pelimList 
                  (\x xs -> 
                    plet (pfield @"credential" # (pfield @"address" # (pfield @"resolved" # x))) $ \cred -> 
                      pif (pisScriptCredential cred)
                          ( pif (cred #== ownCred) 
                                perror 
                                (self # xs)
                          )  
                          (self # xs)
                  )
                  (pconstant True)
     in go  

pisScriptCredential :: Term s PCredential -> Term s PBool 
pisScriptCredential cred = (pfstBuiltin # (pasConstr # punsafeCoerce cred) #== 1)

plovelaceValueOf ::
  forall (anyKey :: KeyGuarantees) (anyAmount :: AmountGuarantees) (s :: S).
  Term s (PValue anyKey anyAmount) -> Term s PInteger
plovelaceValueOf txoVal = 
  let adaCSEntry = phead # (pto $ pto txoVal)
  in pfromData (psndBuiltin #$ phead #$ pto $ pfromData $ psndBuiltin # adaCSEntry) 

pmustFind :: PIsListLike l a => Term s ((a :--> PBool) :--> l a :--> a)
pmustFind =
  phoistAcyclic $ plam $ \f -> pfix #$ plam $ \self xs -> pelimList (\y ys -> pif (f # y) y (self # ys)) perror xs

pmustExist :: PIsListLike l a => Term s ((a :--> PBool) :--> l a :--> PBool)
pmustExist =
  phoistAcyclic $ 
    plam $ \f -> 
      pfix #$ plam $ \self xs -> pelimList (\y ys -> pif (f # y) (pconstant True) (self # ys)) perror xs

-- Get the head of the list if the list contains exactly one element, otherwise error.
pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList
      (pelimList (\_ _ -> ptraceInfoError "List contains more than one element."))
      (ptraceInfoError "List is empty.")
      xs

pand'List :: [Term s PBool] -> Term s PBool
pand'List ts' =
  case ts' of
    [] -> pconstant True
    ts -> foldl1 (\res x -> pand' # res # x) ts

pcond :: [(Term s PBool, Term s a)] -> Term s a -> Term s a
pcond [] def = def
pcond ((cond, x) : conds) def = pif cond x $ pcond conds def