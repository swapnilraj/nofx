module Lift where

import Prelude (class Show, (<$>), (<>), ($), (<<<), show, bind, discard, not, pure, otherwise)
import Data.List (List(..), (:), concatMap, filter, foldl, null, zip)
import Data.Set
  ( Set(..)
  , delete
  , difference
  , empty
  , fromFoldable
  , insert
  , member
  , union
  , unions
  , singleton
  , toUnfoldable
  )
import Data.Tuple (Tuple(..), fst, snd, lookup)
import Data.Tuple.Nested
import Data.Maybe (Maybe(..))
import Data.Generic.Rep
import Data.Generic.Rep.Show
import Control.Apply
import Partial.Unsafe (unsafePartial)
import AST
import Utility


type AnnAlter a b
  = { caseTag :: Int
    , vars :: List a
    , rhs :: AnnExpr a b
    , cons :: AnnExpr a b -- Constructor
    , arity :: Int
    , name :: Name
    }

data AnnSC a b
  = AnnFunc Name (List a) (AnnExpr a b)

derive instance genericAnnSC :: Generic (AnnSC a b) _

instance showAnnSC :: (Show b, Show a) => Show (AnnSC a b) where
  show x = genericShow x

type AnnProgram a b
  = List (AnnSC a b)

type AnnDefn a b
  = Tuple a (AnnExpr a b)

type AnnExpr a b
  = Tuple b (AnnExpr' a b)

data AnnExpr' a b
  = AVar Name
  | ANum Int
  | AApp (AnnExpr a b) (AnnExpr a b)
  | AConstr
    Name
    Int -- Tag
    Int -- Arity of constructor
  | ALam (List a) (AnnExpr a b)
  | ALet
    IsRec
    (List (AnnDefn a b))
    (AnnExpr a b)
  | ACase
    (AnnExpr a b)
    (List (AnnAlter a b))

derive instance genericAnnExpr' :: Generic (AnnExpr' a b) _

instance showAnnExpr' :: (Show b, Show a) => Show (AnnExpr' a b) where
  show x = genericShow x

globalOperators :: Set Name
globalOperators = fromFoldable
  [ "add"
  , "sub"
  , "mul"
  , "div"
  , "eq"
  , "neq"
  , "lt"
  , "le"
  , "gt"
  , "geq"
  ]

lambdaLift :: CoreProgram -> CoreProgram
lambdaLift = collectSCs <<< abstract <<< freeVars

bindersOf :: forall b r. (List (b /\ r)) -> List b
bindersOf def = fst <$> def

rhsOf :: forall b r. (List (b /\ r)) -> List r
rhsOf def = snd <$> def

freeVars :: CoreProgram -> AnnProgram Name (Set Name)
freeVars prog = ado
  (Func scName args body) <- prog
  in (AnnFunc scName args $
      freeVarsExpression (union (fromFoldable args) globalOperators) body)

freeVarsExpression :: (Set Name) -> CoreExpr -> AnnExpr Name (Set Name)
freeVarsExpression lv (Var n)
  | member n lv = p empty $ AVar n
  | otherwise = p (singleton n) $ AVar n
freeVarsExpression lv (Num n) = p empty $ ANum n
freeVarsExpression lv (App e1 e2) =
  let
    e1' = freeVarsExpression lv e1
    e2' = freeVarsExpression lv e2
  in
    (p (union (freeVarsOf e1') (freeVarsOf e2')) (AApp e1' e2'))
freeVarsExpression lv (Constr n t a) = p empty $ AConstr n t a
freeVarsExpression lv (Lam args body) =
  let
    lv' = union lv (fromFoldable args)
    body' = freeVarsExpression lv' body
  in
    p (difference (freeVarsOf body') (fromFoldable args)) (ALam args body')
freeVarsExpression lv (Let isRec defs body) =
  let
    binders = bindersOf defs
    binderSet = fromFoldable binders
    body_lv = union lv binderSet
    rhs_lv
      | isRec = body_lv
      | otherwise = lv
    rhs' = freeVarsExpression rhs_lv <$> (rhsOf defs)
    freeInRhs = unions $ freeVarsOf <$> rhs'
    defsFree
      | isRec = difference freeInRhs binderSet
      | otherwise = freeInRhs
    body' = freeVarsExpression body_lv body
    bodyFree = difference (freeVarsOf body') binderSet
  in
    p (union defsFree bodyFree) $ ALet isRec (zip binders rhs') body'
freeVarsExpression lv (Case e alts) =
  let
    e' = freeVarsExpression lv e
    alts' = annotateAlters <$> alts
    annotateAlters :: Alter Name -> AnnAlter Name (Set Name)
    annotateAlters { caseTag, vars, rhs, cons, arity, name } =
      { caseTag: caseTag
      , vars: vars
      , rhs: freeVarsExpression (union lv (fromFoldable vars)) rhs
      , cons: p empty $ AConstr name caseTag arity
      , arity: arity
      , name: name
      }
    free = unions $ freeVarsOf_alt alts'
  in
    p (union (freeVarsOf e') free) (ACase e' alts')
freeVarsOf_alt alters = freeVarsOf_alt' <$> alters
  where
  freeVarsOf_alt' { caseTag, vars, rhs, cons } = difference (freeVarsOf rhs) (fromFoldable vars)

p = Tuple

freeVarsOf (Tuple freevs _) = freevs

abstract :: AnnProgram Name (Set Name) -> CoreProgram
abstract prog = ado
  (AnnFunc scName args rhs) <- prog
  in (Func scName args (abstractExpression rhs))

abstractExpression :: AnnExpr Name (Set Name) -> CoreExpr
abstractExpression (_ /\ (AVar v)) = Var v
abstractExpression (_ /\ (ANum n)) = Num n
abstractExpression (_ /\ (AApp e1 e2))
  = App (abstractExpression e1) (abstractExpression e2)
abstractExpression (_ /\ (AConstr t a n)) = Constr t a n
abstractExpression (free /\ (ALam args body)) =
  let
    fvList = toUnfoldable free
    sc = Let false (Cons ("$" /\ scRhs) Nil) (Var "$")
    scRhs = Lam (fvList <> args) (abstractExpression body)
  in
    foldl App sc (Var <$> fvList)
abstractExpression (_ /\ (ALet isRec defs body)) =
  let
    defs' = abstractDefs defs
    abstractDefs defs = ado
      (name /\ rhs) <- defs
      in name /\ (abstractExpression rhs)
  in
    Let isRec defs' (abstractExpression body)
abstractExpression (_ /\ (ACase e alts)) =
  let
    abstractAlts { caseTag, vars, rhs, cons, arity, name } =
      { caseTag: caseTag
      , vars: vars
      , rhs: abstractExpression rhs
      , cons: abstractExpression cons
      , arity: arity
      , name: name
      }
  in
    Case (abstractExpression e) (abstractAlts <$> alts)

collectSCs prog = concatMap collectOneSC prog
  where
  collectOneSC (Func scName args rhs) = (Func scName args rhs') : scs
    where
    (scs /\ rhs') = collectSCsExpression rhs

collectSCsExpression :: CoreExpr -> (List CoreSC) /\ CoreExpr
collectSCsExpression (Num k) = p Nil $ Num k
collectSCsExpression (Var n) = p Nil $ Var n
collectSCsExpression (App e1 e2) =
  let
    (scs1 /\ e1') = collectSCsExpression e1
    (scs2 /\ e2') = collectSCsExpression e2
  in
    p (scs1 <> scs2) $ App e1' e2'
collectSCsExpression (Lam args body) =
  let
    (scs /\ body') = collectSCsExpression body
  in
    p scs $ Lam args body'
collectSCsExpression (Constr n t a) = p Nil $ Constr n t a
collectSCsExpression (Let isRec defs body) =
  let
    collectSCsExpressionDef scs (name /\ rhs) =
      let
        (rhsScs /\ rhs') = collectSCsExpression rhs
      in
        (scs <> rhsScs) /\ (name /\ rhs)
    (rhssScs /\ defs') = mapAccuml collectSCsExpressionDef Nil defs
    scs' = filter (\(name /\ rhs) -> isLam rhs) defs'
    nonScs' = filter (\(name /\ rhs) -> not (isLam rhs)) defs'

    localScs =
      unsafePartial
        $ ( \(name /\ (Lam args body)) ->
              (Func name args body)
          )
        <$> scs'
    (bodyScs /\ body') = collectSCsExpression body
  in
    {-- p (bodyScs <> localScs <> rhssScs) $ (Let isRec nonScs' body') --}
    p (bodyScs <> localScs <> rhssScs) $
    if null nonScs'
      then body'
      else (Let isRec nonScs' body')
collectSCsExpression (Case e alts) =
  let
    collectSCs_alt scs alters =
      let
        (scs_rhs /\ rhs') = collectSCsExpression alters.rhs
      in
        p (scs <> scs_rhs) $ alters { rhs = rhs' }
    (scsE /\ e') = collectSCsExpression e
    (scsAlters /\ alts') = mapAccuml collectSCs_alt Nil alts
  in
    p (scsE <> scsAlters) $ Case e' alts'

isLam :: CoreExpr -> Boolean
isLam (Lam _ _) = true
isLam _ = false

rename :: CoreProgram -> CoreProgram
rename prog = snd $ mapAccuml renameSC initialNameSupply prog
  where
  renameSC ns (Func scName args rhs) =
    let
      (ns1 /\ args' /\ env) = newNames ns args
      (ns2 /\ rhs') = renameExpression env ns1 rhs
    in
      (ns2 /\ Func scName args' rhs')

renameExpression :: ASSOC Name Name -> NameSupply -> CoreExpr -> (NameSupply /\ CoreExpr)
renameExpression _ ns (Num k) = ns /\ Num k
renameExpression env ns (Var n) = case lookup n env of
  Just n' -> ns /\ Var n'
  Nothing -> ns /\ Var n
renameExpression env ns (App e1 e2) =
  let
    (ns1 /\ e1') = renameExpression env ns e1
    (ns2 /\ e2') = renameExpression env ns1 e2
  in
    ns2 /\ App e1' e2'
renameExpression env ns (Lam args body) =
  let
    (ns1 /\ args' /\ env') = newNames ns args
    (ns2 /\ body') = renameExpression (env <> env') ns1 body
  in
    ns2 /\ Lam args' body'
renameExpression _ ns (Constr n t a) = ns /\ Constr n t a
renameExpression env ns (Let isRec defs body) =
  let
    binders = bindersOf defs
    (ns1 /\ _) = renameExpression env ns body -- Hack to bring ns1 into scope
    (ns2 /\ binders' /\ env') = newNames ns1 binders
    bodyEnv = env' <> env
    (_ /\ body') = renameExpression bodyEnv ns body -- Correct body'
    rhsEnv
      | isRec = bodyEnv
      | otherwise = env
    (ns3 /\ rhs') = mapAccuml (renameExpression rhsEnv) ns2 (rhsOf defs)
  in
    ns3 /\ Let isRec (zip binders' rhs') body'
renameExpression env ns (Case e alts) =
  let
    renameAlt ns alter =
      let
        (ns1 /\ vars' /\ env') = newNames ns alter.vars
        (ns2 /\ rhs') = renameExpression (env' <> env) ns1 alter.rhs
      in
        ns2 /\ alter { vars = vars', rhs = rhs' }
    (ns1 /\ e') = renameExpression env ns e
    (ns2 /\ alts') = mapAccuml renameAlt ns alts
  in
    ns2 /\ Case e' alts'
