module Desugar where

import Prelude((<>), (<$>), ($), (<<<), show)

import Data.Maybe(Maybe(..), fromJust)
import Data.Tuple (Tuple(..), lookup)
import Data.List(List(..), (:), fromFoldable, reverse)

import Partial.Unsafe(unsafePartial)

import AST as AST
import Syntax as S

packConstructor :: Array (Tuple S.CorePAST { name  :: String
                                           , arity :: Int
                                           , tag   :: Int
                                           })
packConstructor = [ p (S.Constr "Cons") (constructorTriple "Cons" 0 2)
                  , p (S.Constr "Nil" ) (constructorTriple "Nil" 1 0)
                  , p (S.Constr "true") (constructorTriple "true" 2 0)
                  , p (S.Constr "false") (constructorTriple "false" 3 0)
                  ]
                  where p = Tuple

constructorTriple n t a
  = { name: n
    , tag: t
    , arity: a
    }
mkConstructor { name, tag, arity } = AST.Constr name tag arity

desugarSCs :: S.Program -> List AST.CoreSC
desugarSCs pscs = desugarSC <$> pscs
  where
        desugarSC (S.Func fname arg body)
                          = AST.Func fname arg (desugarAST body)

desugarAST :: S.CorePAST -> AST.CoreExpr
desugarAST (S.Var n) = AST.Var n
desugarAST (S.App f e) = AST.App (desugarAST f) (desugarAST e)
desugarAST cons@(S.Constr _) = mkConstructor $ unsafePartial $ fromJust $
  lookup cons packConstructor
desugarAST (S.Lam args body) = AST.Lam args (desugarAST body)
desugarAST (S.Let rec bind body)
  = AST.Let rec (desugarBinders <$> bind) (desugarAST body)
desugarAST (S.Case sucritinise alters)
  = AST.Case (desugarAST sucritinise) (desugarAlters <$> alters)
desugarAST (S.If cond consequent alternative)
  = AST.Case (desugarAST cond) $
      fromFoldable
        [ { caseTag: 2
          , vars: Nil
          , rhs: desugarAST consequent
          , cons: AST.Constr "true" 2 0
          , arity: 0
          , name: "true"
          }
        , { caseTag: 3
          , vars: Nil
          , rhs: desugarAST alternative
          , cons: AST.Constr "false" 3 0
          , arity: 0
          , name: "false"
          }
        ]
desugarAST (S.Lit (S.LInt num)) = AST.Num num
desugarAST (S.Lit (S.LBool bool)) = mkConstructor $
                                    unsafePartial $
                                    fromJust $
                                    lookup ((S.Constr (show bool)) :: S.CorePAST)
                                      packConstructor

desugarBinders (Tuple n exp) = Tuple n (desugarAST exp)
desugarAlters { cons, rhs } = let { name, tag, arity }
                                    = unsafePartial $
                                      fromJust $
                                      lookup cons packConstructor
                              in { caseTag: tag
                                 , vars: unsafePartial $ collectVars $ cons
                                 , rhs: desugarAST rhs
                                 , cons: desugarAST cons
                                 , arity: arity
                                 , name: name
                                 }
  where
    desugarConstr :: Partial => S.CorePAST -> AST.CoreExpr
    desugarConstr cons@(S.Constr _) = mkConstructor $
                                      fromJust $
                                      lookup cons packConstructor
    desugarConstr (S.App cons val) = desugarConstr cons

    collectVars :: Partial => S.CorePAST -> List AST.Name
    collectVars = reverse <<< collectVars'
      where
        collectVars' :: Partial => S.CorePAST -> List AST.Name
        collectVars' (S.App (S.Constr _) (S.Var v)) = v : Nil
        collectVars' (S.App cons (S.Var v)) = v : (collectVars cons)
        collectVars' _ = Nil
