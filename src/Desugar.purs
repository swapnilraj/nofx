module Desugar where

import Prelude((<>), (<$>), ($), (<<<), show)

import Data.Maybe(Maybe(..), fromJust)
import Data.Tuple (Tuple(..), lookup)
import Data.List(List(..), (:), fromFoldable, reverse)

import Partial.Unsafe(unsafePartial)

import AST as AST
import Syntax as S

packConstructor :: Array (Tuple String AST.CoreExpr)
packConstructor = [ p "Cons" $ AST.Constr "Cons" 0 2
                  , p "Nil" $ AST.Constr "Nil" 1 0
                  , p "true" $ AST.Constr "true" 2 0
                  , p "false" $ AST.Constr "false" 3 0
                  ]
                  where p = Tuple

desugarSCs :: S.Program -> List AST.CoreSC
desugarSCs pscs = desugarSC <$> pscs
  where
        desugarSC (S.Func fname arg body)
                          = AST.Func fname arg (desugarAST body)

desugarAST :: S.CorePAST -> AST.CoreExpr
desugarAST (S.Var n) = AST.Var n
desugarAST (S.App f e) = AST.App (desugarAST f) (desugarAST e)
desugarAST (S.Constr name) = unsafePartial $ fromJust $ lookup name packConstructor
desugarAST (S.Lam arg body) = AST.Lam arg (desugarAST body)
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
          }
        , { caseTag: 3
          , vars: Nil
          , rhs: desugarAST alternative
          , cons: AST.Constr "false" 3 0
          }
        ]
desugarAST (S.Lit (S.LInt num)) = AST.Num num
desugarAST (S.Lit (S.LBool bool)) = unsafePartial $
                                    fromJust $
                                    lookup (show bool) packConstructor

desugarBinders (Tuple n exp) = Tuple n (desugarAST exp)
desugarAlters { cons, rhs } = { caseTag: unsafePartial $
                                         getTag $
                                         desugarConstr cons
                              , vars: unsafePartial $ collectVars cons
                              , rhs: desugarAST rhs
                              , cons: desugarAST cons
                              }
  where
    getTag :: Partial => AST.CoreExpr -> Int
    getTag (AST.Constr _ tag _) = tag

    desugarConstr :: Partial => S.CorePAST -> AST.CoreExpr
    desugarConstr (S.Constr n) = unsafePartial $ fromJust $ lookup n packConstructor
    desugarConstr (S.App cons val) = desugarConstr cons

    collectVars :: Partial => S.CorePAST -> List AST.Name
    collectVars = reverse <<< collectVars'
      where
        collectVars' :: Partial => S.CorePAST -> List AST.Name
        collectVars' (S.App (S.Constr _) (S.Var v)) = v : Nil
        collectVars' (S.App cons (S.Var v)) = v : (collectVars cons)
