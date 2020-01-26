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


desugarParsedSCs :: S.Program -> List AST.CoreSC
desugarParsedSCs pscs = desugarParsedSC <$> pscs
  where
        desugarParsedSC (S.Func fname arg body)
                          = AST.Func fname arg (desugarParsedExp body)

desugarParsedExp :: S.CorePAST -> AST.CoreExpr
desugarParsedExp (S.Var n) = AST.Var n
desugarParsedExp (S.App f e) = AST.App (desugarParsedExp f) (desugarParsedExp e)
desugarParsedExp (S.Constr name) = unsafePartial $ fromJust $ lookup name packConstructor
desugarParsedExp (S.Lam arg body) = AST.Lam arg (desugarParsedExp body)
desugarParsedExp (S.Let rec bind body)
  = AST.Let rec (desugarBinders <$> bind) (desugarParsedExp body)
desugarParsedExp (S.Case sucritinise alters)
  = AST.Case (desugarParsedExp sucritinise) (desugarAlters <$> alters)
desugarParsedExp (S.If cond consequent alternative)
  = AST.Case (desugarParsedExp cond) $
      fromFoldable
        [ { caseTag: 2
          , vars: Nil
          , rhs: (desugarParsedExp consequent)
          , cons: AST.Constr "true" 2 0
          }
        , { caseTag: 3
          , vars: Nil
          , rhs: (desugarParsedExp alternative)
          , cons: AST.Constr "false" 3 0
          }
        ]
desugarParsedExp (S.Lit (S.LInt num)) = AST.Num num
desugarParsedExp (S.Lit (S.LBool bool)) = unsafePartial $ fromJust $ lookup (show bool) packConstructor

desugarBinders (Tuple n exp) = Tuple n (desugarParsedExp exp)
desugarAlters { cons, rhs } = { caseTag: unsafePartial $ getTag $
                                         unsafePartial $ desugarParsedConstr
                                         cons
                              , vars: unsafePartial $ collectVars cons
                              , rhs: desugarParsedExp rhs
                              , cons: desugarParsedExp cons
                              }

getTag :: Partial => AST.CoreExpr -> Int
getTag (AST.Constr _ tag _) = tag

collectVars :: Partial => S.CorePAST -> List AST.Name
collectVars = reverse <<< collectVars'
  where
    collectVars' :: Partial => S.CorePAST -> List AST.Name
    collectVars' (S.App (S.Constr _) (S.Var v)) = v : Nil
    collectVars' (S.App cons (S.Var v)) = v : (collectVars cons)

desugarParsedConstr :: Partial => S.CorePAST -> AST.CoreExpr
desugarParsedConstr (S.Constr n) = unsafePartial $ fromJust $ lookup n packConstructor
desugarParsedConstr (S.App cons val) = desugarParsedConstr cons
