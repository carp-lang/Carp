module GenerateConstraints (genConstraints) where

import Data.List (foldl', sort)
import Control.Arrow
import Control.Monad.State
import Data.Maybe (mapMaybe)

import Types
import Obj
import Constraints
import Util
import TypeError

-- | Will create a list of type constraints for a form.
genConstraints :: XObj -> Either TypeError [Constraint]
genConstraints root = fmap sort (gen root)
  where gen xobj =
          case obj xobj of
            Lst lst -> case lst of
                           -- Defn
                           [XObj Defn _ _, _, XObj (Arr args) _ _, body] ->
                             do insideBodyConstraints <- gen body
                                xobjType <- toEither (ty xobj) (DefnMissingType xobj)
                                bodyType <- toEither (ty body) (ExpressionMissingType xobj)
                                let (FuncTy argTys retTy) = xobjType
                                    bodyConstr = Constraint retTy bodyType xobj body OrdDefnBody
                                    argConstrs = zipWith3 (\a b aObj -> Constraint a b aObj xobj OrdArg) (map forceTy args) argTys args
                                return (bodyConstr : argConstrs ++ insideBodyConstraints)

                           -- Def
                           [XObj Def _ _, _, expr] ->
                             do insideExprConstraints <- gen expr
                                xobjType <- toEither (ty xobj) (DefMissingType xobj)
                                exprType <- toEither (ty expr) (ExpressionMissingType xobj)
                                let defConstraint = Constraint xobjType exprType xobj expr OrdDefExpr
                                return (defConstraint : insideExprConstraints)

                           -- Let
                           [XObj Let _ _, XObj (Arr bindings) _ _, body] ->
                             do insideBodyConstraints <- gen body
                                insideBindingsConstraints <- fmap join (mapM gen bindings)
                                bodyType <- toEither (ty body) (ExpressionMissingType body)
                                let Just xobjTy = ty xobj
                                    wholeStatementConstraint = Constraint bodyType xobjTy body xobj OrdLetBody
                                    bindingsConstraints = zipWith (\(symTy, exprTy) (symObj, exprObj) ->
                                                                     Constraint symTy exprTy symObj exprObj OrdLetBind)
                                                                  (map (forceTy *** forceTy) (pairwise bindings))
                                                                  (pairwise bindings)
                                return (wholeStatementConstraint : insideBodyConstraints ++
                                        bindingsConstraints ++ insideBindingsConstraints)

                           -- If
                           [XObj If _ _, expr, ifTrue, ifFalse] ->
                             do insideConditionConstraints <- gen expr
                                insideTrueConstraints <- gen ifTrue
                                insideFalseConstraints <- gen ifFalse
                                exprType <- toEither (ty expr) (ExpressionMissingType expr)
                                trueType <- toEither (ty ifTrue) (ExpressionMissingType ifTrue)
                                falseType <- toEither (ty ifFalse) (ExpressionMissingType ifFalse)
                                let expected = XObj (Sym (SymPath [] "condition in if-value")) (info xobj) (Just BoolTy)
                                    conditionConstraint = Constraint exprType BoolTy expr expected OrdIfCondition
                                    sameReturnConstraint = Constraint trueType falseType ifTrue ifFalse OrdIfReturn
                                    Just t = ty xobj
                                    wholeStatementConstraint = Constraint trueType t ifTrue xobj OrdIfWhole
                                return (conditionConstraint : sameReturnConstraint :
                                        wholeStatementConstraint : insideConditionConstraints ++
                                        insideTrueConstraints ++ insideFalseConstraints)

                           -- While
                           [XObj While _ _, expr, body] ->
                             do insideConditionConstraints <- gen expr
                                insideBodyConstraints <- gen body
                                exprType <- toEither (ty expr) (ExpressionMissingType expr)
                                bodyType <- toEither (ty body) (ExpressionMissingType body)
                                let expectedCond = XObj (Sym (SymPath [] "condition in while-expression")) (info xobj) (Just BoolTy)
                                    expectedBody = XObj (Sym (SymPath [] "body in while-expression")) (info xobj) (Just UnitTy)
                                    conditionConstraint = Constraint exprType BoolTy expr expectedCond OrdWhileCondition
                                    wholeStatementConstraint = Constraint bodyType UnitTy body expectedBody OrdWhileBody
                                return (conditionConstraint : wholeStatementConstraint :
                                        insideConditionConstraints ++ insideBodyConstraints)

                           -- Do
                           XObj Do _ _ : expressions ->
                             case expressions of
                               [] -> Left (NoStatementsInDo xobj)
                               _ -> let lastExpr = last expressions
                                    in do insideExpressionsConstraints <- fmap join (mapM gen expressions)
                                          xobjType <- toEither (ty xobj) (DefMissingType xobj)
                                          lastExprType <- toEither (ty lastExpr) (ExpressionMissingType xobj)
                                          let retConstraint = Constraint xobjType lastExprType xobj lastExpr OrdDoReturn
                                              must = XObj (Sym (SymPath [] "statement in do-expression")) (info xobj) (Just UnitTy)
                                              mkConstr x@(XObj _ _ (Just t)) = Just (Constraint t UnitTy x must OrdDoStatement)
                                              mkConstr _ = Nothing
                                              expressionsShouldReturnUnit = mapMaybe mkConstr (init expressions)
                                          return (retConstraint : insideExpressionsConstraints ++ expressionsShouldReturnUnit)

                           -- Address
                           [XObj Address _ _, value] ->
                             gen value

                           -- Set!
                           [XObj SetBang _ _, variable, value] ->
                             do insideValueConstraints <- gen value
                                variableType <- toEither (ty variable) (ExpressionMissingType variable)
                                valueType <- toEither (ty value) (ExpressionMissingType value)
                                let sameTypeConstraint = Constraint variableType (RefTy valueType) variable value OrdSetBang
                                return (sameTypeConstraint : insideValueConstraints)

                           -- The
                           [XObj The _ _, _, value] ->
                             do insideValueConstraints <- gen value
                                xobjType <- toEither (ty xobj) (DefMissingType xobj)
                                valueType <- toEither (ty value) (DefMissingType value)
                                let theTheConstraint = Constraint xobjType valueType xobj value OrdThe
                                return (theTheConstraint : insideValueConstraints)

                           -- Ref
                           [XObj Ref _ _, value] ->
                             gen value

                           -- Function application
                           func : args ->
                             do insideArgsConstraints <- fmap join (mapM gen args)
                                funcTy <- toEither (ty func) (ExpressionMissingType func)
                                case funcTy of
                                  (FuncTy argTys retTy) ->
                                    if length args /= length argTys then
                                      Left (WrongArgCount func)
                                    else
                                      let expected = XObj (Sym (SymPath [] ("expected argument to '" ++ getName func ++ "'")))
                                                          (info func) (Just funcTy)
                                          argConstraints = zipWith3 (\a t aObj -> Constraint a t aObj expected OrdFuncAppArg)
                                                                    (map forceTy args)
                                                                    argTys
                                                                    args
                                          Just xobjTy = ty xobj
                                          retConstraint = Constraint xobjTy retTy xobj func OrdFuncAppRet
                                      in  return (retConstraint : argConstraints ++ insideArgsConstraints)
                                  funcVarTy@(VarTy _) ->
                                    let fabricatedFunctionType = FuncTy (map forceTy args) (forceTy xobj)
                                        expected = XObj (Sym (SymPath [] ("calling '" ++ getName func ++ "'"))) (info func) Nothing
                                        wholeTypeConstraint = Constraint funcVarTy fabricatedFunctionType func expected OrdFuncAppVarTy
                                    in  return (wholeTypeConstraint : insideArgsConstraints)
                                  _ -> Left (NotAFunction func)

                           -- Empty list
                           [] -> Right []

            (Arr arr) ->
              case arr of
                [] -> Right []
                x:xs -> do insideExprConstraints <- fmap join (mapM gen arr)
                           let Just headTy = ty x
                               Just (StructTy "Array" [t]) = ty xobj
                               betweenExprConstraints = map (\o -> Constraint headTy (forceTy o) x o OrdArrBetween) xs
                               headConstraint = Constraint headTy t x xobj OrdArrHead
                           return (headConstraint : insideExprConstraints ++ betweenExprConstraints)

            _ -> Right []
