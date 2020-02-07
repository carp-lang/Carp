module GenerateConstraints (genConstraints) where

import Data.List (foldl', sort, zipWith4)
import Control.Arrow
import Control.Monad.State
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

import Types
import Obj
import Constraints
import Util
import TypeError
import Lookup

-- | Will create a list of type constraints for a form.
genConstraints :: TypeEnv -> XObj -> Either TypeError [Constraint]
genConstraints typeEnv root = fmap sort (gen root)
  where genF xobj args body =
         do insideBodyConstraints <- gen body
            xobjType <- toEither (ty xobj) (DefnMissingType xobj)
            bodyType <- toEither (ty body) (ExpressionMissingType xobj)
            let (FuncTy argTys retTy) = xobjType
                bodyConstr = Constraint retTy bodyType xobj body xobj OrdDefnBody
                argConstrs = zipWith3 (\a b aObj -> Constraint a b aObj xobj xobj OrdArg) (map forceTy args) argTys args
            return (bodyConstr : argConstrs ++ insideBodyConstraints)
        gen xobj =
          case obj xobj of
            Lst lst -> case lst of
                           -- Defn
                           [XObj Defn _ _, _, XObj (Arr args) _ _, body] ->
                             genF xobj args body

                           -- Fn
                           [XObj (Fn _ _ _) _ _, XObj (Arr args) _ _, body] ->
                             genF xobj args body

                           -- Def
                           [XObj Def _ _, _, expr] ->
                             do insideExprConstraints <- gen expr
                                xobjType <- toEither (ty xobj) (DefMissingType xobj)
                                exprType <- toEither (ty expr) (ExpressionMissingType xobj)
                                let defConstraint = Constraint xobjType exprType xobj expr xobj OrdDefExpr
                                return (defConstraint : insideExprConstraints)

                           -- Let
                           [XObj Let _ _, XObj (Arr bindings) _ _, body] ->
                             do insideBodyConstraints <- gen body
                                insideBindingsConstraints <- fmap join (mapM gen bindings)
                                bodyType <- toEither (ty body) (ExpressionMissingType body)
                                let Just xobjTy = ty xobj
                                    wholeStatementConstraint = Constraint bodyType xobjTy body xobj xobj OrdLetBody
                                    bindingsConstraints = zipWith (\(symTy, exprTy) (symObj, exprObj) ->
                                                                     Constraint symTy exprTy symObj exprObj xobj OrdLetBind)
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
                                let expected = XObj (Sym (SymPath [] "Condition in if value") Symbol) (info expr) (Just BoolTy)
                                let lol = XObj (Sym (SymPath [] "lol") Symbol) (info expr) (Just BoolTy)
                                    conditionConstraint = Constraint exprType BoolTy expr expected xobj OrdIfCondition
                                    sameReturnConstraint = Constraint trueType falseType ifTrue ifFalse xobj OrdIfReturn
                                    Just t = ty xobj
                                    wholeStatementConstraint = Constraint trueType t ifTrue xobj xobj OrdIfWhole
                                return (conditionConstraint : sameReturnConstraint :
                                        wholeStatementConstraint : insideConditionConstraints ++
                                        insideTrueConstraints ++ insideFalseConstraints)

                           -- Match
                           XObj Match _ _ : expr : cases ->
                             do insideExprConstraints <- gen expr
                                casesLhsConstraints <- fmap join (mapM (gen . fst) (pairwise cases))
                                casesRhsConstraints <- fmap join (mapM (gen .snd) (pairwise cases))
                                exprType <- toEither (ty expr) (ExpressionMissingType expr)
                                xobjType <- toEither (ty xobj) (DefMissingType xobj)

                                let
                                  -- Each case rhs should have the same return type as the whole match form:
                                  mkRetConstr x@(XObj _ _ (Just t)) = Just (Constraint t xobjType x xobj xobj OrdArg) -- | TODO: Ord
                                  mkRetConstr _ = Nothing
                                  returnConstraints = mapMaybe (\(_, rhs) -> mkRetConstr rhs) (pairwise cases)

                                  -- Each case lhs should have the same type as the expression matching on
                                  mkExprConstr x@(XObj _ _ (Just t)) = Just (Constraint t exprType x expr xobj OrdArg) -- | TODO: Ord
                                  mkExprConstr _ = Nothing
                                  exprConstraints = mapMaybe (\(lhs, _) -> mkExprConstr lhs) (pairwise cases)

                                  -- Constraints for the variables in the left side of each matching case,
                                  -- like the 'r'/'g'/'b' in (match col (RGB r g b) ...) being constrained to Int.
                                  -- casesLhsConstraints = concatMap (genLhsConstraintsInCase typeEnv exprType) (map fst (pairwise cases))

                                  -- exprConstraint =
                                  --   -- | TODO: Only guess if there isn't already a type set on the expression!
                                  --   case guessExprType typeEnv cases of
                                  --     Just guessedExprTy ->
                                  --       let expected = XObj (Sym (SymPath [] "Expression in match-statement") Symbol)
                                  --                      (info expr) (Just guessedExprTy)
                                  --       in  [Constraint exprType guessedExprTy expr expected OrdIfCondition] -- | TODO: Ord
                                  --     Nothing ->
                                  --       []

                                return (insideExprConstraints ++
                                        casesLhsConstraints ++
                                        casesRhsConstraints ++
                                        returnConstraints ++
                                        exprConstraints)

                           -- While
                           [XObj While _ _, expr, body] ->
                             do insideConditionConstraints <- gen expr
                                insideBodyConstraints <- gen body
                                exprType <- toEither (ty expr) (ExpressionMissingType expr)
                                bodyType <- toEither (ty body) (ExpressionMissingType body)
                                let expectedCond = XObj (Sym (SymPath [] "Condition in while-expression") Symbol) (info expr) (Just BoolTy)
                                    expectedBody = XObj (Sym (SymPath [] "Body in while-expression") Symbol) (info xobj) (Just UnitTy)
                                    conditionConstraint = Constraint exprType BoolTy expr expectedCond xobj OrdWhileCondition
                                    wholeStatementConstraint = Constraint bodyType UnitTy body expectedBody xobj OrdWhileBody
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
                                          let retConstraint = Constraint xobjType lastExprType xobj lastExpr xobj OrdDoReturn
                                              must = XObj (Sym (SymPath [] "Statement in do-expression") Symbol) (info xobj) (Just UnitTy)
                                              mkConstr x@(XObj _ _ (Just t)) = Just (Constraint t UnitTy x must xobj OrdDoStatement)
                                              mkConstr _ = Nothing
                                              expressionsShouldReturnUnit = mapMaybe mkConstr (init expressions)
                                          return (retConstraint : insideExpressionsConstraints ++ expressionsShouldReturnUnit)

                           -- Address
                           [XObj Address _ _, value] ->
                             gen value

                           -- Set!
                           [XObj SetBang _ _, variable, value] ->
                             do insideValueConstraints <- gen value
                                insideVariableConstraints <- gen variable
                                variableType <- toEither (ty variable) (ExpressionMissingType variable)
                                valueType <- toEither (ty value) (ExpressionMissingType value)
                                let sameTypeConstraint = Constraint variableType valueType variable value xobj OrdSetBang
                                return (sameTypeConstraint : insideValueConstraints ++ insideVariableConstraints)

                           -- The
                           [XObj The _ _, _, value] ->
                             do insideValueConstraints <- gen value
                                xobjType <- toEither (ty xobj) (DefMissingType xobj)
                                valueType <- toEither (ty value) (DefMissingType value)
                                let theTheConstraint = Constraint xobjType valueType xobj value xobj OrdThe
                                return (theTheConstraint : insideValueConstraints)

                           -- Ref
                           [XObj Ref _ _, value] ->
                             gen value

                           -- Deref
                           [XObj Deref _ _, value] ->
                             do insideValueConstraints <- gen value
                                xobjType <- toEither (ty xobj) (ExpressionMissingType xobj)
                                valueType <- toEither (ty value) (ExpressionMissingType value)
                                let theTheConstraint = Constraint (RefTy xobjType) valueType xobj value xobj OrdDeref
                                return (theTheConstraint : insideValueConstraints)

                           -- Break
                           [XObj Break _ _] ->
                             return []

                           -- Function application
                           func : args ->
                             do funcConstraints <- gen func
                                insideArgsConstraints <- fmap join (mapM gen args)
                                funcTy <- toEither (ty func) (ExpressionMissingType func)
                                case funcTy of
                                  (FuncTy argTys retTy) ->
                                    if length args /= length argTys then
                                      Left (WrongArgCount func (length argTys) (length args))
                                    else
                                      let expected t n =
                                            XObj (Sym (SymPath [] ("Expected " ++ enumerate n ++ " argument to '" ++ getName func ++ "'")) Symbol)
                                            (info func) (Just t)
                                          argConstraints = zipWith4 (\a t aObj n -> Constraint a t aObj (expected t n) xobj OrdFuncAppArg)
                                                                    (map forceTy args)
                                                                    argTys
                                                                    args
                                                                    [0..]
                                          Just xobjTy = ty xobj
                                          retConstraint = Constraint xobjTy retTy xobj func xobj OrdFuncAppRet
                                      in  return (retConstraint : funcConstraints ++ argConstraints ++ insideArgsConstraints)
                                  funcVarTy@(VarTy _) ->
                                    let fabricatedFunctionType = FuncTy (map forceTy args) (forceTy xobj)
                                        expected = XObj (Sym (SymPath [] ("Calling '" ++ getName func ++ "'")) Symbol) (info func) Nothing
                                        wholeTypeConstraint = Constraint funcVarTy fabricatedFunctionType func expected xobj OrdFuncAppVarTy
                                    in  return (wholeTypeConstraint : funcConstraints ++ insideArgsConstraints)
                                  _ -> Left (NotAFunction func)

                           -- Empty list
                           [] -> Right []

            (Arr arr) ->
              case arr of
                [] -> Right []
                x:xs -> do insideExprConstraints <- fmap join (mapM gen arr)
                           let Just headTy = ty x
                               genObj o n = XObj (Sym (SymPath [] ("Whereas the " ++ enumerate n ++ " element in the array is " ++ show (getPath o))) Symbol)
                                  (info o) (ty o)
                               headObj = XObj (Sym (SymPath [] ("I inferred the type of the array from its first element " ++ show (getPath x))) Symbol)
                                  (info x) (Just headTy)
                               Just (StructTy "Array" [t]) = ty xobj
                               betweenExprConstraints = zipWith (\o n -> Constraint headTy (forceTy o) headObj (genObj o n) xobj OrdArrBetween) xs [1..]
                               headConstraint = Constraint headTy t headObj (genObj x 1) xobj OrdArrHead
                           return (headConstraint : insideExprConstraints ++ betweenExprConstraints)

            _ -> Right []
