{-# LANGUAGE NamedFieldPuns #-}

module TemplateGenerator where

import Obj
import Types
import qualified TypeCandidate as TC

--------------------------------------------------------------------------------
-- Template Generators
--
-- Template generators define a standardized way to construct templates given a fixed set of arguments.

-- | GeneratorArg is an argument to a template generator.
data GeneratorArg a = GeneratorArg {
  tenv      :: TypeEnv,
  env       :: Env,
  originalT :: Ty,
  instanceT :: Ty,
  value     :: a
}

type TypeGenerator a  = GeneratorArg a -> Ty
type TokenGenerator a = GeneratorArg a -> [Token]
type DepenGenerator a = GeneratorArg a -> [XObj]

data TemplateGenerator a = TemplateGenerator {
  genT :: TypeGenerator a,
  decl :: TokenGenerator a,
  body :: TokenGenerator a,
  deps :: DepenGenerator a
}

mkTemplateGenerator :: TypeGenerator a -> TokenGenerator a -> TokenGenerator a -> DepenGenerator a -> TemplateGenerator a
mkTemplateGenerator f g h j = TemplateGenerator f g h j

generateConcreteTypeTemplate :: TC.TypeCandidate -> TemplateGenerator TC.TypeCandidate -> Template
generateConcreteTypeTemplate candidate gen =
  let arg = GeneratorArg
              (TC.getTypeEnv candidate)
              (TC.getValueEnv candidate)
              (TC.toType candidate)
              (TC.toType candidate)
              candidate
      t = (genT gen) $ arg
      d = (\tt -> (decl gen) $ (arg {instanceT = tt}))
      b = (\tt -> (body gen) $ (arg {instanceT = tt}))
      p = (\tt -> (deps gen) $ (arg {instanceT = tt}))
   in Template t d b p

generateConcreteFieldTemplate :: TC.TypeCandidate -> TC.TypeField -> TemplateGenerator TC.TypeField -> Template
generateConcreteFieldTemplate candidate field gen =
  let arg = GeneratorArg
              (TC.getTypeEnv candidate)
              (TC.getValueEnv candidate)
              (TC.toType candidate)
              (TC.toType candidate)
              field
      t = (genT gen) $ arg
      d = (\tt -> (decl gen) $ (arg {instanceT = tt}))
      b = (\tt -> (body gen) $ (arg {instanceT = tt}))
      p = (\tt -> (deps gen) $ (arg {instanceT = tt}))
   in Template t d b p

generateGenericFieldTemplate :: TC.TypeCandidate -> TC.TypeField -> TemplateGenerator TC.TypeField -> TemplateCreator
generateGenericFieldTemplate candidate field gen =
  let arg = GeneratorArg
              (TC.getTypeEnv candidate)
              (TC.getValueEnv candidate)
              (TC.toType candidate)
              (TC.toType candidate)
              field
      t = (genT gen) arg
   in TemplateCreator $
        \tenv env ->
          Template
            t
            (\tt -> (decl gen) $ (arg {instanceT = tt, tenv = tenv, env = env}))
            (\tt -> (body gen) $ (arg {instanceT = tt, tenv = tenv, env = env}))
            (\tt -> (deps gen) $ (arg {instanceT = tt, tenv = tenv, env = env}))

generateGenericTypeTemplate :: TC.TypeCandidate -> TemplateGenerator TC.TypeCandidate -> TemplateCreator
generateGenericTypeTemplate candidate gen =
  let arg = GeneratorArg
              (TC.getTypeEnv candidate)
              (TC.getValueEnv candidate)
              (TC.toType candidate)
              (TC.toType candidate)
              candidate
      t = (genT gen) arg
   in TemplateCreator $
        \tenv env ->
          Template
            t
            (\tt -> (decl gen) $ (arg {instanceT = tt, tenv = tenv, env = env}))
            (\tt -> (body gen) $ (arg {instanceT = tt, tenv = tenv, env = env}))
            (\tt -> (deps gen) $ (arg {instanceT = tt, tenv = tenv, env = env}))
