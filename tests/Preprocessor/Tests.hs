{-# LANGUAGE OverloadedStrings #-}

module Preprocessor.Tests (preprocTests) where

import Test.Tasty
import Test.Tasty.HUnit

import HSynth.Preprocessor.FuncAppReducer

import G2.Language
import qualified G2.Language.ExprEnv as E

preprocTests :: TestTree
preprocTests =
    testGroup "Preprocessor"
        [ testFAF fafExpr1
        , testFAF fafExpr2
        , testFAF fafExpr3
        , testFAF fafExpr4
        , notTestFAF notFAFExpr1
        ]

testFAF :: Expr -> TestTree
testFAF e = testCase "FAF test"
            $ assertBool ("Failed on " ++ show e) (isFuncAppForm eenv1 e) 

notTestFAF :: Expr -> TestTree
notTestFAF e = testCase "Not FAF test"
            $ assertBool ("Failed on " ++ show e) (not $ isFuncAppForm eenv1 e) 


fafExpr1 :: Expr
fafExpr1 = Var (Id f tyVarA)

fafExpr2 :: Expr
fafExpr2 = App fV hV

fafExpr3 :: Expr
fafExpr3 = Lit (LitInt 1)

fafExpr4 :: Expr
fafExpr4 = App fV (App hV (Lit (LitInt 1)))

notFAFExpr1 :: Expr
notFAFExpr1 = App fV (Case hV (Id g tyVarA) [])

eenv1 :: E.ExprEnv
eenv1 = E.fromList
    [ (f, fV)
    , (g, gV)
    , (h, hV) ]

fV :: Expr
fV = Var (Id f tyVarA)

gV :: Expr
gV = Var (Id g tyVarA)

hV :: Expr
hV = Var (Id h tyVarA)

f :: Name
f = Name "f" Nothing 0 Nothing

g :: Name
g = Name "g" Nothing 0 Nothing

h :: Name
h = Name "h" Nothing 0 Nothing

tyVarA :: Type
tyVarA = TyVar (Id (Name "a" Nothing 0 Nothing) TYPE)