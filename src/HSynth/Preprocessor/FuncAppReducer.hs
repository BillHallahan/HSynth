{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HSynth.Preprocessor.FuncAppReducer ( FAFState
                                          , FAFReducer (..)
                                          , initFAFState
                                          , isFuncAppForm ) where

import G2.Execution.NormalForms
import G2.Execution.Reducer
import G2.Language
import qualified G2.Language.Stack as S

-- | An expression is in Function Application Form (FAF) if it is either
--   1. In SWHNF
--   2. A function application, where the arguments are in FAF
isFuncAppForm :: ExprEnv -> Expr -> Bool
isFuncAppForm eenv e
    | Var _ <- e = True
    | isExprValueForm eenv e = True
    | App _ _ <- e
    , es <- unApp e
    , all (isFuncAppForm eenv) es = True
    | otherwise = False

-- | A state set up for reduction to FAF Form
type FAFState = State (S.Stack FAStack)

data AppFrame = AppFrame Expr

-- | Helps build a FAF expression from a SWHNF expression.
-- A stack of FAStacks is kept in the states tracker.
-- When a Expr reaches SWHNF, it is unapped and put on the stack.
-- Then, each individual piece of it is reduced to FAF and put in the
-- built expression.  When the Stack is empty, the built expression
-- is returned
data FAStack = FAStack { stack :: S.Stack AppFrame
                       , built_expr :: Expr }

data FAFReducer = FAFReducer

instance Reducer FAFReducer (S.Stack FAStack) where
    redRules = redFuncAppForm

initFAFState :: State t -> FAFState
initFAFState s = s { track = S.empty }

-- | Reduces to FAF Form.
-- (See also, @`FAStack`@)
redFuncAppForm :: FAFReducer -> FAFState -> IO (ReducerRes, [FAFState], FAFReducer)
redFuncAppForm fr s =
    let
        (rres, ss) = redFuncAppForm' s
    in
    return (rres, ss, fr)

redFuncAppForm' :: FAFState -> (ReducerRes, [FAFState])
redFuncAppForm' s@(State { expr_env = eenv
                        , curr_expr = (CurrExpr _ ce)
                        , track = t })
    | Just (FAStack { stack = stck, built_expr = be}, t') <- S.pop t =
        let be' = App be ce in
        case S.pop stck of
            Just (AppFrame se, stck') -> 
                let
                    fa = FAStack { stack = stck', built_expr = be' }
                in
                ( InProgress
                , [s { curr_expr = CurrExpr Return se
                     , track = S.push fa t' }
                  ]
                )
            Nothing -> 
                ( InProgress
                , [s { curr_expr = CurrExpr Return be'
                     , track = t'}])

    | isFuncAppForm eenv ce = (InProgress, [s { curr_expr = CurrExpr Return ce }])

    | otherwise = (NoProgress, [s])