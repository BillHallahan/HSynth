{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HSynth.Preprocessor.FuncAppForm ( FAFState
                                       , FAStacks
                                       , AcceptFAFHalter (..)
                                       , isFuncAppForm
                                       , initFAFState
                                       , fafReducer ) where

import G2.Execution.NormalForms
import G2.Execution.Reducer
import G2.Language
import qualified G2.Language.Stack as S
import G2.Solver

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

-- | Has the `FAFState` been fully reduced to FAF form
isStateFuncAppForm :: FAFState -> Bool
isStateFuncAppForm s@(State { track = stck }) =
    S.null stck && isExecValueForm s

-- | A state set up for reduction to FAF Form
type FAFState = State FAStacks

data AppFrame = AppFrame Expr

-- | Helps build a FAF expression from a SWHNF expression.
-- A stack of FAStacks is kept in the states tracker.
-- When a Expr reaches SWHNF, it is unapped and put on the stack.
-- Then, each individual piece of it is reduced to FAF and put in the
-- built expression.  When the Stack is empty, the built expression
-- is returned
data FAStack = FAStack { stack :: S.Stack AppFrame
                       , built_expr :: Expr }

type FAStacks = S.Stack FAStack

data FAFReducer = FAFReducer

instance Reducer FAFReducer () FAStacks where
    initReducer _ _ = ()
    redRules = redFuncAppForm

-- Note: In theory, fafReducer could be given a more specific type, rather than
-- being wrapped in a SomeReducer. However, wrapping it in a SomeReducer
-- prevents the :<~ from being taken apart by some outside caller.
-- This is desirable: we don't want to give access to a lone FAFReducer.

-- | Returns a (wrapped) Reducer to FAF form
fafReducer :: Solver sol => sol -> SomeReducer (S.Stack FAStack)
fafReducer sol = SomeReducer (StdRed sol :<~? FAFReducer)

initFAFState :: State t -> FAFState
initFAFState s = s { track = S.empty }

-- | When used in concert with a standard reducer, reduces to FAF Form.
-- (See also, @`FAStack`@)
redFuncAppForm :: FAFReducer -> () -> FAFState -> IO (ReducerRes, [(FAFState, ())], FAFReducer)
redFuncAppForm fr _ s =
    let
        (rres, ss) = redFuncAppForm' s
    in
    return (rres, zip ss (repeat ()), fr)

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

-- | Accepts a state when it is in FAF form
data AcceptFAFHalter = AcceptFAFHalter

instance Halter AcceptFAFHalter () (S.Stack FAStack) where
    initHalt _ _ = ()
    updatePerStateHalt _ _ _ _ = ()
    stopRed _ _ _ s =
        case isStateFuncAppForm s of
            True -> Accept
            False -> Continue
    stepHalter _ _ _ _ = ()
