{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module IMP.Semantics where


import           System.IO.Unsafe               ( unsafePerformIO )
import           Control.Lens
import           Data.Map                                          as Map
import           Data.Maybe                                        as Maybe
import           Control.Monad.State                               as State
                                         hiding ( state
                                                , State
                                                )
import           IMP.Grammar             hiding ( (+)
                                                , neg
                                                , (<=)
                                                , not
                                                , (&&)
                                                , program
                                                )


-- =============================================================================
-- Result
-- =============================================================================
-- A `Result a` value is either:
-- + a successful result of type `a`
-- + a failed result with a message (`String`)


data Result a
  = Success a       -- succesful result, yielding a value of type `a`
  | Failure String  -- failed result, with a message
  deriving (Show, Eq)

instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap f (Failure m) = Failure m

instance Applicative Result where
  pure = Success
  Success f <*> Success a = Success (f a)
  Failure m <*> _         = Failure m
  _         <*> Failure m = Failure m

instance Monad Result where
  Success a >>= k = k a
  Failure m >>= _ = Failure m

instance MonadFail Result where
  fail = Failure



-- =============================================================================
-- Configuration
-- =============================================================================


-- A `Configuration` stores all the information needed to start interpretting a
-- program.
data Configuration = Configuration
  { program :: Program                -- the program to be interpretted
  , state   :: InterpretationState }  -- the initial interpretation state
  deriving (Show)

-- A `Map` from each variable's `ID` to its current `Int` value.
type InterpretationState = Map ID Int

-- Smart configuration constructor, which sets the initial interpretation state
-- to the empty state.
initConfiguration :: Program -> Configuration
initConfiguration p =
  Configuration { program = p, state = Map.empty }



-- =============================================================================
-- Interpretation
-- =============================================================================


-- Interprets a `Program`, yielding the `Result`ing state.
interpret :: Program -> Result (InterpretationState)
interpret = interpret' . initConfiguration

-- Variant of `interpret` that allows for custom configuration
-- rather than the default assigned by `initConfiguration`.
interpret' :: Configuration -> Result (InterpretationState)
interpret' (Configuration { program, state }) =
  snd <$> runStateT (interpretProgram program) state


-- An `Interpretation` is a computation that
-- + may read/modify the interpretation state,
-- + may fail (via the `Result` wrapped `StateT`).
type Interpretation a = StateT (InterpretationState) Result a


-- Interprets a `Program` as an `Interpretation`.
interpretProgram :: Program -> Interpretation ()
interpretProgram (Program xs s) = do
  mapM_ initVariable xs
  interpretStatement s


interpretBlock :: Block -> Interpretation ()
interpretBlock = \case
  BlockEmpty       -> return ()
  BlockStatement s -> interpretStatement s


interpretStatement :: Statement -> Interpretation ()
interpretStatement = \case

  Block k        -> interpretBlock k

  -- x #= a
  Assignment x a -> setVariable x =<< evaluateExpressionArithmetic a

  Branch b k1 k2 -> evaluateExpressionBoolean b >>= \case
    True  -> interpretBlock k1
    False -> interpretBlock k2

  -- while b k
  Loop b k -> evaluateExpressionBoolean b >>= \case
    True  -> interpretBlock k >> interpretStatement (Loop b k)
    False -> return ()

  Sequence s1 s2 -> interpretStatement s1 >> interpretStatement s2


evaluateExpressionBoolean :: ExpressionBoolean -> Interpretation Bool
evaluateExpressionBoolean = \case

  Bool b -> return b

  -- a1 <= a2
  LessOrEqual a1 a2 ->
    (<=)
      <$> evaluateExpressionArithmetic a1
      <*> evaluateExpressionArithmetic a2

  -- not b
  Negation b -> not <$> evaluateExpressionBoolean b

  -- b1 && b2
  Conjunction b1 b2 ->
    (&&)
      <$> evaluateExpressionBoolean b1
      <*> evaluateExpressionBoolean b2

  -- (b)
  AssociationBoolean b -> evaluateExpressionBoolean b


evaluateExpressionArithmetic
  :: ExpressionArithmetic -> Interpretation Int
evaluateExpressionArithmetic = \case

  Int          i -> return i

  IDArithmetic x -> getVariable x

  -- -a
  Negative     a -> negate <$> evaluateExpressionArithmetic a

  -- a1 / a2
  Division a1 a2 ->
    div
      <$> evaluateExpressionArithmetic a1
      <*> evaluateExpressionArithmetic a2

  -- a1 + a2
  Addition a1 a2 ->
    (+)
      <$> evaluateExpressionArithmetic a1
      <*> evaluateExpressionArithmetic a2

  -- (a)
  AssociationArithmetic a -> evaluateExpressionArithmetic a



-- -----------------------------------------------------------------------------
-- Variables
-- -----------------------------------------------------------------------------
-- `initVariable` is the only way to declare a new variable, since `getVariable`
-- and `setVariable` require the variable to already be declared (via
-- `requireVariable`).


-- Declare a new variable to be used later in the program,
-- initialized to the value `0`.
initVariable :: ID -> Interpretation ()
initVariable x = at x .= Just 0 -- initial value for each variable


-- Guard interpretation by checking whether the given ID has been declared as a
-- variable.
requireVariable :: ID -> Interpretation ()
requireVariable x = use (at x) >>= \case
  Just _  -> return ()
  Nothing -> fail $ "undeclared ID: " ++ show x


getVariable :: ID -> Interpretation Int
getVariable x = do
  requireVariable x
  fromJust <$> use (at x)


setVariable :: ID -> Int -> Interpretation ()
setVariable x v = do
  requireVariable x
  ix x .= v
