{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}

module IMP.Grammar where


import           Prelude                 hiding ( seq )
import           Data.List                                         as List



-- =============================================================================
-- ID
-- =============================================================================

newtype ID = ID String
  deriving (Eq, Ord)



-- =============================================================================
-- Expression
-- =============================================================================

data ExpressionArithmetic
  = Int Int
  | IDArithmetic ID
  | Negative ExpressionArithmetic
  | Division ExpressionArithmetic ExpressionArithmetic
  | Addition ExpressionArithmetic ExpressionArithmetic
  | AssociationArithmetic ExpressionArithmetic

data ExpressionBoolean
  = Bool Bool
  | LessOrEqual ExpressionArithmetic ExpressionArithmetic
  | Negation ExpressionBoolean
  | Conjunction ExpressionBoolean ExpressionBoolean
  | AssociationBoolean ExpressionBoolean

-- abbreviations

idA :: String -> ExpressionArithmetic
idA = IDArithmetic . ID

infixl 6 /
(/)
  :: ExpressionArithmetic
  -> ExpressionArithmetic
  -> ExpressionArithmetic
(/) = Division

infixl 6 +
(+)
  :: ExpressionArithmetic
  -> ExpressionArithmetic
  -> ExpressionArithmetic
(+) = Addition

neg :: ExpressionArithmetic -> ExpressionArithmetic
neg = Negative

infix 5 <=
(<=)
  :: ExpressionArithmetic -> ExpressionArithmetic -> ExpressionBoolean
(<=) = LessOrEqual

not :: ExpressionBoolean -> ExpressionBoolean
not = Negation

infixl 6 &&
(&&) :: ExpressionBoolean -> ExpressionBoolean -> ExpressionBoolean
(&&) = Conjunction



-- =============================================================================
-- Statement
-- =============================================================================

data Statement
  = Block Block
  | Assignment ID ExpressionArithmetic
  | Branch ExpressionBoolean Block Block
  | Loop ExpressionBoolean Block
  | Sequence Statement Statement

data Block
  = BlockEmpty
  | BlockStatement Statement

-- abbreviations

block :: [Statement] -> Block
block = BlockStatement . seq

infixl 4 #=
(#=) :: String -> ExpressionArithmetic -> Statement
x #= a = Assignment (ID x) a

branch :: ExpressionBoolean -> [Statement] -> [Statement] -> Statement
branch b ss1 ss2 = Branch b (block ss1) (block ss2)

while :: ExpressionBoolean -> [Statement] -> Statement
while b ss = Loop b (block ss)

seq :: [Statement] -> Statement
seq []       = Block BlockEmpty
seq (s : ss) = foldl Sequence s ss


-- =============================================================================
-- Program
-- =============================================================================

data Program = Program [ID] Statement


-- abbreviations

program :: [String] -> [Statement] -> Program
program xs ss = Program (ID <$> xs) (seq ss)

-- =============================================================================
-- Show instances
-- =============================================================================

instance Show ID where
  show (ID name) = name

instance Show ExpressionArithmetic where
  show = \case
    Int          i          -> show i
    IDArithmetic x          -> show x
    Negative     a          -> "- " ++ show a
    Division a1 a2          -> show a1 ++ " / " ++ show a2
    Addition a1 a2          -> show a1 ++ " + " ++ show a2
    AssociationArithmetic a -> "(" ++ show a ++ ")"

instance Show ExpressionBoolean where
  show = \case
    Bool b               -> show b
    LessOrEqual a1 a2    -> show a1 ++ " <= " ++ show a2
    Negation b           -> "! " ++ show b
    Conjunction b1 b2    -> show b1 ++ " && " ++ show b2
    AssociationBoolean b -> "(" ++ show b ++ ")"

instance Show Block where
  show = \case
    BlockEmpty       -> "{ }"
    BlockStatement s -> "{ " ++ show s ++ " }"

instance Show Statement where
  show = \case
    Block k        -> show k
    Assignment x a -> show x ++ " = " ++ show a ++ " ;"
    Branch b k1 k2 ->
      "if (" ++ show b ++ ") " ++ show k1 ++ " else " ++ show k2
    Loop     b  k  -> "while (" ++ show b ++ ") " ++ show k
    Sequence s1 s2 -> show s1 ++ " " ++ show s2

instance Show Program where
  show (Program xs s) = "int " ++ show_ids xs ++ " ; " ++ show s
    where show_ids = List.intercalate ", " . map show
