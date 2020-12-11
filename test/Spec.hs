import           Prelude                 hiding ( (+)
                                                , (/)
                                                , neg
                                                , (<=)
                                                , not
                                                , (&&)
                                                , seq
                                                , sum
                                                )
import           Control.Lens            hiding ( (#=) )

import qualified Data.Map                                          as Map

import           IMP.Grammar
import           IMP.Semantics           hiding ( program )


-- =============================================================================
-- Test Programs
-- =============================================================================


program_sum :: Program
program_sum = program
  ["n", "sum"]
  [ "n" #= Int 100
  , "sum" #= Int 0
  , while (not (n <= Int 0)) ["sum" #= sum + n, "n" #= n + Int (-1)]
  ]


program_collatz = program
  ["m", "n", "q", "r", "s"]
  [ "m" #= Int 10
  , while
    (not (m <= Int 2))
    [ "n" #= m
    , "m" #= m + Int (-1)
    , while
      (not (n <= Int 1))
      [ "s" #= s + Int 1
      , "q" #= n / Int 2
      , "r" #= q + q + Int 1
      , branch (r <= n) ["n" #= n + n + n + Int 1] ["n" #= q]
      ]
    ]
  ]


program_primes = program
  -- counts in s all the prime numbers up to m
  ["i", "m", "n", "q", "r", "s", "t", "x", "y", "z"]
  [ "m" #= Int 10
  , "n" #= Int 2
  , while
    (n <= m)
    [ -- checking primality of n and writing t to 1 or 0
      "i" #= Int 2
    , "q" #= n / i
    , "t" #= Int 1
    , while
      ((i <= q) && (Int 1 <= t))
      [ "x" #= i
      , "y" #= q
            -- fast multiplication (base 2)
      , "z" #= Int 0
      , while
        (not (x <= Int 0))
        [ "q" #= x / Int 2
        , "r" #= q + q + Int 1
        , branch (r <= x) ["z" #= z + y] []
        , "x" #= q
        , "y" #= y + y
        ]
      , branch (n <= z)
               ["t" #= Int 0]
               ["i" #= i + Int 1, "q" #= n / i]
      ]
    , branch (Int 1 <= t) ["s" #= s + Int 1] []
    , "n" #= n + Int 1
    ]
  ]


-- =============================================================================
-- Testing
-- =============================================================================

type Test = (String, Program, InterpretationState)

check :: Test -> IO ()
check (title, p, st) =
  let succeeds = putStrLn $ "✓ " ++ title
      fails ms = do
        putStrLn $ "✗ " ++ title
        mapM_ (\m -> putStrLn $ "  [!] " ++ m) ms
  in  case interpret p of
        Success st' ->
          let wrong_results =
                  Map.filterWithKey (\x v -> st' Map.! x /= v) st
          in  if Map.null wrong_results
                then succeeds
                else fails
                  ( Map.elems
                  . Map.mapWithKey
                      (\x v ->
                        show x
                          ++ ": expected "
                          ++ show v
                          ++ " but found "
                          ++ show (st' Map.! x)
                      )
                  $ wrong_results
                  )
        Failure m -> fails [m]

tests :: [Test]
tests =
  [ ("sum"    , program_sum    , make_state [("n", 0), ("sum", 5050)])
  , ("collatz", program_collatz, make_state [("m", 2), ("s", 66)])
  , ("primes" , program_primes , make_state [("s", 4)])
  ]
  where make_state = Map.fromList . map (_1 %~ ID)

main = mapM_ check tests
-- main = print . interpret $ program_sum


[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]
  = map (idA . \x -> [x]) ['a' .. 'z']
sum = idA "sum"
