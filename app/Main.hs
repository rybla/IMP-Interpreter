module Main where

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



main = do
  putStrLn "[IMP-Interpreter]"
  putStrLn "[program]"
  print prog
  putStrLn "[output]"
  print . interpret $ prog


-- your program here
prog :: Program
prog =
  let
    names    = ["n", "sum"]
    [n, sum] = idA <$> names
  in
    program names
      $ [ "n" #= Int 100
        , "sum" #= Int 0
        , while (not (n <= Int 0))
                ["sum" #= sum + n, "n" #= n + Int (-1)]
        ]
