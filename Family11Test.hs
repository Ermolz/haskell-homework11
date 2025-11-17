{-# OPTIONS_GHC -Wall #-}
module Main where

import Yermolovych11

type Test = (String, Bool)

tests :: [Test]
tests =
  [ ("h23 Empty23 = -1"
    , h23 (Empty23 :: Tree23 Int) == -1
    )
  , ("h23 (Node2 (Leaf 0) 1 (Leaf 2)) = 1"
    , h23 (Node2 (Leaf (0 :: Int)) 1 (Leaf 2)) == 1
    )
  , ("h23 tr5 = 3"
    , h23 tr5 == 3
    )

  , ("minmax23 (Node2 (Leaf 1) 0 (Leaf 2)) = Just (0,2)"
    , minmax23 (Node2 (Leaf (1 :: Int)) 0 (Leaf 2)) == Just (0,2)
    )
  , ("minmax23 tr5 = Just (2,19)"
    , minmax23 tr5 == Just (2,19)
    )

  , ("isTree23 (Node2 (Leaf 0) 1 (Leaf 2)) = False"
    , not (isTree23 (Node2 (Leaf (0 :: Int)) 1 (Leaf 2)))
    )
  , ("isTree23 tr3 = True"
    , isTree23 tr3
    )

  , ("eqTree23 tr1 tr2 = True"
    , eqTree23 tr1 tr2
    )

  , ("elemTree23 tr3 12 = True"
    , elemTree23 tr3 12
    )
  , ("elemTree23 tr3 13 = False"
    , not (elemTree23 tr3 13)
    )

  , ("insNode 18 tr3 = (tr4, Nothing)"
    , insNode 18 tr3 == (tr4, Nothing)
    )
  , ("insTree23 tr4 10 = tr5"
    , insTree23 tr4 10 == tr5
    )

  , ("sortList \"bca\" = \"abc\""
    , sortList "bca" == "abc"
    )
  , ("sortList \"Capablanca\" = \"Caaaabclnp\""
    , sortList "Capablanca" == "Caaaabclnp"
    )
  ]

runTests :: IO ()
runTests = mapM_ printResult tests
  where
    printResult (name, ok) =
      putStrLn (name ++ " -> " ++ if ok then "OK" else "FAIL")

main :: IO ()
main = runTests
