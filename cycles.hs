-- import Data.List

-- type Cycle = [Int]
-- type Permutation = [Cycle]

-- applyCycle :: Cycle -> Int -> Int
-- applyCycle cycle x
--   | x `elem` cycle = cycle !! ((indexOf x cycle + 1) `mod` length cycle)
--   | otherwise = x
--   where
--     indexOf :: Int -> [Int] -> Int
--     indexOf el (y:ys)
--       | el == y = 0
--       | otherwise = 1 + indexOf el ys

-- applyPerm :: Permutation -> Int -> Int
-- applyPerm perm x = foldr applyCycle x perm

-- joinPerms :: Permutation -> Permutation -> Permutation
-- joinPerms s1 s2 = s1 ++ s2

-- composeCycles :: Cycle -> Cycle -> Cycle
-- composeCycles cycle1 cycle2 = map (applyCycle cycle1) cycle2

-- -- composePerms :: Permutation -> Permutation -> Permutation
-- -- composePerms perm1 perm2 = map (\cycle -> applyCycle perm1Cycle cycle) perm2
-- --   where
-- --     perm1Cycle = concatMap (applyCycle perm1) perm2

-- -- commutePerms :: Int -> Permutation -> Permutation -> Permutation
-- -- commutePerms n perm1 perm2 = composePerms perm2 perm1

import qualified Data.Map as M

type Permutation = M.Map Int Int

-- Create a permutation
makePerm :: [(Int, Int)] -> Permutation
makePerm = M.fromList

-- Inverse of a permutation
inversePerm :: Permutation -> Permutation
inversePerm = M.fromList . map (\(x,y) -> (y,x)) . M.toList

-- Composition of two permutations
composePerm :: Permutation -> Permutation -> Permutation
composePerm p1 p2 = M.map (\x -> p1 M.! x) p2

-- ghc cycles.hs
-- ./cycles

-- runghc cycles.hs


main :: IO ()
main = do
  let sigma = makePerm [(1,2), (2,3), (3,1), (4,5), (5,6)]
      tau = makePerm [(1,2), (2,1), (3,3), (4,5), (5,8), (8,4)]
      sigmaInv = inversePerm sigma
      sigmaTau = composePerm sigma tau

  putStrLn $ "Sigma: " ++ show sigma
  putStrLn $ "Tau: " ++ show tau
  putStrLn $ "Sigma Inverse: " ++ show sigmaInv
  putStrLn $ "Sigma composed with Tau: " ++ show sigmaTau
