import qualified Data.Map as M
import Data.List (delete)
import Data.Maybe (fromMaybe)
import System.CPUTime
import Text.Printf (printf)

type Permutation = M.Map Int Int
type Cycle = [Int]

-- Create a permutation
makePerm :: [(Int, Int)] -> Either String Permutation
makePerm pairs
  | isValidPerm pairs = Right $ M.fromList pairs
  | otherwise = Left "Invalid Permutation: Not a bijection or invalid elements"
  where
    isValidPerm ps = let keys = map fst ps
                         values = map snd ps
                     in (length keys == length (M.fromList ps)) && all (`elem` values) keys

-- Inverse of a permutation
inversePerm :: Permutation -> Permutation
inversePerm = M.fromList . map (\(x, y) -> (y, x)) . M.toList

-- Composition of two permutations
composePerm :: Permutation -> Permutation -> Permutation
composePerm p1 p2 = M.map (\x -> M.findWithDefault x x p1) p2

-- Convert a permutation to cycle decomposition, excluding 1-cycles
toCycles :: Permutation -> [Cycle]
toCycles perm = filter (\cycle -> length cycle > 1) (toCycles' (M.keys perm) [])
  where
    toCycles' [] acc = acc
    toCycles' (x:xs) acc
      | any (x `elem`) acc = toCycles' xs acc -- already part of a cycle
      | otherwise          = let cycle = findCycle x perm
                             in toCycles' (deleteCycle cycle xs) (cycle : acc)
    
    findCycle start p = findCycle' start start []
      where
        findCycle' s cur acc
          | cur == start && not (null acc) = reverse acc -- cycle is complete
          | cur `elem` acc = reverse acc  -- Avoid loops by returning what is found so far
          | otherwise = let next = M.findWithDefault cur cur p  -- Map to self for identity
                        in findCycle' s next (cur : acc)

    deleteCycle cycle xs = foldl (flip delete) xs cycle

-- Convert a cycle to a permutation
cycleToPerm :: Cycle -> Permutation
cycleToPerm [] = M.empty
cycleToPerm [x] = M.singleton x x
cycleToPerm xs = M.fromList (zip xs (tail xs ++ [head xs]))

-- Compose two cycles into a permutation by converting each to a permutation
composeCycles :: Cycle -> Cycle -> [Cycle]
composeCycles c1 c2 = toCycles (composePerm (cycleToPerm c1) (cycleToPerm c2))

-- Compose multiple cycles by tracking each element's movement through the cycles
composeMultipleCycles :: [Cycle] -> [Cycle]
composeMultipleCycles cycles = toCycles (composeMultiplePerm cycles)

-- Helper function to compose cycles into a single permutation
composeMultiplePerm :: [Cycle] -> Permutation
composeMultiplePerm cycles = foldl composeCyclePerm M.empty cycles
  where
    composeCyclePerm acc cycle =
      let cyclePerm = cycleToPerm cycle
      in foldl (\p x -> M.insert x (applyCycle x acc cyclePerm) p) acc cycle

-- Apply a cycle to a single element
applyCycle :: Int -> Permutation -> Permutation -> Int
applyCycle x acc cyclePerm =
  let mappedInCycle = M.findWithDefault x x cyclePerm
  in M.findWithDefault mappedInCycle mappedInCycle acc

-- Test if a permutation is the identity
isIdentity :: Permutation -> Bool
isIdentity perm = all (\(k, v) -> k == v) (M.toList perm)

-- Function to run the cycle composition and return time taken
runTest :: IO Double
runTest = do
  start <- getCPUTime

  let cycle1 = [1, 2, 3, 4]
      cycle2 = [3, 4, 5]
      cycle3 = [5, 6, 7, 1]
      cycle4 = [5, 6, 7, 1, 2, 3, 4]
      cycle5 = [1, 4, 5, 3, 10]
      cycle6 = [1]

      composedCycles = composeMultipleCycles [cycle1, cycle2, cycle3, cycle4, cycle5, cycle6]

  putStrLn $ "Composing the cycles results in: " ++ show composedCycles

  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12) -- Convert picoseconds -> seconds
  return diff

-- Main function to compute average time over multiple runs
main :: IO ()
main = do
  let iterations = 1000 :: Int -- Had to include the type explicitly
  times <- mapM (\_ -> runTest) [1..iterations]
  let totalTime = sum times
      avgTime = totalTime / fromIntegral iterations
  printf "Average computation time over %d runs: %0.8f seconds\n" iterations (avgTime :: Double)
