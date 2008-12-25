-- Bench program written by Sebastian Fischer <sebf@informatik.uni-kiel.de>

-- prints the speed of value-supply compared to GHC's UniqSupply when
-- generating a list of fresh identifiers.

import Data.Supply

import Unique
import UniqSupply

import System           ( getArgs )
import System.CPUTime   ( getCPUTime, cpuTimePrecision )
import Control.Monad    ( liftM )
import Numeric          ( showGFloat )

splitU :: UniqSupply -> [UniqSupply]
splitU us = vs : splitU ws where (vs,ws) = splitUniqSupply us

demand :: Integer -> (a -> b) -> [a] -> IO ()
demand 0 _ _      = return ()
demand n f (x:xs) = f x `seq` demand (n-1) f xs

main = do
  count <- liftM (read.head) getArgs

  iavorStart <- getCPUTime
  vs <- newEnumSupply :: IO (Supply Int)
  -- vs <- newDupableEnumSupply :: IO (Supply Int)
  demand count supplyValue (split vs)

  ghcStart <- getCPUTime
  us <- mkSplitUniqSupply 'x'
  demand count uniqFromSupply (splitU us)

  end <- getCPUTime
  let iavorTime = fromIntegral (ghcStart - iavorStart)
      ghcTime   = fromIntegral (end - ghcStart)
      ratio
        | ghcTime == 0  = "?"
        | otherwise     = sh (iavorTime / ghcTime)
      scale = fromIntegral cpuTimePrecision
      sh x  = showGFloat (Just 2) x ""

  putStrLn ("count: " ++ show count ++
          "\tval: " ++ sh (iavorTime / scale) ++
          "\tghc: " ++ sh (ghcTime / scale) ++
          "\tratio: " ++ ratio)

