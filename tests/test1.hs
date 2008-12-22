import Data.Supply

main = do test "num supply" =<< newNumSupply
          test "enum supply" =<< newEnumSupply
          test "list supply" =<< newSupply 0 (1+)

test msg x = do putStrLn msg
                let (a,b) = split2 x
                print (map supplyValue [a,x,b,a] == [0 :: Int, 1, 2,0])
