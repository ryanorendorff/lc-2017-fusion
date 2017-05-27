import Weigh
import Fusion

{-# INLINE million #-}
million = 1000000

main :: IO ()
main = mainWith $ do
    func "process"             process            [1..million]
    func "process'"            process'           [1..million]
    func "processmanualfused"  processmanualfused [1..million]
    func "processFuse"         processFuse        [1..million]
    func "processVec"          processVec         million
    func "mapTestUnfused"      mapTestUnfused     [1..million]
    func "mapTestFused"        mapTestFused       [1..million]
    func "mapTestStream"       mapTestStream      [1..million]

