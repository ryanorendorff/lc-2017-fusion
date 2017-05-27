import Criterion.Main
import Fusion

{-# INLINE million #-}
million = 1000000

main :: IO ()
main = defaultMain
    [ bench "process"            (nf process              [1..million])
    , bench "process'"           (nf process'             [1..million])
    , bench "processmanualfused" (nf processmanualfused   [1..million])
    , bench "processFuse"        (nf processFuse          [1..million])
    , bench "processVec"         (nf processVec           million)
    , bench "mapTestUnfused"     (nf mapTestUnfused       [1..million])
    , bench "mapTestFused"       (nf mapTestFused         [1..million])
    , bench "mapTestStream "     (nf mapTestStream        [1..million])
    ]
