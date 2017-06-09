import Data.Foldable (for_)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Huffman (CodeTree(..), codeList, decode)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs =
  describe "Huffman" $ do
    let t1 = Fork (Leaf 'a' 2) (Leaf 'b' 3) ['a', 'b'] 5
    let t2 = Fork t1 (Leaf 'd' 4) ['a', 'b', 'd'] 9
    it "weight of a large tree" $ weight t1 `shouldBe` 5
    it "chars of a large tree" $ chars t2 `shouldBe` ['a', 'b', 'd']
