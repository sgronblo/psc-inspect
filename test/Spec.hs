import Test.Hspec
import PscInspect

main :: IO ()
main = hspec $ do
    describe "hasPursExtension" $ do
        it "should return true for a FilePath ending in .purs" $ do
            PscInspect.hasPursExtension "Thermite.purs" `shouldBe` True
