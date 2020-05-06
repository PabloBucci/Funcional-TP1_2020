import PdePreludat
import Library
import Test.Hspec


main :: IO ()
main = hspec $ do

    describe "PUNTO 1: Costo de reparacion:" $ do 

      it "de un auto cuya patente es 'AT001LN' debe ser $12.500" $ do
            (costoDeReparacion fiatRojo) `shouldBe` 12500

      it "de un auto cuya patente es 'DJV214' debe ser $18.000" $ do
            (costoDeReparacion fiatAzul) `shouldBe` 18000

      it "de un auto cuya patente es 'DJV215' debe ser $20.000" $ do
            (costoDeReparacion fiatVerde) `shouldBe` 20000

      it "de un auto cuya patente es 'DFH029' debe ser $15.000" $ do
            (costoDeReparacion upRojo) `shouldBe` 15000

    describe "PUNTO 2:" $ do

  --      it "Monto de Impuesto para un exento" $ do
  --          (calcularImpuestoGanancias exento) `shouldBe` 0.

    --      it "Monto de Impuesto para un exento" $ do
  --          (calcularImpuestoGanancias exento) `shouldBe` 0.0

      it "Un auto cuyo anio de ultima revision fue 2015 necesita revision" $ do
            (necesitaRevision fiatRojo) `shouldBe` True

      it "Un auto cuyo anio de ultima revision fue 2016 no necesita revision" $ do
            (necesitaRevision upRojo) `shouldBe` False

    describe "PUNTO 3:" $ do

  --      it "Monto de Impuesto para un exento" $ do
  --          (calcularImpuestoGanancias exento) `shouldBe` 0.0

    --      it "Monto de Impuesto para un exento" $ do
  --          (calcularImpuestoGanancias exento) `shouldBe` 0.0

      it "Tango deja al auto tal como lo recibio (no hace nada)" $ do
         ((patente.tango) fiatVerde) `shouldBe` "DJV215"
         ((desgasteLlantas.tango) fiatVerde) `shouldBe` [0,1,1,0]
         ((rpm.tango) fiatVerde) `shouldBe` 125
         ((temperaturaAgua.tango) fiatVerde ) `shouldBe` 42
         ((ultimoArreglo.tango) fiatVerde)  `shouldBe` (12,3,2019)

      it "Zulu pone el agua a 90 y hace lo mismo que Lima" $ do
          ((desgasteLlantas.zulu) fiatRojo) `shouldBe` [0,0,1,1]
          ((temperaturaAgua.zulu) fiatVerde ) `shouldBe` 90

      it "Lima deslima las cubiertas de adelante y las deja en desgaste 0" $ do
            ((desgasteLlantas.lima) upRojo) `shouldBe` [0,0,0,1]

  