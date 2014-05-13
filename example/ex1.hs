{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
import Data.Csv.TH
import Data.Csv
import Data.Vector as V
import Language.Haskell.TH
import qualified Data.ByteString.Lazy as L

-- need some abbreviated way to specify types?
-- or the default entry might be usable in other cases...
mkHeader def{ sampleFile = const "ex1.csv",
              types = const [[t| Double |]] }

main = do
    f <- L.readFile "ex1.csv"
    let Right xs = decode HasHeader f
    print (V.take 10  $ xs :: V.Vector Ex1)
