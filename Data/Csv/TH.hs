{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- | generates some boilerplate suggested by "Data.Csv" if the csv (or one with
-- the same columns) is available at compile time
module Data.Csv.TH (mkHeader, MkHeaderOptions(..), module Data.Default) where

import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.Csv
import Data.Csv.Parser
import Data.Default
import Data.Generics
import Data.List
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax as TH
import qualified Data.Attoparsec.ByteString -- (parseOnly)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import qualified Data.Vector as V
import System.FilePath
import System.IO


mkHeader' sanitize namesToDerive decodeOptions cName tName f tys = do
    f' <- runIO $ do
        h <- openFile f ReadMode
        r <- B.hGetLine h
        hClose h
        return r
    let rh @ ~ (Right h) = Data.Attoparsec.ByteString.parseOnly (header (decDelimiter decodeOptions))
                        (B.append f' (B8.pack "\n"))
    tys' <- sequence $ take (V.length h) (extend tys)

    tn <- tName
    cn <- cName


    dd <- dataD
        (return [])
        tn
        (map PlainTV (getVarTs tys'))
        [do
            hs <- sanitize (V.toList h)
            return $ RecC cn
                [ (h, NotStrict, ty) | (h, ty) <- zip hs tys'  ]]
        namesToDerive


    let instHead :: TH.Name -> TypeQ
        instHead className =
            [t| $(conT className) $(foldl (\a b -> [t| $a $(varT b) |]) (conT tn) (getVarTs tys')) |]

        mkCxt :: TH.Name -> CxtQ
        mkCxt n = (return [ ClassP n [VarT v] | v <- getVarTs tys'])


        fixName xs = everywhere (mkT $ \x -> case nameBase x of
                                    "parseRecord" -> 'parseRecord
                                    _ -> x)
                        xs

    fr <- instanceD (mkCxt ''FromField) (instHead ''FromRecord) . map (return . fixName) =<<
            [d| parseRecord v
                    | V.length v /= $(TH.lift (V.length h)) = mzero
                    | otherwise = $(foldl
                                        (\a n -> [| $a <*> (v .! n) |])
                                        [| pure $(conE tn) |]
                                        [0 .. V.length h - 1])
                                 |]

    tr <- instanceD (mkCxt ''ToField) (instHead ''ToRecord)
           [valD (varP 'toRecord) (normalB $
                let vs = [ mkName ("x"++show n) | n <- [1 .. V.length h]]
                in lamE [conP cn (map varP vs)]
                        [| Data.Csv.record
                            $(listE [ [| toField  $(varE v) |] | v <- vs]) |])
            []]

    return [dd, fr, tr]

mkHeader x@MkHeaderOptions { .. } = mkHeader'
    (sanitize x) (namesToDerive x) (decodeOptions x) (cName x) (tName x) (sampleFile x) (types x)


data MkHeaderOptions = MkHeaderOptions
    { sanitize :: MkHeaderOptions -> [Data.Csv.Name] -> Q [TH.Name],
      namesToDerive :: MkHeaderOptions -> [TH.Name],
      decodeOptions :: MkHeaderOptions -> DecodeOptions,
      cName, tName :: MkHeaderOptions -> Q TH.Name,
      sampleFile :: MkHeaderOptions -> FilePath,
      types :: MkHeaderOptions -> [Q Type] }

instance Default MkHeaderOptions where
    def = let
            onHead f (x:xs) = f x : xs
            onHead _ xs = xs
     in
     MkHeaderOptions {
      sanitize = \_ ->
        let
            -- separate out subs and onHead toLower?
            subs '.' = '_'
            subs ' ' = '_'
            subs x = x
            f n = do
                let s = map subs . onHead toLower . B8.unpack $ n
                modify $ M.insertWith (+) s 1
                return s

            -- adds suffixes to give names like: foo1 foo2 foo3
            -- when there is more than one column named foo
            renumber xs = do
                modify $ M.map (const 1) . M.filter (> 1)
                mapM (\x -> do
                    m <- get
                    fmap mkName $ maybe (return x)
                            (\n -> do
                                modify (M.insertWith (+) x 1)
                                return $ x ++ show n)
                            (M.lookup x m)) xs
        in \xs -> flip evalStateT M.empty $ renumber =<< mapM f xs ,

        namesToDerive = \_ -> [''Read, ''Show, ''Data, ''Typeable ],

        decodeOptions = \_ -> defaultDecodeOptions,
        cName = \self -> tName self self,
        tName = \self -> return . mkName . dropExtension . onHead toUpper $  sampleFile self self,
        sampleFile = \_ -> "sample.csv",
        types = \ _ -> [ varT =<< newName "t" ]
    }

getVarTs = nub . everything (++) (mkQ [] (\x -> case x of
    VarT t -> [t]
    _ -> []))

extend (x:y:xs) = x : extend (y:xs)
extend [y] = repeat y
extend [] = error "extend needs at least 1 element"

