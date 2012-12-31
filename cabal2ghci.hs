{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}
module Main where
import            Control.Applicative
import           Control.Monad
import qualified Data.HashMap.Strict                   as HM
import           Data.List
import           Data.Maybe
import qualified Data.Text                             as T
import           Data.Typeable
import           Data.Yaml
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
import           Filesystem
import           Filesystem.Path.CurrentOS             hiding (encode)
import           Language.Haskell.Extension
import           Language.Haskell.Stylish
import           Prelude                               hiding (FilePath,
                                                        readFile, writeFile)
import           System.Console.CmdArgs

default (T.Text)

data Option = Option { cabal :: Maybe String , noStylish :: Bool }
              deriving (Show, Eq, Ord, Typeable, Data)

defOption :: Option
defOption = Option { cabal = def &= help "cabal file" &= opt Nothing
                   , noStylish = def
                   }

main :: IO ()
main = do
  Option{..} <- cmdArgs defOption
  cabp <- case cabal of
    Nothing -> do
         cabs <- filter ((== Just "cabal") . extension) <$> listDirectory "."
         case cabs of
           []  -> error "no cabal file here."
           [f] -> return f
           _  -> error $ "There are more than two  cabal files: " ++ unwords (map encodeString cabs)
    Just fp -> do
      ext <- isFile $ decodeString fp
      if ext then return (decodeString fp) else error $ "no such file: " ++ fp
  result <- parsePackageDescription . T.unpack <$> readTextFile cabp
  case result of
    ParseFailed pe -> error $ show pe
    ParseOk _ gpack -> do
      let pack = packageDescription gpack
          bInfos = allBuildInfo pack ++ gpackBuildInfos gpack
          exts = nub $ concatMap allExtensions bInfos
          dirs = nub $ concatMap hsSourceDirs bInfos
          srcLines = map ((":set -X" ++) . showExt) exts
          dirLines = map (":set -i" ++) dirs
          ghci = T.pack $ unlines srcLines ++ unlines dirLines
      unless (T.null ghci) $ writeTextFile ".ghci" ghci
      unless noStylish $ do
        mfp <- configFilePath (const $ return ()) Nothing
        Object obj <-
            case mfp of
              Just fp -> do
                fromMaybe (object []) <$> decodeFile fp
              Nothing -> return $ object []
        let obj' = HM.insert "language_extensions" (toJSON $ map showExt exts) obj
        encodeFile ".stylish-haskell.yaml" $ Object obj'

showExt :: Extension -> String
showExt (EnableExtension kext)  = show kext
showExt (DisableExtension kext) = "No" ++ show kext
showExt (UnknownExtension ext)  = ext

getChildren :: CondTree v c a -> [a]
getChildren (CondNode a _ cs) = a : concatMap (\(_, b, mc) -> getChildren b ++ maybe [] getChildren mc) cs

gpackLib :: GenericPackageDescription -> [Library]
gpackLib = maybe [] getChildren . condLibrary

gpackExes :: GenericPackageDescription -> [Executable]
gpackExes = concatMap (getChildren . snd) . condExecutables

gpackBuildInfos :: GenericPackageDescription -> [BuildInfo]
gpackBuildInfos gp = map libBuildInfo (gpackLib gp) ++ map buildInfo (gpackExes gp)
