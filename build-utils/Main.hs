module Main where

import Prelude
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import System.Exit
import System.Environment
import System.FilePath
import System.Directory
import Language.Haskell.Exts
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.ModuleName (fromString)
import Distribution.Verbosity
import Distribution.Version
import Distribution.License


-- Takes keysymdef.h as stdin and writes Graphics.XHB.KeySym.{Defs,Names} as
-- into defsOut and namesOut

main :: IO ()
main = do
    args <- getArgs
    case args of
        [xhbIn, hsOut] -> do
            vs <- getVersion xhbIn
            writePackageDescription "test.cabal" $ buildDesc 1 vs ["Hello"]
        _ -> die "Usage: gen-xhb-monad <xhbIn> <hsOut>"


getVersion :: FilePath -> IO [Int]
getVersion = fmap f . readPackageDescription silent . (</> "xhb.cabal")
  where f = versionBranch . pkgVersion . package . packageDescription


buildDesc :: Int -> [Int] -> [String] -> PackageDescription
buildDesc v vs mods = emptyPackageDescription
    { package = PackageIdentifier (PackageName "xhb-monad") (Version (v:vs) [])
    , license = MIT
    , licenseFiles = ["LICENSE"]
    , author = "Nick Spinale"
    , maintainer = "spinalen@carleton.edu"
    , buildType = Just Simple
    , extraSrcFiles = ["README.md"]
    , specVersionRaw = Left (Version [1, 10] [])
    , buildDepends =
        [ Dependency (PackageName "xhb") (thisVersion (Version vs []))
        ]
    , library = Just emptyLibrary
        { exposedModules = [fromString "Graphics.XBH.Monad"]
        , libBuildInfo = emptyBuildInfo
            { otherModules = fromString "Graphics.XHB.Monad.Internal.Classes"
                           : map (fromString . ("Graphics.XBH.Monad.Internal.Instances." ++)) mods
            }
        }
    }


---------------------
-- CODE GENERATION --
---------------------


emptyLoc :: SrcLoc
emptyLoc = SrcLoc "" 0 0

emptyImport :: String -> ImportDecl
emptyImport mod = ImportDecl
    { importLoc = emptyLoc
    , importModule = ModuleName mod
    , importQualified = False
    , importSrc = False
    , importSafe = False
    , importPkg = Nothing
    , importAs = Nothing
    , importSpecs = Nothing
    }


instances :: String -> Module -> Module
instances = undefined
