module Main (main) where


import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Exit

import System.FilePath
import System.Directory

import Language.Haskell.Exts

import Distribution.License
import Distribution.ModuleName (ModuleName, fromString, components)
import Distribution.Package
import Distribution.PackageDescription hiding (Var)
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse hiding (ParseOk)
import Distribution.Verbosity
import Distribution.Version


thePkgName :: String
thePkgName = "xhb-requests"

baseModulePath, classesPath, instancesPath :: [String]
baseModulePath = ["Graphics", "XHB", "Requests"]
classesPath = baseModulePath ++ ["Internal", "Classes"]
instancesPath = baseModulePath ++ ["Internal" ,"Instances"]


main :: IO ()
main = do
    args <- getArgs
    case args of
        [inDir, outDir] -> generate inDir outDir
        _ -> die $ "Usage: gen-" ++ thePkgName ++ " <inDir> <outDir>"


generate :: FilePath -> FilePath -> IO ()
generate inDir outDir = do
    desc <- readPackageDescription silent (inDir </> "xhb.cabal")
    let (vs, targs) = getInfo desc
        cabalOut = outDir </> thePkgName <.> "cabal"
        instDir = outDir </> "gen" </> joinPath instancesPath
        genDir = inDir </> "patched" </> "Graphics" </> "XHB" </> "Gen"
    createDirectoryIfMissing True instDir
    writePackageDescription cabalOut $ buildDesc vs targs
    writeFile (instDir <.> "hs") . prettyPrint $ allInstances targs
    forM_ targs $ \targ -> do
        ParseOk mod <- parseFile $ genDir </> targ <.> "hs"
        writeFile (instDir </> targ <.> "hs") . prettyPrint $ instances targ mod


getInfo :: GenericPackageDescription -> ([Int], [String])
getInfo gpd = (vs, targs)
  where
    pd = flattenPackageDescription gpd
    vs = versionBranch . pkgVersion $ package pd
    targs = catMaybes . map (f . components) . exposedModules . fromJust $ library pd
    f ["Graphics", "XHB", "Gen", mod] = Just mod
    f _ = Nothing


----------------------
-- CABAL GENERATION --
----------------------


cabalModuleName :: [String] -> Distribution.ModuleName.ModuleName
cabalModuleName = fromString . intercalate "."


buildDesc :: [Int] -> [String] -> PackageDescription
buildDesc vs mods = emptyPackageDescription
    { package = PackageIdentifier (PackageName thePkgName) (Version (0:1:vs) [])
    , license = MIT
    , licenseFiles = ["LICENSE"]
    , author = "Nick Spinale"
    , maintainer = "spinalen@carleton.edu"
    , buildType = Just Simple
    , extraSrcFiles = ["README.md"]
    , specVersionRaw = Left (Version [1, 10] [])
    , library = Just emptyLibrary
        { exposedModules = [cabalModuleName baseModulePath]
        , libBuildInfo = emptyBuildInfo
            { otherModules = cabalModuleName classesPath
                           : cabalModuleName instancesPath
                           : map (cabalModuleName . (++) instancesPath . (:[])) mods
            , targetBuildDepends =
                [ Dependency (PackageName "base") anyVersion
                , Dependency (PackageName "xhb") (thisVersion (Version vs []))
                ]
            , hsSourceDirs = ["src", "gen"]
            }
        }
    }


---------------------
-- CODE GENERATION --
---------------------


allInstances :: [String] -> Module
allInstances files = Module emptyLoc name [] Nothing (Just []) imps []
  where
    name = ModuleName $ intercalate "." instancesPath
    prag = LanguagePragma emptyLoc [Ident "MultiParamTypeClasses"]
    imps = map imp files
    imp file = (emptyImport (intercalate "." (instancesPath ++ [file])))
                    { importSpecs = Just (False, []) }


instances :: String -> Module -> Module
instances file (Module _ _ _ _ _ _ decls) = Module emptyLoc name [prag] Nothing (Just []) imps ds
  where
    name = hsModuleName $ instancesPath ++ [file]
    prag = LanguagePragma emptyLoc [Ident "MultiParamTypeClasses"]
    f (mod, vars) = (emptyImport mod) { importSpecs = Just (False, map (IVar . Ident) vars) }
    imps = emptyImport ("Graphics.XHB.Gen." ++ file)
         : emptyImport (intercalate "." classesPath)
         : map f [ ("Prelude", ["fmap"])
                 , ("Data.Bifunctor", ["second"])
                 , ("Graphics.XHB", ["getReply"])
                 ]
    ds = map (\(x, y, z) -> convert x y z) . catMaybes $ map flatten decls


flatten :: Decl -> Maybe (Name, [Type], Maybe Type)
flatten (TypeSig _ [name] (TyFun (TyCon (Qual (ModuleName "Graphics.XHB.Connection.Types") (Ident "Connection"))) rest)) =
    Just (name, ts, mt)
  where
    (ts, mt) = go rest
    go (TyFun a b) = first (a:) $ go b
    go (TyApp (TyCon (UnQual (Ident "IO"))) inner) = (,) [] $ case inner of
        TyCon (Special UnitCon) -> Nothing
        TyParen (TyApp (TyCon (UnQual (Ident "Receipt"))) outCon) -> Just outCon

--safety
flatten (TypeSig _ [Ident name] _) =
    if name `elem` ["extension", "deserializeError", "deserializeEvent"]
     then Nothing
     else undefined
flatten _ = Nothing


convert :: Name -> [Type] -> Maybe Type -> Decl
convert (Ident name) args ret = InstDecl emptyLoc Nothing [] []
    (UnQual (Ident klazz))
    types
    [InsDecl (FunBind [Match emptyLoc (Ident fun) pats Nothing (UnGuardedRhs rhs) Nothing])]

  where

    title = case name of (n:ame) -> toUpper n : ame
    vars = zipWith const (map (:[]) ['a'..]) args

    (types, klazz, fun) =
        if isJust ret
        then ([inType, outType], "RequestWithReply", "requestWithReplyIO")
        else ([inType], "Request", "requestIO")

    (pats, outArgs) =
        let (pats', outArgs') =
                if args == [inType]
                then (pvarId "req", ["req"])
                else (PParen (PApp (unQual ("Mk" ++ title)) (map pvarId vars)), vars)
        in (pvarId "conn" : [pats'], map varOf outArgs')

    prerhs = case ret of
                Nothing -> id
                Just typ ->
                    let tomap =
                            if typ == outType
                            then id
                            else App (fmapTo (fmapTo (App (varOf "second") (varOf ("Mk" ++ title ++ "Reply")))))
                    in tomap . App (fmapTo (varOf "getReply"))

    rhs = prerhs $ foldl App (App (varOf name) (varOf "conn")) outArgs

    pvarId = PVar . Ident
    unQual = UnQual . Ident
    varOf = Var . unQual
    fmapVar = varOf "fmap"
    fmapTo = App fmapVar

    inType = TyCon (unQual title)
    outType = TyCon (unQual (title ++ "Reply"))


hsModuleName :: [String] -> Language.Haskell.Exts.ModuleName
hsModuleName = ModuleName . intercalate "."

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
