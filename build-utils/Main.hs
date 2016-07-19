module Main where

import Prelude
import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Bifunctor
import Data.Char
import Data.Maybe
import System.Exit
import System.Environment
import System.FilePath
import System.Directory
import Language.Haskell.Exts
import Distribution.Package
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription hiding (Var)
import Distribution.PackageDescription.Parse hiding (ParseOk)
import Distribution.ModuleName (fromString, components)
import Distribution.Verbosity
import Distribution.Version
import Distribution.License
import System.IO.Unsafe


-- Takes keysymdef.h as stdin and writes Graphics.XHB.KeySym.{Defs,Names} as
-- into defsOut and namesOut

main :: IO ()
main = do
    args <- getArgs
    case args of
        [xhbIn, cabalOut, hsOut] -> do
            desc <- readPackageDescription silent (xhbIn </> "xhb.cabal")
            let (vs, targs) = getInfo desc
            writePackageDescription cabalOut $ buildDesc 1 vs targs
            writeFile (hsOut </> "Instances" <.> "hs") . prettyPrint $ allInstances targs
            forM_ targs $ \targ -> do
                ParseOk mod <- parseFile $ xhbIn </> "patched"
                                                 </> "Graphics"
                                                 </> "XHB"
                                                 </> "Gen"
                                                 </> targ <.> "hs"
                writeFile (hsOut </> "Instances" </> targ <.> "hs") . prettyPrint $ instances targ mod
        _ -> die "Usage: gen-xhb-monad <xhbIn> <cabalOut> <hsOut>"


----------------------
-- CABAL GENERATION --
----------------------


getInfo :: GenericPackageDescription -> ([Int], [String])
getInfo gpd = (vs, targs)
  where
    pd = flattenPackageDescription gpd
    vs = versionBranch . pkgVersion $ package pd
    targs = catMaybes . map (f . components) . exposedModules . fromJust $ library pd
    f ["Graphics", "XHB", "Gen", mod] = Just mod
    f _ = Nothing


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
    , library = Just emptyLibrary
        { exposedModules = [fromString "Graphics.XHB.Monad"]
        , libBuildInfo = emptyBuildInfo
            { otherModules = fromString "Graphics.XHB.Monad.Internal.Class"
                           : fromString "Graphics.XHB.Monad.Internal.Instances"
                           : map (fromString . ("Graphics.XHB.Monad.Internal.Instances." ++)) mods
            , targetBuildDepends =
                [ Dependency (PackageName "base") (anyVersion)
                , Dependency (PackageName "xhb") (thisVersion (Version vs []))
                ]
            }
        }
    }


---------------------
-- CODE GENERATION --
---------------------


allInstances :: [String] -> Module
allInstances files = Module emptyLoc name [] Nothing (Just []) imps []
  where
    name = ModuleName ("Graphics.XHB.Monad.Internal.Instances")
    prag = LanguagePragma emptyLoc [Ident "MultiParamTypeClasses"]
    imps = map (\file -> emptyImport ("Graphics.XHB.Monad.Internal.Instances." ++ file)) files


instances :: String -> Module -> Module
instances file (Module _ _ _ _ _ _ decls) = Module emptyLoc name [prag] Nothing (Just []) imps ds
  where
    name = ModuleName ("Graphics.XHB.Monad.Internal.Instances." ++ file)
    prag = LanguagePragma emptyLoc [Ident "MultiParamTypeClasses"]
    f (mod, vars) = (emptyImport mod) { importSpecs = Just (False, map (IVar . Ident) vars) }
    imps = emptyImport ("Graphics.XHB.Gen." ++ file)
         : emptyImport "Graphics.XHB.Monad.Internal.Class"
         : map f [ ("Prelude", ["fmap"])
                 , ("Data.Bifunctor", ["second"])
                 , ("Graphics.XHB", ["getReply"])
                 ]
    ds = map (\(x, (y, z)) -> convert x y z) . catMaybes $ map relevant decls


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


relevant :: Decl -> Maybe (Name, ([Type], Maybe Type))
relevant (TypeSig _ [name] typ) =
    if name `elem` ignore
     then Nothing
     else Just $ (name, flatten typ)
     -- else Just undefined
relevant _ = Nothing


flatten :: Type -> ([Type], Maybe Type)
flatten (TyFun (TyCon (Qual (ModuleName "Graphics.XHB.Connection.Types") (Ident "Connection"))) rest) = go rest
  where
    go (TyFun a b) = first (a:) $ go b
    go (TyApp (TyCon (UnQual (Ident "IO"))) inner) = (,) [] $ case inner of
        TyCon (Special UnitCon) -> Nothing
        TyParen (TyApp (TyCon (UnQual (Ident "Receipt"))) outCon) -> Just outCon


ignore :: [Name]
ignore = map Ident ["extension", "deserializeError", "deserializeEvent"]


convert :: Name -> [Type] -> Maybe Type -> Decl
convert (Ident name) args ret = InstDecl emptyLoc Nothing [] []
    (UnQual (Ident klazz))
    types
    [InsDecl $ FunBind [Match emptyLoc (Ident fun) pats Nothing (UnGuardedRhs rhs) Nothing]]
  where
    req = isJust ret
    types = inType : if req then [outType] else []
    title = case name of (n:ame) -> toUpper n : ame
    (klazz, fun) = if req then ("Request", "request") else ("Notice", "notice")

    pats = PVar (Ident "conn") : [ if args == [inType]
                                    then PVar (Ident "req")
                                    else PParen $ PApp (UnQual (Ident ("Mk" ++ title))) $ map PVar vars
                                 ]

    vars = zipWith const (map (Ident . (:[])) ['a'..]) args
    prerhs = case ret of
                Nothing -> id
                Just typ -> let tomap = if typ == outType
                                         then id
                                         else InfixApp (App (Var (UnQual (Ident "fmap")))
                                                            (App (Var (UnQual (Ident "second")))
                                                                 (Var (UnQual (Ident ("Mk" ++ title ++ "Reply"))))))
                                                       (QVarOp (UnQual (Ident "fmap")))
                            in tomap . App (App (Var (UnQual (Ident "fmap")))
                                                (Var (UnQual (Ident "getReply"))))

    outArgs = map (Var . UnQual) $ if args == [inType] then [Ident "req"] else vars
    rhs = prerhs $ foldl App (App (Var (UnQual (Ident name))) (Var (UnQual (Ident "conn")))) outArgs

    inType = TyCon (UnQual (Ident title))
    outType = TyCon (UnQual (Ident (title ++ "Reply")))
