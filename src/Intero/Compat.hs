{-# LANGUAGE CPP #-}

-- | Compatibility between GHC API versions.

module Intero.Compat
  ( ghc_getModuleGraph
  , ghc_getInfo
  , ghc_defaultDynFlags
  , ghc_topSortModuleGraph
  , ghc_mkWarn
  , ghc_mkErr
  , ghc_errMsg
  , ghc_warnMsg
  , ghc_tyConFlavour
  , StageReaderName
  , StageReaderRdrName
  , StageReaderId
  ) where

#if __GLASGOW_HASKELL__ > 800
import           TyCoRep
#endif
import           TyCon
#if __GLASGOW_HASKELL__ > 802
import           CmdLineParser
#endif
#if __GLASGOW_HASKELL__ >= 800
import qualified Data.Graph as SCC
#else
import qualified Digraph as SCC
#endif
import           DynFlags
import           GHC

ghc_tyConFlavour :: TyCon -> String
#if __GLASGOW_HASKELL__ > 802
ghc_tyConFlavour n =
  if tyConFlavour n == ClassFlavour
    then "class"
    else ""
#else
#if __GLASGOW_HASKELL__ > 800
ghc_tyConFlavour = tyConFlavour
#else
ghc_tyConFlavour _ = ""
#endif
#endif

ghc_defaultDynFlags :: Settings -> DynFlags
#if __GLASGOW_HASKELL__ <= 802
ghc_defaultDynFlags = defaultDynFlags
#else
ghc_defaultDynFlags s = defaultDynFlags s []
#endif

ghc_getInfo :: GhcMonad m => Bool -> Name -> m (Maybe (TyThing, Fixity, [ClsInst], [FamInst]))
#if __GLASGOW_HASKELL__ <= 802
ghc_getInfo = getInfo
#else
ghc_getInfo x y = fmap (fmap (\(a,b,c,d,_) -> (a,b,c,d))) (getInfo x y)
#endif

ghc_getModuleGraph :: GhcMonad m => m [ModSummary]
#if __GLASGOW_HASKELL__ <= 802
ghc_getModuleGraph = GHC.getModuleGraph
#else
ghc_getModuleGraph = fmap mgModSummaries GHC.getModuleGraph
#endif

ghc_topSortModuleGraph :: Bool -> [ModSummary] -> Maybe ModuleName -> [SCC.SCC ModSummary]
#if __GLASGOW_HASKELL__ <= 802
ghc_topSortModuleGraph = GHC.topSortModuleGraph
#else
ghc_topSortModuleGraph bool sums may = GHC.topSortModuleGraph bool (mkModuleGraph sums) may
#endif

#if __GLASGOW_HASKELL__ <= 802
type StageReaderName = Name
#else
type StageReaderName = GhcRn
#endif

#if __GLASGOW_HASKELL__ <= 802
type StageReaderRdrName = RdrName
#else
type StageReaderRdrName = GhcPs
#endif

#if __GLASGOW_HASKELL__ <= 802
type StageReaderId = Id
#else
type StageReaderId = GhcTc
#endif

#if __GLASGOW_HASKELL__ > 802
ghc_mkWarn :: Located String -> Warn
ghc_mkWarn = Warn CmdLineParser.NoReason
#else
ghc_mkWarn :: a -> a
ghc_mkWarn = id
#endif

#if __GLASGOW_HASKELL__ > 802
ghc_mkErr :: Located String -> Err
ghc_mkErr = Err
#else
ghc_mkErr :: a -> a
ghc_mkErr = id
#endif

#if __GLASGOW_HASKELL__ > 802
ghc_errMsg :: Err -> Located String
ghc_errMsg = errMsg
#else
ghc_errMsg :: a -> a
ghc_errMsg = id
#endif

#if __GLASGOW_HASKELL__ > 802
ghc_warnMsg :: Warn -> Located String
ghc_warnMsg = warnMsg
#else
ghc_warnMsg :: a -> a
ghc_warnMsg = id
#endif
