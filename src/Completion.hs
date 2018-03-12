{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- | A GHC code completion module.

module Completion
  ( getCompletableModule
  , declarationByLine
  , declarationHoles
  , holeSubstitutions
  , Declaration(..)
  , Hole(..)
  , Substitution(..)
  , LineNumber(..)
  ) where

import           Bag
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Generics
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Ord
import           DynFlags
import           FastString
import           GHC
import           HscTypes
import           Intero.Compat
import           Name
import           OccName
import           Outputable
import           RdrName
import           TcRnDriver
import           TcRnTypes (tcg_rdr_env)
import           TyCoRep
import           TysWiredIn
import           Unify
import           Unique
import           Var

--------------------------------------------------------------------------------
-- Types

-- | A module which can be completed. Cannot contain type errors,
-- including deferred ones.
data CompletableModule =
  CompletableModule TypecheckedModule

-- | All the context we need to generate completions for a declaration
-- in a module.
data Declaration = Declaration
  { declarationBind :: !(HsBindLR StageReaderName StageReaderName)
    -- ^ The actual declaration, which we use to find holes and
    -- substitute them with candidate replacements.
    -- ^ A sample source, which we use merely for debugging.
  , declarationRealSrcSpan :: !RealSrcSpan
    -- ^ A source span which we can provide to the client IDE.
  , declarationParsedModule :: !ParsedModule
   -- ^ The declaration belongs to a parsed module which we'll use to
   -- try out alterations to the tree and see if they type-check.
  , declarationRenamedModule :: !RenamedSource
     -- ^ The renamed module contains 'UnboundedVar', which marks a hole.
  , declarationModuleInfo :: !ModuleInfo
  , declarationTypecheckedModule :: !TypecheckedSource
    -- ^ Used to get type of holes.
  , declarationGlobalRdrEnv :: !GlobalRdrEnv
  }

instance Show Declaration where
  showsPrec p (Declaration b real _parsedModule _renamedSource _ _ _) =
    showString "Declaration {declarationBind = " .
    gshows b .
    showString ", declarationRealSrcSpan = " .
    showsPrec (p + 1) real . showString "}"

-- | An identifier for a declaration in the module.
newtype DeclarationId = DeclarationId String
  deriving (Show)

-- | Line number from the module.
newtype LineNumber = LineNumber Int
  deriving (Show)

-- | A hole written `_` or `_foo` in the user-inputed source, which we
-- can fill in with candidates.
data Hole = Hole
  { holeRealSrcSpan :: !RealSrcSpan
  , holeName :: !OccName
  , holeType :: !Type
  , holeDf :: !DynFlags
  , holeDeclaration :: !Declaration
  }

instance Show Hole where
  showsPrec p (Hole realSrcSpan name ty df _) =
    showString "Hole {holeRealSrcSpan = " .
    showsPrec (p + 1) realSrcSpan .
    showString ", holeName = " . gshows name . showString ", holeType = " .
    showString (showPpr df ty) . showString "}"

-- | Substition of a source span in the source code with a new string.
data Substitution = Substitution
  { substitutionReplacement :: !Name
  , substitutionString :: !String
  , substitutionType :: !Type
  }

instance Show Substitution where
  showsPrec _p (Substitution name _q _ty) =
    showString "Substitution {substitutionReplacement = " .
    gshows name . showString "}"

--------------------------------------------------------------------------------
-- Top-level API

-- | Get a module which can be completed. Cannot contain type errors,
-- including deferred ones.
getCompletableModule :: GhcMonad m => ModSummary -> m CompletableModule
getCompletableModule ms =
  fmap CompletableModule (parseModule ms >>= typecheckModuleNoDeferring)

-- | Find a declaration by line number. If the line is within a
-- declaration in the module, return that declaration.
declarationByLine :: CompletableModule -> LineNumber -> Maybe Declaration
declarationByLine (CompletableModule typecheckedModule) (LineNumber line) = do
  renamedModule <- tm_renamed_source typecheckedModule
  let binds = renamedSourceToBag renamedModule
  located <- find ((`realSpans` (line, 1)) . getLoc) (bagToList binds)
  realSrcSpan <- getRealSrcSpan (getLoc located)
  pure
    (Declaration
     { declarationBind = unLoc located
     , declarationRealSrcSpan = realSrcSpan
     , declarationRenamedModule = renamedModule
     , declarationParsedModule = tm_parsed_module typecheckedModule
     , declarationTypecheckedModule = tm_typechecked_source typecheckedModule
     , declarationModuleInfo = tm_checked_module_info typecheckedModule
     , declarationGlobalRdrEnv = tcg_rdr_env (fst (tm_internals_ typecheckedModule))
     })

-- | Get all the holes in the given declaration.
declarationHoles :: DynFlags -> Declaration -> [Hole]
declarationHoles df declaration = go declaration
  where
    go =
      mapMaybe
        (\h -> do
           (name, src) <- getHoleName h
           case listToMaybe
                  (listify
                     (isJust . typeAt src)
                     (declarationTypecheckedModule declaration)) >>=
                typeAt src of
             Nothing -> Nothing
             Just typ ->
               pure
                 (Hole
                  { holeRealSrcSpan = src
                  , holeName = name
                  , holeType = typ
                  , holeDf = df
                  , holeDeclaration = declaration
                  })) .
      listify (isJust . getHoleName) . declarationBind
    typeAt :: RealSrcSpan -> LHsExpr StageReaderId -> Maybe Type
    typeAt rs expr =
      if getLoc expr == RealSrcSpan rs
        then case expr of
               L _ (HsVar (L _ i)) -> pure (idType i)
               _ -> Nothing
        else Nothing
    getHoleName :: LHsExpr StageReaderName -> Maybe (OccName, RealSrcSpan)
    getHoleName =
      \case
        L someSpan (HsUnboundVar (TrueExprHole name)) -> do
          rs <- getRealSrcSpan someSpan
          pure (name, rs)
        _ -> Nothing

-- | Get completions for a declaration.
holeSubstitutions :: GhcMonad m => Hole -> m [Substitution]
holeSubstitutions hole =
  do let names =
           filter
             isValName
             (fromMaybe
                []
                (modInfoTopLevelScope (declarationModuleInfo declaration)))
     hscEnv <- getSession
     typedNames <-
       liftIO
         (foldM
            (\(!names') rdrName -> do
               (_, ty) <-
                 tcRnExpr
                   hscEnv
                   TM_Inst
                   (rdrNameToLHsExpr (nameRdrName rdrName))
               pure (maybe names' (: names') (fmap (rdrName, ) ty)))
            []
            names)
     subs <-
       getWellTypedFills
         (declarationParsedModule declaration)
         hole
         typedNames
     pure
       (sortBy
          (flip (comparing (typeSpecificity . substitutionType)))
          (map
             (\(name, ty, _) ->
                Substitution
                { substitutionReplacement = name
                , substitutionType = ty
                , substitutionString =
                    makeReplacementString
                      (declarationGlobalRdrEnv declaration)
                      name
                })
             subs))
  where
    declaration = holeDeclaration hole

-- | A vague weighting for relevance of types. We assume that more
-- specific types are more appropriate.
typeSpecificity :: Type -> Int
typeSpecificity t = sum (map rate (listify ((> 0) . rate) t))
  where
    rate =
      \case
        TyConApp {} -> 10
        LitTy {} -> 5
        FunTy {} -> 1
        _ -> 0

-- | Make a string, qualified if necessary.
makeReplacementString :: GlobalRdrEnv -> Name -> String
makeReplacementString gre name =
  case lookupGRE_Name gre name of
    Nothing -> unqualified
    Just grelt ->
      if greltUnqualified grelt && unambiguous grelt
        then unqualified
        else maybe unqualified qualified (greltQualification grelt)
  where
    unqualified = occNameString (nameOccName name)
    qualified m = moduleNameString m ++ "." ++ unqualified
    unambiguous grelt = null conflicts
      where
        conflicts =
          filter
            greltUnqualified
            (filter
               (/= grelt)
               (lookupGlobalRdrEnv gre (nameOccName (gre_name grelt))))

-- | First the first available qualification for a name.
greltQualification :: GlobalRdrElt -> Maybe ModuleName
greltQualification grelt =
  case gre_imp grelt of
    (ImpSpec (ImpDeclSpec {is_as = m}) _:_) -> Just m
    _ -> Nothing

-- | The element is not qualified.
greltUnqualified :: GlobalRdrElt -> Bool
greltUnqualified grelt = local || importedUnqualified
  where
    local = gre_lcl grelt
    importedUnqualified = any unQualSpecOK (gre_imp grelt)

--------------------------------------------------------------------------------
-- Testing out completions

data StringEquality = StringEquality
  { _stringEqualityDf :: DynFlags
  , _stringEqualityType :: Type
  }
instance Show StringEquality where
  show (StringEquality df x) = showPpr df x
instance Eq StringEquality where
  StringEquality df t1 == StringEquality df' t2 =
    showPpr df t1 == showPpr df' t2
instance Ord StringEquality where
  compare (StringEquality df t1) (StringEquality df' t2) =
    compare (showPpr df t1) (showPpr df' t2)

-- | Get a set of well-typed fills for the given hole.
--
-- Candidates with the same type are cached, to avoid recompiling the
-- module more than neccessary.
getWellTypedFills ::
     GhcMonad m
  => ParsedModule
  -> Hole
  -> [(Name, Type)]
  -> m [(Name, Type, ParsedModule)]
getWellTypedFills pm hole names = do
  df <- getSessionDynFlags
  let hty = normalize df (holeType hole)
  fmap
    snd
    (foldM
       (\(!cache, !candidates) (!rdrname, !typ) ->
          (do mparsedModule <-
                case M.lookup (StringEquality df typ) cache of
                  Just mparsedModule -> pure mparsedModule
                  Nothing ->
                    tryWellTypedFill pm hole (rdrNameToHsExpr (nameRdrName rdrname))
              let !cache' = M.insert (StringEquality df typ) mparsedModule cache
                  !candidates' =
                    case mparsedModule of
                      Nothing -> candidates
                      Just parsedModule -> (rdrname, typ, parsedModule) : candidates
              pure (cache', candidates')))
       (mempty, [])
       (filter (\(name, ty) -> unifies' df hty (normalize df ty) name) names))

unifies' :: DynFlags -> Type -> Type -> Name -> Bool
unifies' df x y _name =
  -- trace
  --   ("Unifies? " ++
  --    showPpr df name ++
  --    " :: " ++
  --    showPpr df y ++
  --    "\n    " ++
  --    show (T df x) ++
  --    "\n    against\n    " ++ show (T df y) ++ "\n    => " ++ show (unifies df x y))
    (unifies df x y)

-- | The purpose of this function is to eliminate types that should
-- not be tested with a full module type-check. This checker is
-- stricter than GHC's own unifier, much stricter than Hoogle; it
-- produces false negatives. But it should not produce false positives
-- ideally.
unifies :: DynFlags -> Type -> Type -> Bool
unifies _df t1 t2 = theirs t1 t2 && ours t1 t2
  where
    theirs x y =
      -- trace
      --   ("theirs(" ++
      --    showPpr df x ++
      --    "," ++ showPpr df y ++ ")=>" ++ show (isJust (tcUnifyTyKi x y)))
        (isJust (tcUnifyTyKi x y))
    -- Let them deal with lits:
    ours x@LitTy {} y@LitTy {} = theirs x y
    -- We assume a type variable unifies with anything, leave it to
    -- them:
    ours x@TyVarTy {} y = theirs x y
    ours x y@TyVarTy {} = theirs x y
    -- We ignore forall's:
    ours (ForAllTy _ x) y = ours x y
    ours x (ForAllTy _ y) = ours x y
    -- We ignore casts:
    ours (CastTy x _) y = ours x y
    ours x (CastTy y _) = ours x y
    -- We assume they know what to do with a coercion:
    ours x y@CoercionTy {} = theirs x y
    ours x@CoercionTy {} y = theirs x y
    -- We only let functions unify with functions, and apps unify with apps:
    ours (FunTy x y) (FunTy x' y') = ours x x' && ours y y'
    ours (AppTy f x) (AppTy f' x') = ours f f' && ours x x'
    -- We let them deal with this:
    ours x@TyConApp {} y@TyConApp {} = theirs x y
    -- These three should unify, so we let them deal with it:
    ours x@AppTy {} y@TyConApp {} = theirs x y
    ours y@TyConApp {} x@AppTy {} = theirs x y
    -- The rest SHOULD NOT be allowed to unify, because it's too
    -- general to produce DWIM results:
    ours FunTy {} _ = False
    ours _ FunTy {} = False
    ours AppTy {} _ = False
    ours _ AppTy {} = False
    ours TyConApp {} _ = False
    ours _ TyConApp {} = False

isAny :: DynFlags -> Type -> Bool
isAny df t = showPpr df t == "Any"

data T = T DynFlags Type
instance Show T where
  showsPrec p (T df ty0) =
    case ty0 of
      TyVarTy v ->
        showString "(TyVarTy " . showString (showPpr df v) . showString ")"
      AppTy t1 t2 ->
        showString "(AppTy " .
        showsPrec (p + 1) (T df t1) .
        showString " " . showsPrec (p + 1) (T df t2) . showString ")"
      TyConApp tyCon tys ->
        showString "(TyConApp " .
        showString (showPpr df tyCon) .
        showString " " . showsPrec (p + 1) (map (T df) tys) . showString ")"
      ForAllTy _tyvar ty ->
        showString "(ForAllTy _ " . showsPrec (p + 1) (T df ty) . showString ")"
      FunTy x y ->
        showString "(FunTy " .
        showsPrec p (T df x) .
        showString " " . showsPrec p (T df y) . showString ")"
      LitTy litTy ->
        showString "(LitTy " . showString (showPpr df litTy) . showString ")"
      CastTy ty _k ->
        showString "(CastTy " . showsPrec (p + 1) (T df ty) . showString " _)"
      CoercionTy _ -> showString "(Coercion _)"

-- | Strip out weird things from GHC's type system.
normalize :: DynFlags -> Type -> Type
normalize df t0 = evalState (go t0) 1
  where
    go =
      \case
        t@TyConApp {}
          | isAny df t -> do
            u <- get
            modify (+ 1)
            pure (makeTypeVariable u "was_Any")
        FunTy (TyConApp (ghc_tyConFlavour -> "class") _) x -> go x
        ForAllTy _ x -> go x
        CastTy x _ -> go x
        FunTy x y -> FunTy <$> (go x) <*> (go y)
        AppTy x y -> AppTy <$> (go x) <*> (go y)
        TyConApp tycon xs -> TyConApp <$> pure tycon <*> (mapM go xs)
        t@TyVarTy {} -> pure t
        t@LitTy {} -> pure t
        t@CoercionTy {} -> pure t

-- | Make a type variable. I have no idea how to create a truly unique
-- name. This is bothersome.
makeTypeVariable :: Int -> String -> Type
makeTypeVariable u n = TyVarTy (mkTyVar name liftedTypeKind)
  where
    name =
      mkInternalName (mkUnique 'Z' u) (mkOccName OccName.varName n) noSrcSpan

-- | Try to fill a hole with the given expression; if it type-checks,
-- we return the newly updated parse tree. Otherwise, we return Nothing.
tryWellTypedFill ::
     GhcMonad m
  => ParsedModule
  -> Hole
  -> HsExpr StageReaderRdrName
  -> m (Maybe ParsedModule)
tryWellTypedFill pm hole expr =
  handleSourceError
    (const (pure Nothing))
    (fmap
       (Just . tm_parsed_module)
       (typecheckModuleNoDeferring (fillHole pm hole expr)))

--------------------------------------------------------------------------------
-- Filling holes in the AST

-- | Fill the given hole in the module with the given expression.
fillHole :: ParsedModule -> Hole -> HsExpr StageReaderRdrName -> ParsedModule
fillHole pm hole expr =
  pm {pm_parsed_source = everywhere (mkT replace) (pm_parsed_source pm)}
  where
    replace :: LHsExpr StageReaderRdrName -> LHsExpr StageReaderRdrName
    replace =
      (\case
         L someSpan _
           | Just realSrcSpan <- getRealSrcSpan someSpan
           , realSrcSpan == holeRealSrcSpan hole -> L someSpan expr
         e -> e)

--------------------------------------------------------------------------------
-- Helpers

rdrNameToLHsExpr :: RdrName -> GenLocated SrcSpan (HsExpr StageReaderRdrName)
rdrNameToLHsExpr rdrname =
  L (UnhelpfulSpan (mkFastString "Generated by rdrNameToLHsExpr"))
    (HsVar
       (L (UnhelpfulSpan (mkFastString "Generated by getWellTypedFills"))
          rdrname))

rdrNameToHsExpr :: RdrName -> HsExpr StageReaderRdrName
rdrNameToHsExpr rdrname =
  HsVar
    (L (UnhelpfulSpan (mkFastString "Generated by rdrNameToHsExpr")) rdrname)

-- | Type-check the module without deferring type errors, and without
-- logging messages.
typecheckModuleNoDeferring :: GhcMonad m => ParsedModule -> m TypecheckedModule
typecheckModuleNoDeferring parsed = do
  typecheckModule
    parsed
    { GHC.pm_mod_summary =
        (GHC.pm_mod_summary parsed)
        { HscTypes.ms_hspp_opts =
            unSetGeneralFlag'
              Opt_DeferTypeErrors
              (HscTypes.ms_hspp_opts (GHC.pm_mod_summary parsed))
              {log_action = nullLogAction}
        }
    }
  where
    nullLogAction _df _reason _sev _span _style _msgdoc = pure ()

-- | Convert parsed source groups into one bag of binds.
_parsedModuleToBag :: ParsedModule -> Bag (LHsBindLR StageReaderRdrName StageReaderRdrName)
_parsedModuleToBag =
  listToBag . mapMaybe valD . hsmodDecls . unLoc . pm_parsed_source
  where
    valD =
      \case
        L l (ValD hsBind) -> pure (L l hsBind)
        _ -> Nothing

-- | Convert renamed source groups into one bag of binds.
renamedSourceToBag :: RenamedSource -> Bag (LHsBindLR StageReaderName StageReaderName)
renamedSourceToBag (hsGroup, _, _, _) = unHsValBindsLR (hs_valds hsGroup)
  where
    unHsValBindsLR =
      \case
        ValBindsIn binds _ -> binds
        ValBindsOut pairs _ -> unionManyBags (map snd pairs)

-- | Does X span over the point Y?
realSpans :: SrcSpan -> (Int, Int) -> Bool
realSpans x y =
  fromMaybe
    False
    (do _ <- getRealSrcSpan x
        pure (spans x y))

-- | Try to get a real span.
getRealSrcSpan :: SrcSpan  -> Maybe RealSrcSpan
getRealSrcSpan =
  \case
    RealSrcSpan r -> pure r
    _ -> Nothing
