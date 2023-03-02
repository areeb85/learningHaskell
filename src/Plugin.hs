module Plugin (plugin) where
import GHC.Plugins  hiding (substExpr, substTy, substCo, substTickish, substBndr, substBind, substBndrs, substRecBndrs)
import Prelude hiding (intercalate,map)
import Data.List 
import Data.Monoid (Alt, Alt)
import GHC.Core.SimpleOpt 
import GHC.Core.FVs
import GHC
import GHC.Core
import Data.String
import GHC.Core.Utils (exprType)
--import GhcPlugins
import Data.Text hiding (intercalate,map, elem, zip, find, null)
import System.Console.Isocline
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Class (lift)
--import System.Console.Haskeline
import Control.Monad.Catch
import Generics.SYB
import GHC.Core.Opt.OccurAnal
import GHC.Core.Opt.Specialise
import GHC.Core.Rules
import GHC.Builtin.PrimOps
import GHC.Core.Tidy
import qualified Data.Set as Set
--import Linenoise


import GHC.Core.Unfold



--import Data.Maybe (fromMaybe)
--import qualified Data.Map.Strict as Map
--import GHC.Core hiding (Expr)
--import GHC.Types.Unique
--import GHC.Plugins (Subst)

--import qualified Data.List as L
--
--import qualified Data.Text as T 


import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Writer (runWriterT)
import Text.PrettyPrint.HughesPJ (Doc, render)
import GHC.IO.Unsafe (unsafePerformIO)


primOpIds :: Set.Set Id
primOpIds = Set.fromList (map primOpWrapperId allThePrimOps)


foldInts :: CoreExpr -> CoreM CoreExpr
foldInts ((Var plus) `App` _ `App` _ `App` (App i1 (Lit (LitNumber lnt n))) `App` (App i2 (Lit (LitNumber _ m))))
  -- | plus == && i1 == && i2 ==
  = do
    dflags <- getDynFlags
    liftIO $ putStrLn $ show (isGlobalId plus)
    liftIO $ putStrLn $ show $ (moduleNameString (moduleName (nameModule (varName plus)))) == "GHC.Num"
    liftIO $ putStrLn $ show $ (nameOccName (varName plus)) == mkVarOcc "+"
    liftIO $ putStrLn $ showSDoc dflags $ ppr (idDetails plus)
    liftIO $ putStrLn $ showSDocDebug dflags $ ppr (nameModule (varName plus))
    liftIO $ putStrLn $ showSDocDebug dflags $ ppr (moduleUnit (nameModule (varName plus)))
    liftIO $ putStrLn $ showSDocDebug dflags $ ppr (moduleName (nameModule (varName plus)))
    liftIO $ putStrLn $ showSDocDebug dflags $ ppr (occNameFS (nameOccName (varName plus)))
    liftIO $ putStrLn $ showSDocDebug dflags $ ppr (isExternalName (varName plus))
    liftIO $ putStrLn $ showSDoc dflags $ ppr (varType plus)
    liftIO $ putStrLn $ showSDoc dflags $ ppr (idInfo plus)
--    liftIO $ putStrLn (gshow (isGlobalId plus, idDetails plus, varName plus, varType plus, idInfo plus))
    return (App i1 (Lit (LitNumber lnt (n + m))))
foldInts x = return x

examplePass :: ModGuts -> CoreM ModGuts
examplePass guts = do
  dflags <- getDynFlags
  loop guts dflags
  return guts

loop :: ModGuts -> DynFlags -> CoreM ()
loop guts dflags = do
  liftIO $ putStrLn "line"
  minput <- liftIO $ readlineMaybe "Haskeline"
  case minput of
    Nothing  -> return ()
    Just "quit" -> return ()
    Just "print" -> do
      let binds = mg_binds guts
      let output = intercalate "\n\n" (map (showSDoc dflags . ppr ) binds)
      liftIO $ putStrLn output
      loop guts dflags
    Just "foldInts" -> do
      mg_binds' <- everywhereM (mkM foldInts) (mg_binds guts)
      loop (guts { mg_binds = mg_binds' }) dflags
    Just "substitute" -> do
      let mg_binds' = everywhere (mkT letSubstTrivial) (mg_binds guts)
      let mg_binds'' = everywhere (mkT letrecSubstTrivialR) mg_binds'
      loop (guts { mg_binds = mg_binds'' }) dflags
    Just "simpleOpt" -> do
      let unfoldingOpts =  updateCreationThreshold maxBound defaultUnfoldingOpts
      let (mg_binds', rules', _) = GHC.Core.SimpleOpt.simpleOptPgm (GHC.Core.SimpleOpt.defaultSimpleOpts {so_uf_opts = unfoldingOpts}) (mg_module guts) (mg_binds guts) (mg_rules guts)
--      liftIO $putStrLn (showSDoc dflags (ppr mg_binds'))
--      liftIO $ putStrLn "----------------------------"
--      liftIO $ putStrLn (showSDoc dflags (ppr rules'))
--      liftIO $ putStrLn "----------------------------"
--      liftIO $ putStrLn (showSDoc dflags  (ppr core_prgs))
      loop (guts {mg_binds = mg_binds', mg_rules = rules'}) dflags
    Just "occurence" -> do 
      let mg_binds' = occurAnalysePgm (mg_module guts) (\_ -> True) (\_ -> False) (mg_rules guts) (mg_binds guts)
      loop (guts {mg_binds = mg_binds'}) dflags
    Just "specialize" -> do
      guts' <- specProgram guts
      loop guts' dflags
    Just "printRules" -> do
      liftIO $ putStrLn (showSDoc dflags (ppr (mg_rules  guts)))
      liftIO $ putStrLn (showSDoc dflags (ppr (rulesOfBinds (mg_binds guts))))
      loop guts dflags
    Just "collectRules" -> do
      let rules' = rulesOfBinds (mg_binds guts)
      loop (guts {mg_rules = rules' ++ (mg_rules guts) }) dflags
    Just "showUnfolding" -> do
--      liftIO $ putStrLn $ (showPpr dflags allThePrimOps)
      everywhereM (mkM (showUnfolding dflags)) (mg_binds guts)
      loop guts dflags
    Just "tidy" -> do
      let go (NonRec b e) = NonRec b (tidyExpr emptyTidyEnv e)
          go (Rec bes) = Rec [(b, tidyExpr emptyTidyEnv e) | (b, e) <- bes]
      let binds = map go (mg_binds guts)
      loop (guts { mg_binds = binds }) dflags
    Just "force" -> do
      let go (NonRec b e)
           | "foo2" == occNameString (nameOccName (idName b)) = NonRec b (force dflags False e)
          go e = e
      let binds = map go (mg_binds guts)
      loop (guts {mg_binds = binds}) dflags
    Just "printUnfolding" -> do
      everywhereM (mkM (printUnfoldings)) (mg_binds guts)
      loop guts dflags
    Just "printVarUnfolding" -> do
      everywhereM (mkM (printVarUnfoldings)) (mg_binds guts)
      loop guts dflags
    Just unknown -> do
      liftIO $ putStrLn ("unknown command <" ++ unknown ++ ">")
      loop guts dflags

printIdUnfoldings b = do
  dflags <- getDynFlags
  liftIO $ putStrLn $ showPpr dflags b ++ " = " ++ showPpr (gopt_unset dflags Opt_SuppressUnfoldings) (realIdUnfolding b)

printUnfoldings (NonRec b e) = do
  printIdUnfoldings b
  return (NonRec b e)
printUnfoldings (Rec bes) = do
  dflags <- getDynFlags
  mapM printIdUnfoldings $ map fst bes
  return (Rec bes)
printUnfoldings e = return e

printVarUnfoldings :: CoreExpr -> CoreM CoreExpr
printVarUnfoldings e@(Var i) = do
  printIdUnfoldings i
  return e
printVarUnfoldings e = return e

--updateUnfoldings :: CoreBinds -> CoreBinds
--undateUnfoldings (NonRec b e) = NonRec b' e where
--  b' = setIdUnfolding b unfolding
--  unfolding = case realIdUnfolding of
--
--    mkUnfolding unfoldingOpts InlineRhs topLevel False e
--  oldUnfolding = realIdUnfolding id




force :: DynFlags -> Bool -> CoreExpr -> CoreExpr
force dflags deep expr = unsafePerformIO $ do
-- putStrLn "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
-- putStrLn $ showPpr dflags expr
-- putStrLn ""
 return $ case expr of
  Var i -> unsafePerformIO $ do
--    putStrLn $ showPpr dflags i ++ " = " ++ showPpr dflags (getUnfolding i)
    return (getUnfolding i)
  Lit _ -> expr
  App fun arg -> App (force dflags deep  fun) (if deep || isPrimOpApp fun then force dflags True arg else arg)
  Lam _ _ -> expr
  Let bind expr -> Let bind (force dflags deep expr)
  Case expr id ty alts -> Case (force dflags deep expr) id ty alts
  Cast expr coerce -> Cast (force dflags deep expr) coerce
  Tick tickish expr -> Tick tickish (force dflags deep expr)
  Type ty -> expr
  Coercion coerce -> expr


isPrimOpApp :: CoreExpr -> Bool
isPrimOpApp expr = case expr of
  Var i ->  Set.member i primOpIds
  App fun _ -> (isPrimOpApp fun)
  _ -> False


      


showUnfolding :: DynFlags -> CoreExpr -> CoreM CoreExpr
showUnfolding dflags (Var i) = do
  liftIO $ putStrLn $ (showPpr dflags i) ++ " = " ++ (showPpr dflags (getUnfolding  i))
  return (Var i)
showUnfolding _ e = return e



--specProgram' :: ModGuts -> CoreM ModGuts
--specProgram' guts@(ModGuts { mg_module = this_mod
--                          , mg_rules = local_rules
--                          , mg_binds = binds })
--  = do { dflags <- getDynFlags
--
--              -- We need to start with a Subst that knows all the things
--              -- that are in scope, so that the substitution engine doesn't
--              -- accidentally re-use a unique that's already in use
--              -- Easiest thing is to do it all at once, as if all the top-level
--              -- decls were mutually recursive
--       ; let top_env = SE { se_subst = Core.mkEmptySubst $ mkInScopeSet $ mkVarSet $
--                                       bindersOfBinds binds
--                          , se_interesting = emptyVarSet
--                          , se_module = this_mod
--                          , se_dflags = dflags }
--
--             go []           = return ([], emptyUDs)
--             go (bind:binds) = do (binds', uds) <- go binds
--                                  (bind', uds') <- specBind top_env bind uds
--                                  return (bind' ++ binds', uds')
--
--             -- Specialise the bindings of this module
--       ; (binds', uds) <- runSpecM (go binds)
--
--       ; (spec_rules, spec_binds) <- specImports top_env local_rules uds
--
--       ; return (guts { mg_binds = spec_binds ++ binds'
--                      , mg_rules = spec_rules ++ local_rules }) }




getUnfolding :: Id -> CoreExpr
getUnfolding i =
  case unfoldingInfo (idInfo i) of
    CoreUnfolding { uf_tmpl = uft } -> uft
    dunf@(DFunUnfolding {})         -> mkCoreLams (df_bndrs dunf) $ mkCoreConApps (df_con dunf) (df_args dunf)
    _                               -> Var i

letSubstTrivial :: CoreExpr -> CoreExpr
letSubstTrivial expr = case expr of
  Let (NonRec b be@(Var _)) e -> substExprSC (extendSubst emptySubst b be) e
  Let (NonRec b be@(Lit _)) e -> substExprSC (extendSubst emptySubst b be) e
  _                           -> expr
    



letrecSubstTrivialR :: CoreExpr -> CoreExpr
letrecSubstTrivialR = everywhere (mkT subst)
  where
    subst (Let (Rec bs) e)
      | Just (b, be, bs', e') <- findTrivial e [] bs =
          let bs'' = map (substExprSC (extendSubst emptySubst b be) . snd) bs'
              e'' = substExprSC (extendSubst emptySubst b be) e'
          in Let (Rec (zip (map fst bs') bs'')) e''
    subst expr = expr

findTrivial _ _ [] = Nothing
findTrivial e bs' ((b,be) : bs) =
  case be of
    Var _ -> Just (b, be, bs' ++ bs, e)
    Lit _ -> Just (b, be, bs' ++ bs, e)
    _ -> findTrivial e (bs'++[(b,be)]) bs



--substCoreExpr :: CoreBndr -> CoreExpr -> CoreExpr -> CoreExpr
--substCoreExpr bndr expr (Var id)
--  | id == bndr = expr
--  | otherwise = Var id
--substCoreExpr bndr expr (Lit lit) = Lit lit
--substCoreExpr bndr expr (App fun arg) =
--  App (substCoreExpr bndr expr fun) (substCoreExpr bndr expr arg)
--substCoreExpr bndr expr (Lam bndr' body)
--  | bndr' == bndr = Lam bndr' body
--  | otherwise =
--    let body' = substCoreExpr bndr expr body
--    in Lam bndr' body'
--substCoreExpr bndr expr (Let bind body) =
--  let bind' = case bind of
--                NonRec bndr' expr' ->
--                  if bndr' == bndr then
--                    NonRec bndr' expr'
--                  else
--                    NonRec bndr' (substCoreExpr bndr expr expr')
--                Rec pairs ->
--                  Rec [(bndr', substCoreExpr bndr expr expr')
--                      | (bndr', expr') <- pairs]
--  in Let bind' (substCoreExpr bndr expr body)
--substCoreExpr bndr expr (Case scrut bndr' ty alts) =
--  let alts' = map (\(con, vars, body') ->
--                      let vars' = if bndr' `elem` vars then vars else bndr' : vars
--                          body'' = substCoreExpr bndr expr body'
--                      in (con, vars', body'')
--                  ) alts
--      scrut' = substCoreExpr bndr expr scrut
--  in Case scrut' bndr' ty alts'
--substCoreExpr bndr expr (Cast e c) =
--  Cast (substCoreExpr bndr expr e) c
--substCoreExpr bndr expr (Tick t e) =
--  Tick t (substCoreExpr bndr expr e)
--substCoreExpr bndr expr (Type ty) = Type ty





plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = installTwo
  }



installTwo :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installTwo _ todo = do
  return (CoreDoPluginPass "Example Pass" examplePass : todo)


---- write recursive function here
--recursivePrinter :: DynFlags -> CoreExpr -> String
--recursivePrinter dflags (Var a) =  showSDoc dflags (ppr a)
--recursivePrinter dflags (Lit a) = showSDoc dflags (ppr a)
--recursivePrinter dflags (App e1 e2) = "( " ++ recursivePrinter dflags e1 ++ "@" ++ recursivePrinter dflags e2 ++ " )"
--recursivePrinter dflags (Lam b e) = "\\( " ++ showSDoc dflags (ppr b)  ++ "-> " ++ recursivePrinter dflags e ++ " )"
--recursivePrinter dflags (Let b e) = " Let " ++ recursiveBindPrinter dflags b ++ " in " ++  recursivePrinter dflags e
--recursivePrinter dflags (Case e b t as ) = "Case values are: " ++ recursivePrinter dflags e ++  " - " ++ showSDoc dflags (ppr b) ++ " - " ++ showSDoc dflags (ppr t) ++ " - " ++ recursiveCasePrinter dflags as
--recursivePrinter dflags (Cast b _) = "Cast values are: " ++ recursivePrinter dflags b 
--recursivePrinter dflags (Tick _ b) = " Tick values are: " ++ recursivePrinter dflags b
--recursivePrinter _ _ = "Not implemented"
--
--
--recursiveCasePrinter :: DynFlags -> [CoreAlt] -> String
--recursiveCasePrinter dflags alts = intercalate ", " (map (\(x,y,z) -> showSDoc dflags (ppr x) ++ " - " ++ showSDoc dflags (ppr y) ++ " - " ++ recursivePrinter dflags z) alts)
--
--
--recursiveBindPrinter :: DynFlags -> CoreBind -> String
--recursiveBindPrinter dflags (NonRec b e) = showSDoc dflags (ppr b)  ++ " = "  ++ recursivePrinter dflags e--(rewriteExpr caseReduceNoDC e)
--recursiveBindPrinter dflags (Rec binds) = intercalate "," (map (\(b,e) -> showSDoc dflags (ppr b) ++ " = " ++ recursivePrinter dflags e) binds)
--recursiveBindPrinter dflags _ = "nothing"

--caseReduceNoDC :: CoreExpr -> CoreExpr -- this rewrite is merely for a boolean data constructor. 
--caseReduceNoDC c@(Case e _ _ as ) = 
--  let in_scope = mkInScopeSet (exprSomeFreeVars isLocalVar e) in 
--  case exprIsConApp_maybe (in_scope, idUnfolding) e of
--    Just (_, [], dc, [], [] ) -> case findAlt (DataAlt dc) as of
--      Nothing -> c
--      Just (_, _, rhs) -> rhs
--    _ -> c
--caseReduceNoDC e = e
   



--rewriteExpr :: (CoreExpr -> CoreExpr) -> CoreExpr -> CoreExpr
--rewriteExpr f e@(Var a) = f e 
--rewriteExpr f e@(Lit a) = f e
--rewriteExpr f e@(App e1 e2) = f (App (rewriteExpr f e1) (rewriteExpr f e2))
--rewriteExpr f e@(Lam b body) = f (Lam b (rewriteExpr f body))
--rewriteExpr f e@(Let b body) = f (Let (rewriteBind f b) (rewriteExpr f body)) 
--rewriteExpr f e@(Case s b t as ) = f (Case (rewriteExpr f s) b t (rewriteAlts f as))
--rewriteExpr f e@(Cast e1 c) = f (Cast (rewriteExpr f e1) c) 
--rewriteExpr f e@(Tick t b) = f (Tick t (rewriteExpr f b))
--rewriteExpr f e = e
  
  
--substExpr :: HasDebugCallStack => Subst -> CoreExpr -> CoreExpr
--   -- HasDebugCallStack so we can track failures in lookupIdSubst
--substExpr subst expr
--  = go expr
--  where
--    go (Var v)         = lookupIdSubst subst v
--    go (Type ty)       = Type (substTy subst ty)
--    go (Coercion co)   = Coercion (substCo subst co)
--    go (Lit lit)       = Lit lit
--    go (App fun arg)   = App (go fun) (go arg)
--    go (Tick tickish e) = mkTick (substTickish subst tickish) (go e)
--    go (Cast e co)     = Cast (go e) (substCo subst co)
--       -- Do not optimise even identity coercions
--       -- Reason: substitution applies to the LHS of RULES, and
--       --         if you "optimise" an identity coercion, you may
--       --         lose a binder. We optimise the LHS of rules at
--       --         construction time
--
--    go (Lam bndr body) = Lam bndr' (go subst' body)
--                       where
--                         (subst', bndr') = substBndr subst bndr
--    -- special case of Let                    
--    go (Let (NonRec bndr (Type rhs)) body) 
--    -- TODO: use Var Type on bndr instead of exprKindorType on rhs
--      | ["*", "BOX"] `inType` exprKindOrType rhs = go subst' body
--        where subst' = extendTySubst subst bndr rhs
--    
--    go (Let bind body) = Let bind' (go subst' body)
--                       where
--                         (subst', bind') = substBind subst bind
--
--    go (Case scrut bndr ty alts) = Case (go scrut) bndr' (substTy subst ty) (map (go_alt subst') alts)
--                                 where
--                                 (subst', bndr') = substBndr subst bndr
--
--    go_alt subst (Alt con bndrs rhs) = Alt con bndrs' (go subst' rhs)
--                                 where
--                                   (subst', bndrs') = substBndrs subst bndrs
--                              
--
--inType :: [String] -> Type -> Bool
--inType names ty = go ty where
-- go (TyVarTy _) = False
-- go (AppTy t1 t2) = go t1 || go t2
-- go (TyConApp ctor args) =  any (`cmpString2Name` tyConName ctor) names || any (go) args
-- go (FunTy t1 t2) = go t1 || go t2
-- go (ForAllTy _ t) = go t
--
--substCo :: Subst -> Coercion -> Coercion
--substCo _ _ = error "SubstCo not impelemented"
--
--
--substBind :: HasDebugCallStack => Subst -> CoreBind -> (Subst, CoreBind)
--substBind subst (NonRec bndr rhs)
--  = (subst', NonRec bndr' (substExpr subst rhs))
--  where
--    (subst', bndr') = substBndr subst bndr
--
--substBind subst (Rec pairs)
--   = (subst', Rec (bndrs' `zip` rhss'))
--   where
--       (bndrs, rhss)    = unzip pairs
--       (subst', bndrs') = substRecBndrs subst bndrs
--       rhss' = map (substExpr subst') rhss



                   

--rewriteBind :: (CoreExpr -> CoreExpr) -> CoreBind -> CoreBind
--rewriteBind f (NonRec b e) = NonRec b (f e)
--rewriteBind f (Rec binds) = Rec (map (\(x, e) -> (x, (f e))) binds) 

--rewriteAlts :: (CoreExpr -> CoreExpr) -> [CoreAlt] -> [CoreAlt]
--rewriteAlts f alts = map (\(dc, args, rhs) -> (dc, args, rewriteExpr f rhs)) alts

pass :: ModGuts -> CoreM ModGuts
pass guts = do dflags <- getDynFlags
               --let dflagsPrime = dopt_set dflags Opt_D_ppr_debug
               bindsOnlyPass (mapM (printBind dflags)) guts   
  where printBind :: DynFlags -> CoreBind -> CoreM CoreBind
        printBind dflags bndr =
          do
          -- call the recursive function here to print the values of underscore.
--          putMsgS $ "binding named " ++  recursiveBindPrinter dflags bndr
          return bndr
        --printBind _ bndr = return bndr

