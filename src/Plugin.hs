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
import Data.Text hiding (intercalate,map)
import System.Console.Isocline
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Class (lift)
--import System.Console.Haskeline
import Control.Monad.Catch
import Generics.SYB
--import Linenoise



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
--    Just "substitute" -> do
--      let mg_binds' = everywhereM (mkM letSubstTrivial) (mg_binds guts)
--      loop (guts { mg_binds = mg_binds' }) dflags
    Just unknown -> do
      liftIO $ putStrLn ("unknown command <" ++ unknown ++ ">")
      loop guts dflags
      
      
--letSubstTrivial :: CoreExpr -> CoreExpr
--letSubstTrivial expr = case expr of
--  Let (NonRec b be@(Var _)) e -> substCoreExpr b be e
--  Let (NonRec b be@(Lit _)) e -> substCoreExpr b be e
--  _                           -> expr
    

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


-- commented the install function here.

--install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
--install _ todo = do
--  return (CoreDoPluginPass "Say name" examplePass : todo)

installTwo :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installTwo _ todo = do
  return (CoreDoPluginPass "Example Pass" examplePass : todo)


-- write recursive function here
recursivePrinter :: DynFlags -> CoreExpr -> String
recursivePrinter dflags (Var a) =  showSDoc dflags (ppr a)
recursivePrinter dflags (Lit a) = showSDoc dflags (ppr a)
recursivePrinter dflags (App e1 e2) = "( " ++ recursivePrinter dflags e1 ++ "@" ++ recursivePrinter dflags e2 ++ " )"
recursivePrinter dflags (Lam b e) = "\\( " ++ showSDoc dflags (ppr b)  ++ "-> " ++ recursivePrinter dflags e ++ " )"
recursivePrinter dflags (Let b e) = " Let " ++ recursiveBindPrinter dflags b ++ " in " ++  recursivePrinter dflags e
recursivePrinter dflags (Case e b t as ) = "Case values are: " ++ recursivePrinter dflags e ++  " - " ++ showSDoc dflags (ppr b) ++ " - " ++ showSDoc dflags (ppr t) ++ " - " ++ recursiveCasePrinter dflags as
recursivePrinter dflags (Cast b _) = "Cast values are: " ++ recursivePrinter dflags b 
recursivePrinter dflags (Tick _ b) = " Tick values are: " ++ recursivePrinter dflags b
recursivePrinter _ _ = "Not implemented"


recursiveCasePrinter :: DynFlags -> [CoreAlt] -> String
recursiveCasePrinter dflags alts = intercalate ", " (map (\(x,y,z) -> showSDoc dflags (ppr x) ++ " - " ++ showSDoc dflags (ppr y) ++ " - " ++ recursivePrinter dflags z) alts)


recursiveBindPrinter :: DynFlags -> CoreBind -> String
recursiveBindPrinter dflags (NonRec b e) = showSDoc dflags (ppr b)  ++ " = "  ++ recursivePrinter dflags e--(rewriteExpr caseReduceNoDC e)
recursiveBindPrinter dflags (Rec binds) = intercalate "," (map (\(b,e) -> showSDoc dflags (ppr b) ++ " = " ++ recursivePrinter dflags e) binds)
recursiveBindPrinter dflags _ = "nothing"

caseReduceNoDC :: CoreExpr -> CoreExpr -- this rewrite is merely for a boolean data constructor. 
caseReduceNoDC c@(Case e _ _ as ) = 
  let in_scope = mkInScopeSet (exprSomeFreeVars isLocalVar e) in 
  case exprIsConApp_maybe (in_scope, idUnfolding) e of
    Just (_, [], dc, [], [] ) -> case findAlt (DataAlt dc) as of
      Nothing -> c
      Just (_, _, rhs) -> rhs
    _ -> c
caseReduceNoDC e = e
   



rewriteExpr :: (CoreExpr -> CoreExpr) -> CoreExpr -> CoreExpr
rewriteExpr f e@(Var a) = f e 
rewriteExpr f e@(Lit a) = f e
rewriteExpr f e@(App e1 e2) = f (App (rewriteExpr f e1) (rewriteExpr f e2))
rewriteExpr f e@(Lam b body) = f (Lam b (rewriteExpr f body))
rewriteExpr f e@(Let b body) = f (Let (rewriteBind f b) (rewriteExpr f body)) 
rewriteExpr f e@(Case s b t as ) = f (Case (rewriteExpr f s) b t (rewriteAlts f as))
rewriteExpr f e@(Cast e1 c) = f (Cast (rewriteExpr f e1) c) 
rewriteExpr f e@(Tick t b) = f (Tick t (rewriteExpr f b))
rewriteExpr f e = e
  
  
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



                   

rewriteBind :: (CoreExpr -> CoreExpr) -> CoreBind -> CoreBind
rewriteBind f (NonRec b e) = NonRec b (f e)
rewriteBind f (Rec binds) = Rec (map (\(x, e) -> (x, (f e))) binds) 

rewriteAlts :: (CoreExpr -> CoreExpr) -> [CoreAlt] -> [CoreAlt]
rewriteAlts f alts = map (\(dc, args, rhs) -> (dc, args, rewriteExpr f rhs)) alts

pass :: ModGuts -> CoreM ModGuts
pass guts = do dflags <- getDynFlags
               --let dflagsPrime = dopt_set dflags Opt_D_ppr_debug
               bindsOnlyPass (mapM (printBind dflags)) guts   
  where printBind :: DynFlags -> CoreBind -> CoreM CoreBind
        printBind dflags bndr =
          do
          -- call the recursive function here to print the values of underscore.
          putMsgS $ "binding named " ++  recursiveBindPrinter dflags bndr
          return bndr
        --printBind _ bndr = return bndr

