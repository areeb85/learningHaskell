module Plugin (plugin) where
import GHC.Plugins  hiding (substExpr, substTy, substCo, substTickish, substBndr, substBind, substBndrs, substRecBndrs)
import Data.List (intercalate)
import Data.Monoid (Alt, Alt)
import GHC.Core.SimpleOpt
import GHC.Core.FVs
import GHC 
import GHC.Core

import GHC.Core.Utils (exprType)
--import GhcPlugins
import Data.Text
--import System.Console.Isocline


import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Class (lift)
--import System.Console.Haskeline
import Control.Monad.Catch

import Linenoise


-- A new pass that runs the haskeline prompt
--examplePass :: ModGuts -> CoreM ModGuts
--examplePass guts = do
--  lift $ runInputT defaultSettings loop
--  return guts
--  where
--    loop :: InputT CoreM ModGuts
--    loop = do
--      minput <- getInputLine "Haskeline> "
--      case minput of
--        Nothing -> return guts
--        Just "quit" -> return guts
--        Just input -> do
--          dflags <- lift getDynFlags
--          lift $ bindsOnlyPass (mapM (printBind dflags)) guts
--          loop
--          --loop
--        where printBind :: DynFlags -> CoreBind -> CoreM CoreBind
--              printBind dflags bndr = do
--              putMsgS $ "binding named " ++  recursiveBindPrinter dflags bndr
--              return bndr

--printBindingsPass :: ModGuts -> CoreM ModGuts
--printBindingsPass guts = do
--  liftIO $ runInputT defaultSettings loop
--  return guts
--  where
--    loop :: InputT CoreM ()
--    loop = do
--      minput <- liftIO $ runInputT defaultSettings (getInputLine "Haskeline> ")--liftIO $ getInputLine "Enter a command (or 'quit' to exit)> "
--      case minput of
--        Nothing -> return ()
--        Just "quit" -> return ()
--        Just _ -> do
--          dflags <- getDynFlags
--          let binds = mg_binds guts
--          let exprs = [b | NonRec b _ <- binds]
--          --let pprExpr expr = pprExprWithFlags dflags expr
----          mapM_ (putStrLn . showSDoc dflags . pprExpr) exprs
--          mapM_ (\bndr -> liftIO $ putStrLn $ "binding named " ++ recursiveBindPrinter dflags bndr) (mg_binds guts)
--          loop

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Writer (runWriterT)
import Text.PrettyPrint.HughesPJ (Doc, render)
--import GHC.Core.Utils (exprType)
--import GHC.Plugins (exprType)
--import GHC.Core.Type (exprType)
--import GHC.Core.TyCo.Rep (exprType)

examplePass :: ModGuts -> CoreM ModGuts
examplePass guts = do
  dflags <- getDynFlags
  loop guts dflags
  return guts


loop :: ModGuts -> DynFlags -> CoreM ()
loop guts dflags = do
  liftIO $ putStrLn "line"
  minput <- liftIO $ getInputLine (fromString "Haskeline> ")
  case minput of
    "quit" -> return ()
    "print" -> do
      let binds = mg_binds guts
      let output = intercalate "\n\n" (map (recursiveBindPrinter dflags) binds)
      liftIO $ putStrLn output
      loop guts dflags
    _ -> do
      liftIO $ putStrLn ("unknown command <" ++ minput ++ ">")
      loop guts dflags


--examplePass :: ModGuts -> CoreM ModGuts
--examplePass guts = do
--  _ <- lift $ runInputT defaultSettings loop
--  return guts
--  where
--    loop :: InputT IO ()
--    loop = do
--      minput <- getInputLine "Haskeline> "
--      case minput of
--        Nothing -> return ()
--        Just "quit" -> return ()
--        Just input -> do
--          dflags <- lift getDynFlags
--          bindsOnlyPassTwo (mapM (printBindTwo dflags)) guts
--          loop
--        where printBind :: DynFlags -> CoreBind -> CoreM CoreBind
--              printBind dflags bndr = do
--                putMsgS $ "binding named " ++  recursiveBindPrinter dflags bndr
--                return bndr
--
--printBindTwo :: DynFlags -> CoreBind -> CoreM CoreBind
--printBindTwo dflags bndr = do
--  putMsgS $ "binding named " ++  recursiveBindPrinter dflags bndr
--  return bndr
--
--bindsOnlyPassTwo :: (CoreBind -> CoreM CoreBind) -> ModGuts -> CoreM ModGuts
--bindsOnlyPassTwo f guts = do
--  binds' <- mapM f (mg_binds guts)
--  return guts { mg_binds = binds' }



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
   
--caseReduceNoDC :: CoreExpr -> CoreExpr 
--caseReduceNoDC c@(Case e b t as ) = c
--caseReduceNoDC e = e


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

