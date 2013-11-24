module Typechecker (typecheck) where

import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Arrow
import Data.List
import Data.Maybe
import Ast

type TI = State Integer

type Subst = Map.Map Integer Type

newtype Context = Context (Map.Map String Type)

class Substitutable a where
    apply :: Subst -> a -> a

class FreeContainer a where
    free :: a -> Set.Set Integer

nullSubst :: Subst
nullSubst = Map.empty

genTypeId :: TI Type
genTypeId = do
    st <- get
    put (st + 1)
    return $ TypeIdentifier st

insertCtx :: String -> Type -> Context -> Context
insertCtx name ty (Context con) = Context $ Map.insert name ty con

insertManyCtx :: [(String, Type)] -> Context -> Context
insertManyCtx array (Context con) = Context $ foldl' (flip $ uncurry Map.insert) con array

removeCtx :: String -> Context -> Context
removeCtx name (Context con) = Context $ Map.delete name con

instance FreeContainer Type where
    free (TypeAbstraction name abst) = Set.delete name (free abst)
    free (TypeIdentifier iden) = Set.singleton iden
    free (TypeConstructor _ args) = foldl' Set.union Set.empty (map free args)

instance Substitutable Type where
    apply change x@(TypeIdentifier iden) = fromMaybe x (Map.lookup iden change)
    apply change (TypeAbstraction name ty) = TypeAbstraction name (apply (Map.delete name change) ty)
    apply change (TypeConstructor name args) = TypeConstructor name (map (apply change) args)

instance FreeContainer Context where
    free (Context x) = Set.unions (map free (Map.elems x))

instance Substitutable Context where
    apply change (Context sub) = Context $ Map.map (apply change) sub

inst :: Type -> TI Type
inst (TypeAbstraction name abst) = do
    newId <- genTypeId
    newAbst <- inst abst
    return $ apply (Map.singleton name newId) newAbst
inst x = return x

gen :: Context -> Type -> Type
gen ctx ty = let freeVars = free ty Set.\\ free ctx in
             Set.foldr TypeAbstraction ty freeVars

composeSubst :: Subst -> Subst -> Subst
composeSubst lefts rights = Map.map (apply lefts) rights `Map.union` lefts
--composeSubst s1 s2 = Map.union (Map.foldWithKey (appl s2) s1 s1) s2
--  where appl s v t s' = Map.insert v (apply s t) s'

mgu :: Type -> Type -> Subst
mgu l@(TypeIdentifier leftt) x
    | l == x = nullSubst
    | leftt `Set.member` free x = error $ "Occurs check: Cannot construct the infinite type " ++ show l ++ " == " ++ show x
    | otherwise = Map.singleton leftt x
mgu a x@(TypeIdentifier _) = mgu x a
mgu (TypeConstructor name1 abst1) (TypeConstructor name2 abst2)
    | name1 == name2 && length abst1 == length abst2 = step abst1 abst2
  where step [] [] = nullSubst
        step (x:xs) (y:ys) =
                let s1 = mgu x y
                    s2 = step (map (apply s1) xs) (map (apply s1) ys) in
                (s2 `composeSubst` s1)
        step _ _ = error "Internal error: mgu TypeConstructor"
mgu (TypeAbstraction name1 abst1) (TypeAbstraction name2 abst2) =
    let ty = firstFree 1 (free abst1 `Set.union` free abst2)
        abst1' = apply (Map.singleton name1 ty) abst1
        abst2' = apply (Map.singleton name2 ty) abst2 in
        mgu abst1' abst2'
  where firstFree counter set
            | Set.notMember counter set = TypeIdentifier counter
            | otherwise = firstFree (counter + 1) set
mgu leftt rightt = error $ "Cannot unify types: " ++ show leftt ++ " vs. " ++ show rightt

algW :: Context -> Expression -> TI (Subst, Type)
algW (Context ctx) (Identifier str)
    | str == "#fix" = do
        varty <- genTypeId
        let t = TypeConstructor "->" [TypeConstructor "->" [varty, varty], varty]
        t' <- inst t
        return (nullSubst, t')
    | otherwise = case Map.lookup str ctx of
        Nothing -> error $ "Identifier " ++ str ++ " not bound"
        Just t -> do
            t' <- inst t
            return (nullSubst, t')
algW _ (Constant _) = return (nullSubst, TypeConstructor "Int" [])
algW _ (Primop PrimAdd) = return (nullSubst, TypeConstructor "->" [TypeConstructor "Int" [], TypeConstructor "->" [TypeConstructor "Int" [], TypeConstructor "Int" []]])
algW env (Let name value expr) = do
    (s1, t1) <- algW env (Application (Identifier "#fix") (Abstraction name value))
    --(s1, t1) <- algW env value
    let env' = removeCtx name env
        t' = gen (apply s1 env') t1
        env'' = insertCtx name t' env'
    (s2, t2) <- algW (apply s1 env'') expr
    return (s2 `composeSubst` s1, t2)
algW ctx (Abstraction name expr) = do
    nameTy <- genTypeId
    (subst, exprTy) <- algW (insertCtx name nameTy ctx) expr
    return (subst, TypeConstructor "->" [apply subst nameTy, exprTy])
algW ctx (Application e1 e2) = do
    (subst1, t1) <- algW ctx e1
    (subst2, t2) <- algW (apply subst1 ctx) e2
    retty <- genTypeId
    let subst3 = mgu (apply subst2 t1) (TypeConstructor "->" [t2, retty])
    return ((subst3 `composeSubst` subst2) `composeSubst` subst1, apply subst3 retty)
algW ctx (DataDef typeCon typeArgsOld dataCons expr) = do
    eliminatorType <- genTypeId
    typeArgIds <- mapM (const genTypeId) typeArgsOld
    let typeArgSubst = Map.fromList $ zip typeArgsOld typeArgIds
        typeConType = TypeConstructor typeCon typeArgIds
        constructors = map (second (gen ctx . createConstructor typeConType . map (apply typeArgSubst))) dataCons
        ctx' = insertManyCtx constructors ctx
        eliminator = gen ctx' $ createConstructor (TypeConstructor "->" [typeConType, eliminatorType]) (map (createConstructor eliminatorType . map (apply typeArgSubst) . snd) dataCons)
        ctx'' = insertCtx ((\n -> '_' : n) typeCon) eliminator ctx'
    algW ctx'' expr
  where createConstructor = foldr (\l r -> TypeConstructor "->" [l, r])

typecheck :: Expression -> Type
typecheck expr = let ((subst, ty), _) = flip runState 1 $ algW (Context Map.empty) expr in
                     --gen (Context Map.empty) $
                        apply subst ty
