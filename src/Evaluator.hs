module Evaluator (evaluate) where

import qualified Data.Map.Lazy as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Applicative
import Ast

data Object
    = ObjInteger Integer
    | ObjFunction (Object -> Object)
    | ObjObject Int [Object]

instance Show Object where
    show (ObjInteger n) = show n
    show (ObjFunction _) = "[function]"
    show (ObjObject n o) = show n ++ "{" ++ concatMap ((' ' :) . show) o ++ " }"

imap :: Num n => (n -> t -> a) -> [t] -> [a]
imap f = imapImpl 0
  where imapImpl n (x:xs) = f n x : imapImpl (n + 1) xs
        imapImpl _ [] = []

eval :: Expression -> Reader (Map.Map String Object) Object
eval (Identifier name) = asks (fromMaybe (error $ "Iden " ++ name ++ " not found") . Map.lookup name)
eval (Constant c) = return $ ObjInteger c
eval (Application fun val) = (\(ObjFunction f) -> f) <$> eval fun <*> eval val
eval (Primop PrimAdd) = return $ ObjFunction (\(ObjInteger l) -> ObjFunction (\(ObjInteger r) -> ObjInteger $ l + r))
eval (Abstraction var val) = do
    closure <- ask
    return $ ObjFunction (\v -> runReader (eval val) (Map.insert var v closure))
eval (Let var val expr) = do
    realValue <- mfix (\valObj -> local (Map.insert var valObj) (eval val))
    local (Map.insert var realValue) (eval expr)
eval (DataDef typeCon _ dataCons expr) = do
    let constructors = imap constructorFunc (map snd dataCons)
        conMap = Map.fromList (zip (map fst dataCons) constructors)
        finalMap = Map.insert eliminatorName eliminatorFunc conMap
    local (Map.union finalMap) (eval expr)
  where eliminatorName = '_' : typeCon
        eliminatorFunc = foldFunc createEliminator dataCons []
        constructorFunc index value = foldFunc (createConstructor index) value []
        foldFunc f [] values = f (reverse values)
        foldFunc f (_:xs) values = ObjFunction (\value -> foldFunc f xs (value:values))
        createEliminator destructorFunctions = ObjFunction (\(ObjObject index params) -> foldl (\(ObjFunction f) -> f) (destructorFunctions !! index) params)
        createConstructor = ObjObject

evaluate :: Expression -> String
evaluate = show . flip runReader Map.empty . eval
