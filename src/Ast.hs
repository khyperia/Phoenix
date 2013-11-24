module Ast where

data PrimopType = PrimAdd
    deriving (Eq, Show)

data Type = TypeAbstraction Integer Type
          | TypeIdentifier Integer
          | TypeConstructor String [Type]
    deriving (Eq)

data Expression = Identifier String
                | Constant Integer
                | Abstraction String Expression
                | Application Expression Expression
                | Let String Expression Expression
                | DataDef String [Integer] [(String, [Type])] Expression
                | Primop PrimopType
    deriving (Eq, Show)

instance Show Type where
    show (TypeAbstraction i abst) = "(forall " ++ show i ++ ". " ++ show abst ++ ")"
    show (TypeIdentifier i) = show i
    show x@(TypeConstructor "->" _) = "(" ++ aggArrow x ++ ")"
        where aggArrow (TypeConstructor "->" [l, r]) = show l ++ " -> " ++ aggArrow r
              aggArrow a = show a
    show (TypeConstructor con []) = con
    show (TypeConstructor con args) = "(" ++ con ++ concatMap ((" " ++) . show) args ++ ")"
