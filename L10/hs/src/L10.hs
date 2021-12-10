module L01 where

import Container
import YesNo
import RevSort

{-
    Q 10.8.1
    > data Stuff = Stuff
    > *

    > data Option a = None | Some a
    > * -> * 

    >
    >

    >
    >
-}

{- E 10.9.1. Given the following data definition: data Op = Add | Sub | Mul | Div , implement Show
such that the corresponding mathematical operator is shown for each instance (i.e. + for
Add , − for Sub , etc). -}

data Op = Add | Sub | Mul | Div
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

{- E 10.9.2 Implement the YesNo type class for the Maybe type.-}

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _ = True

{- E 10.9.3 Implement the Container type class for a binary tree data type, which is defined as:
data Tree a = Nil | Node (Tree a) a (Tree a) .-}

data Tree a = Nil | Node (Tree a) a (Tree a)


instance Container Tree where
    hasElem Nil _ = False
    hasElem t e = 
        case t of
            Nil -> False
            Node left us right -> if us == e then True 
                        else (hasElem left e) || (hasElem right e)

    nrElems t =
        case t of
            Nil -> 0
            Node left us right -> 1 + (nrElems right) + (nrElems left)
    

{- E 10.9.4 Given the newtype Rev , defined as newtype Rev a = Rev a deriving (Eq, Show) im-
plement Ord such that the sortRev :: (Ord a) => [a] -> [a] function, defined as
sortRev = sortOn Rev will sort the list in reverse.-}

instance Ord (Rev a) where
    compare x y = 
        case x of
            Rev a ->
                case y of
                    Rev b -> if a >= b then LT 
                        else if a <= b then GT
                        else EQ 