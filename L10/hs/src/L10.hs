module L01 where

import Container
import YesNo

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

instance YesNo Maybe where
    yesno Nothing = False
    yesno _ = True

{- E 10.10.2 Implement the Container type class for a binary tree data type, which is defined as:
data Tree a = Nil | Node (Tree a) a (Tree a) .-}

data Tree a = Nil | Node (Tree a) a (Tree a)

instance Container Tree where
    hasElem _ Nil = False
    hasElem Nil _ = False
    hasElem t e = 
        case t of
            Nil -> False
            Node left us right -> if us == e then True 
                        else if any (True ==) (hasElem left e) then True
                        else any (True ==) (hasElem right e)

        

