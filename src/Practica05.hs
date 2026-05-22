module Practica05 where

import Terminos

buscaNombre :: Nombre -> Subst -> Maybe Term
buscaNombre nombre [] = Nothing
buscaNombre nombre ((x, t) : rest) = if nombre == x then Just t else buscaNombre nombre rest

-- Aplicar una sustitucion a un termino
apsubT :: Term -> Subst -> Term
apsubT (Var x) sbsts = case buscaNombre x sbsts of
  Just t -> t
  Nothing -> Var x
apsubT (Fun f args) sbsts = Fun f (aplicarLista args sbsts)

-- Funcion auxiliar para aplicar la sustitucion a una lista de terminos
aplicarLista :: [Term] -> Subst -> [Term]
aplicarLista ts sbsts = map (\t -> apsubT t sbsts) ts

-- Funcion que elimina los pares que son de la forma x=x
simpSus :: Subst -> Subst
simpSus sbsts = filter (\(x, t) -> t /= Var x) sbsts

-- Funcion que calcula la composicion de dos sustituciones
compSus :: Subst -> Subst -> Subst
compSus sbsts1 sbsts2 = (map (\(x, t) -> (x, apsubT t sbsts2)) sbsts1) ++ clean_sbsts2
  where
    nombres_sustituidos = map fst sbsts1
    clean_sbsts2 = filter (\(y, _) -> y `notElem` nombres_sustituidos) sbsts2

-- Funcion que devuelve un umg de dos terminos, si es que lo hay
unifica :: Term -> Term -> [Subst]
unifica = undefined

-- Funcion que devuelve un unificador de dos términos funcionales, si es que lo hay
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas = undefined

-- Funcion que devuelve un umg de una lista de termino, si es que lo hay
unificaConj :: [Term] -> [Subst]
unificaConj = undefined
