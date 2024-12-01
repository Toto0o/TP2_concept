-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> not (isAscii c)
                                          || c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Type = Terror String        -- Utilisé quand le type n'est pas connu.
          | Tnum                 -- Type des nombres entiers.
          | Tbool                -- Type des booléens.
          | Tfob [Type] Type     -- Type des fobjets.
          deriving (Show, Eq)

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltype Lexp Type      -- Annotation de type.
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.
          | Lfob [(Var, Type)] Lexp -- Construction de fobjet.
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- fonction prise telle quelle dans le corrigé
s2list :: Sexp -> [Sexp]
s2list Snil = []
s2list (Snode se1 ses) = se1 : ses
s2list se = error ("Pas une liste: " ++ showSexp se)

-- **
-- modifiée pour suivre la construction du data
-- on bind le type à la variable pour la verification
svar2lvar :: Sexp -> (Var, Type)
svar2lvar (Snode (Ssym v) [t]) = (v, s2ltype t)
svar2lvar se = error ("Pas un symbole: " ++ showSexp se)

-- ** 
-- détermine le type d'une var : Num & Bool & Fob 
s2ltype :: Sexp -> Type
s2ltype (Ssym "Num") = Tnum
s2ltype (Ssym "Bool") = Tbool
-- type Terror temporaire pour l'enlever apres avec filter
s2ltype (Ssym "->") = Terror "Temporary"
s2ltype (Snode type1 typen) =
    let lf = s2list (Snode type1 typen)
        -- utilise filter sur la liste des types Type (map sur declarations) 
        -- pour enlever les Terror "Temporary" 
        ltype = filter (\t -> case t of 
                                Terror "Temporary" -> False
                                _ -> True) (map s2ltype lf)
    in Tfob (init ltype) (last ltype)

s2ltype se = error (show se ++ " is not a type")


-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp

----- Code du corrigé tp1 ------
s2l (Snum n) = Lnum n

s2l (Ssym s) = Lvar s

s2l (Snode (Ssym ":") [e, t]) =
     Ltype (s2l e) (s2ltype t)

s2l (Snode (Ssym "if") [e1, e2, e3])
  = Ltest (s2l e1) (s2l e2) (s2l e3)

s2l (Snode (Ssym "fob") [args, body])
  = Lfob (map svar2lvar (s2list args)) (s2l body)

s2l (Snode (Ssym "let") [Ssym x, e1, e2])
  = Llet x (s2l e1) (s2l e2)

-- **
----- section modifiée -----
s2l (Snode (Ssym "fix") [decls, body])
  = let sdecl2ldecl :: Sexp -> (Var, Lexp)
        -- declaration de variable
        sdecl2ldecl (Snode (Ssym v) [e]) = (v, s2l e)
        -- declaration typé
        sdecl2ldecl (Snode (Ssym v) [t, e]) = (v, (Ltype (s2l e) (s2ltype t)))
        -- fob non typé
        sdecl2ldecl (Snode (Snode (Ssym v) args) [e])
          = (v, Lfob (map svar2lvar args) (s2l e))
        -- fob typé
        sdecl2ldecl (Snode (Snode (Ssym v) args) [t, e]) =
            (v, Lfob (map svar2lvar args) (Ltype (s2l e) (s2ltype t)))
          ----- fin des modifications -----       
        sdecl2ldecl se = error ("Declation Psil inconnue: " ++ showSexp se)

    in Lfix (map sdecl2ldecl (s2list decls)) (s2l body)

s2l (Snode f args)
  = Lsend (s2l f) (map s2l args)

s2l se = error ("Expression Psil inconnue: " ++ showSexp se)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv Int Dexp -- L'entier indique le nombre d'arguments.

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type Env = [(Var, Type, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = let binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")
           intbin = Tfob [Tnum, Tnum] Tnum
           boolbin = Tfob [Tnum, Tnum] Tbool

       in [("+", intbin,  binop Vnum (+)),
           ("*", intbin,  binop Vnum (*)),
           ("/", intbin,  binop Vnum div),
           ("-", intbin,  binop Vnum (-)),
           ("<", boolbin, binop Vbool (<)),
           (">", boolbin, binop Vbool (>)),
           ("≤", boolbin, binop Vbool (<=)),
           ("≥", boolbin, binop Vbool (>=)),
           ("=", boolbin, binop Vbool (==)),
           ("true",  Tbool, Vbool True),
           ("false", Tbool, Vbool False)]

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

-- Fonction auxiliaire pour vérifier si type est un Terror
isTerror :: Type -> Bool
isTerror (Terror _) = True
isTerror _ = False


type TEnv = [(Var, Type)]

-- `check c Γ e` renvoie le type de `e` dans l'environnement `Γ`.
-- Si `c` est vrai, on fait une vérification complète, alors que s'il
-- est faux, alors on présume que le code est typé correctement et on
-- se contente de chercher le type.
check :: Bool -> TEnv -> Lexp -> Type

check _ _ (Lnum _) = Tnum
check _ _ (Lbool _) = Tbool

-- ** -> tout le reste de la fonction
-- Variable --
-- pour les variables, on retourne le type
-- associé dans l'environnement
check _ tenv (Lvar x) = 
    case lookup x tenv of
        Just t -> t
        Nothing -> Terror  $ "Variable " ++ x ++ " not found"

-- Expression typé --
-- pour les expressions typés, on vérifie que
-- le type déclaré concorde avec l'expression
check True tenv (Ltype e t) =
    let texp = check True tenv e
    in case texp of
        Terror errMsg -> Terror ("Error in annotated expression: " ++ errMsg)
        _ -> if texp == t
             then t
             else Terror ("Expression type (" 
                        ++ show texp ++ 
                        ") does not match expected type (" ++ show t ++ ")")

-- Condition --
-- on verifie que la condition est boolean
-- on verifie que les branches de retour (true / false)
-- ont le même type (qui sera le type de l'expression)
check True tenv (Ltest cond etrue efalse) =
    case check True tenv cond of
        Tbool ->
            let t1 = check True tenv etrue
                t2 = check True tenv efalse
            in if t1 == t2
                then t1
                else Terror ("Branch do not match types -> " 
                ++ show etrue ++ " : " ++ show t1 ++ " and " 
                ++ show efalse ++ " : " ++ show t2)
        _ -> Terror  "Condition is not a boolean"

-- Fonction --
-- extrait le type déclaré des arguments et 
-- ajoutes à l'environnement
-- evalue ensuite l'expression de la fonction
-- renvoit type fonctions
check True tenv (Lfob args e) =
    let targs = map snd args
        tenv' = tenv ++ args
        texp = check True tenv' e
    in Tfob targs texp

-- Appel de fonction --
-- trouve le type de e et verifie que c'est bien une fonction
-- verifie que le type des arguments respectent la déclaration (l'ordre compte)
-- renvoi le type de retour de la fonction (type de l'appel)
-- check True tenv (Lsend e args) =
--     case check True tenv e of
--         Tfob targs treturn ->
--             let actualTypes = map (check True tenv) args
--                 matches = foldr (\(expected, actual) acc -> (expected == actual) && acc)
--                                 True
--                                 (zip targs actualTypes)
--             in if matches
--                 then treturn
--                 else Terror $ "Arguments types " ++ show actualTypes ++ " do not match expected types"
--         v -> Terror $ show v ++ " is not a function"

check True tenv (Lsend e args) =
    case check True tenv e of
        Tfob targs treturn ->
            let actualTypes = map (check True tenv) args
            in case () of
                _ | any isTerror actualTypes ->
                      Terror ("Error in function arguments: " 
                            ++ show actualTypes)
                  | length targs /= length actualTypes ->
                      Terror ("Incorrect number of arguments. Expected " 
                            ++ show (length targs) ++
                            " but got " ++ show (length actualTypes))
                  | all (uncurry (==)) (zip targs actualTypes) ->
                      treturn
                  | otherwise ->
                      Terror ("Arguments types " 
                            ++ show actualTypes ++ 
                            " do not match expected types " ++ show targs)
        v -> Terror $ show v ++ " is not a function"


-- Déclaration locale simple --
-- bind le type de e1 et x dans l'environnement
-- renvoi le type de e2
check True tenv (Llet var e1 e2) = 
    let tvar = check True tenv e1
    in check True ((var, tvar) : tenv) e2

-- Déclarations fix --
-- Lfix sera toujours appelé avec True
-- première étape de guessing des types des déclarations
-- ajouter à l'environnement temporaire et verifier avec True les types
-- trouver le type de l'expression de fix avec les types des déclarations 
-- vérifiés
check _ tenv (Lfix decl body) =
    let 
        guessedTypes = map (\(vars, e) -> (vars, check False tenv e)) decl
        tenv' = guessedTypes ++ map (\(var, e) -> (var, check True (guessedTypes ++ tenv) e)) decl 
    in check True (tenv ++ tenv') body
-- ERREUR DETECTEE APRES DEBUGGING : in check True tenv' body

-- Seulement pour fix --
-- assume que les expressions sont bien typés --
check False _ (Ltype _ t) = t
check False tenv (Ltest _ etrue _) =
    check False tenv etrue
check False tenv (Lfob args e) =
    Tfob (map snd args) (check False (args ++ tenv) e)
check False tenv (Lsend e _) =
    case check False tenv e of
        Tfob targs treturn -> Tfob targs treturn
        _ -> Terror "Not a function"
check False tenv (Llet _ _ e2) = check False tenv e2 


---------------------------------------------------------------------------
-- Pré-évaluation
---------------------------------------------------------------------------

-- Dexp simplifie le code en éliminant deux aspects qui ne sont plus
-- utiles lors de l'évaluation:
-- - Les annotations de types.
-- - Les noms de variables, remplacés par des entiers qui représentent
--   la position de la variable dans l'environnement.  On appelle ces entiers
--   des [indexes de De Bruijn](https://en.wikipedia.org/wiki/De_Bruijn_index).

type VarIndex = Int

data Dexp = Dnum Int             -- Constante entière.
          | Dbool Bool           -- Constante Booléenne.
          | Dvar VarIndex        -- Référence à une variable.
          | Dtest Dexp Dexp Dexp -- Expression conditionelle.
          | Dfob Int Dexp        -- Construction de fobjet de N arguments.
          | Dsend Dexp [Dexp]    -- Appel de fobjet.
          | Dlet Dexp Dexp       -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Dfix [Dexp] Dexp
          deriving (Show, Eq)

-- Renvoie le "De Buijn index" de la variable, i.e. sa position
-- dans le contexte.
lookupDI :: TEnv -> Var -> Int -> Int
lookupDI ((x1, _) : xs) x2 n = if x1 == x2 then n else lookupDI xs x2 (n + 1)
lookupDI env x _ = error ("Variable inconnue: " ++ show x ++ " in env :" ++ show env)

-- Conversion de Lexp en Dexp.
-- Les types contenus dans le "TEnv" ne sont en fait pas utilisés.
l2d :: TEnv -> Lexp -> Dexp
l2d _ (Lnum n) = Dnum n
l2d _ (Lbool b) = Dbool b
l2d tenv (Lvar v) = Dvar (lookupDI tenv v 0)

-- ** -> tout le reste de la fonction; ressemble pas mal au tp1, tres facile a faire car 
-- la structure data Dexp facilite la manipulation des données
-- Expression typé --
l2d tenv (Ltype e _) =
    l2d tenv e

-- conditions / test
l2d tenv (Ltest cond etrue efalse) =
    let dcond = l2d tenv cond
        detrue = l2d tenv etrue
        defalse = l2d tenv efalse
    in Dtest dcond detrue defalse

-- Fonction 
-- calcule le nombre d'argument
l2d tenv (Lfob args body) =
    let n = length args
        dbody = l2d (args ++ tenv) body
    in Dfob n dbody

-- Appel de fonction
l2d tenv (Lsend f args) =
    let
        df = l2d tenv f
        dargs = map (l2d tenv) args
    in Dsend df dargs

-- Déclaration locale simple --
l2d tenv (Llet x e1 e2) =
    let
        d1 = l2d tenv e1
        d2 = l2d ((x, check True tenv e1) : tenv) e2
    in Dlet d1 d2

-- Déclaration fix --
-- ajoute les déclaration avec des types temporaire dans l'environnement
-- pour permettre la récursion mutuelle; 
l2d tenv (Lfix decl body) =
    let 
        -- Ajout de type temporaire pour permettre la recursion mutuelle
        -- les variables sont ajoute a l'environnement avec un type Terror "Temporaire"
        tenv' = map (\(var,_) -> (var, Terror "Temporary")) decl ++ tenv
        ddecl = map (l2d tenv' . snd) decl
        dbody = l2d tenv' body
    in Dfix ddecl dbody

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

type VEnv = [Value]

eval :: VEnv -> Dexp -> Value
eval _   (Dnum n) = Vnum n
eval _   (Dbool b) = Vbool b

-- ** Ressemble beaucoup au tp1, quelques détails près comme
-- les références aux variables avev l'index de De Brujin au
-- lieu de leur nom!
-- Variables --
-- Renvoi la valeur de l'environnement à l'indice donné
eval env (Dvar i) = 
    -- equivalent a "element = list[i]"
    env !! i

-- Condition --
eval env (Dtest cond etrue efalse) =
    case eval env cond of
        Vbool True -> eval env etrue
        Vbool False -> eval env efalse
        _ -> error "Not a boolean"

-- Fonction --
eval env (Dfob n body) =
    Vfob env n body

-- ** -> seul changement est dans le nombre d'argument 
-- au lieu des noms de variables; permet la verification
-- que la fonction est appele avec le bon nombre d'arg!
-- Appel de gonction --
-- fonction builtin (binaire) ou custom (fob)
eval env (Dsend body args) =
    let fun = eval env body
        vargs = map (eval env) args
    in
    case fun of 
        Vbuiltin f -> f vargs
        Vfob fenv n fbody -> 
            if n == length vargs
            then eval (vargs ++ fenv) fbody
            else error "Number of args don't match"
        _ -> error "Not a function"

-- Déclaration locale simple --
eval env (Dlet e1 e2) =
    let env' = eval env e1 : env
    in eval env' e2

-- Déclaration fix --
-- même principe qu'au TP1; on retarde l'evaluation des arguments
-- pour éviter une erreur de variables non déclarées
eval env (Dfix decl body) =
    let env' = map (\e -> eval env' e) decl ++ env
    in eval env' body

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

tenv0 :: TEnv
tenv0 = map (\(x,t,_v) -> (x,t)) env0

venv0 :: VEnv
venv0 = map (\(_x,_t,v) -> v) env0

-- Évaluation sans vérification de types.
evalSexp :: Sexp -> Value
evalSexp = eval venv0 . l2d tenv0 . s2l

checkSexp :: Sexp -> Type
checkSexp = check True tenv0 . s2l

tevalSexp :: Sexp -> Either (Type, Value) String
tevalSexp se = let le = s2l se
               in case check True tenv0 le of
                    Terror err -> Right err
                    t -> Left (t, eval venv0 (l2d tenv0 le))

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map tevalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
