{-# LANGUAGE TemplateHaskell, DeriveLift, StandaloneDeriving #-}

module Mooc.Th (testing, testing', timeLimit,
                isDefined, withDefined, hasType, hasType', importsOnly, show',
                reifyType, DataType(..), FieldType(..), Constructor(..),
                withInstance, withInstance1, withInstances1, classContains, defineInstance)
where

import Data.Char
import Data.List
import Data.Maybe
import Language.Haskell.TH hiding (reifyType)
import Language.Haskell.TH.Syntax hiding (reifyType)

-- testing presence of definitions

isDefined :: String -> Q Exp
isDefined s = do
  mn <- lookupValueName s
  let defined = isJust mn
  [|counterexample ("You haven't defined '"++s++"' yet!") defined|]

varOrCon :: Name -> Q Exp
varOrCon n@(Name (OccName s) _)
  | isUpper (head s) = return $ ConE n
  | otherwise = return $ VarE n

withDefined :: String -> Q Exp
withDefined s = do
  mn <- lookupValueName s
  case mn of
    Nothing -> [|(\_ -> counterexample ("You haven't defined '"++s++"' yet!") False)|]
    Just n -> [|(\f -> f $(varOrCon n))|]

getType :: String -> Q (Maybe Type)
getType s = do
  mn <- lookupValueName s
  case mn of Nothing -> return Nothing
             Just n -> do i <- reify n
                          case i of VarI _ typ _ -> return (Just typ)
                                    DataConI _ typ _ -> return (Just typ)
                                    _ -> return Nothing

showType :: Type -> String
showType (ConT name) = nameBase name
showType (AppT (AppT ArrowT arg) typ) = showType arg ++ " -> " ++ showType typ
showType (AppT ListT typ) = "[" ++ showType typ ++ "]"
showType t = show t
-- TODO: more cases as needed

hasType :: String -> Q Type -> Q Exp
hasType s qtyp = do
  mt <- getType s
  typ <- qtyp
  let correct = mt == Just typ
  let expected = showType typ
  let actual = case mt of Nothing -> "nothing"
                          Just t -> showType t
  if correct
    then [|\k -> counterexample s (k $(varOrCon $ mkName s))|]
    else [|\k -> counterexample ("The type of '"++s++"'\n  Expected: "++expected++"\n  Was: "++actual) False|]

hasType' :: String -> String -> Q Exp
hasType' s expected = do
  mt <- getType s
  let actual = case mt of Nothing -> "nothing"
                          Just t -> showType t
  if (actual==expected)
    then [|\k -> counterexample s (k $(varOrCon $ mkName s))|]
    else [|\k -> counterexample ("The type of '"++s++"'\n  Expected: "++expected++"\n  Was: "++actual) False|]

-- testing types

data FieldType = SimpleType String | WeirdType
  deriving (Eq,Show,Lift)

interpretFieldType (ConT n) = SimpleType $ nameBase n
interpretFieldType (VarT n) = SimpleType $ nameBase n
interpretFieldType (AppT t1 t2) =
  case (interpretFieldType t1, interpretFieldType t2) of
    (SimpleType s1, SimpleType s2) -> SimpleType $ s1 ++ " " ++ s2
    _ -> WeirdType
interpretFieldType _ = WeirdType

data Constructor = Constructor String [FieldType] | Weird String
  deriving (Eq,Show,Lift)

interpretConstructor (NormalC n bts) = Constructor (nameBase n) args
  where (_,typs) = unzip bts
        args = map interpretFieldType typs
interpretConstructor c = Weird (constructorName c)

data DataType = DataType [String] [Constructor]
  deriving (Eq,Show,Lift)

reifyType :: String -> Q Exp
reifyType s = do
  n <- lookupTypeName s
  case n of
    Nothing -> [|\k -> counterexample ("Type "++s++" not defined!") False|]
    Just n -> do info <- reify n
                 case info of
                   TyConI (DataD _ _ vs _ cs _) -> let cons = map interpretConstructor cs
                                                       vars = map bndrName vs
                                                   in [|\k -> counterexample ("The type "++s) $ k (DataType vars cons)|]
                   _ -> [|\k -> counterexample ("Definition "++s++" is not a data declaration!") False|]

bndrName (PlainTV n) = nameBase n
bndrName (KindedTV n _) = nameBase n

constructorName = nameBase . constructorName'

constructorName' :: Con -> Name
constructorName' c = case c of
  NormalC n _ -> n
  RecC n _ -> n
  InfixC _ n _ -> n
  ForallC _ _ c -> constructorName' c
  GadtC (n:_) _ _ -> n
  RecGadtC (n:_) _ _ -> n

-- testing classes

data Instance = Found | NotFound String String | NoClass String | NoType String
  deriving (Show, Eq)

lookupInstance :: String -> String -> Q Instance
lookupInstance cln typn = do
  cl <- lookupTypeName cln
  typ <- lookupTypeName typn
  case (cl,typ) of
    (Nothing,_) -> return $ NoClass cln
    (_,Nothing) -> return $ NoType typn
    (Just c, Just t) -> do b <- isInstance c [(ConT t)]
                           if b
                             then return Found
                             else return $ NotFound cln typn

withInstance :: String -> String -> Q Exp -> Q Exp
withInstance cln typn val = do
  ins <- lookupInstance cln typn
  case ins of
    NoClass cln -> [|\k -> counterexample ("Class "++cln++" not found") (property False)|]
    NoType typn -> [|\k -> counterexample ("Type "++typn++" not found") (property False)|]
    NotFound cln typn -> [|\k -> counterexample ("Type "++typn++" is not an instance of class "++cln) (property False)|]
    Found -> [|\k -> k $val|]

classContains :: String -> String -> Q Exp
classContains cln varn = do
  var <- lookupValueName varn
  case var of
    Nothing -> [|counterexample ("Function "++varn++" not found") (property False)|]
    Just cl -> do
      info <- reify cl
      case info of
        (ClassOpI _ _ parent) -> let pn = nameBase parent in [|counterexample ("Function "++varn++" is in the wrong class!") (pn ?== cln)|]
        _ -> [|counterexample ("Function "++varn++" is not a method of class "++cln) (property False)|]

lookupConstructor :: String -> Q (Maybe Type)
lookupConstructor "[]" = return (Just ListT)
lookupConstructor x = (fmap.fmap) ConT $ lookupTypeName x

lookupInstance1 :: String -> String -> Q Instance
lookupInstance1 cln typn = do
  cl <- lookupTypeName cln
  cons <- lookupConstructor typn
  case (cl,cons) of
    (Nothing,_) -> return $ NoClass cln
    (_,Nothing) -> return $ NoType typn
    (Just c, Just t) -> do b <- isInstance c [AppT t (VarT (mkName "a"))]
                           if b
                             then return Found
                             else return $ NotFound cln typn

withInstance1 :: String -> String -> Q Exp -> Q Exp
withInstance1 cln typn val = do
  ins <- lookupInstance1 cln typn
  case ins of
    NoClass cln -> [|\k -> counterexample ("Class "++cln++" not found") (property False)|]
    NoType typn -> [|\k -> counterexample ("Type "++typn++" not found") (property False)|]
    NotFound cln typn -> [|\k -> counterexample ("Type "++typn++" is not an instance of class "++cln) (property False)|]
    Found -> [|\k -> k $val|]

withInstances1 :: String -> [String] -> Q Exp -> Q Exp
withInstances1 cln typns val = do
  instances <- mapM (lookupInstance1 cln) typns
  case filter (/=Found) instances of
    (NoClass cln:_) -> [|\k -> counterexample ("Class "++cln++" not found") (property False)|]
    (NoType typn:_) -> [|\k -> counterexample ("Type "++typn++" not found") (property False)|]
    (NotFound cln typn:_) -> [|\k -> counterexample ("Type "++typn++" is not an instance of class "++cln) (property False)|]
    _ -> [|\k -> k $val|]

defineInstance :: String -> Name -> String -> Q Exp -> Q [Dec]
defineInstance cln typn methodn body = do
  cl <- lookupTypeName cln
  --let method = Name (OccName methodn) NameS
  case cl of
    Nothing -> return []
    (Just c) -> sequence [instanceD (cxt []) (appT (conT c) (conT typn)) [funD (mkName methodn) [clause [] (normalB body) []]]]

-- verbose testing

data Call = Prefix Name [Exp] | Operator Name Exp Exp

parseCall :: Exp -> Call
parseCall (VarE name) = Prefix name []
parseCall (UnboundVarE name) = Prefix name []
parseCall (AppE f arg) = Prefix name (args ++ [arg])
  where Prefix name args = parseCall f
parseCall (InfixE (Just l) (VarE op) (Just r)) = Operator op l r
parseCall x = error $ "Unsupported: " ++ show x

show' v = showsPrec 11 v ""

showArgs :: Call -> Q Exp
showArgs (Prefix name args) = [|fstring ++ " " ++ intercalate " " $arglist|]
  where fstring = nameBase name
        arglist = foldr (\arg exp -> [| show' $arg : $exp |]) [| [] |] (map return args)
showArgs (Operator name left right) = [| show' $(return left) ++ " " ++ opstring ++ " " ++ show' $(return right) |]
  where opstring = nameBase name

timeLimit :: Int
timeLimit = 5 * 1000 * 1000 -- 5 seconds in microseconds, less than the toplevel timeout of 10 seconds

testing :: Q Exp -> Q Exp
testing call = do
  parsed <- fmap parseCall call
  [| \k -> counterexample $(showArgs parsed) (within timeLimit (k $call)) |]

-- TH pprint prints all names as qualified, let's convert the names to unqualified locals
unqualifyName (Name n _) = Name n NameS

unqualify :: Exp -> Exp
unqualify (VarE n) = VarE (unqualifyName n)
unqualify (UnboundVarE n) = UnboundVarE (unqualifyName n)
unqualify (ConE n) = ConE (unqualifyName n)
unqualify (AppE f x) = AppE (unqualify f) (unqualify x)
unqualify (InfixE mleft op mright) = InfixE (fmap unqualify mleft) (unqualify op) (fmap unqualify mright)
unqualify (UInfixE left op right) = UInfixE (unqualify left) (unqualify op) (unqualify right)
unqualify (LitE l) = LitE l
unqualify (TupE exps) = TupE (map (fmap unqualify) exps)
unqualify (ListE exps) = ListE (map unqualify exps)
unqualify (ArithSeqE (FromR x)) = ArithSeqE (FromR (unqualify x))
unqualify x = error $ "Unsupported: " ++ show x

testing' :: Q Exp -> Q Exp
testing' call = do
  str <- fmap (pprint.unqualify) call
  --str <- fmap show call
  [| \k -> counterexample str (within timeLimit (k $call)) |]

-- checking imports

importsOnly :: String -> [String] -> Q Exp
importsOnly name allowed = do
  (ModuleInfo pkgImports) <- reifyModule (Module (PkgName "main") (ModName name))
  let imports = [nam | (Module _ (ModName nam)) <- pkgImports]
  let forbidden = imports \\ allowed
  case forbidden of
    [] -> [|property True|]
    xs -> [|counterexample ("forbidden imports: " ++ show xs) False|]
