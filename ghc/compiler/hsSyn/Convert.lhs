%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

This module converts Template Haskell syntax into HsSyn


\begin{code}
module Convert( convertToHsExpr, convertToHsDecls ) where

#include "HsVersions.h"

import Language.Haskell.THSyntax as Meta

import HsSyn as Hs
	(	HsExpr(..), HsLit(..), ArithSeqInfo(..), 
		HsStmtContext(..), 
		Match(..), GRHSs(..), GRHS(..), HsPred(..),
		HsDecl(..), InstDecl(..), ConDecl(..),
		Stmt(..), HsBinds(..), MonoBinds(..), Sig(..),
		Pat(..), HsConDetails(..), HsOverLit, BangType(..),
		placeHolderType, HsType(..), HsTupCon(..),
		HsTyVarBndr(..), HsContext,
		mkSimpleMatch
	) 

import RdrName	( RdrName, mkRdrUnqual, mkRdrQual, mkOrig )
import Module   ( mkModuleName )
import RdrHsSyn	( mkHsIntegral, mkHsFractional, mkClassDecl, mkTyData )
import OccName
import SrcLoc	( SrcLoc, generatedSrcLoc )
import TyCon	( DataConDetails(..) )
import Type	( Type )
import BasicTypes( Boxity(..), RecFlag(Recursive), 
		   NewOrData(..), StrictnessMark(..) )
import FastString( mkFastString )
import Char 	( ord, isAlphaNum )
import List	( partition )
import Outputable


-------------------------------------------------------------------
convertToHsDecls :: [Meta.Dec] -> [HsDecl RdrName]
convertToHsDecls ds = map cvt_top ds


cvt_top d@(Val _ _ _) = ValD (cvtd d)
cvt_top d@(Fun _ _)   = ValD (cvtd d)
 
cvt_top (Data tc tvs constrs derivs)
  = TyClD (mkTyData DataType 
		    (noContext, tconName tc, cvt_tvs tvs)
		    (DataCons (map mk_con constrs))
		    (mk_derivs derivs) loc0)
  where
    mk_con (Constr c tys)
	= ConDecl (cName c) noExistentials noContext
		    (PrefixCon (map mk_arg tys)) loc0

    mk_arg ty = BangType NotMarkedStrict (cvtType ty)

    mk_derivs [] = Nothing
    mk_derivs cs = Just [HsClassP (tconName c) [] | c <- cs]

cvt_top (Class ctxt cl tvs decs)
  = TyClD (mkClassDecl (cvt_context ctxt, tconName cl, cvt_tvs tvs)
		       noFunDeps
		       sigs (Just binds) loc0)
  where
    (binds,sigs) = cvtBindsAndSigs decs

cvt_top (Instance tys ty decs)
  = InstD (InstDecl inst_ty binds sigs Nothing loc0)
  where
    (binds, sigs) = cvtBindsAndSigs decs
    inst_ty = HsForAllTy Nothing 
			 (cvt_context tys) 
			 (HsPredTy (cvt_pred ty))

cvt_top (Proto nm typ) = SigD (Sig (vName nm) (cvtType typ) loc0)

noContext      = []
noExistentials = []
noFunDeps      = []

-------------------------------------------------------------------
convertToHsExpr :: Meta.Exp -> HsExpr RdrName
convertToHsExpr = cvt

cvt (Var s) 	  = HsVar(vName s)
cvt (Con s) 	  = HsVar(cName s)
cvt (Lit l) 
  | overloadedLit l = HsOverLit (cvtOverLit l)
  | otherwise	    = HsLit (cvtLit l)

cvt (App x y)     = HsApp (cvt x) (cvt y)
cvt (Lam ps e)    = HsLam (mkSimpleMatch (map cvtp ps) (cvt e) void loc0)
cvt (Tup es)	  = ExplicitTuple(map cvt es) Boxed
cvt (Cond x y z)  = HsIf (cvt x) (cvt y) (cvt z) loc0
cvt (Let ds e)	  = HsLet (cvtdecs ds) (cvt e)
cvt (Case e ms)   = HsCase (cvt e) (map cvtm ms) loc0
cvt (Do ss)	  = HsDo DoExpr (cvtstmts ss) [] void loc0
cvt (Comp ss)     = HsDo ListComp (cvtstmts ss) [] void loc0
cvt (ArithSeq dd) = ArithSeqIn (cvtdd dd)
cvt (ListExp xs)  = ExplicitList void (map cvt xs)
cvt (Infix (Just x) s (Just y)) = OpApp (cvt x) (HsVar(vName s)) undefined (cvt y)
cvt (Infix Nothing  s (Just y)) = SectionR (HsVar(vName s)) (cvt y)
cvt (Infix (Just x) s Nothing ) = SectionL (cvt x) (HsVar(vName s))
cvt (Infix Nothing  s Nothing ) = HsVar(vName s) -- Can I indicate this is an infix thing?
cvt (SigExp e t)		= ExprWithTySig (cvt e) (cvtType t)

cvtdecs :: [Meta.Dec] -> HsBinds RdrName
cvtdecs [] = EmptyBinds
cvtdecs ds = MonoBind binds sigs Recursive
	   where
	     (binds, sigs) = cvtBindsAndSigs ds

cvtBindsAndSigs ds 
  = (cvtds non_sigs, map cvtSig sigs)
  where 
    (sigs, non_sigs) = partition sigP ds

cvtSig (Proto nm typ) = Sig (vName nm) (cvtType typ) loc0

cvtds :: [Meta.Dec] -> MonoBinds RdrName
cvtds []     = EmptyMonoBinds
cvtds (d:ds) = AndMonoBinds (cvtd d) (cvtds ds)

cvtd :: Meta.Dec -> MonoBinds RdrName
-- Used only for declarations in a 'let/where' clause,
-- not for top level decls
cvtd (Val (Pvar s) body ds) = FunMonoBind (vName s) False 
					  (panic "what now?") loc0
cvtd (Fun nm cls)   	    = FunMonoBind (vName nm) False (map cvtclause cls) loc0
cvtd (Val p body ds)	    = PatMonoBind (cvtp p) (GRHSs (cvtguard body) 
							  (cvtdecs ds) 
							  void) loc0
cvtd x = panic "Illegal kind of declaration in where clause" 


cvtclause :: Meta.Clause (Meta.Pat) (Meta.Exp) (Meta.Dec) -> Hs.Match RdrName
cvtclause (ps,body,wheres) = Match (map cvtp ps) Nothing 
                             (GRHSs (cvtguard body) (cvtdecs wheres) void)



cvtdd :: Meta.DDt -> ArithSeqInfo RdrName
cvtdd (Meta.From x) 	      = (Hs.From (cvt x))
cvtdd (Meta.FromThen x y)     = (Hs.FromThen (cvt x) (cvt y))
cvtdd (Meta.FromTo x y)	      = (Hs.FromTo (cvt x) (cvt y))
cvtdd (Meta.FromThenTo x y z) = (Hs.FromThenTo (cvt x) (cvt y) (cvt z))


cvtstmts :: [Meta.Stm] -> [Hs.Stmt RdrName]
cvtstmts [] = [] -- this is probably an error as every [stmt] should end with ResultStmt
cvtstmts [NoBindSt e]      = [ResultStmt (cvt e) loc0]      -- when its the last element use ResultStmt
cvtstmts (NoBindSt e : ss) = ExprStmt (cvt e) void loc0     : cvtstmts ss
cvtstmts (BindSt p e : ss) = BindStmt (cvtp p) (cvt e) loc0 : cvtstmts ss
cvtstmts (LetSt ds : ss)   = LetStmt (cvtdecs ds)	    : cvtstmts ss
cvtstmts (ParSt dss : ss)  = ParStmt(map cvtstmts dss)      : cvtstmts ss


cvtm :: Meta.Mat -> Hs.Match RdrName
cvtm (p,body,wheres) = Match [cvtp p] Nothing 
                             (GRHSs (cvtguard body) (cvtdecs wheres) void)
                             
cvtguard :: Meta.Rhs -> [GRHS RdrName]
cvtguard (Guarded pairs) = map cvtpair pairs
cvtguard (Normal e) 	 = [GRHS [  ResultStmt (cvt e) loc0 ] loc0]

cvtpair :: (Meta.Exp,Meta.Exp) -> GRHS RdrName
cvtpair (x,y) = GRHS [BindStmt truePat (cvt x) loc0,
		      ResultStmt (cvt y) loc0] loc0

cvtOverLit :: Lit -> HsOverLit
cvtOverLit (Int i)      = mkHsIntegral (fromInt i)
cvtOverLit (Rational r) = mkHsFractional r
-- An Int is like an an (overloaded) '3' in a Haskell source program
-- Similarly 3.5 for fractionals

cvtLit :: Lit -> HsLit
cvtLit (Char c)	  = HsChar (ord c)
cvtLit (String s) = HsString (mkFastString s)

cvtp :: Meta.Pat -> Hs.Pat RdrName
cvtp (Plit l)
  | overloadedLit l = NPatIn (cvtOverLit l) Nothing	-- Not right for negative
							-- patterns; need to think
							-- about that!
  | otherwise	    = LitPat (cvtLit l)
cvtp (Pvar s)     = VarPat(vName s)
cvtp (Ptup ps)    = TuplePat (map cvtp ps) Boxed
cvtp (Pcon s ps)  = ConPatIn (cName s) (PrefixCon (map cvtp ps))
cvtp (Ptilde p)   = LazyPat (cvtp p)
cvtp (Paspat s p) = AsPat (vName s) (cvtp p)
cvtp Pwild        = WildPat void

-----------------------------------------------------------
--	Types and type variables

cvt_tvs :: [String] -> [HsTyVarBndr RdrName]
cvt_tvs tvs = map (UserTyVar . tName) tvs

cvt_context :: Cxt -> HsContext RdrName 
cvt_context tys = map cvt_pred tys

cvt_pred :: Typ -> HsPred RdrName
cvt_pred ty = case split_ty_app ty of
	   	(Tvar tc, tys) -> HsClassP (tconName tc) (map cvtType tys)
		other -> panic "Malformed predicate"

cvtType :: Meta.Typ -> HsType RdrName
cvtType ty = trans (root ty [])
  where root (Tapp a b) zs = root a (cvtType b : zs)
        root t zs 	   = (t,zs)

        trans (Tcon (Tuple n),args) | length args == n
				    = HsTupleTy (HsTupCon Boxed n) args
        trans (Tcon Arrow,   [x,y]) = HsFunTy x y
        trans (Tcon List,    [x])   = HsListTy x

	trans (Tvar nm, args)	    = foldl HsAppTy (HsTyVar (tName nm)) args
        trans (Tcon tc, args)       = foldl HsAppTy (HsTyVar (tc_name tc)) args

	tc_name (TconName nm) = tconName nm
	tc_name Arrow	      = tconName "->"
	tc_name List	      = tconName "[]"
	tc_name (Tuple 0)     = tconName "()"
   	tc_name (Tuple n)     = tconName ("(" ++ replicate (n-1) ',' ++ ")")

split_ty_app :: Typ -> (Typ, [Typ])
split_ty_app ty = go ty []
  where
    go (Tapp f a) as = go f (a:as)
    go f as 	     = (f,as)

-----------------------------------------------------------
sigP :: Dec -> Bool
sigP (Proto _ _) = True
sigP other	 = False


-----------------------------------------------------------
-- some useful things

truePat  = ConPatIn (cName "True") (PrefixCon [])
falsePat = ConPatIn (cName "False") (PrefixCon [])

overloadedLit :: Lit -> Bool
-- True for literals that Haskell treats as overloaded
overloadedLit (Int l) = True
overloadedLit l	      = False

void :: Type.Type
void = placeHolderType

loc0 :: SrcLoc
loc0 = generatedSrcLoc

fromInt :: Int -> Integer
fromInt x = toInteger x

-- variable names
vName :: String -> RdrName
vName = mkName varName

-- Constructor function names
cName :: String -> RdrName
cName = mkName dataName

-- Type variable names
tName :: String -> RdrName
tName = mkName tvName

-- Type Constructor names
tconName = mkName tcName

mkName :: NameSpace -> String -> RdrName
-- Parse the string to see if it has a "." or ":" in it
-- so we know whether to generate a qualified or original name
-- It's a bit tricky because we need to parse 
--	Foo.Baz.x as Qual Foo.Baz x
-- So we parse it from back to front

mkName ns str
  = split [] (reverse str)
  where
    split occ [] = mkRdrUnqual (mk_occ occ)
    split occ (c:d:rev) 	-- 'd' is the last char before the separator
	|  is_sep c 		-- E.g.		Fo.x	d='o'
	&& isAlphaNum d		--		Fo.+:	d='+' perhaps
	= mk_qual (reverse (d:rev)) c occ
    split occ (c:rev) = split (c:occ) rev

    mk_qual mod '.' occ = mkRdrQual (mk_mod mod) (mk_occ occ)
    mk_qual mod ':' occ = mkOrig    (mk_mod mod) (mk_occ occ)

    mk_occ occ = mkOccFS ns (mkFastString occ)
    mk_mod mod = mkModuleName mod

    is_sep '.' 	 = True
    is_sep ':' 	 = True
    is_sep other = False
\end{code}
