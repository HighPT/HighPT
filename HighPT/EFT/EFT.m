(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`EFT`*)


(* ::Subtitle:: *)
(*General EFT utilities*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["EFTTruncate"]


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


(* ::Subsection:: *)
(*Truncation function*)


Options[EFTTruncate] = {
	EFTorder :> GetEFTorder[],
	OperatorDimension :> GetOperatorDimension[]
};


EFTTruncate[expr_, OptionsPattern[]] := Module[
	{
	tmpexpr,
	exprwithdimensions, eps,
	toexpand,
	expanded,
	var
	}
	,
	OptionCheck[EFTorder,OptionValue[EFTorder]];
	
	(* Define properties of the expansion parameter, possibly move this outside of the function *)
	eps/:Conjugate[eps]:=eps;
	
	(* Kill dimension-eight operators if needed *)
	If[
		MatchQ[OptionValue[OperatorDimension],6],
		tmpexpr = expr/.WC[l:Alternatives@@Join[$WCList0d8,$WCList2d8,$WCList4d8],_]:>0,
		tmpexpr = expr
	];
	
	(* Extract all the variables (and their conjugates) *)
	var = Cases[tmpexpr, _WC|_WCL, All]//DeleteDuplicates;
	var = Join[var, Conjugate/@var]//DeleteDuplicates;	
	
	(* substitute mass dimension counting parameter *)
	exprwithdimensions = tmpexpr/.WC[lab_,ind_]:>Power[eps,MassDimension[lab]-4]*WC[lab,ind]/.WCL[lab_,ind_]:>Power[eps,MassDimension[lab]-4]*WCL[lab,ind];
	
	(* Expand absolute values and conjugates *)
	toexpand = ExpandEverything[exprwithdimensions];
	
	(* Find type of expression for more efficient Taylor expansion, and expand *)
	If[
		PolynomialQ[toexpand,var],
		expanded = Collect[toexpand,eps]/.Power[eps,n_]/;n>OptionValue[EFTorder]->0/.eps->1,
		expanded = Normal[Series[toexpand,{eps,0,OptionValue[EFTorder]}]]/.eps->1
	];
	
	(* Return result *)
	Return[expanded]
];
