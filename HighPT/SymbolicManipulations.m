(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`SymbolicManipulations`*)


(* ::Subtitle:: *)
(*Various functions to manipulate expressions*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


(* ::Subsection:: *)
(*Internal*)


PackageScope["AbsExpand"]


PackageScope["ExpandConjugates"]
PackageScope["ExpandAbsolutes"]
PackageScope["ExpandEverything"]


PackageScope["GetVariables"]


(* ::Chapter:: *)
(*Private:*)


(* ::Subsection:: *)
(*Expanding stuff*)


AbsExpand[x_] := Module[
	{expr,var,rules,WCs,coefs},
	
	(* Check if we have WCL in the expression *)
	If[(!FreeQ[x, WCL|WC])==True,

		(* Choosing variables *)
		var = Cases[x, _WC|_WCL, All]//DeleteDuplicates;
		var = Join[var, Conjugate/@var]//DeleteDuplicates;

		(* Selects the coefficients of each WCL^a Conj[WCL]^b combination  *)
		rules = CoefficientRules[x,var];

		(* Saves the non-zero WCL^a Conj[WCL]^b and their coefficients *)
		WCs = Times@@(var^#1)&@@@rules;
		coefs = (#2)&@@@rules;

		(* We then expand Abs[a1 WCs[[1]]+ ...]^2=Abs[a1]^2Abs[WCs[[1]]]^2+ ... + a1 a2\[Conjugate] WCs[[1]] WCs[[2]]\[Conjugate] + ...  *)
		Table[If[i==j,
			Together[coefs[[i]]]Conjugate[Together[coefs[[i]]]]WCs[[i]]Conjugate[WCs[[i]]],coefs[[i]]Conjugate[coefs[[j]]]WCs[[i]]Conjugate[WCs[[j]]]
			],
			{i,1,Length[rules]},{j,1,Length[rules]}
		]//Flatten//Total,
		Together[x] Conjugate[Together[x]]
	]
]


ExpandConjugates[expr_]:=
expr/. Conjugate[a_]:>Distribute[Conjugate[a],Plus]//.Conjugate[Times[x_,y_]]:>Times[Conjugate[x],Conjugate[y]]/. Conjugate[a_]:>Distribute[Conjugate[a],Plus]//.Conjugate[Times[x_,y_]]:>Times[Conjugate[x],Conjugate[y]]


ExpandAbsolutes[expr_] := expr/.Abs[x_]^2->AbsExpand[x]/.Abs[x_]:>Power[AbsExpand[x],1/2]


ExpandEverything[expr_]:=expr/.Re[x_]:>1/2 (x+Conjugate[x])/.Im[x_]:>-(I/2)(x-Conjugate[x])//ExpandAbsolutes//ExpandConjugates


GetVariables[expr_]:=Module[
	{varlist,var}
	,
	varlist=Variables/@(Level[expr,Depth[expr]]//.Conjugate[a_]:>a//.Re[a_]:>a//.Im[a_]:>a);
	var=DeleteDuplicates[Join@@varlist];
	Return[DeleteCases[var,_String]]
]
