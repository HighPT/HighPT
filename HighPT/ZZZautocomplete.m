(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPTio`EventYield`*)


(* ::Subtitle:: *)
(*Cross-section computation for the semi-leptonic processes pp -> ll and pp -> l\[Nu] in the SMEFT up to order O(\[CapitalLambda]^-4)*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageScope["AddAutoCompletion"]


(* ::Section:: *)
(*Auto-completion function*)


AddAutoCompletion[function_String][args___]:=Module[{processed},
	processed=ReplaceAll[{args},
	{
		None->0,
		"AbsoluteFileName"->2,
		"RelativeFileName"->3,
		"Color"->4,
		"PackageName"->7,
		"DirectoryName"->8,
		"InterpreterType"->9
	}
	];
	Function[FE`Evaluate@FEPrivate`AddSpecialArgCompletion@#][function->processed]
]


(* ::Section:: *)
(*Auto-completions*)


AddAutoCompletion["Yield"][Keys@LHCSearch[]];
AddAutoCompletion["ChiSquareLHC"][Keys@LHCSearch[]];
AddAutoCompletion["LHCSearch"][Keys@LHCSearch[]];
AddAutoCompletion["PythonExport"][Keys@LHCSearch[]];


AddAutoCompletion["DefineBasisAlignment"][{"down","up"}]
