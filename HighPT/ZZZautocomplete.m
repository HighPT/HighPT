(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`ZZZautocomplete`*)


(* ::Subtitle:: *)
(*Provides an autocomplete function.*)
(*Notice that this file must be the last one loaded in the package!*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Internal*)


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


AddAutoCompletion["EventYield"][Keys@LHCSearch[]];
AddAutoCompletion["ChiSquareLHC"][Keys@LHCSearch[]];
AddAutoCompletion["LHCSearch"][Keys@LHCSearch[]];
(*AddAutoCompletion["PythonExport"][Keys@LHCSearch[]];*)


AddAutoCompletion["DefineBasisAlignment"][{"down","up"}]


AddAutoCompletion["InitializeModel"][{"SMEFT", "Mediators"}]


AddAutoCompletion["SetPDF"][$PDFsets]


AddAutoCompletion["DefineParameters"][{"\[Alpha]EM", "GF", "mZ", "\[CapitalGamma]Z", "\[CapitalGamma]W", "Wolfenstein"}]


AddAutoCompletion["WC"][GetAllWC]


AddAutoCompletion["Coupling"][{"g1u","g1d","g1e","g1q","g1l","g3q","g3l","y1L","y1R","y1Rb","y1Rt","x1L","x1R","x1Rb","x1Rt","y2L","y2R","y2Lt","y2Rt","x2L","x2R","x2Lt","x2Rt","y3L","x3L"}]


AddAutoCompletion["WCL"][GetAllWCL]
