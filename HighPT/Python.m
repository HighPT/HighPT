(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPTio`Python`*)


(* ::Subtitle:: *)
(*Cross-section computation for the semi-leptonic processes pp -> ll and pp -> l\[Nu] in the SMEFT up to order O(\[CapitalLambda]^-4)*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["PythonExport"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*WCxf \[LongDash] Wilson coefficient exchange format*)


WCxf::usage= "WCxf[\"coef\"]
	Denotes the Wilson coefficient that is labeld by coef in the Warsaw basis as specified by the WCxf format.";


Cxf::usage "Cxf[\"coupling\"]
	Denotes the NP coupling constant that is labeld by \"coupling\"."


MapToWCxf::usage= "MapToWCxf
	List of replacement rules that maps the HighPTio Wilson coefficient notation to the WCxf conventions.";


MapToWCxf= {
	(* WILSON COEFFICIENTS *)
	(* C_qe *)
	WC["eq", {\[Alpha]_,\[Beta]_,i_,j_}] :> Module[{ret, ind={i,j,\[Alpha],\[Beta]}},
		If[(ind[[1]]>ind[[2]]) || (ind[[1]]==ind[[2]] && ind[[3]]>ind[[4]]),
			ind={ind[[2]],ind[[1]],ind[[4]],ind[[3]]};
			ret= Conjugate@ WCxf["'" <> "qe" <> "_" <> ToString[ind[[1]]] <> ToString[ind[[2]]] <> ToString[ind[[3]]] <> ToString[ind[[4]]] <> "'"]
			,
			ret= WCxf["'" <> "qe" <> "_" <> ToString[ind[[1]]] <> ToString[ind[[2]]] <> ToString[ind[[3]]] <> ToString[ind[[4]]] <> "'"]
		];
		ret
	],
	
	(* four fermion operators except for C_qe *)
	WC[a:Except["eq",_String], {\[Alpha]_,\[Beta]_,i_,j_}] :> WCxf["'" <> a <> "_" <> ToString[\[Alpha]] <> ToString[\[Beta]] <> ToString[i] <> ToString[j] <> "'"],
	
	(* two fermion operators *)
	WC[a_String, {p_,r_}] :> WCxf["'" <> StringReplace[a, "H"->"phi"] <> "_" <> ToString[p] <> ToString[r] <> "'"],
	
	(* COUPLINGS *)
	Coupling[label_String,{i_,a_}] :> Cxf["'" <> label <> "_"<> ToString[i] <> ToString[a] <> "'"]
};


(* ::Section:: *)
(*Export results to a python file*)


PythonExport::usage= "PythonExport[\"label\", list]
	Exports the expressions given in list to a python file \"label.py\" using the Wilson coefficient exchange format (WCxf).
	Each element of list will be stored as a separate python function, where the function for the \!\(\*SuperscriptBox[\(n\), \(th\)]\) element is named label_n.
	By default functions are stored in the directory given by NotebookDirectory[]. Alternative locations can be specified through the Option Directory.
	Example:
		PythonExport[\"tata\", {1711.9`\[VeryThinSpace]+0.15737` WC[\"lq1\",{3,3,3,3}]+0.043601` WC[\"lq1\",{3,3,3,3}\!\(\*SuperscriptBox[\(]\), \(2\)]\)}] will create the file \"tata.py\" with the content:
		def tata_1(C):
			return 1711.9\[VeryThinSpace]+0.15737*C['lq1_3333']+0.043601*C['lq1_3333']**2";


(* by default files should always be saved in the directory for the current notebook *)
Options[PythonExport]= {Directory:>NotebookDirectory[]};


PythonExport[proc_String, expr_List, OptionsPattern[]]:= Module[
	{
		pyproc = StringReplace[proc,"-"->"_"],
		file,
		counter= 1,
		dir,
		exprWCxf = expr/.MapToWCxf
	}
	,
	(* set directory *)
	dir= FileNameJoin[{OptionValue[Directory], pyproc<>".py"}];
	
	(* open an new python file *)
	file= OpenWrite[dir];
	
	(* import numpy *)
	WriteString[file, "import numpy as np" <> "\n\n"];
	
	(* list all coefficients and couplings in this file *)
	WriteString[file, "parameters = " <> StringReplace[ToString@DeleteDuplicates@Cases[exprWCxf, (WCxf[name_] | Cxf[name_]) :> name, All], {"{"->"[", "}"->"]"}] <> "\n\n"];
	(*WriteString[file, "# Coupling constants:  " <> ToString@DeleteDuplicates@Cases[exprWCxf, Cxf[name_]:>name, All] <> "\n\n"];*)
	
	(* write experimental data *)
	WriteSearchInfo[file, proc];
	
	(* write results *)
	WriteNEvents[file, expr, pyproc];
	
	(* close the output stream *)
	Close[file];
	Print["Output saved in: " <> ToString[dir]];
]


(* ::Subsection:: *)
(*Write the python function for a single bin*)


WriteNEvents[file_, expr_, label_] := Module[
	{
		pyStr = ToPythonString/@expr
	},
	(* define new function *)
	WriteString[file, "def " <> label <> "(C):\n"];
	
	WriteString[file, "\t" <> "result = [" <> "\n"];
	Do[
		WriteString[file, "\t\t" <> str <> ", \n"]
		,
		{str,pyStr}
	];
	
	WriteString[file, "\t" <> "]" <> "\n"];
	
	(* write return value *)
	WriteString[file, "\t" <> "return result" <> "\n\n"];
]


(* ::Subsection:: *)
(*Write experimental data*)


WriteSearchInfo[file_, proc_String]:=Module[
	{
		search = LHCSearch[proc]
	}
	,
	(* function header *)
	WriteString[file, "def " <> StringReplace[proc,"-"->"_"] <> "_data():" <> "\n"];
	WriteString[file, "\t" <> "source = {" <> "\n"];
	
	(* write bins *)
	WriteString[file, "\t\t" <> "\"binning\":          " <> StringReplace[ToString[LHCSearch[proc]["INFO"]["BINS"]["OBSERVABLE"]],{"{"->"[","}"->"]"}] <> ", \n\n"];
	(* write data *)
	WriteString[file, "\t\t" <> "\"data\":             " <> StringReplace[ToString[LHCSearch[proc]["DATA"]],{"{"->"[","}"->"]"}] <> ", \n\n"];
	(* write background *)
	WriteString[file, "\t\t" <> "\"background\":       " <> StringReplace[ToString[LHCSearch[proc]["BACKGROUND"]],{"{"->"[","}"->"]"}] <> ", \n\n"];
	(* write background-error *)
	WriteString[file, "\t\t" <> "\"background_error\": " <> StringReplace[ToString[LHCSearch[proc]["ERROR-BKG"]],{"{"->"[","}"->"]"}] <> "\n"];
	
	WriteString[file, "\t" <> "}" <> "\n"];
	WriteString[file, "\t" <> "return source" <> "\n\n"];
]


(* ::Subsection:: *)
(*Converts the MMA expression for a cross section or  (\[Chi]^2) to the corresponding python expression that is stored as a string*)


ToPythonString[expr_]:= Module[
	{temp= expr/.MapToWCxf} (* use WCxf conventions *)
	,
	(* use FortranForm to handle Power *)
	temp= ToString[temp /. Complex[x_,y_]:>complex[x,y] ,FortranForm];
	
	(* write complex numbers and conjugate *)
	temp= StringReplace[temp, "HighPTxPythonxPackagePrivatexcomplex"->"complex"];
	temp= StringReplace[temp, "Conjugate"->"np.conjugate"];
	
	(* remove " *)
	temp= StringDelete[temp,"\""];
	
	(* addapt to WCxf convention using dictionaries *)
	temp= StringReplace[temp,"HighPTxPythonxPackagePrivatexWCxf("~~a__~~")"/;StringFreeQ[a,")"] :> "C["<>a<>"]"];
	
	(* addapt to WCxf like notation for coupling constants *)
	temp= StringReplace[temp,"HighPTxPythonxPackagePrivatexCxf("~~a__~~")"/;StringFreeQ[a,")"] :> "C["<>a<>"]"];
	
	(* * *)
	Return[temp]
]
