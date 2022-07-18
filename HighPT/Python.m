(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`Python`*)


(* ::Subtitle:: *)
(*Module for exporting results to python.*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["PythonExport"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section::Closed:: *)
(*WCxf \[LongDash] Wilson coefficient exchange format [also contains couplings]*)


WCxf::usage= "WCxf[\"coef\"] denotes the Wilson coefficient that is labeld by \"coef\" in the Warsaw basis as specified by the WCxf format.";


Cxf::usage "Cxf[\"coupling\"] cenotes the NP coupling constant that is labeld by \"coupling\"."


MapToWCxf::usage= "MapToWCxf list of replacement rules that maps the HighPT Wilson coefficient notation to the WCxf conventions.";


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


(* ::Section::Closed:: *)
(*Export results to a python file*)


PythonExport::usage= "PythonExport[\"file_name\", list] exports the expressions given in list to a python file \"file_name.py\" using the Wilson coefficient exchange format (WCxf). A similar convention is defined for new physics coupling constants. The created file inlcudes a variable \"parameters\" that contains a list of all Wilson coefficients or coupling constants that appear in the argument list. Furthermore a function named \"file_name(C)\" is included that returns a list corresponding to the argument list in WCxf conventions. The argument C of that function must be given as a python dictionary in WCxf conventions. By default functions are stored in the directory given by NotebookDirectory[]. Alternative locations can be specified through the Option Directory.
PythonExport[\"file_name\", list, \"proc\"] includes a further function named \"proc_data()\" that returns all available data for the experimental search specified by \"proc\". Otherwise it works as PythonExport[\"file_name\", list]."


(* by default files should always be saved in the directory for the current notebook *)
Options[PythonExport] = {Directory :> NotebookDirectory[]};


PythonExport::ffdetected = "The given expression contains form factors, which can currently not be exported to python manually.";


PythonExport::unknownsearch = "The given search is unknown. Allowed searche labels are: `1`.";


PythonExport[name_String, expr_List, Optional[proc_,None], OptionsPattern[]]:= Module[
	{
		pyname = StringReplace[name,"-"->"_"],
		file,
		counter= 1,
		dir,
		exprWCxf = expr/.MapToWCxf
	}
	,
	(* check for FF which are not supported *)
	If[!FreeQ[exprWCxf, _FF],
		Message[PythonExport::ffdetected];
	];
	
	(* set directory *)
	dir= FileNameJoin[{OptionValue[Directory], pyname<>".py"}];
	
	(* open an new python file *)
	file= OpenWrite[dir];
	
	(* import numpy *)
	WriteString[file, "import numpy as np" <> "\n\n"];
	
	(* list all coefficients and couplings in this file *)
	WriteString[file, "parameters = " <> StringReplace[ToString@DeleteDuplicates@Cases[exprWCxf, (WCxf[label_] | Cxf[label_]) :> label, All], {"{"->"[", "}"->"]"}] <> "\n\n"];
	
	(* write experimental data *)
	If[proc=!=None,
		If[KeyExistsQ[LHCSearch[], proc],
			WriteSearchInfo[file, proc],
			Message[PythonExport::unknownsearch, LHCSearch[]]
		];
	];	
	
	(* write results *)
	WriteNEvents[file, expr, pyname];
	
	(* close the output stream *)
	Close[file];
	Print["Output saved in: " <> ToString[dir]];
]


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
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
