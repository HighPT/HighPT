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


PackageExport["WCxf"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*WCxf \[LongDash] Wilson coefficient exchange format*)


WCxf::usage= "WCxf[\"coef\"]
	Denotes the Wilson coefficient that is labeld by coef in the Warsaw basis as specified by the WCxf format.";


MapToWCxf::usage= "MapToWCxf
	List of replacement rules that maps the HighPTio Wilson coefficient notation to the WCxf conventions.";


MapToWCxf= {
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
	WC[a_String, {p_,r_}] :> WCxf["'" <> StringReplace[a, "H"->"phi"] <> "_" <> ToString[p] <> ToString[r] <> "'"]
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


PythonExport[label_String, expr_List, OptionsPattern[]]:= Module[
	{
		file,
		counter= 1,
		dir= FileNameJoin[{OptionValue[Directory],label<>".py"}]
	}
	,
	(* open an new python file *)
	file= OpenWrite[dir];
	
	(* write one function for each element of expr *)
	Do[
		WriteBin[file, bin, label, counter++]
		,
		{bin, expr}
	];
	
	(* close the output stream *)
	Close[file];
	Print["Output saved in: " <> ToString[dir]];
]


(* ::Subsection:: *)
(*Write the python function for a single bin*)


WriteBin[file_, expr_, label_, bin_]:=Module[{},
	(* define new function *)
	WriteString[file, "def "<>label<>"_"<>ToString[bin]<>"(C):\n"];
	(* write return value *)
	WriteString[file, "\t"<>"return "<>ToPythonString[expr]<>"\n\n"];
]


(* ::Subsection:: *)
(*Converts the MMA expression for a cross section or  (\[Chi]^2) to the corresponding python expression that is stored as a string*)


ToPythonString[expr_]:= Module[
	{temp= expr/.MapToWCxf} (* use WCxf conventions *)
	,
	(* use FortranForm to handle Power *)
	temp= ToString[temp,FortranForm];
	(* remove " *)
	temp= StringDelete[temp,"\""];
	(* addapt to flavio/smelli conventions using dictionaries *)
	temp= StringReplace[temp,"WCxf("~~a__~~")"/;StringFreeQ[a,")"] :> "C["<>a<>"]"];
	(* * *)
	Return[temp]
]
