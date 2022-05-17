(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPTio`Parameters`*)


(* ::Subtitle:: *)
(*Cross-section computation for the semi-leptonic processes pp -> ll and pp -> l\[Nu] in the SMEFT up to order O(\[CapitalLambda]^-4)*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["Mass"]


PackageExport["Width"]


PackageExport["DefineParameters"]
PackageExport["GetParameters"]


PackageExport["ConstantInput"]


PackageExport["DefineBasisAlignment"]


(* ::Text:: *)
(*Everything below should be combined in one head, e.g. Parameter["..."]*)
(*OR: PackageScope everything*)


(*
(*PackageExport["VEV"]*)
PackageExport["\[Alpha]EM"]
PackageExport["GF"]
PackageExport["mZ"]
PackageExport["\[CapitalGamma]Z"]
PackageExport["\[CapitalGamma]W"]
PackageExport["sW"]
PackageExport["cW"]
*)


PackageExport["CKM"]
(*PackageExport["Wolfenstein"]*)
PackageExport["Vckm"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["ReplaceConstants"]


PackageScope["Charge"]
PackageScope["WeakIsospin3"]


PackageScope["Vu"]
PackageScope["Vd"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Constants*)


ConstantInput::usage="ConstantInput[\"label\"]
	Denotes the constant parameter \"label\". The defined parameters are: 
		\"\[Alpha]EM\" : electromagnetic fine structure constant,
		\"GF\" : Fermi's constant,
		\"vev\" : electroweak vacuum expectation value,
		\"sW\" : sine of the weak mixing angle,
		\"cW\" : cosine of the weak mixing angle."


(* ::Subsection:: *)
(*Vacuum expectation value*)


(*VEV::usage="VEV
	Denotes the electroweak vacuum expectation value and is formated as \[ScriptV] in TraditionalForm.";*)


(*\[Alpha]EM::usage="\[Alpha]EM
	Denotes the electromagnetic fine structure constant and is formated as \!\(\*SubscriptBox[\(\[Alpha]\), \(EM\)]\) in TraditionalForm.";*)


(*sW::usage= "sW
	Denotes the sine of the weak mixing angle and is formated as \!\(\*SubscriptBox[\(s\), \(W\)]\) in TraditionalForm."*)


(*cW::usage= "cW
	Denotes the cosine of the weak mixing angle and is formated as \!\(\*SubscriptBox[\(c\), \(W\)]\) in TraditionalForm."*)


Mass::usage= "Mass[\[Phi]]
	Denotes the mass of the particle \[Phi] and is formated as \!\(\*SubscriptBox[\(M\), \(\[Phi]\)]\) in TraditionalForm.";


Width::usage= "Width[\[Phi]]
	Denotes the width of the particle \[Phi] and is formated as \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Phi]\)]\) in TraditionalForm.";


CKM::usage= "CKM
	Denotes the CKM matrix, with CKM[[n,m]] given by Vckm[n,m].";


Vckm::usage= "Vckm[n,m]
	Denotes the element of the CKM matrix in the \!\(\*SuperscriptBox[\(n\), \(th\)]\) row and \!\(\*SuperscriptBox[\(m\), \(th\)]\) column.";


(* ::Section:: *)
(*Masses and Widths*)


ReplaceMassWidth[]:= Module[
	{
		mediators= GetMediators[],
		replacements= <||>
	}
	,
	Do[
		AssociateTo[replacements, Mass[med]->First[mediators[med]]];
		AssociateTo[replacements, Width[med]->((mediators[med])[[2]])]
		,
		{med, Keys[mediators]}
	];
	Return[replacements]
]


(* ::Section:: *)
(*Replacement*)


ReplaceConstants::usage= "ReplaceConstants[]
	Returns a list of replacement rules for all constants.";


ReplaceConstants[]:= Join[GetParameters[], ReplaceMassWidth[]]


(* ::Section:: *)
(*Real constants*)


$realParameters = Alternatives[
	"vev", "\[Alpha]EM", "sW", "cW", "GF"
]


ConstantInput/:Conjugate[ConstantInput[x:$realParameters]] := ConstantInput[x]


(*VEV/:Conjugate[VEV]:= VEV*)


(*\[Alpha]EM/:Conjugate[\[Alpha]EM]:= \[Alpha]EM*)


(*sW/:Conjugate[sW]:= sW*)


(*cW/:Conjugate[cW]:= cW*)


Mass/:Conjugate[Mass[a_]]:= Mass[a]


Width/:Conjugate[Width[a_]]:= Width[a]


(* ::Section:: *)
(*Formatting*)


Format[Mass[f_], TraditionalForm]:= Subscript["M",f]


Format[Width[f_], TraditionalForm]:= Subscript["\[CapitalGamma]",f]


(*Format[VEV, TraditionalForm]:= "\[ScriptV]"*)


(*Format[sW, TraditionalForm]:= Subscript["s","W"]
Format[cW, TraditionalForm]:= Subscript["c","W"]*)


(*Format[\[Alpha]EM, TraditionalForm]:= Subscript["\[Alpha]","EM"]*)


Format[ConstantInput["vev"], TraditionalForm]:= "\[ScriptV]"
Format[ConstantInput["\[Alpha]EM"], TraditionalForm]:= Subscript["\[Alpha]","EM"]
Format[ConstantInput["sW"], TraditionalForm]:= Subscript["s","W"]
Format[ConstantInput["cW"], TraditionalForm]:= Subscript["c","W"]
Format[ConstantInput["GF"], TraditionalForm]:= Subscript["G","F"]


Format[CKM, TraditionalForm]:= Subscript["V","CKM"]
Format[Vckm[x_,y_], TraditionalForm]:= Subscript["V",ToString[x]<>ToString[y]]


(* ::Section:: *)
(*Experimental Inputs*)


(* ::Subsection:: *)
(*The default values*)


(* ::Subsubsection:: *)
(*couplings, masses, widths*)


(* values used by MadGraph5 *)
\[Alpha]EM$default = 1/127.9;
GF$default = 1.16637*10^(-5);
mZ$default = 91.1876;
\[CapitalGamma]Z$default = 2.4952; (*2.44140351;*)
\[CapitalGamma]W$default = 2.085; (*2.04759951;*)


(* ::Subsubsection:: *)
(*CKM*)


\[Lambda]Wolfenstein$default    = 0.2233;
AWolfenstein$default    = 0.810;
\[Rho]BarWolfenstein$default = 0.153;
\[Eta]BarWolfenstein$default = 0.393;

Wolfenstein$default = {
	\[Lambda]Wolfenstein$default,
	AWolfenstein$default,
	\[Rho]BarWolfenstein$default,
	\[Eta]BarWolfenstein$default
}

(*\[Rho]Wolfenstein= \[Rho]BarWolfenstein/(1-\[Lambda]Wolfenstein^2/2);
\[Eta]Wolfenstein= \[Eta]BarWolfenstein/(1-\[Lambda]Wolfenstein^2/2);*)


CKM= {
	{Vckm[1,1], Vckm[1,2], Vckm[1,3]},
	{Vckm[2,1], Vckm[2,2], Vckm[2,3]},
	{Vckm[3,1], Vckm[3,2], Vckm[3,3]}
};


(* Rotation matrices for left-handed up- and down-type quarks *)
(* By default down alignment is assumed *)
Vu = ConjugateTranspose[CKM]

Vd= {
	{1,0,0},
	{0,1,0},
	{0,0,1}
}


Identity


(* ::Subsection:: *)
(*Define up- / down-alignment*)


DefineBasisAlignment::usage=
"DefineBasisAlignment[\"down\"] specifies to work in the down-aligned basis, where \!\(\*SubscriptBox[\(V\), \(d\)]\)=\!\(\*SubscriptBox[\(1\), \(3  x3\)]\) and \!\(\*SubscriptBox[\(V\), \(u\)]\)=\!\(\*SubscriptBox[\(V\), \(CKM\)]\)\[ConjugateTranspose]. The left-handed rotation matrices are defined by \!\(\*SuperscriptBox[SubscriptBox[\(d\), \(i\)], \(mass\)]\)=[\!\(\*SubscriptBox[\(V\), \(d\)]\)\!\(\*SubscriptBox[\(]\), \(ij\)]\)\!\(\*SuperscriptBox[SubscriptBox[\(d\), \(j\)], \(weak\)]\) and \!\(\*SuperscriptBox[SubscriptBox[\(u\), \(i\)], \(mass\)]\)=[\!\(\*SubscriptBox[\(V\), \(u\)]\)\!\(\*SubscriptBox[\(]\), \(ij\)]\)\!\(\*SuperscriptBox[SubscriptBox[\(u\), \(j\)], \(weak\)]\), respectively. Down-alignment is the default choice.
DefineBasisAlignment[\"up\"] specifies to work in the up-aligned basis, where \!\(\*SubscriptBox[\(V\), \(d\)]\)=\!\(\*SubscriptBox[\(V\), \(CKM\)]\) and \!\(\*SubscriptBox[\(V\), \(u\)]\)=\!\(\*SubscriptBox[\(1\), \(3  x3\)]\).
DefineBasisAlignment[matrix] sets the rotation matrix for left-handed down-type quarks \!\(\*SubscriptBox[\(V\), \(d\)]\) equal to the argument matrix, which must be a unitary 3x3 matrix. Consequently the up-rotation matrix is defined by \!\(\*SubscriptBox[\(V\), \(u\)]\)=\!\(\*SubscriptBox[\(V\), \(d\)]\).\!\(\*SubscriptBox[\(V\), \(CKM\)]\)."


DefineBasisAlignment[] := DefineBasisAlignment["down"]
DefineBasisAlignment["down"] := Module[{},
	(* set the new Vd matrix*)
	Vd = DiagonalMatrix[{1,1,1}];
	(* define Vu matrix such that CKM=Vu\[ConjugateTranspose].Vd *)
	Vu = ConjugateTranspose[CKM];
	(* Print *)
	Print["Defined new mass basis alignment:"];
	Print["\!\(\*SubscriptBox[\(V\), \(u\)]\) = ", MatrixForm[Vu/.GetParameters[]]];
	Print["\!\(\*SubscriptBox[\(V\), \(d\)]\) = ", MatrixForm[Vd/.GetParameters[]]];
];


DefineBasisAlignment["up"] := Module[{},
	(* set the new Vd matrix*)
	Vu = DiagonalMatrix[{1,1,1}];
	(* define Vu matrix such that CKM=Vu\[ConjugateTranspose].Vd *)
	Vd = CKM;
	(* Print *)
	Print["Defined new mass basis alignment:"];
	Print["\!\(\*SubscriptBox[\(V\), \(u\)]\) = ", MatrixForm[Vu/.GetParameters[]]];
	Print["\!\(\*SubscriptBox[\(V\), \(d\)]\) = ", MatrixForm[Vd/.GetParameters[]]];
];


DefineBasisAlignment::notunitary="Cound not verify that the given matrix is unitary."


(* function that defines a down aligned basis  *)
DefineBasisAlignment[matrix_ /; (Dimensions[matrix]==={3,3})] := Module[{},
	(* check unitarity *)
	If[!UnitaryMatrixQ[matrix/.GetParameters[], Tolerance->10^-2],
		Message[DefineBasisAlignment::notunitary]
	];
	(* set the new Vd matrix*)
	Vd = matrix;
	(* define Vu matrix such that CKM=Vu\[ConjugateTranspose].Vd *)
	Vu = Vd . ConjugateTranspose[CKM];
	(* Print *)
	Print["Defined new mass basis alignment:"];
	Print["\!\(\*SubscriptBox[\(V\), \(u\)]\) = ", MatrixForm[Vu/.GetParameters[]]];
	Print["\!\(\*SubscriptBox[\(V\), \(d\)]\) = ", MatrixForm[Vd/.GetParameters[]]];
];


(* initialize down aligned *)
(*DefineBasisAlignment["down"]*) (* does not work since GetParameters[] is not yet defined *)


DefineBasisAlignment::invalidarg="The argument `1` is not a 3x3 matrix."


DefineBasisAlignment[arg:Except["up"|"down"]/;(Dimensions[arg]=!={3,3})] := (Message[DefineBasisAlignment::invalidarg,arg/.GetParameters[]];Abort[])


(* ::Subsection:: *)
(*Function to modify experimental values*)


DefineParameters::usage= "DefineParameters[] 
	defines all SM parameters such as ConstantInput[\"\[Alpha]EM\" | \"vev\" | \"sW\" | \"cW\" | \"GF\"], Mass[ZBoson | WBoson | Photon], Width[ZBoson | WBoson | Photon], and the CKM.
	Furthermore NP parameters, i.e. the Mass and Width of BSM mediators can be changed.
	The input scheme uses the parameters \"\[Alpha]EM\", \"GF\", \"mZ\", \"\[CapitalGamma]Z\", \"\[CapitalGamma]W\", \"Wolfenstein\" which can be specified via Options.
	Each option value must be a number, except for the optionvalue of \"Wolfenstein\" which msut be a list of the Wolfenstein parameters {\[Lambda],A,\!\(\*OverscriptBox[\(\[Rho]\), \(_\)]\),\!\(\*OverscriptBox[\(\[Eta]\), \(_\)]\)}.
	To obtain the current values of the parameters the routine GetParameters[] can be used.
	Example:
		To set the electromagnetic fine structure constant to \!\(\*SubscriptBox[\(\[Alpha]\), \(EM\)]\) = \!\(\*SuperscriptBox[\(137\), \(-1\)]\) use
		DefineParameters[\"\[Alpha]EM\" \[Rule] 1/137].
";


Options[DefineParameters]= {
	"\[Alpha]EM"         -> \[Alpha]EM$default,
	"GF"          -> GF$default,
	"mZ"          -> mZ$default,
	"\[CapitalGamma]Z"          -> \[CapitalGamma]Z$default,
	"\[CapitalGamma]W"          -> \[CapitalGamma]W$default,
	"Wolfenstein" -> Wolfenstein$default,
	Mass          -> {},
	Width         -> {}
};


DefineParameters::unknownmass= "The mass `1` is undefined."


DefineParameters[OptionsPattern[]] := Module[
	{
		$\[Alpha]EM = OptionValue["\[Alpha]EM"],
		$GF  = OptionValue["GF"],
		$mZ  = OptionValue["mZ"],
		$\[CapitalGamma]Z  = OptionValue["\[CapitalGamma]Z"],
		$\[CapitalGamma]W  = OptionValue["\[CapitalGamma]W"],
		$mW,
		$sW,
		$vev,
		$Wolfenstein = OptionValue["Wolfenstein"], 
		$\[Lambda], $A, $\[Rho]Bar, $\[Eta]Bar, $\[Rho], $\[Eta],
		masses = OptionValue[Mass],
		widths = OptionValue[Width],
		mediators = GetMediators[]
	}
	,
	(* set the Wolfentein parameters *)
	{$\[Lambda], $A, $\[Rho]Bar, $\[Eta]Bar} = $Wolfenstein;
	$\[Rho] = $\[Rho]Bar/(1-$\[Lambda]^2/2);
	$\[Eta] = $\[Eta]Bar/(1-$\[Lambda]^2/2);
	
	(* compute the non-input parameters *)
	$mW = Sqrt[$mZ^2/2.+Sqrt[$mZ^4/4.-($\[Alpha]EM*\[Pi]*$mZ^2)/($GF*Sqrt[2])]];
	$sW = Sqrt[1. - $mW^2/$mZ^2];
	$vev = ($mW*$sW)/Sqrt[\[Pi]*$\[Alpha]EM];
	
	(* modify the masses of mediators *)
	If[masses=!={},
		Do[
			If[MatchQ[mass,Alternatives@@Keys[mediators]->_],
				ModifyMediator[First[mass], Mass->Last[mass]]
				,
				Message[DefineParameters::unknownmass,mass]
			]
			,
			{mass, masses}
		]
	];
	
	(* Create the appropriate supstitution rule *)
	ExperimentalParameters = <|
		ConstantInput["\[Alpha]EM"] -> $\[Alpha]EM,
		ConstantInput["vev"] -> $vev,
		ConstantInput["sW"]  -> $sW,
		ConstantInput["cW"]  -> Sqrt[1. - $sW^2],
		(* CKM *)
		Vckm[1,1] -> 1-$\[Lambda]^2/2,
		Vckm[1,2] -> $\[Lambda],
		Vckm[1,3] -> $A * $\[Lambda]^3 * ($\[Rho] - I*$\[Eta]),
		
		Vckm[2,1] -> -$\[Lambda],
		Vckm[2,2] -> 1-$\[Lambda]^2/2,
		Vckm[2,3] -> $A * $\[Lambda]^2,
		
		Vckm[3,1] -> $A * $\[Lambda]^3 * (1 - $\[Rho] - I*$\[Eta]),
		Vckm[3,2] -> -$A * $\[Lambda]^2,
		Vckm[3,3] -> 1,
		(* masses & widths *)
		Mass[ZBoson]         -> $mZ,
		Width[ZBoson]        -> $\[CapitalGamma]Z,
		Mass[WBoson]         -> $mW,
		Width[WBoson]        -> $\[CapitalGamma]W,
		Mass[Photon]         -> 0,
		Width[Photon]        -> 0
	|>;
]


ExperimentalParameters= <||>;


(* initialize the parameters with default values *)
DefineParameters[]


GetParameters::usage= "GetParameters[]
	Returns an Association of all SM parameters and their values.
	To modify the values use DefineParameters.";


(* returns the current value of the (B)SM parameters *)
GetParameters[]:= Join[ExperimentalParameters, ReplaceMassWidth[]]


(* ::Section:: *)
(*Charge definitions*)


(* ::Subsection:: *)
(*electric charges*)


Charge[e|_e]= -1;
Charge[\[Nu]|_\[Nu]]= 0;
Charge[u|_u]= +2/3;
Charge[d|_d]= -1/3;


(* ::Subsection:: *)
(*weak isospin 3rd-component*)


WeakIsospin3[_,Right]=0;
WeakIsospin3[u|_u,Left]=+(1/2);
WeakIsospin3[\[Nu]|_ \[Nu],Left]=+(1/2);
WeakIsospin3[d|_d,Left]=-(1/2);
WeakIsospin3[e|_e,Left]=-(1/2);
