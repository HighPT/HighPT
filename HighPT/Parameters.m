(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`Parameters`*)


(* ::Subtitle:: *)
(*Definition of all SM and BSM parameters.*)


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


PackageExport["Param"]


PackageExport["DefineBasisAlignment"]


PackageExport["CKM"]
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


(* ::Section::Closed:: *)
(*Usage messages*)


Param::usage="Param[\"label\"] denotes the parameter specified by \"label\". The defined parameters are: the electromagnetic fine structure constant \"\[Alpha]EM\"; Fermi's constant\"GF\"; the electroweak vacuum expectation value \"vev\"; the sine of the weak mixing angle \"sW\"; the cosine of the weak mixing angle \"cW\"."


Mass::usage= "Mass[\[Phi]] denotes the mass of the particle \[Phi].";


Width::usage= "Width[\[Phi]] denotes the width of the particle \[Phi].";


(* ::Section::Closed:: *)
(*Masses and Widths*)


(* ::Text:: *)
(*Returns an association containing all mediator masses and widths*)


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


(* ::Section::Closed:: *)
(*ReplaceConstants*)


ReplaceConstants::usage= "ReplaceConstants[] returns a list of replacement rules for all constants.";


ReplaceConstants[]:= Join[GetParameters[], ReplaceMassWidth[]]


(* ::Section::Closed:: *)
(*Make constants real*)


$realParameters = Alternatives["vev", "\[Alpha]EM", "sW", "cW", "GF"]


Param/:Conjugate[Param[x:$realParameters]] := Param[x]


Mass/:Conjugate[Mass[a_]]:= Mass[a]


Width/:Conjugate[Width[a_]]:= Width[a]


(* ::Section::Closed:: *)
(*Formatting*)


Format[Mass[f_] , TraditionalForm] := Subscript["M",f]
Format[Width[f_], TraditionalForm] := Subscript["\[CapitalGamma]",f]


Format[Param["vev"], TraditionalForm] := "\[ScriptV]"
Format[Param["\[Alpha]EM"], TraditionalForm] := Subscript["\[Alpha]","EM"]
Format[Param["sW"] , TraditionalForm] := Subscript["s","W"]
Format[Param["cW"] , TraditionalForm] := Subscript["c","W"]
Format[Param["GF"] , TraditionalForm] := Subscript["G","F"]


Format[CKM, TraditionalForm]         := Subscript["V","CKM"]
Format[Vckm[x_,y_], TraditionalForm] := Subscript["V",ToString[x]<>ToString[y]]


(* ::Section::Closed:: *)
(*Experimental Inputs*)


(* ::Subsection::Closed:: *)
(*EW input*)


(* ::Text:: *)
(*List of default parameter values*)


\[Alpha]EM$default = 1/127.9;
GF$default = 1.16637*10^(-5);
mZ$default = 91.1876;
\[CapitalGamma]Z$default = 2.4952;
\[CapitalGamma]W$default = 2.085;


(* ::Subsection::Closed:: *)
(*CKM input*)


\[Lambda]Wolfenstein$default    = 0.22650;
AWolfenstein$default    = 0.790;
\[Rho]BarWolfenstein$default = 0.141;
\[Eta]BarWolfenstein$default = 0.357;

Wolfenstein$default = {
	\[Lambda]Wolfenstein$default,
	AWolfenstein$default,
	\[Rho]BarWolfenstein$default,
	\[Eta]BarWolfenstein$default
}


(* ::Subsubsection::Closed:: *)
(*Define CKM matrix*)


CKM::usage= "CKM denotes the CKM matrix, with CKM[[n,m]] given by Vckm[n,m].";


Vckm::usage= "Vckm[n,m] denotes the element of the CKM matrix in the \!\(\*SuperscriptBox[\(n\), \(th\)]\) row and \!\(\*SuperscriptBox[\(m\), \(th\)]\) column.";


CKM= {
	{Vckm[1,1], Vckm[1,2], Vckm[1,3]},
	{Vckm[2,1], Vckm[2,2], Vckm[2,3]},
	{Vckm[3,1], Vckm[3,2], Vckm[3,3]}
};


(* ::Subsubsection::Closed:: *)
(*Define rotation matrices for left-handed up and down quarks*)


(* By default down alignment is assumed *)
Vu = ConjugateTranspose[CKM]

Vd= {
	{1,0,0},
	{0,1,0},
	{0,0,1}
}


(* ::Subsection::Closed:: *)
(*Save current values of inputs [separate from default values]*)


\[Alpha]EM$current = \[Alpha]EM$default;
GF$current = GF$default;
mZ$current = mZ$default;
\[CapitalGamma]Z$current = \[CapitalGamma]Z$default;
\[CapitalGamma]W$current = \[CapitalGamma]W$default;

\[Lambda]Wolfenstein$current    = \[Lambda]Wolfenstein$default;
AWolfenstein$current    = AWolfenstein$default;
\[Rho]BarWolfenstein$current = \[Rho]BarWolfenstein$default;
\[Eta]BarWolfenstein$current = \[Eta]BarWolfenstein$default;

Wolfenstein$current = {
	\[Lambda]Wolfenstein$current,
	AWolfenstein$current,
	\[Rho]BarWolfenstein$current,
	\[Eta]BarWolfenstein$current
}


(* ::Section:: *)
(*Define alignment of mass basis and flavor basis*)


DefineBasisAlignment::usage=
"DefineBasisAlignment[\"down\"] specifies to work in the down-aligned basis, where \!\(\*SubscriptBox[\(V\), \(d\)]\)=\!\(\*SubscriptBox[\(1\), \(3  x3\)]\) and \!\(\*SubscriptBox[\(V\), \(u\)]\)=\!\(\*SubscriptBox[\(V\), \(CKM\)]\)\[ConjugateTranspose]. The left-handed rotation matrices are defined by \!\(\*SuperscriptBox[SubscriptBox[\(d\), \(i\)], \(mass\)]\)=[\!\(\*SubscriptBox[\(V\), \(d\)]\)\!\(\*SubscriptBox[\(]\), \(ij\)]\)\!\(\*SuperscriptBox[SubscriptBox[\(d\), \(j\)], \(weak\)]\) and \!\(\*SuperscriptBox[SubscriptBox[\(u\), \(i\)], \(mass\)]\)=[\!\(\*SubscriptBox[\(V\), \(u\)]\)\!\(\*SubscriptBox[\(]\), \(ij\)]\)\!\(\*SuperscriptBox[SubscriptBox[\(u\), \(j\)], \(weak\)]\), respectively. Down-alignment is the default choice.
DefineBasisAlignment[\"up\"] specifies to work in the up-aligned basis, where \!\(\*SubscriptBox[\(V\), \(d\)]\)=\!\(\*SubscriptBox[\(V\), \(CKM\)]\) and \!\(\*SubscriptBox[\(V\), \(u\)]\)=\!\(\*SubscriptBox[\(1\), \(3  x3\)]\).
DefineBasisAlignment[matrix] sets the rotation matrix for left-handed down-type quarks \!\(\*SubscriptBox[\(V\), \(d\)]\) equal to the argument matrix, which must be a unitary 3x3 matrix. Consequently the up-rotation matrix is defined by \!\(\*SubscriptBox[\(V\), \(u\)]\)=\!\(\*SubscriptBox[\(V\), \(d\)]\).\!\(\*SubscriptBox[\(V\), \(CKM\)]\)."


(* ::Subsection::Closed:: *)
(*down*)


DefineBasisAlignment[] := DefineBasisAlignment["down"]


DefineBasisAlignment["down"] := Module[{},
	(* set the new Vd matrix*)
	Vd = DiagonalMatrix[{1,1,1}];
	(* define Vu matrix such that CKM=Vu.Vd\[ConjugateTranspose] *)
	Vu = CKM;
	(* Print *)
	Print["Defined new mass basis alignment:"];
	Print["\!\(\*SubscriptBox[\(V\), \(u\)]\) = ", MatrixForm[Vu/.GetParameters[]]];
	Print["\!\(\*SubscriptBox[\(V\), \(d\)]\) = ", MatrixForm[Vd/.GetParameters[]]];
];


(* ::Subsection::Closed:: *)
(*up*)


DefineBasisAlignment["up"] := Module[{},
	(* set the new Vd matrix*)
	Vu = DiagonalMatrix[{1,1,1}];
	(* define Vu matrix such that CKM=Vu.Vd\[ConjugateTranspose] *)
	Vd = ConjugateTranspose[CKM];
	(* Print *)
	Print["Defined new mass basis alignment:"];
	Print["\!\(\*SubscriptBox[\(V\), \(u\)]\) = ", MatrixForm[Vu/.GetParameters[]]];
	Print["\!\(\*SubscriptBox[\(V\), \(d\)]\) = ", MatrixForm[Vd/.GetParameters[]]];
];


(* ::Subsection:: *)
(*general*)


DefineBasisAlignment::notunitary="Warning: Cound not verify that the given matrix is unitary. This might happen if the matrix elements are not numeric."


DefineBasisAlignment::invalidarg="The argument `1` is not a 3x3 matrix."


(* function that defines a down aligned basis  *)
DefineBasisAlignment[matrix_ /; (Dimensions[matrix]==={3,3})] := Module[{},
	(* check unitarity *)
	If[!UnitaryMatrixQ[matrix/.GetParameters[], Tolerance->10^-2],
		Message[DefineBasisAlignment::notunitary]
	];
	(* set the new Vd matrix*)
	Vd = matrix;
	(* define Vu matrix such that CKM=Vu\[ConjugateTranspose].Vd *)
	Vu = CKM . Vd;
	(* Print *)
	Print["Defined new mass basis alignment:"];
	Print["\!\(\*SubscriptBox[\(V\), \(u\)]\) = ", MatrixForm[Vu/.GetParameters[]]];
	Print["\!\(\*SubscriptBox[\(V\), \(d\)]\) = ", MatrixForm[Vd/.GetParameters[]]];
];


DefineBasisAlignment[arg:Except["up"|"down"]/;(Dimensions[arg]=!={3,3})] := (Message[DefineBasisAlignment::invalidarg,arg/.GetParameters[]];Abort[])


(* ::Section::Closed:: *)
(*DefineParameters*)


DefineParameters::usage= "DefineParameters[] defines all SM parameters. The electroweak input scheme \"\[Alpha]EM\", \"GF\", \"mZ\" is used, whereas for the CKM the input is given by the Wolfentein parameters \[Lambda], A, \!\(\*OverscriptBox[\(\[Rho]\), \(_\)]\), \!\(\*OverscriptBox[\(\[Eta]\), \(_\)]\). The allowed Options are \"\[Alpha]EM\", \"GF\", \"mZ\", \"\[CapitalGamma]Z\", \"\[CapitalGamma]W\", and \"Wolfenstein\", where, e.g., the latter should be given as \"Wolfenstein\" \[Rule] {\[Lambda],A,\!\(\*OverscriptBox[\(\[Rho]\), \(_\)]\),\!\(\*OverscriptBox[\(\[Eta]\), \(_\)]\)}. All other OptionValues must be given as numbers. The input scheme uses the parameters \"\[Alpha]EM\", \"GF\", \"mZ\", \"\[CapitalGamma]Z\", \"\[CapitalGamma]W\", \"Wolfenstein\" which can be specified via Options. The masses and widths of BSM mediators can be modified via the Mediators Option. For example Mediator \[Rule] {\"U1\"\[Rule]{2000,20}} would change the mass of a \!\(\*SubscriptBox[\(U\), \(1\)]\) leptoquark to 2 TeV and its width to 20 GeV. If some parameters are not specified their current value is maintained. To obtain the current values of the parameters the routine GetParameters[] can be used.
DefineParameters[Default] resets all parameters to the HighPT default values. For BSM mediator masses and widths this corresponds to the value given in the last call of InitializeModel.";


(* keep current values if not specified *)
Options[DefineParameters]= {
	"\[Alpha]EM"         :> \[Alpha]EM$current,
	"GF"          :> GF$current,
	"mZ"          :> mZ$current,
	"\[CapitalGamma]Z"          :> \[CapitalGamma]Z$current,
	"\[CapitalGamma]W"          :> \[CapitalGamma]W$current,
	"Wolfenstein" :> Wolfenstein$current,
	
	Mediators     -> {}
};


(* reset all parameters to their default values *)
DefineParameters[Default] := DefineParameters[
	"\[Alpha]EM"         -> \[Alpha]EM$default,
	"GF"          -> GF$default,
	"mZ"          -> mZ$default,
	"\[CapitalGamma]Z"          -> \[CapitalGamma]Z$default,
	"\[CapitalGamma]W"          -> \[CapitalGamma]W$default,
	"Wolfenstein" -> Wolfenstein$default,
	
	Mediators     :> $defaultMediatorProperties
]


DefineParameters[OptionsPattern[]] := Module[
	{
		(* input *)
		$\[Alpha]EM         = OptionValue["\[Alpha]EM"],
		$GF          = OptionValue["GF"],
		$mZ          = OptionValue["mZ"],
		$\[CapitalGamma]Z          = OptionValue["\[CapitalGamma]Z"],
		$\[CapitalGamma]W          = OptionValue["\[CapitalGamma]W"],
		$Wolfenstein = OptionValue["Wolfenstein"],
		
		(* output *)
		$mW, $sW, $vev,
		$\[Lambda], $A, $\[Rho]Bar, $\[Eta]Bar, $\[Rho], $\[Eta],
		
		$mediator         = OptionValue[Mediators]/.Association->List,
		mediators$current = GetMediators[]
	}
	,
	(* OPTION CHECKS *)
	OptionCheck[#,OptionValue[#]]& /@ {"\[Alpha]EM", "GF", "mZ", "\[CapitalGamma]Z", "\[CapitalGamma]W", "Wolfenstein", Mediators};
	(* check that all mediator labels are known *)
	Do[
		If[!MatchQ[med, Alternatives@@Keys[$MediatorList]],
			Message[InitializeModel::undefmed, med, Keys[$MediatorList]];
			Abort[]
		]
		,
		{med, Keys@$mediator}
	];
	
	(* save current values of parameters or change to default value if required *)
	\[Alpha]EM$current = If[MatchQ[$\[Alpha]EM, Default], $\[Alpha]EM = \[Alpha]EM$default, $\[Alpha]EM];
	GF$current  = If[MatchQ[$GF,  Default], $GF  = GF$default , $GF ];
	mZ$current  = If[MatchQ[$mZ,  Default], $mZ  = mZ$default , $mZ ];
	\[CapitalGamma]Z$current  = If[MatchQ[$\[CapitalGamma]Z,  Default], $\[CapitalGamma]Z  = \[CapitalGamma]Z$default , $\[CapitalGamma]Z ];
	\[CapitalGamma]W$current  = If[MatchQ[$\[CapitalGamma]W,  Default], $\[CapitalGamma]W  = \[CapitalGamma]W$default , $\[CapitalGamma]W ];
	
	{$\[Lambda], $A, $\[Rho]Bar, $\[Eta]Bar} = $Wolfenstein;
	\[Lambda]Wolfenstein$current    = If[MatchQ[$\[Lambda],    Default], $\[Lambda]    = \[Lambda]Wolfenstein$default   , $\[Lambda]   ];
	AWolfenstein$current    = If[MatchQ[$A,    Default], $A    = AWolfenstein$default   , $A   ];
	\[Rho]BarWolfenstein$current = If[MatchQ[$\[Rho]Bar, Default], $\[Rho]Bar = \[Rho]BarWolfenstein$default, $\[Rho]Bar];
	\[Eta]BarWolfenstein$current = If[MatchQ[$\[Eta]Bar, Default], $\[Eta]Bar = \[Eta]BarWolfenstein$default, $\[Eta]Bar];
	Wolfenstein$current = {
		\[Lambda]Wolfenstein$current,
		AWolfenstein$current,
		\[Rho]BarWolfenstein$current,
		\[Eta]BarWolfenstein$current
	};
	
	(* set the non-bared Wolfentein parameters *)
	$\[Rho] = $\[Rho]Bar/(1-$\[Lambda]^2/2);
	$\[Eta] = $\[Eta]Bar/(1-$\[Lambda]^2/2);
	
	(* compute the non-input parameters *)
	$mW = Sqrt[$mZ^2/2.+Sqrt[$mZ^4/4.-($\[Alpha]EM*\[Pi]*$mZ^2)/($GF*Sqrt[2])]];
	$sW = Sqrt[1. - $mW^2/$mZ^2];
	$vev = ($mW*$sW)/Sqrt[\[Pi]*$\[Alpha]EM];
	
	(* BSM mediators *)
	If[$mediator===Default, $mediator=$defaultMediatorProperties];
	(* built current BSM mediator assoc *)
	mediators$current = Association@Table[
		med -> {mediators$current[med][Mass],mediators$current[med][Width]}
		,
		{med, Keys@mediators$current}
	];
	
	(* overwrite with new definitions *)
	AssociateTo[mediators$current, $mediator];
	AssociateTo[mediators$current, {"ZBoson"->{$mZ,$\[CapitalGamma]Z}, "WBoson"->{$mW,$\[CapitalGamma]W}}];
	
	(* check mass and width *)
	Do[
		If[!MatchQ[First[mediators$current[mediator]], $AllowedMasses],
			Message[InitializeModel::undefmass, mediator, First[mediators$current[mediator]], List@@$AllowedMasses];
		];
		(* allow for all widths *)
		(*If[!MatchQ[Last[mediators$current[mediator]], 0],
			Message[InitializeModel::undefwidth];
		]*)
		,
		{mediator, Keys@KeyDrop[mediators$current,{"Photon","ZBoson","WBoson"}]}
	];	
	
	(* Modify all masses and widths *)
	ModifyMediator[Mediators->mediators$current];
	
	(* Create the appropriate supstitution rule *)
	ExperimentalParameters = <|
		Param["\[Alpha]EM"] -> $\[Alpha]EM,
		Param["vev"] -> $vev,
		Param["sW"]  -> $sW,
		Param["cW"]  -> Sqrt[1. - $sW^2],
		(* CKM *)
		Vckm[1,1] -> 1-$\[Lambda]^2/2,
		Vckm[1,2] -> $\[Lambda],
		Vckm[1,3] -> $A * $\[Lambda]^3 * ($\[Rho] - I*$\[Eta]),
		
		Vckm[2,1] -> -$\[Lambda],
		Vckm[2,2] -> 1-$\[Lambda]^2/2,
		Vckm[2,3] -> $A * $\[Lambda]^2,
		
		Vckm[3,1] -> $A * $\[Lambda]^3 * (1 - $\[Rho] - I*$\[Eta]),
		Vckm[3,2] -> -$A * $\[Lambda]^2,
		Vckm[3,3] -> 1
		,
		(* masses & widths *)
		Mass["ZBoson"]         -> $mZ,
		Width["ZBoson"]        -> $\[CapitalGamma]Z,
		Mass["WBoson"]         -> $mW,
		Width["WBoson"]        -> $\[CapitalGamma]W,
		Mass["Photon"]         -> 0,
		Width["Photon"]        -> 0
	|>;
]


ExperimentalParameters= <||>;


(* initialize the parameters with default values *)
DefineParameters[Default]


(* ::Section::Closed:: *)
(*GetParameters*)


GetParameters::usage= "GetParameters[] returns an Association of all (B)SM parameters and their values. To modify the values use the DefineParameters routine.";


(* returns the current value of the (B)SM parameters *)
GetParameters[]:= Join[ExperimentalParameters, ReplaceMassWidth[]]


(* ::Section::Closed:: *)
(*Charge definitions*)


(* ::Subsection::Closed:: *)
(*electric charges*)


Charge[e|_e] = -1;
Charge[\[Nu]|_\[Nu]] = 0;
Charge[u|_u] = +2/3;
Charge[d|_d] = -1/3;


(* ::Subsection::Closed:: *)
(*weak isospin 3rd-component*)


WeakIsospin3[_,Right]    = 0;
WeakIsospin3[u|_u,Left]  = +(1/2);
WeakIsospin3[\[Nu]|_ \[Nu],Left] = +(1/2);
WeakIsospin3[d|_d,Left]  = -(1/2);
WeakIsospin3[e|_e,Left]  = -(1/2);
