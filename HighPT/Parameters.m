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


PackageExport["GetBasisAlignment"]


PackageExport["CKM"]
PackageExport["Vckm"]


PackageExport["Yukawa"]


PackageExport["Errors"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["ReplaceConstants"]


PackageScope["Charge"]
PackageScope["WeakIsospin3"]


PackageScope["Vu"]
PackageScope["Vd"]


PackageScope["\[CapitalGamma]Z$default"]
PackageScope["\[CapitalGamma]W$default"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Usage messages*)


Param::usage="Param[\"label\"] denotes the parameter specified by \"label\". The defined parameters are: the electromagnetic fine structure constant \"\[Alpha]EM\"; Fermi's constant\"GF\"; the electroweak vacuum expectation value \"vev\"; the sine of the weak mixing angle \"sW\"; the cosine of the weak mixing angle \"cW\"."


Mass::usage= "Mass[\[Phi]] denotes the mass of the particle \[Phi].";


Width::usage= "Width[\[Phi]] denotes the width of the particle \[Phi].";


Yukawa::usage="Yukawa[\"label\",{i,j}] denotes the {i,j} entry of the Yukawa matrix. Allowed values for \"label\" are \"u\", \"d\", \"e\"";


(* ::Section:: *)
(*Masses and Widths*)


(* ::Text:: *)
(*Returns an association containing all mediator masses and widths*)


Options[ReplaceMassWidth]={
	Errors -> False
};


ReplaceMassWidth[OptionsPattern[]]:= Module[
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
	If[MatchQ[OptionValue[Errors],True],
		Return[replacements],
		Return[replacements/.Around[a_,b_]->a]
	];
]


(* ::Section:: *)
(*ReplaceConstants*)


ReplaceConstants::usage= "ReplaceConstants[] returns a list of replacement rules for all constants.";


ReplaceConstants[]:= Join[GetParameters[], ReplaceMassWidth[]]


(* ::Section:: *)
(*Make constants real*)


$realParameters = Alternatives["vev", "\[Alpha]EM", "sW", "cW", "GF", "\[Alpha]S"]


Param/:Conjugate[Param[x:$realParameters]] := Param[x]


Mass/:Conjugate[Mass[a_]]:= Mass[a]


Width/:Conjugate[Width[a_]]:= Width[a]


(* ::Section:: *)
(*Yukawas*)


Yukawa[l:Except[Alternatives@@Join[{"u","d","e"}, {_Pattern, _Blank, _Except, _BlankNullSequence, _BlankSequence}]],___]:=(
	Message[Yukawa::unknownYukawa,l];
	Abort[]
)


(* ::Section:: *)
(*Formatting*)


Format[Mass[f_] , TraditionalForm] := Subscript["M",f]
Format[Width[f_], TraditionalForm] := Subscript["\[CapitalGamma]",f]


Format[Param["vev"], TraditionalForm] := "\[ScriptV]"
Format[Param["\[Alpha]EM"], TraditionalForm] := Subscript["\[Alpha]","EM"]
Format[Param["sW"] , TraditionalForm] := Subscript["s","W"]
Format[Param["cW"] , TraditionalForm] := Subscript["c","W"]
Format[Param["GF"] , TraditionalForm] := Subscript["G","F"]
Format[Param["\[Alpha]S"] , TraditionalForm] := Subscript["\[Alpha]","s"]
Format[Param["g3"] , TraditionalForm] := Subscript["g","3"]
Format[Param["g2"] , TraditionalForm] := Subscript["g","2"]
Format[Param["g1"] , TraditionalForm] := Subscript["g","1"]
Format[Param["\[Lambda]"]  , TraditionalForm] := "\[Lambda]"


Format[CKM, TraditionalForm]         := Subscript["V","CKM"]
Format[Vckm[x_,y_], TraditionalForm] := Subscript["V",ToString[x]<>ToString[y]]


Format[Yukawa[l_,{i_,j_}], TraditionalForm] := Subscript["[Y"<>l<>"]",ToString[i]<>ToString[j]]


Format[Param["Wolfenstein\[Lambda]"],TraditionalForm] := "\[Lambda]"
Format[Param["WolfensteinA"],TraditionalForm] := "A"
Format[Param["Wolfenstein\[Rho]bar"],TraditionalForm] := OverBar["\[Rho]"]
Format[Param["Wolfenstein\[Eta]bar"],TraditionalForm] := OverBar["\[Eta]"]


(* ::Section:: *)
(*Experimental Inputs*)


(* ::Subsection:: *)
(*EW input*)


(* ::Text:: *)
(*List of default parameter values*)


\[Alpha]EM$default = Around[127.925,0.016]^-1;
GF$default = Around[1.1663787*10^(-5),0.0000006*10^(-5)];
mZ$default = Around[91.1876,0.0026];
\[CapitalGamma]Z$default = Around[2.4955,0.0023];
\[CapitalGamma]W$default = Around[2.085,0.042];


(*\[Lambda]$default  = 0.2813;*)
mH$default = Around[125.25,0.17]; 


(* ::Subsection:: *)
(*QCD input*)


\[Alpha]S$default = Around[0.1179,0.0010];


(* ::Subsection:: *)
(*CKM input*)


\[Lambda]Wolfenstein$default    = Around[0.2217,0.0009];
AWolfenstein$default    = Around[0.822,0.009];
\[Rho]BarWolfenstein$default = Around[0.154,0.030];
\[Eta]BarWolfenstein$default = Around[0.396,0.023];

Wolfenstein$default = {
	\[Lambda]Wolfenstein$default,
	AWolfenstein$default,
	\[Rho]BarWolfenstein$default,
	\[Eta]BarWolfenstein$default
}


(* ::Subsubsection:: *)
(*Define CKM matrix*)


CKM::usage= "CKM denotes the CKM matrix, with CKM[[n,m]] given by Vckm[n,m].";


Vckm::usage= "Vckm[n,m] denotes the element of the CKM matrix in the \!\(\*SuperscriptBox[\(n\), \(th\)]\) row and \!\(\*SuperscriptBox[\(m\), \(th\)]\) column.";


CKM= {
	{Vckm[1,1], Vckm[1,2], Vckm[1,3]},
	{Vckm[2,1], Vckm[2,2], Vckm[2,3]},
	{Vckm[3,1], Vckm[3,2], Vckm[3,3]}
};


(* ::Subsubsection:: *)
(*Define rotation matrices for left-handed up and down quarks*)


(* By default down alignment is assumed *)
Vu = CKM

Vd= {
	{1,0,0},
	{0,1,0},
	{0,0,1}
}


(* ::Subsection:: *)
(*Flavor input*)


(* ::Subsubsection:: *)
(*Masses*)


(* ::Text:: *)
(*Leptons*)


me$default = Around[0.510998928 10^-3,0.00000000015 10^-3]
m\[Mu]$default = Around[.105658357,0.0000000023];
m\[Tau]$default = Around[1.77686,0.00012];


(* ::Text:: *)
(*Quarks (MSbar, PDG)*)


(* at 2 GeV *)
md$default = Around[0.00467,{0.00017,0.00048}];
ms$default = Around[0.093,{0.0034,0.0086}];
(* at mb *)
mb$default = Around[4.18,{0.02,0.03}];


(* at 2 GeV *)
mu$default = Around[0.00216,{000026,0.00049}];
(* at mc *)
mc$default = Around[1.27,0.02];
(* at mt *)
mt$default = Around[162.5,{1.5,2.1}];


(* ::Text:: *)
(*Mesons (PDG)*)


(* pions *)
m\[Pi]plus$default = Around[0.13957039,0.00000018];
m\[Pi]0$default = Around[0.1349768,0.0000005];
(* kaons *)
mKplus$default = Around[0.493677,0.000016];
mK0$default = Around[0.497611,0.000013];
(* D *)
mDplus$default = Around[1.86966,0.00005];
mD0$default = Around[1.86484,0.00005];
mDs$default = Around[1.96835,0.00007];
(* B *)
mBd$default = Around[5.27966,0.00012];
mBs$default = Around[5.36692,0.00010];
mBc$default = Around[6.27447,0.00032];


(* ::Subsection:: *)
(*Save current values of inputs [separate from default values]*)


\[Alpha]EM$current = \[Alpha]EM$default;
GF$current = GF$default;
mZ$current = mZ$default;
\[CapitalGamma]Z$current = \[CapitalGamma]Z$default;
\[CapitalGamma]W$current = \[CapitalGamma]W$default;

(*\[Lambda]$current  = \[Lambda]$default;*)
mH$current = mH$default;

\[Alpha]S$current = \[Alpha]S$default;

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


me$current = me$default;
m\[Mu]$current = m\[Mu]$default;
m\[Tau]$current = m\[Tau]$default;

md$current = md$default;
ms$current = ms$default;
mb$current = mb$default;

mu$current = mu$default;
mc$current = mc$default;
mt$current = mt$default;

m\[Pi]plus$current = m\[Pi]plus$default;
m\[Pi]0$current = m\[Pi]0$default;

mKplus$current = mKplus$default;
mK0$current = mK0$default;

mDplus$current = mDplus$default;
mD0$current = mD0$default;
mDs$current = mDs$default;

mBd$current = mBd$default;
mBs$current = mBs$default;
mBc$current = mBc$default;


(* ::Section:: *)
(*Define alignment of mass basis and flavor basis*)


BasisAlignment$default = "down";


BasisAlignment$current = BasisAlignment$default;


GetBasisAlignment[] := BasisAlignment$current


DefineBasisAlignment::usage=
"DefineBasisAlignment[\"down\"] specifies to work in the down-aligned basis, where \!\(\*SubscriptBox[\(V\), \(d\)]\)=\!\(\*SubscriptBox[\(1\), \(3  x3\)]\) and \!\(\*SubscriptBox[\(V\), \(u\)]\)=\!\(\*SubscriptBox[\(V\), \(CKM\)]\)\[ConjugateTranspose]. The left-handed rotation matrices are defined by \!\(\*SuperscriptBox[SubscriptBox[\(d\), \(i\)], \(mass\)]\)=[\!\(\*SubscriptBox[\(V\), \(d\)]\)\!\(\*SubscriptBox[\(]\), \(ij\)]\)\!\(\*SuperscriptBox[SubscriptBox[\(d\), \(j\)], \(weak\)]\) and \!\(\*SuperscriptBox[SubscriptBox[\(u\), \(i\)], \(mass\)]\)=[\!\(\*SubscriptBox[\(V\), \(u\)]\)\!\(\*SubscriptBox[\(]\), \(ij\)]\)\!\(\*SuperscriptBox[SubscriptBox[\(u\), \(j\)], \(weak\)]\), respectively. Down-alignment is the default choice.
DefineBasisAlignment[\"up\"] specifies to work in the up-aligned basis, where \!\(\*SubscriptBox[\(V\), \(d\)]\)=\!\(\*SubscriptBox[\(V\), \(CKM\)]\) and \!\(\*SubscriptBox[\(V\), \(u\)]\)=\!\(\*SubscriptBox[\(1\), \(3  x3\)]\).
DefineBasisAlignment[matrix] sets the rotation matrix for left-handed down-type quarks \!\(\*SubscriptBox[\(V\), \(d\)]\) equal to the argument matrix, which must be a unitary 3x3 matrix. Consequently the up-rotation matrix is defined by \!\(\*SubscriptBox[\(V\), \(u\)]\)=\!\(\*SubscriptBox[\(V\), \(d\)]\).\!\(\*SubscriptBox[\(V\), \(CKM\)]\)."


(* ::Subsection:: *)
(*down*)


DefineBasisAlignment[] := DefineBasisAlignment["down"]


DefineBasisAlignment[Default] := DefineBasisAlignment["down"]


DefineBasisAlignment["down"] := Module[{},
	BasisAlignment$current = "down";
	(* set the new Vd matrix*)
	Vd = DiagonalMatrix[{1,1,1}];
	(* define Vu matrix such that CKM=Vu\[ConjugateTranspose].Vd *)
	Vu = CKM;
	(* Print *)
	Print["Defined new mass basis alignment:"];
	Print["\!\(\*SubscriptBox[\(V\), \(u\)]\) = ", MatrixForm[Vu/.GetParameters[]]];
	Print["\!\(\*SubscriptBox[\(V\), \(d\)]\) = ", MatrixForm[Vd/.GetParameters[]]];
];


(* ::Subsection:: *)
(*up*)


DefineBasisAlignment["up"] := Module[{},
	BasisAlignment$current = "up";
	(* set the new Vd matrix*)
	Vu = DiagonalMatrix[{1,1,1}];
	(* define Vu matrix such that CKM=Vu\[ConjugateTranspose].Vd *)
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
	BasisAlignment$current = "custom";
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


(* ::Section:: *)
(*DefineParameters*)


DefineParameters::usage= "DefineParameters[] defines all SM parameters. The electroweak input scheme \"\[Alpha]EM\", \"GF\", \"mZ\" is used, whereas for the CKM the input is given by the Wolfentein parameters \[Lambda], A, \!\(\*OverscriptBox[\(\[Rho]\), \(_\)]\), \!\(\*OverscriptBox[\(\[Eta]\), \(_\)]\). The allowed Options are \"\[Alpha]EM\", \"GF\", \"mZ\", \"\[CapitalGamma]Z\", \"\[CapitalGamma]W\", \"\[Lambda]\", \"mH\", \"\[Alpha]S\", \"me\", \"m\[Mu]\", \"m\[Tau]\", \"md\", \"ms\", \"mb\", \"mu\", \"mc\", \"mt\", \"m\[Pi]+\", \"m\[Pi]0\", \"mK+\", \"mK0\", \"mD+\", \"mD0\", \"mDs\", \"mBd\", \"mBs\", \"mBc\", and \"Wolfenstein\", where, e.g., the latter should be given as \"Wolfenstein\" \[Rule] {\[Lambda],A,\!\(\*OverscriptBox[\(\[Rho]\), \(_\)]\),\!\(\*OverscriptBox[\(\[Eta]\), \(_\)]\)}. All other OptionValues must be given as numbers. The input scheme uses the parameters \"\[Alpha]EM\", \"GF\", \"mZ\", \"\[CapitalGamma]Z\", \"\[CapitalGamma]W\", \"Wolfenstein\" which can be specified via Options. The masses and widths of BSM mediators can be modified via the Mediators Option. For example Mediator \[Rule] {\"U1\"\[Rule]{2000,20}} would change the mass of a \!\(\*SubscriptBox[\(U\), \(1\)]\) leptoquark to 2 TeV and its width to 20 GeV. If some parameters are not specified their current value is maintained. To obtain the current values of the parameters the routine GetParameters[] can be used.
DefineParameters[Default] resets all parameters to the HighPT default values. For BSM mediator masses and widths this corresponds to the value given in the last call of InitializeModel.";


(* keep current values if not specified *)
Options[DefineParameters]= {
	"\[Alpha]EM"         :> \[Alpha]EM$current,
	"GF"          :> GF$current,
	"mZ"          :> mZ$current,
	"\[CapitalGamma]Z"          :> \[CapitalGamma]Z$current,
	"\[CapitalGamma]W"          :> \[CapitalGamma]W$current,
	(*"\[Lambda]"           :> \[Lambda]$current,*)
	"mH"          :> mH$current,
	"\[Alpha]S"          :> \[Alpha]S$current,
	"Wolfenstein" :> Wolfenstein$current,
	
	Mediators     -> {},
	
	"me"          :> me$current,
	"m\[Mu]"          :> m\[Mu]$current,
	"m\[Tau]"          :> m\[Tau]$current,
	"md"          :> md$current,
	"ms"          :> ms$current,
	"mb"          :> mb$current,
	"mu"          :> mu$current,
	"mc"          :> mc$current,
	"mt"          :> mt$current,
	"m\[Pi]+"         :> m\[Pi]plus$current,
	"m\[Pi]0"         :> m\[Pi]0$current,
	"mK+"         :> mKplus$current,
	"mK0"         :> mK0$current,
	"mD+"         :> mDplus$current,
	"mD0"         :> mD0$current,
	"mDs"         :> mDs$current,
	"mBd"         :> mBd$current,
	"mBs"         :> mBs$current,
	"mBc"         :> mBc$current
};


(* reset all parameters to their default values *)
DefineParameters[Default] := DefineParameters[
	"\[Alpha]EM"         -> \[Alpha]EM$default,
	"GF"          -> GF$default,
	"mZ"          -> mZ$default,
	"\[CapitalGamma]Z"          -> \[CapitalGamma]Z$default,
	"\[CapitalGamma]W"          -> \[CapitalGamma]W$default,
	(*"\[Lambda]"           -> \[Lambda]$default,*)
	"mH"          -> mH$default,
	"\[Alpha]S"          -> \[Alpha]S$default,
	"Wolfenstein" -> Wolfenstein$default,
	
	Mediators     :> $defaultMediatorProperties,
	
	"me"          -> me$default,
	"m\[Mu]"          -> m\[Mu]$default,
	"m\[Tau]"          -> m\[Tau]$default,
	"md"          -> md$default,
	"ms"          -> ms$default,
	"mb"          -> mb$default,
	"mu"          -> mu$default,
	"mc"          -> mc$default,
	"mt"          -> mt$default,
	"m\[Pi]+"         -> m\[Pi]plus$default,
	"m\[Pi]0"         -> m\[Pi]0$default,
	"mK+"         -> mKplus$default,
	"mK0"         -> mK0$default,
	"mD+"         -> mDplus$default,
	"mD0"         -> mD0$default,
	"mDs"         -> mDs$default,
	"mBd"         -> mBd$default,
	"mBs"         -> mBs$default,
	"mBc"         -> mBc$default
]


DefineParameters[OptionsPattern[]] := Module[
	{
		(* input *)
		$\[Alpha]EM         = OptionValue["\[Alpha]EM"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$GF          = OptionValue["GF"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mZ          = OptionValue["mZ"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$\[CapitalGamma]Z          = OptionValue["\[CapitalGamma]Z"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$\[CapitalGamma]W          = OptionValue["\[CapitalGamma]W"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		(*$\[Lambda]H          = OptionValue["\[Lambda]"],*)
		$mH          = OptionValue["mH"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$\[Alpha]S          = OptionValue["\[Alpha]S"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$Wolfenstein = OptionValue["Wolfenstein"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		
		$me          = OptionValue["me"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$m\[Mu]          = OptionValue["m\[Mu]"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$m\[Tau]          = OptionValue["m\[Tau]"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$md          = OptionValue["md"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$ms          = OptionValue["ms"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mb          = OptionValue["mb"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mu          = OptionValue["mu"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mc          = OptionValue["mc"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mt          = OptionValue["mt"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$m\[Pi]plus      = OptionValue["m\[Pi]+"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$m\[Pi]0         = OptionValue["m\[Pi]0"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mKplus      = OptionValue["mK+"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mK0         = OptionValue["mK0"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mDplus      = OptionValue["mD+"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mD0         = OptionValue["mD0"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mDs         = OptionValue["mDs"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mBd         = OptionValue["mBd"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mBs         = OptionValue["mBs"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mBc         = OptionValue["mBc"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		
		(* output *)
		$mW, $sW, $vev, $g2, $g1, $\[Lambda]H,
		$g3,
		$\[Lambda], $A, $\[Rho]Bar, $\[Eta]Bar, $\[Rho], $\[Eta],
		$Yu, $Yd, $Ye, $ckmrep,
		
		$mediator         = OptionValue[Mediators]/.Association->List,
		mediators$current = GetMediators[]
	}
	,
	(* OPTION CHECKS *)
	OptionCheck[#,OptionValue[#]]& /@ {"\[Alpha]EM", "GF", "mZ", "\[CapitalGamma]Z", "\[CapitalGamma]W"(*, "\[Lambda]"*), "mH", "\[Alpha]S", "Wolfenstein", Mediators, "me", "m\[Mu]", "m\[Tau]", "md", "ms", "mb", "mu", "mc", "mt", "m\[Pi]+", "m\[Pi]0", "mK+", "mK0", "mD+", "mD0", "mDs", "mBd", "mBs", "mBc"};
	(* check that all mediator labels are known *)
	Do[
		If[!MatchQ[med, Alternatives@@Keys[$MediatorList]],
			Message[InitializeModel::undefmed, med, Keys[$MediatorList]];
			Abort[]
		]
		,
		{med, Keys@$mediator}
	];
	
	(*Do[
		If[NumericQ[par],par = Around[par,null]],
		{par,{$\[Alpha]EM,$GF,$mZ,$\[CapitalGamma]Z,$\[CapitalGamma]W,(*$\[Lambda]H,*)$mH,$\[Alpha]S(*,$Wolfenstein*),$me,$m\[Mu],$m\[Tau],$md,$ms,$mb,$mu,$mc,$mt,$m\[Pi]plus,$m\[Pi]0,$mKplus,$mK0,$mDplus,$mD0,$mDs,$mBd,$mBs,$mBc}}
	]*)
	
	(* save current values of parameters or change to default value if required *)
	\[Alpha]EM$current = If[MatchQ[$\[Alpha]EM, Default], $\[Alpha]EM = \[Alpha]EM$default, $\[Alpha]EM];
	GF$current  = If[MatchQ[$GF,  Default], $GF  = GF$default , $GF];
	mZ$current  = If[MatchQ[$mZ,  Default], $mZ  = mZ$default , $mZ];
	\[CapitalGamma]Z$current  = If[MatchQ[$\[CapitalGamma]Z,  Default], $\[CapitalGamma]Z  = \[CapitalGamma]Z$default , $\[CapitalGamma]Z];
	\[CapitalGamma]W$current  = If[MatchQ[$\[CapitalGamma]W,  Default], $\[CapitalGamma]W  = \[CapitalGamma]W$default , $\[CapitalGamma]W];
	\[Alpha]S$current  = If[MatchQ[$\[Alpha]S, Default], $\[Alpha]S = \[Alpha]S$default, $\[Alpha]S];
	
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
	
	me$current     = If[MatchQ[$me,  Default], $me  = me$default , $me];
	m\[Mu]$current     = If[MatchQ[$m\[Mu],  Default], $m\[Mu]  = m\[Mu]$default , $m\[Mu]];
	m\[Tau]$current     = If[MatchQ[$m\[Tau],  Default], $m\[Tau]  = m\[Tau]$default , $m\[Tau]];
	md$current     = If[MatchQ[$md,  Default], $md  = md$default , $md];
	ms$current     = If[MatchQ[$ms,  Default], $ms  = m\[Mu]$default , $ms];
	mb$current     = If[MatchQ[$mb,  Default], $mb  = mb$default , $mb];
	mu$current     = If[MatchQ[$mu,  Default], $mu  = mu$default , $mu];
	mc$current     = If[MatchQ[$mc,  Default], $mc  = mc$default , $mc];
	mt$current     = If[MatchQ[$mt,  Default], $mt  = mt$default , $mt];
	m\[Pi]plus$current = If[MatchQ[$m\[Pi]plus,  Default], $m\[Pi]plus  = m\[Pi]plus$default , $m\[Pi]plus];
	m\[Pi]0$current    = If[MatchQ[$m\[Pi]0,  Default], $m\[Pi]0  = m\[Pi]0$default , $m\[Pi]0];
	mKplus$current = If[MatchQ[$mKplus,  Default], $mKplus  = mKplus$default , $mKplus];
	mK0$current    = If[MatchQ[$mK0,  Default], $mK0  = mK0$default , $mK0];
	mDplus$current = If[MatchQ[$mDplus,  Default], $mDplus  = mDplus$default , $mDplus];
	mD0$current    = If[MatchQ[$mD0,  Default], $mD0  = mD0$default , $mD0];
	mDs$current    = If[MatchQ[$mDs,  Default], $mDs  = mDs$default , $mDs];
	mBd$current    = If[MatchQ[$mBd,  Default], $mBd  = mBd$default , $mBd];
	mBs$current    = If[MatchQ[$mBs,  Default], $mBs  = mBs$default , $mBs];
	mBc$current    = If[MatchQ[$mBc,  Default], $mBc  = mBc$default , $mBc];
	
	(* set the non-bared Wolfentein parameters *)
	$\[Rho] = $\[Rho]Bar/(1-$\[Lambda]^2/2);
	$\[Eta] = $\[Eta]Bar/(1-$\[Lambda]^2/2);
	
	(* compute the non-input parameters *)
	$mW = Sqrt[$mZ^2/2.+Sqrt[$mZ^4/4.-($\[Alpha]EM*\[Pi]*$mZ^2)/($GF*Sqrt[2])]];
	$sW = Sqrt[1. - $mW^2/$mZ^2];
	$vev = ($mW*$sW)/Sqrt[\[Pi]*$\[Alpha]EM];
	$g1 = Sqrt[4\[Pi] $\[Alpha]EM]/Sqrt[1-$sW^2];
	$g2 = Sqrt[4\[Pi] $\[Alpha]EM]/$sW;
	$g3 = Sqrt[4\[Pi] $\[Alpha]S];
	$\[Lambda]H = $mH^2/$vev^2;
	
	(*$ckmrep={
		Vckm[1,1] -> 1-$\[Lambda]^2/2,
		Vckm[1,2] -> $\[Lambda],
		Vckm[1,3] -> $A * $\[Lambda]^3 * ($\[Rho] - I*$\[Eta]),
		Vckm[2,1] -> -$\[Lambda],
		Vckm[2,2] -> 1-$\[Lambda]^2/2,
		Vckm[2,3] -> $A * $\[Lambda]^2,
		Vckm[3,1] -> $A * $\[Lambda]^3 * (1 - $\[Rho] - I*$\[Eta]),
		Vckm[3,2] -> -$A * $\[Lambda]^2,
		Vckm[3,3] -> 1
		};*)
	$ckmrep=WolfensteinParametrization[$\[Lambda],$A,$\[Rho],$\[Eta]];
	$Yu = ((Sqrt[2]/$vev*DiagonalMatrix[{$mu,$mc,$mt}]) . Vu)/.$ckmrep;
	$Yd = ((Sqrt[2]/$vev*DiagonalMatrix[{$md,$md,$mb}]) . Vd)/.$ckmrep;
	$Ye = Sqrt[2]/$vev*DiagonalMatrix[{$me,$m\[Mu],$m\[Tau]}];
	
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
	
	(* Create the appropriate substitution rule *)
	ExperimentalParameters = <|
		Param["\[Alpha]EM"] -> $\[Alpha]EM,
		Param["GF"]  -> $GF,
		Param["vev"] -> $vev,
		Param["sW"]  -> $sW,
		Param["cW"]  -> Sqrt[1. - $sW^2],
		Param["\[Lambda]"]   -> $\[Lambda]H,
		Param["g1"]  -> $g1,
		Param["g2"]  -> $g2,
		Param["\[Alpha]S"]  -> $\[Alpha]S,
		Param["g3"]  -> $g3,
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
		
		(* Wolfenstein *)
		Param["Wolfenstein\[Lambda]"]    -> $\[Lambda],
		Param["WolfensteinA"]    -> $A,
		Param["Wolfenstein\[Rho]bar"] -> $\[Rho],
		Param["Wolfenstein\[Eta]bar"] -> $\[Eta],
		
		(* Yukawas *)
		Yukawa["u",{1,1}] -> $Yu[[1,1]],
		Yukawa["u",{1,2}] -> $Yu[[1,2]],
		Yukawa["u",{1,3}] -> $Yu[[1,3]],
		Yukawa["u",{2,1}] -> $Yu[[2,1]],
		Yukawa["u",{2,2}] -> $Yu[[2,2]],
		Yukawa["u",{2,3}] -> $Yu[[2,3]],
		Yukawa["u",{3,1}] -> $Yu[[3,1]],
		Yukawa["u",{3,2}] -> $Yu[[3,2]],
		Yukawa["u",{3,3}] -> $Yu[[3,3]],
		Yukawa["d",{1,1}] -> $Yd[[1,1]],
		Yukawa["d",{1,2}] -> $Yd[[1,2]],
		Yukawa["d",{1,3}] -> $Yd[[1,3]],
		Yukawa["d",{2,1}] -> $Yd[[2,1]],
		Yukawa["d",{2,2}] -> $Yd[[2,2]],
		Yukawa["d",{2,3}] -> $Yd[[2,3]],
		Yukawa["d",{3,1}] -> $Yd[[3,1]],
		Yukawa["d",{3,2}] -> $Yd[[3,2]],
		Yukawa["d",{3,3}] -> $Yd[[3,3]],
		Yukawa["e",{1,1}] -> $Ye[[1,1]],
		Yukawa["e",{1,2}] -> $Ye[[1,2]],
		Yukawa["e",{1,3}] -> $Ye[[1,3]],
		Yukawa["e",{2,1}] -> $Ye[[2,1]],
		Yukawa["e",{2,2}] -> $Ye[[2,2]],
		Yukawa["e",{2,3}] -> $Ye[[2,3]],
		Yukawa["e",{3,1}] -> $Ye[[3,1]],
		Yukawa["e",{3,2}] -> $Ye[[3,2]],
		Yukawa["e",{3,3}] -> $Ye[[3,3]],
		
		(* masses & widths *)
		Mass["ZBoson"]         -> $mZ,
		Width["ZBoson"]        -> $\[CapitalGamma]Z,
		Mass["WBoson"]         -> $mW,
		Width["WBoson"]        -> $\[CapitalGamma]W,
		Mass["Photon"]         -> 0,
		Width["Photon"]        -> 0,
		
		Mass["H"] -> $mH,
		
		Mass["e"] -> $me,
		Mass["\[Mu]"] -> $m\[Mu],
		Mass["\[Tau]"] -> $m\[Tau],
		Mass["d"] -> $md,
		Mass["s"] -> $ms,
		Mass["b"] -> $mb,
		Mass["u"] -> $mu,
		Mass["c"] -> $mc,
		Mass["t"] -> $mt,
		Mass["\[Pi]+"] -> $m\[Pi]plus,
		Mass["\[Pi]0"] -> $m\[Pi]0,
		Mass["K+"] -> $mKplus,
		Mass["K0"] -> $mK0,
		Mass["D+"] -> $mDplus,
		Mass["D0"] -> $mD0,
		Mass["Ds"] -> $mDs,
		Mass["Bd"] -> $mBd,
		Mass["Bs"] -> $mBs,
		Mass["Bc"] -> $mBc
	|>;
]


ExperimentalParameters= <||>;


(* initialize the parameters with default values *)
DefineParameters[Default]


(* ::Section:: *)
(*GetParameters*)


GetParameters::usage= "GetParameters[] returns an Association of all (B)SM parameters and their values. To modify the values use the DefineParameters routine.";


Options[GetParameters]={
	Errors -> False
};


(* returns the current value of the (B)SM parameters *)
GetParameters[OptionsPattern[]]:= If[MatchQ[OptionValue[Errors],True],
	Join[ExperimentalParameters, ReplaceMassWidth[]],
	Join[ExperimentalParameters, ReplaceMassWidth[]]/.Around[a_,b_]->a
	]


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
