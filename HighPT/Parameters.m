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


(* ::Text:: *)
(*Everything below should be combined in one head, e.g. Parameter["..."]*)
(*OR: PackageScope everything*)


PackageExport["VEV"]
PackageExport["\[Alpha]EM"]
PackageExport["GF"]
PackageExport["mZ"]
PackageExport["\[CapitalGamma]Z"]
PackageExport["\[CapitalGamma]W"]
PackageExport["sW"]
PackageExport["cW"]


PackageScope["CKM"]
PackageScope["V"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["ReplaceConstants"]


PackageScope["Charge"]
PackageScope["WeakIsospin3"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Constants*)


(* ::Subsection:: *)
(*Vacuum expectation value*)


VEV::usage="VEV
	Denotes the electroweak vacuum expectation value and is formated as \[ScriptV] in TraditionalForm.";


\[Alpha]EM::usage="\[Alpha]EM
	Denotes the electromagnetic fine structure constant and is formated as \!\(\*SubscriptBox[\(\[Alpha]\), \(EM\)]\) in TraditionalForm.";


sW::usage= "sW
	Denotes the sine of the weak mixing angle and is formated as \!\(\*SubscriptBox[\(s\), \(W\)]\) in TraditionalForm."


cW::usage= "cW
	Denotes the cosine of the weak mixing angle and is formated as \!\(\*SubscriptBox[\(c\), \(W\)]\) in TraditionalForm."


Mass::usage= "Mass[\[Phi]]
	Denotes the mass of the particle \[Phi] and is formated as \!\(\*SubscriptBox[\(M\), \(\[Phi]\)]\) in TraditionalForm.";


Width::usage= "Width[\[Phi]]
	Denotes the width of the particle \[Phi] and is formated as \!\(\*SubscriptBox[\(\[CapitalGamma]\), \(\[Phi]\)]\) in TraditionalForm.";


CKM::usage= "CKM
	Denotes the CKM matrix.";


V::usage= "V[\!\(\*SubscriptBox[\(u\), \(i\)]\)\!\(\*SubscriptBox[\(d\), \(j\)]\)]
	Denotes the ij element of the CKM matrix.";


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


VEV/:Conjugate[VEV]:= VEV


\[Alpha]EM/:Conjugate[\[Alpha]EM]:= \[Alpha]EM


sW/:Conjugate[sW]:= sW


cW/:Conjugate[cW]:= cW


Mass/:Conjugate[Mass[a_]]:= Mass[a]


Width/:Conjugate[Width[a_]]:= Width[a]


(* ::Section:: *)
(*Formatting*)


Format[Mass[f_], TraditionalForm]:= Subscript["M",f]


Format[Width[f_], TraditionalForm]:= Subscript["\[CapitalGamma]",f]


Format[VEV, TraditionalForm]:= "\[ScriptV]"


Format[sW, TraditionalForm]:= Subscript["s","W"]
Format[cW, TraditionalForm]:= Subscript["c","W"]


Format[\[Alpha]EM, TraditionalForm]:= Subscript["\[Alpha]","EM"]


Format[CKM, TraditionalForm]:= Subscript["V","CKM"]
Format[V[x_], TraditionalForm]:= Subscript["V",x]


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


\[Lambda]Wolfenstein= 0.22650;
AWolfenstein= 0.790;
\[Rho]BarWolfenstein= 0.141;
\[Eta]BarWolfenstein= 0.357;
\[Rho]Wolfenstein= \[Rho]BarWolfenstein/(1-\[Lambda]Wolfenstein^2/2);
\[Eta]Wolfenstein= \[Eta]BarWolfenstein/(1-\[Lambda]Wolfenstein^2/2);


CKM= {
	{V["ud"],V["us"],V["ub"]},
	{V["cd"],V["cs"],V["cb"]},
	{V["td"],V["ts"],V["tb"]}
};


(* ::Subsection:: *)
(*Function to modify experimental values*)


DefineParameters::usage= "DefineParameters[]
	Defines all SM parameters: \[Alpha]EM, VEV, sW, cW, Mass[ZBoson], Width[ZBoson], Mass[WBoson], Width[WBoson], Mass[Photon], Width[Photon], CKM.
	The input scheme uses the parameters \[Alpha]EM, GF, mZ, \[CapitalGamma]Z, \[CapitalGamma]W which can be specified via Options.
	Example:
		To set the electromagnetic fine structure constant to \!\(\*SubscriptBox[\(\[Alpha]\), \(EM\)]\) = \!\(\*SuperscriptBox[\(137\), \(-1\)]\) use
		DefineParameters[\[Alpha]EM \[Rule] 1/137].
";


Options[DefineParameters]={\[Alpha]EM->\[Alpha]EM$default, GF->GF$default, mZ->mZ$default, \[CapitalGamma]Z->\[CapitalGamma]Z$default, \[CapitalGamma]W->\[CapitalGamma]W$default};


DefineParameters[OptionsPattern[]] := Module[
	{
		$\[Alpha]EM = OptionValue[\[Alpha]EM],
		$GF  = OptionValue[GF],
		$mZ  = OptionValue[mZ],
		$\[CapitalGamma]Z  = OptionValue[\[CapitalGamma]Z],
		$\[CapitalGamma]W  = OptionValue[\[CapitalGamma]W],
		$mW,
		$sW,
		$vev
	}
	,
	(* compute the non-input parameters *)
	$mW = Sqrt[$mZ^2/2.+Sqrt[$mZ^4/4.-($\[Alpha]EM*\[Pi]*$mZ^2)/($GF*Sqrt[2])]];
	$sW = Sqrt[1. - $mW^2/$mZ^2];
	$vev = ($mW*$sW)/Sqrt[\[Pi]*$\[Alpha]EM];
	(* Create the appropriate supstitution rule *)
	ExperimentalParameters = <|
		\[Alpha]EM           -> $\[Alpha]EM,
		VEV           -> $vev,
		sW            -> $sW,
		cW            -> Sqrt[1. - $sW^2],
		Mass[ZBoson]  -> $mZ,
		Width[ZBoson] -> $\[CapitalGamma]Z,
		Mass[WBoson]  -> $mW,
		Width[WBoson] -> $\[CapitalGamma]W,
		Mass[Photon]  -> 0,
		Width[Photon] -> 0,
		(*CKM-> {
			{1-\[Lambda]Wolfenstein^2/2, \[Lambda]Wolfenstein, AWolfenstein * \[Lambda]Wolfenstein^3 * (\[Rho]Wolfenstein - \[ImaginaryI]*\[Eta]Wolfenstein)},
			{-\[Lambda]Wolfenstein, 1-\[Lambda]Wolfenstein^2/2, AWolfenstein * \[Lambda]Wolfenstein^2},
			{AWolfenstein * \[Lambda]Wolfenstein^3 * (1 - \[Rho]Wolfenstein - \[ImaginaryI]*\[Eta]Wolfenstein), -AWolfenstein * \[Lambda]Wolfenstein^2, 1}
		},*)
		V["ud"] -> 1-\[Lambda]Wolfenstein^2/2,
		V["us"] -> \[Lambda]Wolfenstein,
		V["ub"] -> AWolfenstein * \[Lambda]Wolfenstein^3 * (\[Rho]Wolfenstein - I*\[Eta]Wolfenstein),
		
		V["cd"] -> -\[Lambda]Wolfenstein,
		V["cs"] -> 1-\[Lambda]Wolfenstein^2/2,
		V["cb"] -> AWolfenstein * \[Lambda]Wolfenstein^2,
		
		V["td"] -> AWolfenstein * \[Lambda]Wolfenstein^3 * (1 - \[Rho]Wolfenstein - I*\[Eta]Wolfenstein),
		V["ts"] -> -AWolfenstein * \[Lambda]Wolfenstein^2,
		V["tb"] -> 1
		(*unit conversion*)
		(*,GeV2toPB -> (10^9)/(2.56819)*)
	|>;
]


ExperimentalParameters= <||>;


(* initialize the parameters with default values *)
DefineParameters[]


GetParameters::usage= "GetParameters[]
	Returns an Association of all SM parameters and their values.
	To modify the values use DefineParameters.";


(* returns the current value of the SM parameters *)
GetParameters[]:= ExperimentalParameters


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
