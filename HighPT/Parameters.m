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


PackageExport["Lifetime"]


PackageExport["DecayConstant"]


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


PackageScope["WolfensteinExtract"]


PackageScope["ComplexAround"]


PackageScope["Info$default"]


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Usage messages*)


Param::usage="Param[\"label\"] denotes the parameter specified by \"label\". The defined parameters are: the electromagnetic fine structure constant \"\[Alpha]EM\"; Fermi's constant\"GF\"; the electroweak vacuum expectation value \"vev\"; the sine of the weak mixing angle \"sW\"; the cosine of the weak mixing angle \"cW\"."


Mass::usage= "Mass[\[Phi]] denotes the mass of the particle \[Phi].";


Width::usage= "Width[\[Phi]] denotes the width of the particle \[Phi].";


Yukawa::usage="Yukawa[\"label\",{i,j}] denotes the {i,j} entry of the Yukawa matrix. Allowed values for \"label\" are \"u\", \"d\", \"e\"";


Lifetime::usage="Lifetime[\"label\"] denotes the lifetime of the particle \"label\"";


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


$realParameters = Alternatives["vev", "\[Alpha]EM", "sW", "cW", "GF", "\[Alpha]S", "|Vus|", "|Vcb|", "|Vub|", "\[Gamma]"]


Param/:Conjugate[Param[x:$realParameters]] := Param[x]


Mass/:Conjugate[Mass[a_]]:= Mass[a]


Width/:Conjugate[Width[a_]]:= Width[a]


Lifetime/:Conjugate[Lifetime[a_]]:= Lifetime[a]


(* ::Section:: *)
(*Yukawas*)


Yukawa[l:Except[Alternatives@@Join[{"u","d","e"}, {_Pattern, _Blank, _Except, _BlankNullSequence, _BlankSequence}]],___]:=(
	Message[Yukawa::unknownYukawa,l];
	Abort[]
)


(* ::Section:: *)
(*Formatting*)


Format[Mass[f_]         , TraditionalForm] := Subscript["M",f]
Format[Width[f_]        , TraditionalForm] := Subscript["\[CapitalGamma]",f]
Format[Lifetime[f_]     , TraditionalForm] := Subscript["\[Tau]",f]
Format[DecayConstant[f_], TraditionalForm] := Subscript["f",f]


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
(*ComplexAround*)


Format[ComplexAround[x_,y_]]:=Infix[{Around[Re[x],Abs[Re[y]]],Postfix[{PrecedenceForm[Around[Im[x],Abs[Im[y]]],50]},I]},"+"];

(*ComplexAround[z_?Internal`RealValuedNumericQ,dz_?Internal`RealValuedNumericQ]:=Around[z,dz];*)

ComplexAround[z_,dz_]:=ComplexAround[z,-Re[dz]+Im[dz] I]/;Re[dz]<0;

ComplexAround[z_,dz_]:=ComplexAround[z,Re[dz]-Im[dz] I]/;Im[dz]<0;

ComplexAround[z_,dz_]:=z/;dz==0;

ComplexAround[z_,dz_]["Value"]:=z;

ComplexAround[z_,dz_]["Uncertainty"]:=dz;

ComplexAround/:Conjugate[ComplexAround[z_,dz_]]:=ComplexAround[Conjugate[z],dz]

ComplexAround/:Re[ComplexAround[z_,dz_]]:=Around[Re[z],Re[dz]]
ComplexAround/:Im[ComplexAround[z_,dz_]]:=Around[Im[z],Im[dz]]

ComplexAround/:ComplexAround[z_,dz_]+c_?NumericQ:=ComplexAround[z+c,dz];

ComplexAround/:k_?Internal`RealValuedNumericQ ComplexAround[z_,dz_]:=ComplexAround[k z,Abs[k] dz];

ComplexAround/:Times[Complex[0,1], ComplexAround[z_,dz_]]:=ComplexAround[I z,I dz];

ComplexAround/:Times[Complex[0,-1], ComplexAround[z_,dz_]]:=ComplexAround[-I z,-I dz];

ComplexAround/:ComplexAround[z1_,dz1_]+ComplexAround[z2_,dz2_]:=Module[{r,i,x,dx,y,dy},r=Around[Re[z1],Re[dz1]]+Around[Re[z2],Re[dz2]];
{x,dx}=If[Length[r]==2,List@@r,{r,0}];
i=Around[Im[z1],Im[dz1]]+Around[Im[z2],Im[dz2]];
{y,dy}=If[Length[i]==2,List@@i,{i,0}];
ComplexAround[x+y I,dx+dy I]];

ComplexAround/:Abs[ComplexAround[z_,dz_]]:=Module[{r,i},
r=Around[Re[z],Abs[Re[dz]]];
i=Around[Im[z],Abs[Im[dz]]];
Return[Sqrt[r^2+i^2]]
];

ComplexAround/:Arg[ComplexAround[z_,dz_]]:=Module[{r,i},
r=Around[Re[z],Abs[Re[dz]]];
i=Around[Im[z],Abs[Im[dz]]];
Return[ArcTan[i/r]];
];

ComplexAround/:Times[ComplexAround[z1_,dz1_],ComplexAround[z2_,dz2_]]:=Module[{rz1,iz1,rz2,iz2,rz,drz,iz,diz},
rz1=Around[Re[z1],Abs[Re[dz1]]];
iz1=Around[Im[z1],Abs[Im[dz1]]];
rz2=Around[Re[z2],Abs[Re[dz2]]];
iz2=Around[Im[z2],Abs[Im[dz2]]];
{rz,drz}=If[Length[rz1*rz2-iz1*iz2]==2,List@@(rz1*rz2-iz1*iz2),{rz1*rz2-iz1*iz2,0}];
{iz,diz}=If[Length[rz1*iz2+iz1*rz2]==2,List@@(rz1*iz2+iz1*rz2),{rz1*iz2+iz1*rz2,0}];
ComplexAround[rz+I*iz,drz+I*diz]
];

ComplexAround/:Times[x_Around,ComplexAround[z2_,dz2_]]:=Module[{rz2,iz2,rz,drz,iz,diz},
rz2=Around[Re[z2],Abs[Re[dz2]]];
iz2=Around[Im[z2],Abs[Im[dz2]]];
{rz,drz}=If[Length[x*rz2]==2,List@@(x*rz2),{x*rz2,0}];
{iz,diz}=If[Length[x*iz2]==2,List@@(x*iz2),{x*iz2,0}];
ComplexAround[rz+I*iz,drz+I*diz]
];

ComplexAround/:Power[ComplexAround[z_,dz_],n_?Internal`RealValuedNumericQ]:= Module[{mod,ph,modn,phn,rz,drz,iz,diz},
mod=Abs[ComplexAround[z,dz]];
ph=Arg[ComplexAround[z,dz]];
modn=Power[mod,n];
phn=n*ph;
{rz,drz}=If[Length[modn*Cos[phn]]==2,List@@(modn*Cos[phn]),{modn*Cos[phn],0}];
{iz,diz}=If[Length[modn*Sin[phn]]==2,List@@(modn*Sin[phn]),{modn*Sin[phn],0}];
ComplexAround[rz+I*iz,drz+I*diz]
]


(* ::Section:: *)
(*Other experimental Inputs*)


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
\[CapitalGamma]H$default = Around[4.1,0]*10^-3;


(* ::Subsection:: *)
(*QCD input*)


\[Alpha]S$default = Around[0.1179,0.0010];


(* ::Subsection:: *)
(*Flavor input*)


(* ::Subsubsection:: *)
(*Masses (flavour)*)


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
(* \[Eta] *)
m\[Eta]$default = Around[0.547862,0.000017];
m\[Eta]p$default = Around[0.95778,0.00006];
(* \[Rho] *)
m\[Rho]$default = Around[0.77526,0.00023];
(* \[Phi] *)
m\[Phi]$default = Around[1.019461,0.000016];
(* D *)
mDplus$default = Around[1.86966,0.00005];
mD0$default = Around[1.86484,0.00005];
mDs$default = Around[1.96835,0.00007];
(* B *)
mBd$default = Around[5.27966,0.00012];
mBs$default = Around[5.36692,0.00010];
mBc$default = Around[6.27447,0.00032];


(* ::Text:: *)
(*Baryons (PDG)*)


mp$default = Around[938.27208816,0.00000029]*10^-3;
mn$default = Around[939.5654205,0.000005]*10^-3;


(* ::Subsubsection:: *)
(*Lifetimes*)


GeVtos=6.582*10^-25;
stoGeV=GeVtos^-1;


(* ::Text:: *)
(*Leptons*)


\[Tau]\[Mu]$default = Around[2.1969811,0.0000022]*10^-6*stoGeV;


\[Tau]\[Tau]$default = Around[290.3,0.5]*10^-15*stoGeV;


(* ::Text:: *)
(*Mesons*)


\[Tau]Kplus$default = Around[1.2380,0.0020]*10^-8*stoGeV;
\[Tau]KL$default = Around[5.116,0.021]*10^-8*stoGeV;


\[Tau]Bs$default = Around[1.520,0.005]*10^-12*stoGeV;


\[Tau]B0$default = Around[1.517,0.004]*10^-12*stoGeV;
\[Tau]Bplus$default = Around[1.638,0.004]*10^-12*stoGeV;


(* ::Subsubsection:: *)
(*Decay constants*)


fKplus$default = Around[0.1557,0.0003];
Info$default[DecayConstant["K+"]] := "FLAG 2024 average, Nf = 2+1+1, ..."


fBs$default = Around[228.4,3.7]*10^-3;
Info$default[DecayConstant["Bs"]] := "Taken from ..."


(* ::Subsection:: *)
(*Save current values of inputs [separate from default values]*)


(* ::Subsubsection:: *)
(*Gauge inputs*)


\[Alpha]EM$current = \[Alpha]EM$default;
GF$current = GF$default;
mZ$current = mZ$default;
\[CapitalGamma]Z$current = \[CapitalGamma]Z$default;
\[CapitalGamma]W$current = \[CapitalGamma]W$default;

(*\[Lambda]$current  = \[Lambda]$default;*)
mH$current = mH$default;
\[CapitalGamma]H$current = \[CapitalGamma]H$default;

\[Alpha]S$current = \[Alpha]S$default;


(* ::Subsubsection:: *)
(*Masses (flavour)*)


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

m\[Eta]$current = m\[Eta]$default;
m\[Eta]p$current = m\[Eta]p$default;

m\[Rho]$current = m\[Rho]$default;

m\[Phi]$current = m\[Phi]$default;

mKplus$current = mKplus$default;
mK0$current = mK0$default;

mDplus$current = mDplus$default;
mD0$current = mD0$default;
mDs$current = mDs$default;

mBd$current = mBd$default;
mBs$current = mBs$default;
mBc$current = mBc$default;

mp$current = mp$default;
mn$current = mn$default;


(* ::Subsubsection:: *)
(*Lifetimes*)


\[Tau]\[Mu]$current = \[Tau]\[Mu]$default;
\[Tau]\[Tau]$current = \[Tau]\[Tau]$default;

\[Tau]Kplus$current = \[Tau]Kplus$default;
\[Tau]KL$current = \[Tau]KL$default;

\[Tau]Bs$current = \[Tau]Bs$default;

\[Tau]B0$current = \[Tau]B0$default;
\[Tau]Bplus$current = \[Tau]Bplus$default;


(* ::Subsubsection:: *)
(*Decay Constants*)


fBs$current = fBs$default;


fKplus$current = fKplus$default;


(* ::Section:: *)
(*CKM stuff*)


(* ::Subsection:: *)
(*Vus default*)


(*Vus$default := Around[0.2217,0.0009];*)


Vusplus$default := Sqrt[ExpValue$default["K+->\[Pi]0e\[Nu]"]/((TheoryExpression["K+->\[Pi]0e\[Nu]"]/.a_WCL->SMValue[a,TreeOnly->True]/.SubstitutePsi/.Lifetime["K+"]->\[Tau]Kplus$current/.Vckm[1,2]->1//ParamsAsInputs//FullSimplify)/.Param["GF"]->GF$current)]


VusL$default := Sqrt[ExpValue$default["KL->\[Pi]-e\[Nu]"]/((TheoryExpression["KL->\[Pi]-e\[Nu]"]/.a_WCL->SMValue[a,TreeOnly->True]/.SubstitutePsi/.Lifetime["KL"]->\[Tau]KL$current/.Vckm[1,2]->1//ParamsAsInputs//FullSimplify)/.Param["GF"]->GF$current)]


Vus$default := 1/2 (Vusplus$default+VusL$default)


(* ::Subsection:: *)
(*Vcb default*)


(*Vcb$default := Around[0.0404,0.0003];*)


Vcb$default := Sqrt[ExpValue$default["B->Dl\[Nu]_iso"]/((TheoryExpression["B->Dl\[Nu]_iso"]/.a_WCL->SMValue[a,TreeOnly->True]/.SubstitutePsi/.Lifetime["B0"]->\[Tau]B0$current/.Vckm[2,3]->1//ParamsAsInputs//FullSimplify)/.Param["GF"]->GF$current)]


(* ::Subsection:: *)
(*Vub default*)


(*Vub$default := Around[0.0039,0.0002];*)


Vub$default := Sqrt[ExpValue$default["B0->\[Pi]-l\[Nu]_high"]/((TheoryExpression["B0->\[Pi]-l\[Nu]_high"]/.a_WCL->SMValue[a,TreeOnly->True]/.SubstitutePsi/.Lifetime["B0"]->\[Tau]B0$current/.Vckm[1,3]->1//ParamsAsInputs//FullSimplify)/.Param["GF"]->GF$current)]


(* ::Subsection:: *)
(*\[Gamma] default*)


\[Gamma]CKM$default := Around[68.7,4.2] \[Pi]/180;


(* ::Subsection:: *)
(*CKM default*)


CKM$default := {
	Vus$default,
	Vcb$default,
	Vub$default,
	\[Gamma]CKM$default
}


(*Param["|Vus|"] = Around[0.2217,0.0009];
Param["|Vcb|"] = Around[0.0404,0.0003];
Param["|Vub|"] = Around[0.0039,0.0002];
Param["\[Gamma]CKM"] = Around[68.7,4.2] \[Pi]/180;*)


(* ::Subsection:: *)
(*Extracting Wolfenstein from inputs*)


WolfensteinExtract[Vus_,Vcb_,Vub_,\[Gamma]_] := Module[
	{
		\[Lambda]\[Lambda], AA, \[Rho]\[Rho], \[Eta]\[Eta]
	}
	,
	If[MatchQ[Vus,0],Return[{0,0,0,0}]];
	\[Lambda]\[Lambda] = Vus;
	If[MatchQ[Vcb,0],Return[{\[Lambda]\[Lambda],0,0,0}]];
	AA = Vcb/Vus^2;
	\[Rho]\[Rho] = Vub/(Vcb Vus (1+Vus^2/2))Cos[\[Gamma]];
	\[Eta]\[Eta] = Vub/(Vcb Vus (1+Vus^2/2))Sin[\[Gamma]];
	Return@{\[Lambda]\[Lambda],AA,\[Rho]\[Rho],\[Eta]\[Eta]}
];


(* ::Subsection:: *)
(*Compute default Wolfenstein parameters*)


(*\[Lambda]Wolfenstein$default    = Param["|Vus|"];
AWolfenstein$default    = Param["|Vcb|"]/Param["|Vus|"]^2;
\[Rho]BarWolfenstein$default = Param["|Vub|"]/(Param["|Vcb|"] Param["|Vus|"](1+Param["|Vus|"]^2/2)) Cos[Param["\[Gamma]CKM"]];
\[Eta]BarWolfenstein$default = Param["|Vub|"]/(Param["|Vcb|"] Param["|Vus|"](1+Param["|Vus|"]^2/2)) Sin[Param["\[Gamma]CKM"]];*)

(*{\[Lambda]Wolfenstein$default,AWolfenstein$default,\[Rho]BarWolfenstein$default,\[Eta]BarWolfenstein$default} := WolfensteinExtract[Vus$default,Vcb$default,Vub$default,\[Gamma]CKM$default];

Wolfenstein$default := {
	\[Lambda]Wolfenstein$default,
	AWolfenstein$default,
	\[Rho]BarWolfenstein$default,
	\[Eta]BarWolfenstein$default
}*)


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


(* ::Subsubsection:: *)
(*Set current CKM*)


Vus$current  = Vus$default;
Vcb$current  = Vcb$default;
Vub$current  = Vub$default;
\[Gamma]CKM$current = \[Gamma]CKM$default;

CKM$current = {
	Vus$current,
	Vcb$current,
	Vub$current,
	\[Gamma]CKM$current
}

(*\[Lambda]Wolfenstein$current    = \[Lambda]Wolfenstein$default;
AWolfenstein$current    = AWolfenstein$default;
\[Rho]BarWolfenstein$current = \[Rho]BarWolfenstein$default;
\[Eta]BarWolfenstein$current = \[Eta]BarWolfenstein$default;

Wolfenstein$current = {
	\[Lambda]Wolfenstein$current,
	AWolfenstein$current,
	\[Rho]BarWolfenstein$current,
	\[Eta]BarWolfenstein$current
}*)


(* ::Section:: *)
(*Parameter info*)


(* Default info for parameters *)
Info[x_:(_Param| _DecayConstant | _Lifetime | _Mass | _Yukawa | _Vckm)] := "No information available"


(* ::Section::Closed:: *)
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
	"\[CapitalGamma]H"          :> \[CapitalGamma]H$current,
	"\[Alpha]S"          :> \[Alpha]S$current,
	(*"Vus"         :> Vus$current,
	"Vcb"         :> Vcb$current,
	"Vub"         :> Vub$current,
	"\[Gamma]CKM"        :> \[Gamma]CKM$current,*)
	"CKM"         :> CKM$current,
	(*"Wolfenstein" :> Wolfenstein$current,*)
	
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
	"m\[Eta]"          :> m\[Eta]$current,
	"m\[Eta]'"         :> m\[Eta]p$current,
	"m\[Rho]"          :> m\[Rho]$current,
	"m\[Phi]"          :> m\[Phi]$current,
	"mD+"         :> mDplus$current,
	"mD0"         :> mD0$current,
	"mDs"         :> mDs$current,
	"mBd"         :> mBd$current,
	"mBs"         :> mBs$current,
	"mBc"         :> mBc$current,
	"mp"          :> mp$current,
	"mn"          :> mn$current,
	
	"\[Tau]\[Mu]"          :> \[Tau]\[Mu]$current,
	"\[Tau]\[Tau]"          :> \[Tau]\[Tau]$current,
	"\[Tau]K+"         :> \[Tau]Kplus$current,
	"\[Tau]KL"         :> \[Tau]KL$current,
	"\[Tau]Bs"         :> \[Tau]Bs$current,
	"\[Tau]B0"         :> \[Tau]B0$current,
	"\[Tau]B+"         :> \[Tau]Bplus$current,
	
	"fK+"         :> fKplus$current,
	"fBs"         :> fBs$current
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
	"\[CapitalGamma]H"          -> \[CapitalGamma]H$default,
	"\[Alpha]S"          -> \[Alpha]S$default,
	(*"Vus"         :> Vus$current,
	"Vcb"         :> Vcb$current,
	"Vub"         :> Vub$current,
	"\[Gamma]CKM"        :> \[Gamma]CKM$current,*)
	"CKM"         :> CKM$default,
	(*"Wolfenstein" -> Wolfenstein$default,*)
	
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
	"m\[Eta]"          -> m\[Eta]$default,
	"m\[Eta]'"         -> m\[Eta]p$default,
	"m\[Rho]"          -> m\[Rho]$default,
	"m\[Phi]"          -> m\[Phi]$default,
	"mD+"         -> mDplus$default,
	"mD0"         -> mD0$default,
	"mDs"         -> mDs$default,
	"mBd"         -> mBd$default,
	"mBs"         -> mBs$default,
	"mBc"         -> mBc$default,
	"mp"          -> mp$default,
	"mn"          -> mn$default,
	
	"\[Tau]\[Mu]"          :> \[Tau]\[Mu]$default,
	"\[Tau]\[Tau]"          :> \[Tau]\[Tau]$default,
	"\[Tau]K+"         :> \[Tau]Kplus$default,
	"\[Tau]KL"         :> \[Tau]KL$default,
	"\[Tau]Bs"         :> \[Tau]Bs$default,
	"\[Tau]B0"         :> \[Tau]B0$default,
	"\[Tau]B+"         :> \[Tau]Bplus$default,
	
	"fK+"         :> fKplus$default,
	"fBs"         :> fBs$default
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
		$\[CapitalGamma]H          = OptionValue["\[CapitalGamma]H"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$\[Alpha]S          = OptionValue["\[Alpha]S"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		(*$Vus         = OptionValue["Vus"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$Vcb         = OptionValue["Vcb"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$Vub         = OptionValue["Vub"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$\[Gamma]CKM        = OptionValue["\[Gamma]CKM"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],*)
		$CKM         = OptionValue["CKM"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		(*$Wolfenstein = OptionValue["Wolfenstein"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],*)
		
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
		$m\[Eta]          = OptionValue["m\[Eta]"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$m\[Eta]p         = OptionValue["m\[Eta]'"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$m\[Rho]          = OptionValue["m\[Rho]"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$m\[Phi]          = OptionValue["m\[Phi]"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mDplus      = OptionValue["mD+"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mD0         = OptionValue["mD0"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mDs         = OptionValue["mDs"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mBd         = OptionValue["mBd"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mBs         = OptionValue["mBs"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mBc         = OptionValue["mBc"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mp          = OptionValue["mp"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$mn          = OptionValue["mn"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		
		$\[Tau]\[Mu]          = OptionValue["\[Tau]\[Mu]"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$\[Tau]\[Tau]          = OptionValue["\[Tau]\[Tau]"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$\[Tau]Kplus      = OptionValue["\[Tau]K+"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$\[Tau]KL         = OptionValue["\[Tau]KL"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$\[Tau]Bs         = OptionValue["\[Tau]Bs"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$\[Tau]B0         = OptionValue["\[Tau]B0"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$\[Tau]Bplus      = OptionValue["\[Tau]B+"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		
		$fKplus      = OptionValue["fK+"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		$fBs         = OptionValue["fBs"]/.Around[i_,{j_,k_}]->Around[i,Max[j,k]],
		
		(* output *)
		$mW, $sW, $vev, $g2, $g1, $\[Lambda]H,
		$g3,
		$Vus, $Vcb, $Vub, $\[Gamma]CKM,
		$\[Lambda], $A, $\[Rho]Bar, $\[Eta]Bar, $\[Rho], $\[Eta],
		$Yu, $Yd, $Ye, $ckmrep,
		
		$mediator         = OptionValue[Mediators]/.Association->List,
		mediators$current = GetMediators[]
	}
	,
	(* OPTION CHECKS *)
	OptionCheck[#,OptionValue[#]]& /@ {"\[Alpha]EM", "GF", "mZ", "\[CapitalGamma]Z", "\[CapitalGamma]W"(*, "\[Lambda]"*), "mH", "\[CapitalGamma]H", "\[Alpha]S", (*"Vus", "Vcb", "Vub", "\[Gamma]CKM",*) "CKM"(*, "Wolfenstein"*), Mediators, "me", "m\[Mu]", "m\[Tau]", "md", "ms", "mb", "mu", "mc", "mt", "m\[Pi]+", "m\[Pi]0", "mK+", "mK0", "m\[Eta]", "m\[Eta]'", "m\[Rho]", "m\[Phi]", "mD+", "mD0", "mDs", "mBd", "mBs", "mBc", "mp", "mn", "\[Tau]\[Mu]", "\[Tau]\[Tau]","\[Tau]K+","\[Tau]KL","\[Tau]Bs","\[Tau]B0","\[Tau]B+","fK+","fBs"};
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
	mH$current  = If[MatchQ[$mH,  Default], $mH  = mH$default , $mH];
	\[CapitalGamma]H$current  = If[MatchQ[$\[CapitalGamma]H,  Default], $\[CapitalGamma]H  = \[CapitalGamma]H$default , $\[CapitalGamma]H];
	\[Alpha]S$current  = If[MatchQ[$\[Alpha]S, Default], $\[Alpha]S = \[Alpha]S$default, $\[Alpha]S];
	
	{$Vus, $Vcb, $Vub, $\[Gamma]CKM} = $CKM;
	Vus$current   = If[MatchQ[$Vus, Default], $Vus = Vus$default, $Vus];
	Vcb$current   = If[MatchQ[$Vcb, Default], $Vcb = Vcb$default, $Vcb];
	Vub$current   = If[MatchQ[$Vub, Default], $Vub = Vub$default, $Vub];
	\[Gamma]CKM$current  = If[MatchQ[$\[Gamma]CKM, Default], $\[Gamma]CKM = \[Gamma]CKM$default, $\[Gamma]CKM];
	CKM$current = {
		Vus$current,
		Vcb$current,
		Vub$current,
		\[Gamma]CKM$current
	};
	{$\[Lambda], $A, $\[Rho]Bar, $\[Eta]Bar} = WolfensteinExtract[$Vus,$Vcb,$Vub,$\[Gamma]CKM];
	
	(*{$\[Lambda], $A, $\[Rho]Bar, $\[Eta]Bar} = $Wolfenstein;
	\[Lambda]Wolfenstein$current    = If[MatchQ[$\[Lambda],    Default], $\[Lambda]    = \[Lambda]Wolfenstein$default   , $\[Lambda]   ];
	AWolfenstein$current    = If[MatchQ[$A,    Default], $A    = AWolfenstein$default   , $A   ];
	\[Rho]BarWolfenstein$current = If[MatchQ[$\[Rho]Bar, Default], $\[Rho]Bar = \[Rho]BarWolfenstein$default, $\[Rho]Bar];
	\[Eta]BarWolfenstein$current = If[MatchQ[$\[Eta]Bar, Default], $\[Eta]Bar = \[Eta]BarWolfenstein$default, $\[Eta]Bar];
	Wolfenstein$current = {
		\[Lambda]Wolfenstein$current,
		AWolfenstein$current,
		\[Rho]BarWolfenstein$current,
		\[Eta]BarWolfenstein$current
	};*)
	
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
	m\[Eta]$current     = If[MatchQ[$m\[Eta],  Default], $m\[Eta]  = m\[Eta]$default , $m\[Eta]];
	m\[Eta]p$current    = If[MatchQ[$m\[Eta]p,  Default], $m\[Eta]p  = m\[Eta]p$default , $m\[Eta]p];
	m\[Rho]$current     = If[MatchQ[$m\[Rho],  Default], $m\[Rho]  = m\[Rho]$default , $m\[Rho]];
	m\[Phi]$current     = If[MatchQ[$m\[Phi],  Default], $m\[Phi]  = m\[Phi]$default , $m\[Phi]];
	mDplus$current = If[MatchQ[$mDplus,  Default], $mDplus  = mDplus$default , $mDplus];
	mD0$current    = If[MatchQ[$mD0,  Default], $mD0  = mD0$default , $mD0];
	mDs$current    = If[MatchQ[$mDs,  Default], $mDs  = mDs$default , $mDs];
	mBd$current    = If[MatchQ[$mBd,  Default], $mBd  = mBd$default , $mBd];
	mBs$current    = If[MatchQ[$mBs,  Default], $mBs  = mBs$default , $mBs];
	mBc$current    = If[MatchQ[$mBc,  Default], $mBc  = mBc$default , $mBc];
	mp$current     = If[MatchQ[$mp,  Default], $mp  = mp$default , $mp];
	mn$current     = If[MatchQ[$mn,  Default], $mn  = mn$default , $mn];
	
	\[Tau]\[Mu]$current     = If[MatchQ[$\[Tau]\[Mu],  Default], $\[Tau]\[Mu]  = \[Tau]\[Mu]$default , $\[Tau]\[Mu]];
	\[Tau]\[Tau]$current     = If[MatchQ[$\[Tau]\[Tau],  Default], $\[Tau]\[Tau]  = \[Tau]\[Tau]$default , $\[Tau]\[Tau]];
	\[Tau]Kplus$current = If[MatchQ[$\[Tau]Kplus,  Default], $\[Tau]Kplus  = \[Tau]Kplus$default , $\[Tau]Kplus];
	\[Tau]KL$current    = If[MatchQ[$\[Tau]KL,  Default], $\[Tau]KL  = \[Tau]KL$default , $\[Tau]KL];
	\[Tau]Bs$current    = If[MatchQ[$\[Tau]Bs,  Default], $\[Tau]Bs  = \[Tau]Bs$default , $\[Tau]Bs];
	\[Tau]B0$current    = If[MatchQ[$\[Tau]B0,  Default], $\[Tau]B0  = \[Tau]B0$default , $\[Tau]B0];
	\[Tau]Bplus$current = If[MatchQ[$\[Tau]Bplus,  Default], $\[Tau]Bplus  = \[Tau]Bplus$default , $\[Tau]Bplus];
	
	fKplus$current = If[MatchQ[$fKplus,  Default], $fKplus  = fKplus$default , $fKplus];
	fBs$current    = If[MatchQ[$fBs,  Default], $fBs  = fBs$default , $fBs];
	
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
	(*Print[{$\[Lambda],$A,$\[Rho],$\[Eta]}/.Around->ComplexAround/.List->Sequence];*)
	$ckmrep=WolfensteinParametrization[{$\[Lambda],$A,$\[Rho],$\[Eta]}/.Around->ComplexAround/.List->Sequence](*/.ComplexAround[a_?Internal`RealValuedNumericQ,da_?Internal`RealValuedNumericQ]:>Around[a,da]//Chop*);
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
	ExperimentalParameters = Join[<|
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
		Param["gZ"]  -> $g2/Sqrt[1. - $sW^2],
		
		(* CKM inputs *)
		Param["|Vus|"] -> $Vus,
		Param["|Vcb|"] -> $Vcb,
		Param["|Vub|"] -> $Vub,
		Param["\[Gamma]"] -> $\[Gamma]CKM,
		
		(*(* CKM *)
		Vckm[1,1] -> 1-$\[Lambda]^2/2,
		Vckm[1,2] -> $\[Lambda],
		Vckm[1,3] -> $A * $\[Lambda]^3 * ($\[Rho] - I*$\[Eta]),
		
		Vckm[2,1] -> -$\[Lambda],
		Vckm[2,2] -> 1-$\[Lambda]^2/2,
		Vckm[2,3] -> $A * $\[Lambda]^2,
		
		Vckm[3,1] -> $A * $\[Lambda]^3 * (1 - $\[Rho] - I*$\[Eta]),
		Vckm[3,2] -> -$A * $\[Lambda]^2,
		Vckm[3,3] -> 1,*)
		
		(* Wolfenstein *)
		Param["Wolfenstein\[Lambda]"]    -> $\[Lambda],
		Param["WolfensteinA"]    -> $A,
		Param["Wolfenstein\[Rho]bar"] -> $\[Rho]Bar,
		Param["Wolfenstein\[Eta]bar"] -> $\[Eta]Bar,
		
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
		Width["H"] -> $\[CapitalGamma]H,
		
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
		Mass["\[Eta]"]  -> $m\[Eta],
		Mass["\[Eta]'"] -> $m\[Eta]p,
		Mass["\[Rho]"]  -> $m\[Rho],
		Mass["\[Phi]"]  -> $m\[Phi],
		Mass["D+"] -> $mDplus,
		Mass["D0"] -> $mD0,
		Mass["Ds"] -> $mDs,
		Mass["Bd"] -> $mBd,
		Mass["Bs"] -> $mBs,
		Mass["Bc"] -> $mBc,
		Mass["p"]  -> $mp,
		Mass["n"]  -> $mn,
		
		Lifetime["\[Mu]"]  -> $\[Tau]\[Mu],
		Lifetime["\[Tau]"]  -> $\[Tau]\[Tau],
		Lifetime["K+"] -> $\[Tau]Kplus,
		Lifetime["KL"] -> $\[Tau]KL,
		Lifetime["Bs"] -> $\[Tau]Bs,
		Lifetime["B0"] -> $\[Tau]B0,
		Lifetime["B+"] -> $\[Tau]Bplus,
		
		DecayConstant["K+"] -> $fKplus,
		DecayConstant["Bs"] -> $fBs
	|>,
	Association[$ckmrep]
	];
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
	Join[ExperimentalParameters, ReplaceMassWidth[]]/.Around[a_,b_]->a/.ComplexAround[a_,da_]->a
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
