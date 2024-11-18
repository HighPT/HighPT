(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`SmeftLeftMatching`*)


(* ::Subtitle:: *)
(*Template .m file*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["MatchToSMEFT"]


PackageExport["SetMatchingOrder"]


PackageExport["GetMatchingOrder"]


PackageExport["LoopOrder"]


PackageExport["ToUpBasis"]


PackageExport["ToDownBasis"]


PackageExport["MatchingScale"]


(* ::Subsection:: *)
(*Internal*)


PackageScope["RotateBasis"]


PackageScope["MassRotate"]


PackageScope["TLMatching"]


PackageScope["OneLoopMatching"]


PackageScope["MatchingOrder"]


PackageScope["\[Mu]W"]


PackageScope["ZCoupling"]


(* ::Chapter:: *)
(*Private:*)


MatchingOrder=0;


SetMatchingOrder::invalidorder="Invalid input, matching order must be either 0 or 1"


SetMatchingOrder[ord_]:=If[MatchQ[ord,0]||MatchQ[ord,1],MatchingOrder=ord;,Message[SetMatchingOrder::invalidorder];Abort[]];


GetMatchingOrder[]:=If[MatchingOrder==0,Print["Tree-level matching"],Print["One-loop matching"]];


(* ::Section:: *)
(*Tree - level matching conditions*)


(* ::Subsection:: *)
(*Auxiliary definitions*)


(* ::Subsubsection:: *)
(*Mass rotation*)


MassRotate::error="Error in the mass rotation of `1`"


MassRotate[Conjugate[WC[lab_,flav_]],type_] := Conjugate[MassRotate[WC[lab,flav],type/.{"du"->"ud","ud"->"du","uddu"->"duud","duud"->"uddu"}]]


MassRotate[WC[lab_,flav_],type_]:=Module[
{
rot,i,j,k,l,\[Alpha],\[Beta]
}
,
Switch[
lab,
"Hq1" | "Hq3",
i=flav[[1]];j=flav[[2]];
Switch[
		type,
		"uu",
		rot=Sum[Vu[[i,p]]WC[lab,{p,r}]Vu[[j,r]]\[Conjugate],{p,3},{r,3}],
		"dd",
		rot=Sum[Vd[[i,p]]WC[lab,{p,r}]Vd[[j,r]]\[Conjugate],{p,3},{r,3}],
		"ud",
		rot=Sum[Vu[[i,p]]WC[lab,{p,r}]Vd[[j,r]]\[Conjugate],{p,3},{r,3}],
		"du",
		rot=Sum[Vd[[i,p]]WC[lab,{p,r}]Vu[[j,r]]\[Conjugate],{p,3},{r,3}],
	_,
	Message[MassRotate::error,WC[lab,flav]];Abort[];
	],
"uH" | "dH" | "uG" | "uW" | "uB" | "dG" | "dW" | "dB",
i=flav[[1]];j=flav[[2]];
Switch[
		type,
		"u",
		rot=Sum[Vu[[i,p]]WC[lab,{p,j}],{p,3}],
		"d",
		rot=Sum[Vd[[i,p]]WC[lab,{p,j}],{p,3}],
	_,
	Message[MassRotate::error,WC[lab,flav]];Abort[];
	],
"lq1" | "lq3" | "eq",
\[Alpha]=flav[[1]];\[Beta]=flav[[2]];i=flav[[3]];j=flav[[4]];
Switch[
		type,
		"uu",
		rot=Sum[Vu[[i,p]]WC[lab,{\[Alpha],\[Beta],p,r}]Vu[[j,r]]\[Conjugate],{p,3},{r,3}],
		"dd",
		rot=Sum[Vd[[i,p]]WC[lab,{\[Alpha],\[Beta],p,r}]Vd[[j,r]]\[Conjugate],{p,3},{r,3}],
		"ud",
		rot=Sum[Vu[[i,p]]WC[lab,{\[Alpha],\[Beta],p,r}]Vd[[j,r]]\[Conjugate],{p,3},{r,3}],
		"du",
		rot=Sum[Vd[[i,p]]WC[lab,{\[Alpha],\[Beta],p,r}]Vu[[j,r]]\[Conjugate],{p,3},{r,3}],
	_,
	Message[MassRotate::error,WC[lab,flav]];Abort[];
	],
"qq1" | "qq3",
i=flav[[1]];j=flav[[2]];k=flav[[3]];l=flav[[4]];
Switch[
		type,
		"uuuu",
		rot=Sum[Vu[[i,p]]Vu[[k,s]]WC[lab,{p,r,s,t}]Vu[[j,r]]\[Conjugate]Vu[[l,t]]\[Conjugate],{p,3},{r,3},{s,3},{t,3}],
		"uudd",
		rot=Sum[Vu[[i,p]]Vd[[k,s]]WC[lab,{p,r,s,t}]Vu[[j,r]]\[Conjugate]Vd[[l,t]]\[Conjugate],{p,3},{r,3},{s,3},{t,3}],
		"dduu",
		rot=Sum[Vd[[i,p]]Vu[[k,s]]WC[lab,{p,r,s,t}]Vd[[j,r]]\[Conjugate]Vu[[l,t]]\[Conjugate],{p,3},{r,3},{s,3},{t,3}],
		"dddd",
		rot=Sum[Vd[[i,p]]Vd[[k,s]]WC[lab,{p,r,s,t}]Vd[[j,r]]\[Conjugate]Vd[[l,t]]\[Conjugate],{p,3},{r,3},{s,3},{t,3}],
	"uddu",
		rot=Sum[Vu[[i,p]]Vd[[k,s]]WC[lab,{p,r,s,t}]Vd[[j,r]]\[Conjugate]Vu[[l,t]]\[Conjugate],{p,3},{r,3},{s,3},{t,3}],
		"duud",    
	rot=Sum[Vd[[i,p]]Vu[[k,s]]WC[lab,{p,r,s,t}]Vu[[j,r]]\[Conjugate]Vd[[l,t]]\[Conjugate],{p,3},{r,3},{s,3},{t,3}],
	_,
	Message[MassRotate::error,WC[lab,flav]];Abort[];
	]
,
"qu1" | "qu8" | "qd1" | "qd8",
i=flav[[1]];j=flav[[2]];k=flav[[3]];l=flav[[4]];
Switch[
		type,
		"uu",
		rot=Sum[Vu[[i,p]]WC[lab,{p,r,k,l}]Vu[[j,r]]\[Conjugate],{p,3},{r,3}],
		"dd",
		rot=Sum[Vd[[i,p]]WC[lab,{p,r,k,l}]Vd[[j,r]]\[Conjugate],{p,3},{r,3}],
	_,
	Message[MassRotate::error,WC[lab,flav]];Abort[];
	],
"ledq",
\[Alpha]=flav[[1]];\[Beta]=flav[[2]];i=flav[[3]];j=flav[[4]];
Switch[
		type,
		"u",
		rot=Sum[WC[lab,{\[Alpha],\[Beta],i,r}]Vu[[j,r]]\[Conjugate],{r,3}],
		"d",
		rot=Sum[WC[lab,{\[Alpha],\[Beta],i,r}]Vd[[j,r]]\[Conjugate],{r,3}],
		_,
	Message[MassRotate::error,WC[lab,flav]];Abort[];
	],
"quqd1" | "quqd8",
i=flav[[1]];j=flav[[2]];k=flav[[3]];l=flav[[4]];
Switch[
		type,
		"u",
		rot=Sum[Vu[[i,p]]Vd[[j,s]]WC[lab,{p,j,s,l}],{p,3},{s,3}],
		"d",
		rot=Sum[Vd[[i,p]]Vu[[j,s]]WC[lab,{p,j,s,l}],{p,3},{s,3}],
	_,
	Message[MassRotate::error,WC[lab,flav]];Abort[];
	],
"lequ1" | "lequ3",
\[Alpha]=flav[[1]];\[Beta]=flav[[2]];i=flav[[3]];j=flav[[4]];
Switch[
		type,
		"u",
		rot=Sum[Vu[[i,p]]WC[lab,{\[Alpha],\[Beta],p,j}],{p,3}],
	"d",
		rot=Sum[Vd[[i,p]]WC[lab,{\[Alpha],\[Beta],p,j}],{p,3}],
	_,
	Message[MassRotate::error,WC[lab,flav]];Abort[];
	],
_,
Message[MassRotate::error,WC[lab,flav]];Abort[];
];
Return@rot
]


MassRotate[Conjugate[a_],"du"]:=MassRotate[a,"ud"]\[Conjugate]
MassRotate[Conjugate[a_],"ud"]:=MassRotate[a,"du"]\[Conjugate]
MassRotate[Conjugate[a_],"uu"]:=MassRotate[a,"uu"]\[Conjugate]
MassRotate[Conjugate[a_],"dd"]:=MassRotate[a,"dd"]\[Conjugate]


(* ::Subsubsection:: *)
(*W couplings*)


g22onmW2:=(4\[Pi] Param["\[Alpha]EM"])/(Param["sW"]^2 Mass["WBoson"]^2);


WCoupling["l",{\[Alpha]_,\[Beta]_}]:=KroneckerDelta[\[Alpha],\[Beta]]+Param["vev"]^2 WC["Hl3",{\[Alpha],\[Beta]}];


WCoupling["q",{i_,j_}]:=Vckm[i,j]+Param["vev"]^2 MassRotate[WC["Hq3",{i,j}],"ud"];


WCoupling["ud",{i_,j_}]:=1/2 Param["vev"]^2 WC["Hud",{i,j}];


(* ::Subsubsection:: *)
(*Z couplings*)


gZ2onmZ2:=(4\[Pi] Param["\[Alpha]EM"])/(Param["cW"]^2 Param["sW"]^2 Mass["ZBoson"]^2);


ZCoupling["\[Nu]L",{\[Alpha]_,\[Beta]_}]:=1/2 KroneckerDelta[\[Alpha],\[Beta]]-1/2 Param["vev"]^2 WC["Hl1",{\[Alpha],\[Beta]}]+1/2 Param["vev"]^2 WC["Hl3",{\[Alpha],\[Beta]}];


ZCoupling["eL",{\[Alpha]_,\[Beta]_}]:=(Param["sW"]^2-1/2)KroneckerDelta[\[Alpha],\[Beta]]-1/2 Param["vev"]^2 WC["Hl1",{\[Alpha],\[Beta]}]-1/2 Param["vev"]^2 WC["Hl3",{\[Alpha],\[Beta]}];


ZCoupling["eR",{\[Alpha]_,\[Beta]_}]:=Param["sW"]^2 KroneckerDelta[\[Alpha],\[Beta]]-1/2 Param["vev"]^2 WC["He",{\[Alpha],\[Beta]}];


ZCoupling["uL",{i_,j_}]:=(1/2-2/3 Param["sW"]^2)KroneckerDelta[i,j]-1/2 Param["vev"]^2 MassRotate[WC["Hq1",{i,j}],"uu"]+1/2 Param["vev"]^2 MassRotate[WC["Hq3",{i,j}],"uu"];


ZCoupling["uR",{i_,j_}]:=(-(2/3) Param["sW"]^2)KroneckerDelta[i,j]-1/2 Param["vev"]^2 WC["Hu",{i,j}];


ZCoupling["dL",{i_,j_}]:=(-(1/2)+1/3 Param["sW"]^2)KroneckerDelta[i,j]-1/2 Param["vev"]^2 MassRotate[WC["Hq1",{i,j}],"dd"]-1/2 Param["vev"]^2 MassRotate[WC["Hq3",{i,j}],"dd"];


ZCoupling["dR",{i_,j_}]:=(1/3 Param["sW"]^2)KroneckerDelta[i,j]-1/2 Param["vev"]^2 WC["Hd",{i,j}];


(* ::Subsection::Closed:: *)
(*\[Nu]\[Nu]X*)


TLMatching[WCL["\[Nu]\[Gamma]",{\[Alpha]_,\[Beta]_}]]:=0;


(* ::Subsection:: *)
(*(LR)X*)


(* ::Subsubsection::Closed:: *)
(*Leptonic*)


TLMatching[WCL["e\[Gamma]",{\[Alpha]_,\[Beta]_}]]:=1/Sqrt[2] Param["vev"](-Param["sW"]WC["eW",{\[Alpha],\[Beta]}]+Param["cW"]WC["eB",{\[Alpha],\[Beta]}]);


(* ::Subsubsection::Closed:: *)
(*Nonleptonic*)


TLMatching[WCL["u\[Gamma]",{i_,j_}]]:=1/Sqrt[2] Param["vev"](Param["sW"]MassRotate[WC["uW",{i,j}],"u"]+Param["cW"]MassRotate[WC["uB",{i,j}],"u"]);


TLMatching[WCL["d\[Gamma]",{i_,j_}]]:=1/Sqrt[2] Param["vev"](-Param["sW"]MassRotate[WC["dW",{i,j}],"d"]+Param["cW"]MassRotate[WC["dB",{i,j}],"d"]);


TLMatching[WCL["uG",{i_,j_}]]:=1/Sqrt[2] Param["vev"]MassRotate[WC["uG",{i,j}],"u"];


TLMatching[WCL["dG",{i_,j_}]]:=1/Sqrt[2] Param["vev"]MassRotate[WC["dG",{i,j}],"d"];


(* ::Subsection::Closed:: *)
(*X^3*)


TLMatching[WCL["G",{}]]:=WC["G",{}];


TLMatching[WCL["Gt",{}]]:=WC["Gt",{}];


(* ::Subsection:: *)
(*(LL) (LL)*)


(* ::Subsubsection::Closed:: *)
(*Leptonic*)


TLMatching[WCL["\[Nu]\[Nu]VLL",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}]]:=WC["ll",{\[Alpha],\[Beta],\[Gamma],\[Delta]}]-gZ2onmZ2/4 ZCoupling["\[Nu]L",{\[Alpha],\[Beta]}]ZCoupling["\[Nu]L",{\[Gamma],\[Delta]}]-gZ2onmZ2/4 ZCoupling["\[Nu]L",{\[Alpha],\[Delta]}]ZCoupling["\[Nu]L",{\[Gamma],\[Beta]}];


TLMatching[WCL["eeVLL",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}]]:=WC["ll",{\[Alpha],\[Beta],\[Gamma],\[Delta]}]-gZ2onmZ2/4 ZCoupling["eL",{\[Alpha],\[Beta]}]ZCoupling["eL",{\[Gamma],\[Delta]}]-gZ2onmZ2/4 ZCoupling["eL",{\[Alpha],\[Delta]}]ZCoupling["eL",{\[Gamma],\[Beta]}];
TLMatching[WCL["\[Nu]eVLL",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}]]:=WC["ll",{\[Alpha],\[Beta],\[Gamma],\[Delta]}]+WC["ll",{\[Gamma],\[Delta],\[Alpha],\[Beta]}]-g22onmW2/2 WCoupling["l",{\[Alpha],\[Delta]}]WCoupling["l",{\[Beta],\[Gamma]}]\[Conjugate]-gZ2onmZ2 ZCoupling["\[Nu]L",{\[Alpha],\[Beta]}]ZCoupling["eL",{\[Gamma],\[Delta]}];


(* ::Subsubsection:: *)
(*Semileptonic*)


TLMatching[WCL["\[Nu]uVLL",{\[Alpha]_,\[Beta]_,i_,j_}]]:=MassRotate[WC["lq1",{\[Alpha],\[Beta],i,j}],"uu"]+MassRotate[WC["lq3",{\[Alpha],\[Beta],i,j}],"uu"]-gZ2onmZ2 ZCoupling["\[Nu]L",{\[Alpha],\[Beta]}]ZCoupling["uL",{i,j}];


TLMatching[WCL["\[Nu]dVLL",{\[Alpha]_,\[Beta]_,i_,j_}]]:=MassRotate[WC["lq1",{\[Alpha],\[Beta],i,j}],"dd"]-MassRotate[WC["lq3",{\[Alpha],\[Beta],i,j}],"dd"]-gZ2onmZ2 ZCoupling["\[Nu]L",{\[Alpha],\[Beta]}]ZCoupling["dL",{i,j}];


TLMatching[WCL["euVLL",{\[Alpha]_,\[Beta]_,i_,j_}]]:=MassRotate[WC["lq1",{\[Alpha],\[Beta],i,j}],"uu"]-MassRotate[WC["lq3",{\[Alpha],\[Beta],i,j}],"uu"]-gZ2onmZ2 ZCoupling["eL",{\[Alpha],\[Beta]}]ZCoupling["uL",{i,j}];


TLMatching[WCL["edVLL",{\[Alpha]_,\[Beta]_,i_,j_}]]:=MassRotate[WC["lq1",{\[Alpha],\[Beta],i,j}],"dd"]+MassRotate[WC["lq3",{\[Alpha],\[Beta],i,j}],"dd"]-gZ2onmZ2 ZCoupling["eL",{\[Alpha],\[Beta]}]ZCoupling["dL",{i,j}];


TLMatching[WCL["\[Nu]eduVLL",{\[Alpha]_,\[Beta]_,i_,j_}]]:=2 MassRotate[WC["lq3",{\[Alpha],\[Beta],i,j}],"du"]-g22onmW2/2 WCoupling["l",{\[Alpha],\[Beta]}]WCoupling["q",{j,i}]\[Conjugate];


(* ::Subsubsection:: *)
(*Nonleptonic*)


TLMatching[WCL["uuVLL",{i_,j_,k_,l_}]]:=MassRotate[WC["qq1",{i,j,k,l}],"uuuu"]+MassRotate[WC["qq3",{i,j,k,l}],"uuuu"]-gZ2onmZ2/2 ZCoupling["uL",{i,j}]ZCoupling["uL",{k,l}];


TLMatching[WCL["ddVLL",{i_,j_,k_,l_}]]:=MassRotate[WC["qq1",{i,j,k,l}],"dddd"]+MassRotate[WC["qq3",{i,j,k,l}],"dddd"]-gZ2onmZ2/2 ZCoupling["dL",{i,j}]ZCoupling["dL",{k,l}];


TLMatching[WCL["udV1LL",{i_,j_,k_,l_}]]:=MassRotate[WC["qq1",{i,j,k,l}],"uudd"]+MassRotate[WC["qq1",{k,l,i,j}],"uudd"]-MassRotate[WC["qq3",{i,j,k,l}],"uudd"]-MassRotate[WC["qq3",{k,l,i,j}],"uudd"]+2/3 MassRotate[WC["qq3",{i,l,k,j}],"uddu"]+2/3 MassRotate[WC["qq3",{k,j,i,l}],"duud"]-g22onmW2/2 1/3 WCoupling["q",{i,l}]WCoupling["q",{j,k}]\[Conjugate]-gZ2onmZ2 ZCoupling["uL",{i,j}]ZCoupling["dL",{k,l}];


TLMatching[WCL["udV8LL",{i_,j_,k_,l_}]]:=4 MassRotate[WC["qq3",{i,l,k,j}],"uddu"]+4 MassRotate[WC["qq3",{k,j,i,l}],"duud"]-g22onmW2 WCoupling["q",{i,l}]WCoupling["q",{j,k}]\[Conjugate];


(* ::Subsection:: *)
(*(RR) (RR)*)


(* ::Subsubsection::Closed:: *)
(*Leptonic*)


TLMatching[WCL["eeVRR",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}]]:=WC["ee",{\[Alpha],\[Beta],\[Gamma],\[Delta]}]-gZ2onmZ2/4 ZCoupling["eR",{\[Alpha],\[Beta]}]ZCoupling["eR",{\[Gamma],\[Delta]}]-gZ2onmZ2/4 ZCoupling["eR",{\[Alpha],\[Delta]}]ZCoupling["eR",{\[Gamma],\[Beta]}];


(* ::Subsubsection::Closed:: *)
(*Semileptonic*)


TLMatching[WCL["euVRR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=WC["eu",{\[Alpha],\[Beta],i,j}]-gZ2onmZ2 ZCoupling["eR",{\[Alpha],\[Beta]}]ZCoupling["uR",{i,j}];


TLMatching[WCL["edVRR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=WC["ed",{\[Alpha],\[Beta],i,j}]-gZ2onmZ2 ZCoupling["eR",{\[Alpha],\[Beta]}]ZCoupling["dR",{i,j}];


(* ::Subsubsection:: *)
(*Nonleptonic*)


TLMatching[WCL["uuVRR",{i_,j_,k_,l_}]]:=WC["uu",{i,j,k,l}]-gZ2onmZ2/2 ZCoupling["uR",{i,j}]ZCoupling["uR",{k,l}];


TLMatching[WCL["ddVRR",{i_,j_,k_,l_}]]:=WC["dd",{i,j,k,l}]-gZ2onmZ2/2 ZCoupling["dR",{i,j}]ZCoupling["dR",{k,l}];


TLMatching[WCL["udV1RR",{i_,j_,k_,l_}]]:=WC["ud1",{i,j,k,l}]-g22onmW2/2 1/3 WCoupling["ud",{i,l}]WCoupling["ud",{j,k}]\[Conjugate]-gZ2onmZ2 ZCoupling["uR",{i,j}]ZCoupling["dR",{k,l}];


TLMatching[WCL["udV8RR",{i_,j_,k_,l_}]]:=WC["ud8",{i,j,k,l}]-g22onmW2 WCoupling["ud",{i,l}]WCoupling["ud",{j,k}]\[Conjugate];


(* ::Subsection:: *)
(*(LL) (RR)*)


(* ::Subsubsection::Closed:: *)
(*Leptonic*)


TLMatching[WCL["\[Nu]eVLR",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}]]:=WC["le",{\[Alpha],\[Beta],\[Gamma],\[Delta]}]-gZ2onmZ2 ZCoupling["\[Nu]L",{\[Alpha],\[Beta]}]ZCoupling["eR",{\[Gamma],\[Delta]}];


TLMatching[WCL["eeVLR",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}]]:=WC["le",{\[Alpha],\[Beta],\[Gamma],\[Delta]}]-gZ2onmZ2 ZCoupling["eL",{\[Alpha],\[Beta]}]ZCoupling["eR",{\[Gamma],\[Delta]}];


(* ::Subsubsection:: *)
(*Semileptonic*)


TLMatching[WCL["\[Nu]uVLR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=WC["lu",{\[Alpha],\[Beta],i,j}]-gZ2onmZ2 ZCoupling["\[Nu]L",{\[Alpha],\[Beta]}]ZCoupling["uR",{i,j}];


TLMatching[WCL["\[Nu]dVLR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=WC["ld",{\[Alpha],\[Beta],i,j}]-gZ2onmZ2 ZCoupling["\[Nu]L",{\[Alpha],\[Beta]}]ZCoupling["dR",{i,j}];


TLMatching[WCL["euVLR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=WC["lu",{\[Alpha],\[Beta],i,j}]-gZ2onmZ2 ZCoupling["eL",{\[Alpha],\[Beta]}]ZCoupling["uR",{i,j}];


TLMatching[WCL["edVLR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=WC["ld",{\[Alpha],\[Beta],i,j}]-gZ2onmZ2 ZCoupling["eL",{\[Alpha],\[Beta]}]ZCoupling["dR",{i,j}];


TLMatching[WCL["ueVLR",{i_,j_,\[Alpha]_,\[Beta]_}]]:=MassRotate[WC["eq",{\[Alpha],\[Beta],i,j}],"uu"]-gZ2onmZ2 ZCoupling["eR",{\[Alpha],\[Beta]}]ZCoupling["uL",{i,j}];


TLMatching[WCL["deVLR",{i_,j_,\[Alpha]_,\[Beta]_}]]:=MassRotate[WC["eq",{\[Alpha],\[Beta],i,j}],"dd"]-gZ2onmZ2 ZCoupling["eR",{\[Alpha],\[Beta]}]ZCoupling["dL",{i,j}];


TLMatching[WCL["\[Nu]eduVLR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=-(g22onmW2/2)WCoupling["l",{\[Alpha],\[Beta]}]WCoupling["ud",{j,i}]\[Conjugate];


(* ::Subsubsection:: *)
(*Nonleptonic*)


TLMatching[WCL["uuV1LR",{i_,j_,k_,l_}]]:=MassRotate[WC["qu1",{i,j,k,l}],"uu"]-gZ2onmZ2 ZCoupling["uL",{i,j}]ZCoupling["uR",{k,l}];


TLMatching[WCL["uuV8LR",{i_,j_,k_,l_}]]:=MassRotate[WC["qu8",{i,j,k,l}],"uu"];


TLMatching[WCL["udV1LR",{i_,j_,k_,l_}]]:=MassRotate[WC["qd1",{i,j,k,l}],"uu"]-gZ2onmZ2 ZCoupling["uL",{i,j}]ZCoupling["dR",{k,l}];


TLMatching[WCL["udV8LR",{i_,j_,k_,l_}]]:=MassRotate[WC["qd8",{i,j,k,l}],"uu"];


TLMatching[WCL["duV1LR",{i_,j_,k_,l_}]]:=MassRotate[WC["qu1",{i,j,k,l}],"dd"]-gZ2onmZ2 ZCoupling["dL",{i,j}]ZCoupling["uR",{k,l}];


TLMatching[WCL["duV8LR",{i_,j_,k_,l_}]]:=MassRotate[WC["qu8",{i,j,k,l}],"dd"];


TLMatching[WCL["ddV1LR",{i_,j_,k_,l_}]]:=MassRotate[WC["qd1",{i,j,k,l}],"dd"]-gZ2onmZ2 ZCoupling["dL",{i,j}]ZCoupling["dR",{k,l}];


TLMatching[WCL["ddV8LR",{i_,j_,k_,l_}]]:=MassRotate[WC["qd8",{i,j,k,l}],"dd"];


TLMatching[WCL["udduV1LR",{i_,j_,k_,l_}]]:=-(g22onmW2/2)WCoupling["q",{i,j}]WCoupling["ud",{j,i}]\[Conjugate];


TLMatching[WCL["udduV8LR",{i_,j_,k_,l_}]]:=0;


(* ::Subsection::Closed:: *)
(*(LR) (RL)*)


(* ::Subsubsection:: *)
(*Semileptonic*)


TLMatching[WCL["euSRL",{\[Alpha]_,\[Beta]_,i_,j_}]]:=0;


TLMatching[WCL["edSRL",{\[Alpha]_,\[Beta]_,i_,j_}]]:=MassRotate[WC["ledq",{\[Alpha],\[Beta],i,j}],"d"];


TLMatching[WCL["\[Nu]eduSRL",{\[Alpha]_,\[Beta]_,i_,j_}]]:=MassRotate[WC["ledq",{\[Alpha],\[Beta],i,j}],"u"];


(* ::Subsection:: *)
(*(LR) (LR)*)


(* ::Subsubsection::Closed:: *)
(*Leptonic*)


TLMatching[WCL["eeSRR",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}]]:=0;


(* ::Subsubsection::Closed:: *)
(*Semileptonic*)


TLMatching[WCL["euSRR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=-MassRotate[WC["lequ1",{\[Alpha],\[Beta],i,j}],"u"];


TLMatching[WCL["euTRR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=-MassRotate[WC["lequ3",{\[Alpha],\[Beta],i,j}],"u"];


TLMatching[WCL["edSRR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=0;


TLMatching[WCL["edTRR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=0;


TLMatching[WCL["\[Nu]eduSRR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=MassRotate[WC["lequ1",{\[Alpha],\[Beta],i,j}],"d"];


TLMatching[WCL["\[Nu]eduTRR",{\[Alpha]_,\[Beta]_,i_,j_}]]:=MassRotate[WC["lequ3",{\[Alpha],\[Beta],i,j}],"d"];


(* ::Subsubsection:: *)
(*Nonleptonic*)


TLMatching[WCL["uuS1RR",{i_,j_,k_,l_}]]:=0;


TLMatching[WCL["uuS8RR",{i_,j_,k_,l_}]]:=0;


TLMatching[WCL["udS1RR",{i_,j_,k_,l_}]]:=MassRotate[WC["quqd1",{i,j,k,l}],"u"];


TLMatching[WCL["udS8RR",{i_,j_,k_,l_}]]:=MassRotate[WC["quqd8",{i,j,k,l}],"u"];


TLMatching[WCL["ddS1RR",{i_,j_,k_,l_}]]:=0;


TLMatching[WCL["ddS8RR",{i_,j_,k_,l_}]]:=0;


TLMatching[WCL["udduS1RR",{i_,j_,k_,l_}]]:=-MassRotate[WC["quqd1",{k,l,i,j}],"d"];


TLMatching[WCL["udduS8RR",{i_,j_,k_,l_}]]:=-MassRotate[WC["quqd8",{k,l,i,j}],"d"];


(* ::Section:: *)
(*One - loop matching*)


Get@FileNameJoin[{Global`$DirectoryHighPT,"RGE","Matching","OneLoopMatching.dat"}];


ReplaceMasses=<|
MD[1]->Mass["d"],
MD[2]->Mass["s"],
MD[3]->Mass["b"],
MU[1]->Mass["u"],
MU[2]->Mass["c"],
MU[3]->Mass["t"],
ME[1]->Mass["e"],
ME[2]->Mass["\[Mu]"],
ME[3]->Mass["\[Tau]"],
MNu[1]->0,
MNu[2]->0,
MNu[3]->0
|>;


EWScaleParameters = {
	"mu" -> Around[1.27,{0.42,0.50}]*10^-3,
	"mc" -> Around[0.619,0.084],
	"mt" -> Around[162.9,2.8],
	"md" -> Around[2.90,{1.19,1,24}]*10^-3,
	"ms" -> Around[55,15]*10^-3,
	"mb" -> Around[2.89,0.09],
	"me" -> Around[0.486570161,0.000000042]*10^-3,
	"m\[Mu]" -> Around[102.7181359,0.0000092]*10^-3,
	"m\[Tau]" -> Around[1746.24,{0.19,0.20}]*10^-3
};


(* ::Section:: *)
(*Matching function*)


Options[MatchToSMEFT]={
	SM -> False,
	LoopOrder :> MatchingOrder,
	MatchingScale -> DsixTools`EWSCALE,
	Basis :> GetBasisAlignment[]
};


MatchToSMEFT[expr_,OptionsPattern[]]:=Module[
	{
	res,
	currentmasses,
	currentVd = IdentityMatrix@3,
	currentBasis = "custom",
	basischanged = False
	}
	,
	Switch[OptionValue[LoopOrder],
		0,
		If[!MatchQ[OptionValue[Basis],GetBasisAlignment[]],
			basischanged = True;
			If[MatchQ[GetBasisAlignment[],"custom"],
				currentVd = Vd;,
				currentBasis = GetBasisAlignment[];
			];
			DefineBasisAlignment[OptionValue[Basis]];DefineParameters[];
		];
		If[
			!OptionValue[SM],
			res=expr/.a_WCL:>(Series[(TLMatching[a]-(TLMatching[a]/._WC->0))/.b_WC->eps*b/.Conjugate[Times[eps,c_WC]]->Times[eps,Conjugate[c]],{eps,0,1}]//Normal)/.eps->1,
			res=(*Series[*)expr/.a_WCL->TLMatching[a](*,{Param["vev"],0,2}]//Normal*)
		];
		If[basischanged,
			If[MatchQ[currentBasis,"custom"],
				DefineBasisAlignment[currentVd];DefineParameters[];,
				DefineBasisAlignment[currentBasis];DefineParameters[];
			];
		];,
		1,
		currentmasses=Association[Table[i->Mass[i]/.GetParameters[Errors->True],{i,{"u","c","t","d","s","b","e","\[Mu]","\[Tau]"}}]];
		DefineParameters[EWScaleParameters];
		If[
			!OptionValue[SM],
			res=expr/.a_WCL:>(OneLoopMatching[a]-(OneLoopMatching[a]/._WC->0))/.\[Mu]W->OptionValue[MatchingScale]/.ReplaceMasses,
			res=expr/.a_WCL->OneLoopMatching[a]/.\[Mu]W->OptionValue[MatchingScale]/.ReplaceMasses
		];
		DefineParameters[
			"mu" -> currentmasses["u"],
			"mc" -> currentmasses["c"],
			"mt" -> currentmasses["t"],
			"md" -> currentmasses["d"],
			"ms" -> currentmasses["s"],
			"mb" -> currentmasses["b"],
			"me" -> currentmasses["e"],
			"m\[Mu]" -> currentmasses["\[Mu]"],
			"m\[Tau]" -> currentmasses["\[Tau]"]
			]
		];
	Return@res
];


(* ::Section:: *)
(*Basis rotation*)


RotateBasis::undefinedmode="`1` is not defined for newbasis"
RotateBasis::error="Error in the mass rotation of `1`"


RotateBasis[WC[lab_,flav_],newbasis_]:=Module[
	{
	rot,i,j,k,l,\[Alpha],\[Beta],R
	}
	,
	Switch[
		newbasis,
		"up",
		R=CKM,
		"down",
		R=ConjugateTranspose[CKM],
		_,
		Message[RotateBasis::undefinedmode,newbasis];Abort[];
	];
	Switch[
		lab,
		"Hq1" | "Hq3",
		i=flav[[1]];j=flav[[2]];
		rot=Sum[ConjugateTranspose[R][[i,p]]WC[lab,{p,r}]R[[r,j]],{p,3},{r,3}];
		,
		"uH" | "dH" | "uG" | "uW" | "uB" | "dG" | "dW" | "dB",
		i=flav[[1]];j=flav[[2]];
		rot=Sum[ConjugateTranspose[R][[i,p]]WC[lab,{p,j}],{p,3}];
		,
		"lq1" | "lq3" | "eq",
		\[Alpha]=flav[[1]];\[Beta]=flav[[2]];i=flav[[3]];j=flav[[4]];
		rot=Sum[ConjugateTranspose[R][[i,p]]WC[lab,{\[Alpha],\[Beta],p,r}]R[[r,j]],{p,3},{r,3}];
		,
		"qq1" | "qq3",
		i=flav[[1]];j=flav[[2]];k=flav[[3]];l=flav[[4]];
		rot=Sum[ConjugateTranspose[R][[i,p]]ConjugateTranspose[R][[k,s]]WC[lab,{p,r,s,t}]R[[r,j]]R[[t,l]],{p,3},{r,3},{s,3},{t,3}];
		,
		"qu1" | "qu8" | "qd1" | "qd8",
		i=flav[[1]];j=flav[[2]];k=flav[[3]];l=flav[[4]];
		rot=Sum[ConjugateTranspose[R][[i,p]]WC[lab,{p,r,k,l}]R[[r,j]],{p,3},{r,3}];
		,
		"ledq",
		\[Alpha]=flav[[1]];\[Beta]=flav[[2]];i=flav[[3]];j=flav[[4]];
		rot=Sum[WC[lab,{\[Alpha],\[Beta],i,r}]R[[r,j]],{r,3}];
		,
		"quqd1" | "quqd8",
		i=flav[[1]];j=flav[[2]];k=flav[[3]];l=flav[[4]];
		rot=Sum[ConjugateTranspose[R][[i,p]]ConjugateTranspose[R][[k,s]]WC[lab,{p,j,s,l}],{p,3},{s,3}];
		,
		"lequ1" | "lequ3",
		\[Alpha]=flav[[1]];\[Beta]=flav[[2]];i=flav[[3]];j=flav[[4]];
		rot=Sum[ConjugateTranspose[R][[i,s]]WC[lab,{\[Alpha],\[Beta],s,j}],{s,3}];
		,
		_,
		rot=WC[lab,flav];
	];
	Return@rot
];


CKMUnitarityRep=Join[
Table[Vckm[3,i]\[Conjugate]Vckm[3,j]->KroneckerDelta[i,j]-Vckm[2,i]\[Conjugate]Vckm[2,j]-Vckm[1,i]\[Conjugate]Vckm[1,j],{i,3},{j,3}]//Flatten,
Table[If[i!=j,Vckm[i,3]Vckm[j,3]\[Conjugate]->KroneckerDelta[i,j]-Vckm[i,2]Vckm[j,2]\[Conjugate]-Vckm[i,1]Vckm[j,1]\[Conjugate],Nothing[]],{i,3},{j,3}]//Flatten
];


ToUpBasis[expr_]:=(*Expand[*)expr/.a_WC->RotateBasis[a,"up"](*]*)/.CKMUnitarityRep


ToDownBasis[expr_]:=(*Expand[*)expr/.a_WC->RotateBasis[a,"down"](*]*)/.CKMUnitarityRep
