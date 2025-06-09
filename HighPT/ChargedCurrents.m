(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`ChargedCurrents`*)


(* ::Subtitle:: *)
(*Charged current flavor observables*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


(* ::Subsection:: *)
(*Internal*)


PackageScope["SubstitutePsi"]


(* ::Chapter:: *)
(*Private:*)


(*$ChargedCurrentSectors={"b->c"};*)


(*FlavorObservables["ChargedCurrents"] = FlavorObservables/@$ChargedCurrentSectors*)


(*ObsTable["ChargedCurrents"] := Grid[{{"ChargedCurrents",Column[ObsTable/@$ChargedCurrentSectors]}},Dividers->All];*)


ObservableSectors["ChargedCurrents"] := {"b->c","s->u"};
ObservableList["ChargedCurrents"] := ObservableList/@ObservableSectors["ChargedCurrents"]


(* ::Section:: *)
(*Semileptonic inputs NEW*)


(* ::Code:: *)
(**)


SubstitutePsi = {
	(* VV *)
	Psi["VV"]["K+->\[Pi]0e\[Nu]"] -> Around[9.23,0.05]*10^-8,
	Psi["VV"]["KL->\[Pi]-e\[Nu]"] -> Around[9.34,0.05]*10^-8,
	Psi["VV"]["B0->D+e\[Nu]"] -> Around[0.00545,0.00014],
	Psi["VV"]["B-->D0e\[Nu]"] -> Around[0.00548,0.00014],
	Psi["VV"]["B0->D+\[Mu]\[Nu]"] -> Around[0.00543,0.00014],
	Psi["VV"]["B-->D0\[Mu]\[Nu]"] -> Around[0.00546,0.00014],
	Psi["VV"]["B0->\[Pi]-e\[Nu]"] -> Around[0.00112,0.00011],
	Psi["VV"]["B0->\[Pi]-\[Mu]\[Nu]"] -> Around[0.00112,0.00011],
	(* VS *)
	Psi["VS"]["K+->\[Pi]0e\[Nu]"] -> Around[2.423,0.013]*10^-10,
	Psi["VS"]["KL->\[Pi]-e\[Nu]"] -> Around[2.447,0.014]*10^-10,
	Psi["VS"]["B0->D+e\[Nu]"] -> Around[1.350,0.030]*10^-5,
	Psi["VS"]["B-->D0e\[Nu]"] -> Around[1.357,0.030]*10^-5,
	Psi["VS"]["B0->D+\[Mu]\[Nu]"] -> Around[0.00275,0.00006],
	Psi["VS"]["B-->D0\[Mu]\[Nu]"] -> Around[0.00276,0.00006],
	Psi["VS"]["B0->\[Pi]-e\[Nu]"] -> Around[3.79,0.33]*10^-6,
	Psi["VS"]["B0->\[Pi]-\[Mu]\[Nu]"] -> Around[7.8,0.7]*10^-4,
	(* VT *)
	Psi["VT"]["K+->\[Pi]0e\[Nu]"] -> Around[3.83,0.14]*10^-10,
	Psi["VT"]["KL->\[Pi]-e\[Nu]"] -> Around[3.83,0.14]*10^-10,
	Psi["VT"]["B0->D+e\[Nu]"] -> Around[5.06,0.34]*10^-6,
	Psi["VT"]["B-->D0e\[Nu]"] -> Around[5.09,0.34]*10^-6,
	Psi["VT"]["B0->D+\[Mu]\[Nu]"] -> Around[0.00102,0.00007],
	Psi["VT"]["B-->D0\[Mu]\[Nu]"] -> Around[0.00103,0.00007],
	Psi["VT"]["B0->\[Pi]-e\[Nu]"] -> Around[1.17,0.08]*10^-6,
	Psi["VT"]["B0->\[Pi]-\[Mu]\[Nu]"] -> Around[2.41,0.17]*10^-4,
	(* SS *)
	Psi["SS"]["K+->\[Pi]0e\[Nu]"] -> Around[1.209,0.009]*10^-8,
	Psi["SS"]["KL->\[Pi]-e\[Nu]"] -> Around[1.217,0.009]*10^-8,
	Psi["SS"]["B0->D+e\[Nu]"] -> Around[0.0658,0.0012],
	Psi["SS"]["B-->D0e\[Nu]"] -> Around[0.0663,0.0012],
	Psi["SS"]["B0->D+\[Mu]\[Nu]"] -> Around[0.0655,0.0012],
	Psi["SS"]["B-->D0\[Mu]\[Nu]"] -> Around[0.0661,0.0012],
	Psi["SS"]["B0->\[Pi]-e\[Nu]"] -> Around[0.077,0.007],
	Psi["SS"]["B0->\[Pi]-\[Mu]\[Nu]"] -> Around[0.077,0.007],
	(* TT *)
	Psi["TT"]["K+->\[Pi]0e\[Nu]"] -> Around[1.20,0.09]*10^-8,
	Psi["TT"]["KL->\[Pi]-e\[Nu]"] -> Around[1.18,0.09]*10^-8,
	Psi["TT"]["B0->D+e\[Nu]"] -> Around[0.0037,0.0005],
	Psi["TT"]["B-->D0e\[Nu]"] -> Around[0.0037,0.0005],
	Psi["TT"]["B0->D+\[Mu]\[Nu]"] -> Around[0.0037,0.0005],
	Psi["TT"]["B-->D0\[Mu]\[Nu]"] -> Around[0.0037,0.0005],
	Psi["TT"]["B0->\[Pi]-e\[Nu]"] -> Around[0.0053,0.0005],
	Psi["TT"]["B0->\[Pi]-\[Mu]\[Nu]"] -> Around[0.0053,0.0005],
	(* Corrections for Kaons *)
	\[Delta]EM["K+->\[Pi]0e\[Nu]"] -> Around[0.21,0.05]*10^-2,
	\[Delta]EM["KL->\[Pi]-e\[Nu]"] -> Around[1.16,0.03]*10^-2,
	\[Delta]SU2["K+->\[Pi]0e\[Nu]"] -> Around[4.57,0.20]*10^-2,
	\[Delta]SU2["KL->\[Pi]-e\[Nu]"] -> Around[0,0],
	SEW -> Around[1.0232,0.0003]
};


(* ::Section:: *)
(*b -> c NEW*)


ObservableList["b->c"] := {(*"B->D\[Mu]\[Nu]"*)};


(*LowScale$default[Alternatives@@(ObservableList["b->c"]//Flatten)] := Mass["b"]/.GetParameters[];*)


BtoDl\[Nu][proc_String,lep_] := Sum[(Psi["VV"][proc]Abs[WCL["\[Nu]eduVLL",{nu,lep,3,2}] + WCL["\[Nu]eduVLR",{nu,lep,3,2}]]^2+Psi["VS"][proc]/(Mass["b"]-Mass["c"]) Re[(WCL["\[Nu]eduVLL",{nu,lep,3,2}] + WCL["\[Nu]eduVLR",{nu,lep,3,2}])(WCL["\[Nu]eduSRR",{nu,lep,3,2}]+WCL["\[Nu]eduSRL",{nu,lep,3,2}])\[Conjugate]]+Psi["VT"][proc]Re[(WCL["\[Nu]eduVLL",{nu,lep,3,2}] + WCL["\[Nu]eduVLR",{nu,lep,3,2}])WCL["\[Nu]eduTRR",{nu,lep,3,2}]\[Conjugate]]+Psi["SS"][proc]/(Mass["b"]-Mass["c"])^2 Abs[WCL["\[Nu]eduSRR",{nu,lep,3,2}] + WCL["\[Nu]eduSRL",{nu,lep,3,2}]]^2+Psi["TT"][proc]Abs[WCL["\[Nu]eduTRR",{nu,lep,3,2}]]^2),{nu,3}]


(* ::Subsection:: *)
(*B0 -> D+e\[Nu]*)


TheoryExpression["B0->D+e\[Nu]"] := Lifetime["B0"]*BtoDl\[Nu]["B0->D+e\[Nu]",1]


ExpValue$default["B0->D+e\[Nu]"] := 0 


(* ::Subsection:: *)
(*B- -> D0e\[Nu]*)


TheoryExpression["B-->D0e\[Nu]"] := Lifetime["B+"]*BtoDl\[Nu]["B-->D0e\[Nu]",1]


ExpValue$default["B-->D0e\[Nu]"] := 0 


(* ::Subsection:: *)
(*B0 -> D+\[Mu]\[Nu]*)


TheoryExpression["B0->D+\[Mu]\[Nu]"] := Lifetime["B0"]*BtoDl\[Nu]["B0->D+\[Mu]\[Nu]",2]


ExpValue$default["B0->D+\[Mu]\[Nu]"] := Around[2.03,Sqrt[0.04^2+0.08^2]]*(10^-2) (* Just Belle-II, temporary *)


(*SMValue$default["B->D\[Mu]\[Nu]"] := *)


(* ::Subsection:: *)
(*B- -> D0\[Mu]\[Nu]*)


TheoryExpression["B-->D0\[Mu]\[Nu]"] := Lifetime["B+"]*BtoDl\[Nu]["B-->D0\[Mu]\[Nu]",2]


(*TheoryExpression["B-->D0\[InvisibleComma]\[Mu]\[Nu]"] := Lifetime["B+"]*BtoDl\[Nu]["B-->D0\[Mu]\[Nu]",2]*)


ExpValue$default["B-->D0\[Mu]\[Nu]"] := 0 


(* ::Subsection:: *)
(*B0->D+l\[Nu] (e,\[Mu] average)*)


TheoryExpression["B0->D+l\[Nu]"] := 1/2 (TheoryExpression["B0->D+e\[Nu]"]+TheoryExpression["B0->D+\[Mu]\[Nu]"])//Simplify


ExpValue$default["B0->D+l\[Nu]"] := Around[2.12,Sqrt[0.02^2+0.06^2]]*10^-2


(* ::Subsection:: *)
(*B- ->D0l\[Nu] (e,\[Mu] average)*)


TheoryExpression["B-->D0l\[Nu]"] := 1/2 (TheoryExpression["B-->D0e\[Nu]"]+TheoryExpression["B-->D0\[Mu]\[Nu]"])//Simplify


ExpValue$default["B-->D0l\[Nu]"] := Around[2.21 . Sqrt[0.02^2+0.06^2]]*10^-2


(* ::Subsection:: *)
(*Isospin average*)


TheoryExpression["B->Dl\[Nu]_iso"] := 1/2 (TheoryExpression["B0->D+l\[Nu]"]+Lifetime["B0"]/Lifetime["B+"] TheoryExpression["B-->D0l\[Nu]"])//Simplify


ExpValue$default["B->Dl\[Nu]_iso"] := Around[2.11,Sqrt[0.01^2+0.05^2]]*10^-2


SMPrediction$default["B->Dl\[Nu]_iso"] := TheoryExpression["B->Dl\[Nu]_iso"]/.a_WCL->SMValue[a,TreeOnly->True]/.GetParameters[Errors->True]/.SubstitutePsi


ExpInfo["B->Dl\[Nu]_iso"] := "Isospin average of B0->D+ and B-->D0, from HFLAV (insert link)"


(* ::Section:: *)
(*b -> u*)


Bto\[Pi]l\[Nu][proc_String,lep_] := Sum[(Psi["VV"][proc]Abs[WCL["\[Nu]eduVLL",{nu,lep,3,1}] + WCL["\[Nu]eduVLR",{nu,lep,3,1}]]^2+Psi["VS"][proc]/(Mass["b"]-Mass["u"]) Re[(WCL["\[Nu]eduVLL",{nu,lep,3,1}] + WCL["\[Nu]eduVLR",{nu,lep,3,1}])(WCL["\[Nu]eduSRR",{nu,lep,3,1}]+WCL["\[Nu]eduSRL",{nu,lep,3,1}])\[Conjugate]]+Psi["VT"][proc]Re[(WCL["\[Nu]eduVLL",{nu,lep,3,1}] + WCL["\[Nu]eduVLR",{nu,lep,3,1}])WCL["\[Nu]eduTRR",{nu,lep,3,1}]\[Conjugate]]+Psi["SS"][proc]/(Mass["b"]-Mass["u"])^2 Abs[WCL["\[Nu]eduSRR",{nu,lep,3,1}] + WCL["\[Nu]eduSRL",{nu,lep,3,1}]]^2+Psi["TT"][proc]Abs[WCL["\[Nu]eduTRR",{nu,lep,3,1}]]^2),{nu,3}]


(* ::Subsection:: *)
(*B0 -> \[Pi]e\[Nu] (>16 GeV^2)*)


TheoryExpression["B0->\[Pi]-e\[Nu]_high"] := Lifetime["B0"]*Bto\[Pi]l\[Nu]["B0->\[Pi]-e\[Nu]",1]


(* ::Subsection:: *)
(*B0 -> \[Pi]\[Mu]\[Nu] (>16 GeV^2)*)


TheoryExpression["B0->\[Pi]-\[Mu]\[Nu]_high"] := Lifetime["B0"]*Bto\[Pi]l\[Nu]["B0->\[Pi]-\[Mu]\[Nu]",2]


(* ::Subsection:: *)
(*B0->\[Pi]l\[Nu] (e,\[Mu] average) (>16 GeV^2)*)


TheoryExpression["B0->\[Pi]-l\[Nu]_high"] := 1/2 (TheoryExpression["B0->\[Pi]-e\[Nu]_high"]+TheoryExpression["B0->\[Pi]-\[Mu]\[Nu]_high"])//Simplify


ExpValue$default["B0->\[Pi]-l\[Nu]_high"] := Around[4.04,0.21]*10^-5


SMPrediction$default["B0->\[Pi]-l\[Nu]_high"] := TheoryExpression["B0->\[Pi]-l\[Nu]_high"]/.a_WCL->SMValue[a,TreeOnly->True]/.GetParameters[Errors->True]/.SubstitutePsi


(* ::Subsection:: *)
(*Correlations*)


(*ExpCorrelation["RD\[Tau]l","RD*\[Tau]l"] = -0.39;*)


(* ::Section:: *)
(*s -> u semileptonic*)


Kto\[Pi]l\[Nu][proc_String,lep_] := (1+\[Delta]EM[proc]+\[Delta]SU2[proc])Sum[(Psi["VV"][proc]Abs[WCL["\[Nu]eduVLL",{nu,lep,2,1}] + WCL["\[Nu]eduVLR",{nu,lep,2,1}]]^2+Psi["VS"][proc]/(Mass["s"]-Mass["u"]) Re[(WCL["\[Nu]eduVLL",{nu,lep,2,1}] + WCL["\[Nu]eduVLR",{nu,lep,2,1}])(WCL["\[Nu]eduSRR",{nu,lep,2,1}]+WCL["\[Nu]eduSRL",{nu,lep,2,1}])\[Conjugate]]+Psi["VT"][proc]Re[(WCL["\[Nu]eduVLL",{nu,lep,2,1}] + WCL["\[Nu]eduVLR",{nu,lep,2,1}])WCL["\[Nu]eduTRR",{nu,lep,2,1}]\[Conjugate]]+Psi["SS"][proc]/(Mass["s"]-Mass["u"])^2 Abs[WCL["\[Nu]eduSRR",{nu,lep,2,1}] + WCL["\[Nu]eduSRL",{nu,lep,2,1}]]^2+Psi["TT"][proc]Abs[WCL["\[Nu]eduTRR",{nu,lep,2,1}]]^2),{nu,3}]


(* ::Subsection:: *)
(*K+ -> \[Pi]0e\[Nu]*)


TheoryExpression["K+->\[Pi]0e\[Nu]"] := Lifetime["K+"]/2*SEW*Kto\[Pi]l\[Nu]["K+->\[Pi]0e\[Nu]",1]


ExpValue$default["K+->\[Pi]0e\[Nu]"] := Around[5.088,0.027]*10^-2


SMPrediction$default["K+->\[Pi]0e\[Nu]"] := TheoryExpression["K+->\[Pi]0e\[Nu]"]/.a_WCL->SMValue[a,TreeOnly->True]/.GetParameters[Errors->True]/.SubstitutePsi


(* ::Subsection:: *)
(*KL -> \[Pi]-e\[Nu]*)


TheoryExpression["KL->\[Pi]-e\[Nu]"] := Lifetime["KL"]*SEW*Kto\[Pi]l\[Nu]["KL->\[Pi]-e\[Nu]",1]


ExpValue$default["KL->\[Pi]-e\[Nu]"] := Around[4.056,0.009]*10^-1


SMPrediction$default["KL->\[Pi]-e\[Nu]"] := TheoryExpression["KL->\[Pi]-e\[Nu]"]/.a_WCL->SMValue[a,TreeOnly->True]/.GetParameters[Errors->True]/.SubstitutePsi


(* ::Section:: *)
(*s -> u leptonic*)


ObservableList["s->u"] = {"K+->\[Mu]\[Nu]","K+->e\[Nu]"};


me = {Mass["e"],Mass["\[Mu]"],Mass["\[Tau]"]};


Ktol\[Nu][lep_] := Lifetime["K+"]/(64 \[Pi]) DecayConstant["K+"]^2 Mass["K+"]me[[lep]]^2 (1-me[[lep]]^2/Mass["K+"]^2)^2 Sum[Abs[WCL["\[Nu]eduVLL",{j,lep,2,1}]-WCL["\[Nu]eduVLR",{j,lep,2,1}]-Mass["K+"]^2/(Mass["u"]+Mass["s"]) (WCL["\[Nu]eduSRR",{j,lep,2,1}]-WCL["\[Nu]eduSRL",{j,lep,2,1}])]^2,{j,3}]


(* ::Subsection:: *)
(*K -> \[Mu]\[Nu]*)


TheoryExpression["K+->\[Mu]\[Nu]"] := Ktol\[Nu][2]


ExpValue$default["K+->\[Mu]\[Nu]"] := Around[63.56,0.11]*10^-2


\[Delta]K = Around[0.0107,0.0021];
SMPrediction$default["K+->\[Mu]\[Nu]"] := Lifetime["K+"]Param["GF"]^2/(8\[Pi]) DecayConstant["K+"]^2 Mass["\[Mu]"]^2 Mass["K+"](1-Mass["\[Mu]"]^2/Mass["K+"]^2)^2 Abs[Vckm[1,2]]^2 (1+\[Delta]K)/.GetParameters[Errors->True]


NPContribution$default["K+->\[Mu]\[Nu]"] := NPFromTheoryExpression["K+->\[Mu]\[Nu]"]


LowScale$default["K+->\[Mu]\[Nu]"] := Mass["K+"]/.GetParameters[]


(* ::Subsection:: *)
(*K -> e\[Nu]*)


TheoryExpression["K+->e\[Nu]"] := Ktol\[Nu][1]


ExpValue$default["K+->e\[Nu]"] := Around[1.582,0.007]*10^-5


SMPrediction$default["K+->e\[Nu]"] := Lifetime["K+"]Param["GF"]^2/(8\[Pi]) DecayConstant["K+"]^2 Mass["e"]^2 Mass["K+"](1-Mass["e"]^2/Mass["K+"]^2)^2 Abs[Vckm[1,2]]^2 (1+\[Delta]K)/.GetParameters[Errors->True]


NPContribution$default["K+->e\[Nu]"] := NPFromTheoryExpression["K+->e\[Nu]"]


LowScale$default["K+->e\[Nu]"] := Mass["K+"]/.GetParameters[]
