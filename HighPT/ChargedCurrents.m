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


ObservableSectors["ChargedCurrents"] := {"b->c"};
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
(*Semileptonic Inputs*)


(*(* Vectors of Eff. Coeff. *)
(* P\[Rule]P^'\[ScriptL]\[Nu] *)
coeffPPplnu={Abs[1+gV]^2,Abs[gT]^2,Re[(1+gV) Conjugate[gT]],Abs[gS]^2,Re[(1+gV) Conjugate[gS]]};
(* P\[Rule]V\[ScriptL]\[Nu] *)
coeffPVlnu={Abs[1+gVL]^2+Abs[gVR]^2,Re[(1+gVL)Conjugate[gVR]],Abs[gT]^2,Re[(1+gVL)Conjugate[gT]],Re[gVR Conjugate[gT]],Abs[gP]^2,Re[(1+gVL-gVR)Conjugate[gP]]};*)


(*(* Semileptonic replacement *)
RepSemilep={
gVL:>(WCL["\[Nu]eduVLL",ind]\[Conjugate]*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate]))),
gVR:>(WCL["\[Nu]eduVLR",ind]\[Conjugate]*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate]))),
gV:>((+WCL["\[Nu]eduVLL",ind]\[Conjugate]+WCL["\[Nu]eduVLR",ind]\[Conjugate])*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate]))),
gA:>((-WCL["\[Nu]eduVLL",ind]\[Conjugate]+WCL["\[Nu]eduVLR",ind]\[Conjugate])*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate]))),
gS:>((+WCL["\[Nu]eduSRR",ind]\[Conjugate]+WCL["\[Nu]eduSRL",ind]\[Conjugate])*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate]))),
gP:>((-WCL["\[Nu]eduSRR",ind]\[Conjugate]+WCL["\[Nu]eduSRL",ind]\[Conjugate])*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate]))),
gT:>(WCL["\[Nu]eduTRR",ind]\[Conjugate]*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate])))};*)


(* ::Subsection:: *)
(*Auxiliary computations*)


(* ::Subsubsection:: *)
(*B -> D*)


(*(* Reading data *)
{meanBDl\[Nu],\[Sigma]BDl\[Nu],corrBDl\[Nu]}=Get@FileNameJoin[{Global`$DirectoryHighPT,"Flavor","Inputs","data_BDlnu"}];*)


(*(* Magic numbers B\[Rule]Dl\[Nu] and errors: the other is BR + normalized coefficients wrt BR

Cvec={BR/|Subscript[V, cb]|^2; Abs[gT]^2,Re[(1+gV)Conjugate[gT]],Abs[gS]^2,Re[(1+gV)Conjugate[gS]]}
*)
{cvBrB0Dp\[Tau]\[Nu],cvBrB0Dp\[Mu]\[Nu],cvBrB0Dpe\[Nu]}={meanBDl\[Nu][[1;;5]],meanBDl\[Nu][[6;;10]],meanBDl\[Nu][[11;;15]]};
{\[Sigma]BrB0Dp\[Tau]\[Nu],\[Sigma]BrB0Dp\[Mu]\[Nu],\[Sigma]BrB0Dpe\[Nu]}={\[Sigma]BDl\[Nu][[1;;5]],\[Sigma]BDl\[Nu][[6;;10]],\[Sigma]BDl\[Nu][[11;;15]]};*)


(*(* Covariance matrix *)
covBDl\[Nu]=Table[\[Sigma]BDl\[Nu][[i]]corrBDl\[Nu][[i,j]]\[Sigma]BDl\[Nu][[j]],{i,1,Length[\[Sigma]BDl\[Nu]]},{j,1,Length[\[Sigma]BDl\[Nu]]}];*)


(*(* Covariance sub-matrices (w/o SM)  *)
covBD\[Tau]\[Nu]=covBDl\[Nu][[1;;5,1;;5]];
covBD\[Mu]\[Nu]=covBDl\[Nu][[6;;10,6;;10]];
covBDe\[Nu]=covBDl\[Nu][[11;;15,11;;15]];*)


(*(* Individual BRs *)
(* Central value (NB. We only account for the NP part here, that is why we start from the second element *)
\[Delta]BrB0Dp\[Tau]\[Nu]=Abs[1+gV]^2-1+coeffPPplnu[[2;;5]] . cvBrB0Dp\[Tau]\[Nu][[2;;5]]/.RepSemilep/.ind->{3,3,3,2}/.ind3->3/.ind4->2;
\[Delta]BrB0Dp\[Mu]\[Nu]=Abs[1+gV]^2-1+coeffPPplnu[[2;;5]] . cvBrB0Dp\[Mu]\[Nu][[2;;5]]/.RepSemilep/.ind->{2,2,3,2}/.ind3->3/.ind4->2;
\[Delta]BrB0Dpe\[Nu]=Abs[1+gV]^2-1+coeffPPplnu[[2;;5]] . cvBrB0Dpe\[Nu][[2;;5]]/.RepSemilep/.ind->{1,1,3,2}/.ind3->3/.ind4->2;
(* Error: *)
\[Sigma]\[Delta]BrB0Dp\[Tau]\[Nu]=Sqrt[coeffPPplnu[[2;;5]] . covBD\[Tau]\[Nu][[2;;5,2;;5]] . coeffPPplnu[[2;;5]]]/.RepSemilep/.ind->{3,3,3,2}/.ind3->3/.ind4->2//Expand;
\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]=Sqrt[coeffPPplnu[[2;;5]] . covBD\[Mu]\[Nu][[2;;5,2;;5]] . coeffPPplnu[[2;;5]]]/.RepSemilep/.ind->{2,2,3,2}/.ind3->3/.ind4->2//Expand;
\[Sigma]\[Delta]BrB0Dpe\[Nu]=Sqrt[coeffPPplnu[[2;;5]] . covBDe\[Nu][[2;;5,2;;5]] . coeffPPplnu[[2;;5]]]/.RepSemilep/.ind->{1,1,3,2}/.ind3->3/.ind4->2//Expand;
(**)
\[Sigma]\[Delta]BrB0Dp\[Tau]\[Nu]\[Delta]BrB0Dp\[Mu]\[Nu]=(coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{3,3,3,2}/.ind3->3/.ind4->2) . covBDl\[Nu][[2;;5,7;;10]] . (coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{2,2,3,2}/.ind3->3/.ind4->2);
\[Sigma]\[Delta]BrB0Dp\[Tau]\[Nu]\[Delta]BrB0Dpe\[Nu]=(coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{3,3,3,2}/.ind3->3/.ind4->2) . covBDl\[Nu][[2;;5,12;;15]] . (coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{1,1,3,2}/.ind3->3/.ind4->2);
\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]\[Delta]BrB0Dpe\[Nu]=(coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{2,2,3,2}/.ind3->3/.ind4->2) . covBDl\[Nu][[7;;10,12;;15]] . (coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{1,1,3,2}/.ind3->3/.ind4->2);*)


(* ::Subsubsection:: *)
(*B -> D**)


(*(* Reading data *)
{meanBDstl\[Nu],\[Sigma]BDstl\[Nu],corrBDstl\[Nu]}=Get@FileNameJoin[{Global`$DirectoryHighPT,"Flavor","Inputs","data_BDstlnu"}];*)


(*(* Magic numbers B\[Rule]D^*l\[Nu] and errors: the other is BR + normalized coefficients wrt BR

CVvec={
           BR/|Subscript[V, cb]|^2, 
           Abs[1+gVL]^2+Abs[gVR]^2,
           Re[(1+gVL) Conjugate[gVR]],
           Abs[gT]^2,
           Re[(1+gVL) Conjugate[gT]],
           Re[gVR Conjugate[gT]],
           Abs[gP]^2,
           Re[(1+gVL-gVR) Conjugate[gP]]
          }
*)
{cvBrB0Dstp\[Tau]\[Nu],cvBrB0Dstp\[Mu]\[Nu],cvBrB0Dstpe\[Nu]}={meanBDstl\[Nu][[1;;7]],meanBDstl\[Nu][[8;;14]],meanBDstl\[Nu][[15;;21]]};
{\[Sigma]BrB0Dstp\[Tau]\[Nu],\[Sigma]BrB0Dstp\[Mu]\[Nu],\[Sigma]BrB0Dstpe\[Nu]}={\[Sigma]BDstl\[Nu][[1;;7]],\[Sigma]BDstl\[Nu][[8;;14]],\[Sigma]BDstl\[Nu][[15;;21]]};*)


(*(* Covariance matrix *)
covBDstl\[Nu]=Table[\[Sigma]BDstl\[Nu][[i]]corrBDstl\[Nu][[i,j]]\[Sigma]BDstl\[Nu][[j]],{i,1,Length[\[Sigma]BDstl\[Nu]]},{j,1,Length[\[Sigma]BDstl\[Nu]]}];*)


(*(* Covariance sub-matrices (w/o SM)  *)
covBDst\[Tau]\[Nu]=covBDstl\[Nu][[1;;7,1;;7]];
covBDst\[Mu]\[Nu]=covBDstl\[Nu][[8;;14,8;;14]];
covBDste\[Nu]=covBDstl\[Nu][[15;;21,15;;21]];*)


(*(* Individual BRs *)
(* Central value (NB. We only account for the NP part here, that is why we start from the second element *)
\[Delta]BrB0Dstp\[Tau]\[Nu]=Abs[1+gVL]^2+Abs[gVR]^2-1+coeffPVlnu[[2;;7]] . cvBrB0Dstp\[Tau]\[Nu][[2;;7]]/.RepSemilep/.ind->{3,3,3,2}/.ind3->3/.ind4->2;
\[Delta]BrB0Dstp\[Mu]\[Nu]=Abs[1+gVL]^2+Abs[gVR]^2-1+coeffPVlnu[[2;;7]] . cvBrB0Dstp\[Mu]\[Nu][[2;;7]]/.RepSemilep/.ind->{2,2,3,2}/.ind3->3/.ind4->2;
\[Delta]BrB0Dstpe\[Nu]=Abs[1+gVL]^2+Abs[gVR]^2-1+coeffPVlnu[[2;;7]] . cvBrB0Dstpe\[Nu][[2;;7]]/.RepSemilep/.ind->{1,1,3,2}/.ind3->3/.ind4->2;
(* Error: *)
\[Sigma]\[Delta]BrB0Dstp\[Tau]\[Nu]=Sqrt[coeffPVlnu[[2;;7]] . covBDst\[Tau]\[Nu][[2;;7,2;;7]] . coeffPVlnu[[2;;7]]]/.RepSemilep/.ind->{3,3,3,2}/.ind3->3/.ind4->2//Expand;
\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]=Sqrt[coeffPVlnu[[2;;7]] . covBDst\[Mu]\[Nu][[2;;7,2;;7]] . coeffPVlnu[[2;;7]]]/.RepSemilep/.ind->{2,2,3,2}/.ind3->3/.ind4->2//Expand;
\[Sigma]\[Delta]BrB0Dstpe\[Nu]=Sqrt[coeffPVlnu[[2;;7]] . covBDste\[Nu][[2;;7,2;;7]] . coeffPVlnu[[2;;7]]]/.RepSemilep/.ind->{1,1,3,2}/.ind3->3/.ind4->2//Expand;
(**)
\[Sigma]\[Delta]BrB0Dstp\[Tau]\[Nu]\[Delta]BrB0Dstp\[Mu]\[Nu]=(coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{3,3,3,2}/.ind3->3/.ind4->2) . covBDstl\[Nu][[2;;7,9;;14]] . (coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{2,2,3,2}/.ind3->3/.ind4->2);
\[Sigma]\[Delta]BrB0Dstp\[Tau]\[Nu]\[Delta]BrB0Dstpe\[Nu]=(coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{3,3,3,2}/.ind3->3/.ind4->2) . covBDstl\[Nu][[2;;7,16;;21]] . (coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{1,1,3,2}/.ind3->3/.ind4->2);
\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]\[Delta]BrB0Dstpe\[Nu]=(coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{2,2,3,2}/.ind3->3/.ind4->2) . covBDstl\[Nu][[9;;14,16;;21]] . (coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{1,1,3,2}/.ind3->3/.ind4->2);*)


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
(*b -> c*)


(*FlavorObservables["b->c"] = {
	(*"Bc->\[Tau]\[Nu]",*)
	"RD\[Tau]l",
	"RD*\[Tau]l",
	"RD\[Mu]e",
	"RD*\[Mu]e"
};*)


(*ObsTable["b->c"] := Grid[{{"b->c",Column[FlavorObservables["b->c"]]}},Dividers->All];*)


(*(* \[Mu]b = 5 GeV *)
LowScale[Alternatives@@(FlavorObservables["b->c"]//Flatten)] := Mass["b"]/.GetParameters[];*)


(* ::Subsection:: *)
(*Bc -> \[Tau]\[Nu]*)


(* ::Subsubsection:: *)
(*Exp*)


(*ExpValue["Bc->\[Tau]\[Nu]"] = Around[0,0.3];*)


(* ::Subsubsection:: *)
(*SM*)


(*SMPrediction["Bc->\[Tau]\[Nu]"] = Around[0.0208,0.0007];*)


(* ::Subsubsection:: *)
(*NP*)


(* ::Subsubsection:: *)
(*GF*)


(* ::Subsubsection:: *)
(*CKM*)


(* ::Subsection:: *)
(*RD\[Tau]l*)


(* ::Subsubsection:: *)
(*Exp*)


(*ExpValue$default["RD\[Tau]l"] = Around[0.342,0.026];*)


(*ExpInfo["RD\[Tau]l"] = "HFLAV average from Winter 2023"*)


(* ::Subsubsection:: *)
(*SM*)


(*SMPrediction$default["RD\[Tau]l"] = (*Around[0.2938,0.0040];*)Around[0.298,0.004];*)


(*SMInfo["RD\[Tau]l"] = "HFLAV Winter 2023";*)


(* ::Subsubsection:: *)
(*NP*)


(*NPContribution$default["RD\[Tau]l"] := 
	(((1+\[Delta]BrB0Dp\[Tau]\[Nu])/(
		(1+SMPrediction$default["RD\[Mu]e"][[1]]^-1)^-1 (1+\[Delta]BrB0Dp\[Mu]\[Nu])+
		(1+SMPrediction$default["RD\[Mu]e"][[1]])^-1 (1+\[Delta]BrB0Dpe\[Nu])
		)-1)/.GetParameters[])/.Around[a_,b_]->a;*)


(*NPContributionError["RD\[Tau]l"] := 
	Abs[NPContribution$default["RD\[Tau]l"]+1]
	Sqrt[
		(Abs[\[Sigma]\[Delta]BrB0Dp\[Tau]\[Nu]]/(1+\[Delta]BrB0Dp\[Tau]\[Nu]))^2+
		(
			(1+SMPrediction$default["RD\[Mu]e"][[1]]^-1)^-2 Abs[\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]]^2+
			(1+SMPrediction$default["RD\[Mu]e"][[1]])^-2 Abs[\[Sigma]\[Delta]BrB0Dpe\[Nu]]^2
		)/
		(
			(1+SMPrediction$default["RD\[Mu]e"][[1]]^-1)^-1 (1+\[Delta]BrB0Dp\[Mu]\[Nu])+
			(1+SMPrediction$default["RD\[Mu]e"][[1]])^-1 (1+\[Delta]BrB0Dpe\[Nu])
		)^2-
		2(
			(
				(1+SMPrediction$default["RD\[Mu]e"][[1]]^-1)^-1 \[Sigma]\[Delta]BrB0Dp\[Tau]\[Nu]\[Delta]BrB0Dp\[Mu]\[Nu]+
				(1+SMPrediction$default["RD\[Mu]e"][[1]])^-1 \[Sigma]\[Delta]BrB0Dp\[Tau]\[Nu]\[Delta]BrB0Dpe\[Nu]
			)/
			(
				(1+\[Delta]BrB0Dp\[Tau]\[Nu])
					(
						(1+SMPrediction$default["RD\[Mu]e"][[1]]^-1)^-1 (1+\[Delta]BrB0Dp\[Mu]\[Nu])+
						(1+SMPrediction$default["RD\[Mu]e"][[1]])^-1 (1+\[Delta]BrB0Dpe\[Nu])
					)
				)
			)+
			2(
				(1+SMPrediction$default["RD\[Mu]e"][[1]]^-1)^-1 (1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]])^-1 \[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]\[Delta]BrB0Dpe\[Nu]
			)/
			(
				(1+SMPrediction$default["RD\[Mu]e"][[1]]^-1)^-1 (1+\[Delta]BrB0Dp\[Mu]\[Nu])+
				(1+SMPrediction$default["RD\[Mu]e"][[1]])^-1 (1+\[Delta]BrB0Dpe\[Nu])
			)^2
		]/.GetParameters[];*)


(* ::Subsubsection:: *)
(*GF*)


(* ::Subsubsection:: *)
(*CKM*)


(* ::Subsection:: *)
(*RD*\[Tau]l*)


(* ::Subsubsection:: *)
(*Exp*)


(*ExpValue$default["RD*\[Tau]l"] = Around[0.287,0.012]*)


(*ExpInfo["RD*\[Tau]l"] = "HFLAV average from Winter 2023"*)


(* ::Subsubsection:: *)
(*SM*)


(*SMPrediction$default["RD*\[Tau]l"] = (*Around[0.246,0.009];*)Around[0.254,0.005];*)


(*SMInfo["RD*\[Tau]l"] = "HFLAV Winter 2023";*)


(* ::Subsubsection:: *)
(*NP*)


(*NPContribution$default["RD*\[Tau]l"] := 
	((1+\[Delta]BrB0Dstp\[Tau]\[Nu])/
	(
		(1+SMPrediction$default["RD*\[Mu]e"][[1]]^-1)^-1 (1+\[Delta]BrB0Dstp\[Mu]\[Nu])+
		(1+SMPrediction$default["RD*\[Mu]e"][[1]])^-1 (1+\[Delta]BrB0Dstpe\[Nu])
	)-1)/.GetParameters[];*)


(*NPContributionError["RD*\[Tau]l"] := 
	Abs[NPContribution$default["RD*\[Tau]l"]+1]
	Sqrt[
		(Abs[\[Sigma]\[Delta]BrB0Dstp\[Tau]\[Nu]]/(1+\[Delta]BrB0Dstp\[Tau]\[Nu]))^2+
		(
			(1+SMPrediction$default["RD*\[Mu]e"][[1]]^-1)^-
			2 Abs[\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]]^2+
			(1+SMPrediction$default["RD*\[Mu]e"][[1]])^-
			2 Abs[\[Sigma]\[Delta]BrB0Dstpe\[Nu]]^2
		)/(
			(1+SMPrediction$default["RD*\[Mu]e"][[1]]^-1)^-1 (1+\[Delta]BrB0Dstp\[Mu]\[Nu])+
			(1+SMPrediction$default["RD*\[Mu]e"][[1]])^-1 (1+\[Delta]BrB0Dstpe\[Nu])
		)^2-
		2(
			(
				(1+SMPrediction$default["RD*\[Mu]e"][[1]]^-1)^-1 \[Sigma]\[Delta]BrB0Dstp\[Tau]\[Nu]\[Delta]BrB0Dstp\[Mu]\[Nu]+
				(1+SMPrediction$default["RD*\[Mu]e"][[1]])^-1 \[Sigma]\[Delta]BrB0Dstp\[Tau]\[Nu]\[Delta]BrB0Dstpe\[Nu]
			)/(
				(1+\[Delta]BrB0Dstp\[Tau]\[Nu])
				(
					(1+SMPrediction$default["RD*\[Mu]e"][[1]]^-1)^-1 (1+\[Delta]BrB0Dstp\[Mu]\[Nu])+
					(1+SMPrediction$default["RD*\[Mu]e"][[1]])^-1 (1+\[Delta]BrB0Dstpe\[Nu])
				)
			)
		)+
		2 (
			(1+SMPrediction$default["RD*\[Mu]e"][[1]]^-1)^-1 
			(1+SMPrediction$default["RD*\[Mu]e"][[1]])^-1 \[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]\[Delta]BrB0Dstpe\[Nu]
		)/(
			(1+SMPrediction$default["RD*\[Mu]e"][[1]]^-1)^-1 (1+\[Delta]BrB0Dstp\[Mu]\[Nu])+
			(1+SMPrediction$default["RD*\[Mu]e"][[1]])^-1 (1+\[Delta]BrB0Dstpe\[Nu])
		)^2
	]/.GetParameters[];*)


(* ::Subsubsection:: *)
(*GF*)


(* ::Subsubsection:: *)
(*CKM*)


(* ::Subsection:: *)
(*RD\[Mu]e*)


(* ::Subsubsection:: *)
(*Exp*)


(*ExpValue$default["RD\[Mu]e"] = Around[1.005,0.045];*)


(* ::Subsubsection:: *)
(*SM*)


(*SMPrediction$default["RD\[Mu]e"] = Around[0.99595,0.00011];*)


(* ::Subsubsection:: *)
(*NP*)


(*NPContribution$default["RD\[Mu]e"] := ((1+\[Delta]BrB0Dp\[Mu]\[Nu])/(1+\[Delta]BrB0Dpe\[Nu])-1)/.GetParameters[];*)


(*NPContributionError["RD\[Mu]e"] := 
	Abs[(1+\[Delta]BrB0Dp\[Mu]\[Nu])/(1+\[Delta]BrB0Dpe\[Nu])]
	Sqrt[
		(Abs[\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]]/(1+\[Delta]BrB0Dp\[Mu]\[Nu]))^2+
		(Abs[\[Sigma]\[Delta]BrB0Dpe\[Nu]]/(1+\[Delta]BrB0Dpe\[Nu]))^2-
		(2\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]\[Delta]BrB0Dpe\[Nu])/((1+\[Delta]BrB0Dp\[Mu]\[Nu])(1+ \[Delta]BrB0Dpe\[Nu]))
	]/.GetParameters[];*)


(* ::Subsubsection:: *)
(*GF*)


(* ::Subsubsection:: *)
(*CKM*)


(* ::Subsection:: *)
(*RD*\[Mu]e*)


(* ::Subsubsection:: *)
(*Exp*)


(*ExpValue$default["RD*\[Mu]e"] = Around[0.962,0.047];*)


(* ::Subsubsection:: *)
(*SM*)


(*SMPrediction$default["RD*\[Mu]e"] = Around[0.9953,0.0003];*)


(* ::Subsubsection:: *)
(*NP*)


(*NPContribution$default["RD*\[Mu]e"] := ((1+\[Delta]BrB0Dstp\[Mu]\[Nu])/(1+\[Delta]BrB0Dstpe\[Nu])-1)/.GetParameters[];*)


(*NPContributionError["RD*\[Mu]e"] := 
	Abs[(1+\[Delta]BrB0Dstp\[Mu]\[Nu])/(1+\[Delta]BrB0Dstpe\[Nu])]
	Sqrt[
		(Abs[\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]]/(1+\[Delta]BrB0Dstp\[Mu]\[Nu]))^2+
		(Abs[\[Sigma]\[Delta]BrB0Dstpe\[Nu]]/(1+\[Delta]BrB0Dstpe\[Nu]))^2-
		(2\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]\[Delta]BrB0Dstpe\[Nu])/((1+\[Delta]BrB0Dstp\[Mu]\[Nu])(1+ \[Delta]BrB0Dstpe\[Nu]))
	]/.GetParameters[];*)


(* ::Subsubsection:: *)
(*GF*)


(* ::Subsubsection:: *)
(*CKM*)


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
(*s -> u*)


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
