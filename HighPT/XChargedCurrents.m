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


(* ::Chapter:: *)
(*Private:*)


$ChargedCurrentSectors={"b->c"};


FlavorObservables["ChargedCurrents"] = FlavorObservables/@$ChargedCurrentSectors


ObsTable["ChargedCurrents"] := Grid[{{"ChargedCurrents",Column[ObsTable/@$ChargedCurrentSectors]}},Dividers->All];


(* ::Section:: *)
(*Semileptonic Inputs*)


(* Vectors of Eff. Coeff. *)
(* P\[Rule]P^'\[ScriptL]\[Nu] *)
coeffPPplnu={Abs[1+gV]^2,Abs[gT]^2,Re[(1+gV) Conjugate[gT]],Abs[gS]^2,Re[(1+gV) Conjugate[gS]]};
(* P\[Rule]V\[ScriptL]\[Nu] *)
coeffPVlnu={Abs[1+gVL]^2+Abs[gVR]^2,Re[(1+gVL)Conjugate[gVR]],Abs[gT]^2,Re[(1+gVL)Conjugate[gT]],Re[gVR Conjugate[gT]],Abs[gP]^2,Re[(1+gVL-gVR)Conjugate[gP]]};


(* Semileptonic replacement *)
RepSemilep={
gVL:>(WCL["\[Nu]eduVLL",ind]\[Conjugate]*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate]))),
gVR:>(WCL["\[Nu]eduVLR",ind]\[Conjugate]*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate]))),
gV:>((+WCL["\[Nu]eduVLL",ind]\[Conjugate]+WCL["\[Nu]eduVLR",ind]\[Conjugate])*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate]))),
gA:>((-WCL["\[Nu]eduVLL",ind]\[Conjugate]+WCL["\[Nu]eduVLR",ind]\[Conjugate])*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate]))),
gS:>((+WCL["\[Nu]eduSRR",ind]\[Conjugate]+WCL["\[Nu]eduSRL",ind]\[Conjugate])*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate]))),
gP:>((-WCL["\[Nu]eduSRR",ind]\[Conjugate]+WCL["\[Nu]eduSRL",ind]\[Conjugate])*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate]))),
gT:>(WCL["\[Nu]eduTRR",ind]\[Conjugate]*(-Param["vev"]^2/(2 Vckm[ind4,ind3]\[Conjugate])))};


(* ::Subsection:: *)
(*Auxiliary computations*)


(* ::Subsubsection:: *)
(*B -> D*)


(* Reading data *)
{meanBDl\[Nu],\[Sigma]BDl\[Nu],corrBDl\[Nu]}=Get@FileNameJoin[{Global`$DirectoryHighPT,"Flavor","Inputs","data_BDlnu"}];


(* Magic numbers B\[Rule]Dl\[Nu] and errors: the other is BR + normalized coefficients wrt BR

Cvec={BR/|Subscript[V, cb]|^2; Abs[gT]^2,Re[(1+gV)Conjugate[gT]],Abs[gS]^2,Re[(1+gV)Conjugate[gS]]}
*)
{cvBrB0Dp\[Tau]\[Nu],cvBrB0Dp\[Mu]\[Nu],cvBrB0Dpe\[Nu]}={meanBDl\[Nu][[1;;5]],meanBDl\[Nu][[6;;10]],meanBDl\[Nu][[11;;15]]};
{\[Sigma]BrB0Dp\[Tau]\[Nu],\[Sigma]BrB0Dp\[Mu]\[Nu],\[Sigma]BrB0Dpe\[Nu]}={\[Sigma]BDl\[Nu][[1;;5]],\[Sigma]BDl\[Nu][[6;;10]],\[Sigma]BDl\[Nu][[11;;15]]};


(* Covariance matrix *)
covBDl\[Nu]=Table[\[Sigma]BDl\[Nu][[i]]corrBDl\[Nu][[i,j]]\[Sigma]BDl\[Nu][[j]],{i,1,Length[\[Sigma]BDl\[Nu]]},{j,1,Length[\[Sigma]BDl\[Nu]]}];


(* Covariance sub-matrices (w/o SM)  *)
covBD\[Tau]\[Nu]=covBDl\[Nu][[1;;5,1;;5]];
covBD\[Mu]\[Nu]=covBDl\[Nu][[6;;10,6;;10]];
covBDe\[Nu]=covBDl\[Nu][[11;;15,11;;15]];


(* Individual BRs *)
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
\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]\[Delta]BrB0Dpe\[Nu]=(coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{2,2,3,2}/.ind3->3/.ind4->2) . covBDl\[Nu][[7;;10,12;;15]] . (coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{1,1,3,2}/.ind3->3/.ind4->2);


(* ::Subsubsection:: *)
(*B -> D**)


(* Reading data *)
{meanBDstl\[Nu],\[Sigma]BDstl\[Nu],corrBDstl\[Nu]}=Get@FileNameJoin[{Global`$DirectoryHighPT,"Flavor","Inputs","data_BDstlnu"}];


(* Magic numbers B\[Rule]D^*l\[Nu] and errors: the other is BR + normalized coefficients wrt BR

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
{\[Sigma]BrB0Dstp\[Tau]\[Nu],\[Sigma]BrB0Dstp\[Mu]\[Nu],\[Sigma]BrB0Dstpe\[Nu]}={\[Sigma]BDstl\[Nu][[1;;7]],\[Sigma]BDstl\[Nu][[8;;14]],\[Sigma]BDstl\[Nu][[15;;21]]};


(* Covariance matrix *)
covBDstl\[Nu]=Table[\[Sigma]BDstl\[Nu][[i]]corrBDstl\[Nu][[i,j]]\[Sigma]BDstl\[Nu][[j]],{i,1,Length[\[Sigma]BDstl\[Nu]]},{j,1,Length[\[Sigma]BDstl\[Nu]]}];


(* Covariance sub-matrices (w/o SM)  *)
covBDst\[Tau]\[Nu]=covBDstl\[Nu][[1;;7,1;;7]];
covBDst\[Mu]\[Nu]=covBDstl\[Nu][[8;;14,8;;14]];
covBDste\[Nu]=covBDstl\[Nu][[15;;21,15;;21]];


(* Individual BRs *)
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
\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]\[Delta]BrB0Dstpe\[Nu]=(coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{2,2,3,2}/.ind3->3/.ind4->2) . covBDstl\[Nu][[9;;14,16;;21]] . (coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{1,1,3,2}/.ind3->3/.ind4->2);


(* ::Section:: *)
(*b -> c*)


FlavorObservables["b->c"] = {
	(*"Bc->\[Tau]\[Nu]",*)
	"RD\[Tau]l",
	"RD*\[Tau]l",
	"RD\[Mu]e",
	"RD*\[Mu]e"
};


ObsTable["b->c"] := Grid[{{"b->c",Column[FlavorObservables["b->c"]]}},Dividers->All];


(* \[Mu]b = 5 GeV *)
LowScale[Alternatives@@(FlavorObservables["b->c"]//Flatten)] := Mass["b"]/.GetParameters[];


(* ::Subsection:: *)
(*Bc -> \[Tau]\[Nu]*)


(* ::Subsubsection:: *)
(*Exp*)


ExpValue["Bc->\[Tau]\[Nu]"] = Around[0,0.3];


(* ::Subsubsection:: *)
(*SM*)


SMPrediction["Bc->\[Tau]\[Nu]"] = Around[0.0208,0.0007];


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


ExpValue$default["RD\[Tau]l"] = Around[0.342,0.026];


ExpInfo["RD\[Tau]l"] = "HFLAV average from Winter 2023"


(* ::Subsubsection:: *)
(*SM*)


SMPrediction$default["RD\[Tau]l"] = (*Around[0.2938,0.0040];*)Around[0.298,0.004];


SMInfo["RD\[Tau]l"] = "HFLAV Winter 2023";


(* ::Subsubsection:: *)
(*NP*)


NPContribution$default["RD\[Tau]l"] := 
	(((1+\[Delta]BrB0Dp\[Tau]\[Nu])/(
		(1+SMPrediction$default["RD\[Mu]e"][[1]]^-1)^-1 (1+\[Delta]BrB0Dp\[Mu]\[Nu])+
		(1+SMPrediction$default["RD\[Mu]e"][[1]])^-1 (1+\[Delta]BrB0Dpe\[Nu])
		)-1)/.GetParameters[])/.Around[a_,b_]->a;


NPContributionError["RD\[Tau]l"] := 
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
		]/.GetParameters[];


(* ::Subsubsection:: *)
(*GF*)


(* ::Subsubsection:: *)
(*CKM*)


(* ::Subsection:: *)
(*RD*\[Tau]l*)


(* ::Subsubsection:: *)
(*Exp*)


ExpValue$default["RD*\[Tau]l"] = Around[0.287,0.012]


ExpInfo["RD*\[Tau]l"] = "HFLAV average from Winter 2023"


(* ::Subsubsection:: *)
(*SM*)


SMPrediction$default["RD*\[Tau]l"] = (*Around[0.246,0.009];*)Around[0.254,0.005];


SMInfo["RD*\[Tau]l"] = "HFLAV Winter 2023";


(* ::Subsubsection:: *)
(*NP*)


NPContribution$default["RD*\[Tau]l"] := 
	((1+\[Delta]BrB0Dstp\[Tau]\[Nu])/
	(
		(1+SMPrediction$default["RD*\[Mu]e"][[1]]^-1)^-1 (1+\[Delta]BrB0Dstp\[Mu]\[Nu])+
		(1+SMPrediction$default["RD*\[Mu]e"][[1]])^-1 (1+\[Delta]BrB0Dstpe\[Nu])
	)-1)/.GetParameters[];


NPContributionError["RD*\[Tau]l"] := 
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
	]/.GetParameters[];


(* ::Subsubsection:: *)
(*GF*)


(* ::Subsubsection:: *)
(*CKM*)


(* ::Subsection:: *)
(*RD\[Mu]e*)


(* ::Subsubsection:: *)
(*Exp*)


ExpValue$default["RD\[Mu]e"] = Around[1.005,0.045];


(* ::Subsubsection:: *)
(*SM*)


SMPrediction$default["RD\[Mu]e"] = Around[0.99595,0.00011];


(* ::Subsubsection:: *)
(*NP*)


NPContribution$default["RD\[Mu]e"] := ((1+\[Delta]BrB0Dp\[Mu]\[Nu])/(1+\[Delta]BrB0Dpe\[Nu])-1)/.GetParameters[];


NPContributionError["RD\[Mu]e"] := 
	Abs[(1+\[Delta]BrB0Dp\[Mu]\[Nu])/(1+\[Delta]BrB0Dpe\[Nu])]
	Sqrt[
		(Abs[\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]]/(1+\[Delta]BrB0Dp\[Mu]\[Nu]))^2+
		(Abs[\[Sigma]\[Delta]BrB0Dpe\[Nu]]/(1+\[Delta]BrB0Dpe\[Nu]))^2-
		(2\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]\[Delta]BrB0Dpe\[Nu])/((1+\[Delta]BrB0Dp\[Mu]\[Nu])(1+ \[Delta]BrB0Dpe\[Nu]))
	]/.GetParameters[];


(* ::Subsubsection:: *)
(*GF*)


(* ::Subsubsection:: *)
(*CKM*)


(* ::Subsection:: *)
(*RD*\[Mu]e*)


(* ::Subsubsection:: *)
(*Exp*)


ExpValue$default["RD*\[Mu]e"] = Around[0.962,0.047];


(* ::Subsubsection:: *)
(*SM*)


SMPrediction$default["RD*\[Mu]e"] = Around[0.9953,0.0003];


(* ::Subsubsection:: *)
(*NP*)


NPContribution$default["RD*\[Mu]e"] := ((1+\[Delta]BrB0Dstp\[Mu]\[Nu])/(1+\[Delta]BrB0Dstpe\[Nu])-1)/.GetParameters[];


NPContributionError["RD*\[Mu]e"] := 
	Abs[(1+\[Delta]BrB0Dstp\[Mu]\[Nu])/(1+\[Delta]BrB0Dstpe\[Nu])]
	Sqrt[
		(Abs[\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]]/(1+\[Delta]BrB0Dstp\[Mu]\[Nu]))^2+
		(Abs[\[Sigma]\[Delta]BrB0Dstpe\[Nu]]/(1+\[Delta]BrB0Dstpe\[Nu]))^2-
		(2\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]\[Delta]BrB0Dstpe\[Nu])/((1+\[Delta]BrB0Dstp\[Mu]\[Nu])(1+ \[Delta]BrB0Dstpe\[Nu]))
	]/.GetParameters[];


(* ::Subsubsection:: *)
(*GF*)


(* ::Subsubsection:: *)
(*CKM*)


(* ::Subsection:: *)
(*Correlations*)


ExpCorrelation["RD\[Tau]l","RD*\[Tau]l"] = -0.39;
