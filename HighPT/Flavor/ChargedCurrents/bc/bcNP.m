(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`bcNP`*)


(* ::Subtitle:: *)
(*New Physics contributions to b->c observables*)


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


(* ::Section:: *)
(*Leptonic*)


(* ::Subsection:: *)
(*B(Subscript[B, c]->\[Tau]\[Nu])*)


NPContribution["B(\!\(\*SubscriptBox[\(B\), \(c\)]\)\[Rule]\[Tau]\[Nu])"] = Abs[1-(WCL["VRudl\[Nu]",ind]-WCL["VLudl\[Nu]",ind])+(WCL["SRudl\[Nu]",ind]-WCL["SLudl\[Nu]",ind]) Mass["\!\(\*SubscriptBox[\(B\), \(c\)]\)"]^2/(Mass["\[Tau]"] (Mass["c"]+Mass["b"]))]^2-1/.ind->{2,3,3,3};


NPInfo["B(\!\(\*SubscriptBox[\(B\), \(c\)]\)\[Rule]\[Tau]\[Nu])"] = "";


(* ::Section:: *)
(*Semileptonic Inputs*)


(* Vectors of Eff. Coeff. *)
(* P\[Rule]P^'\[ScriptL]\[Nu] *)
coeffPPplnu={Abs[1+gV]^2,Abs[gT]^2,Re[(1+gV) Conjugate[gT]],Abs[gS]^2,Re[(1+gV) Conjugate[gS]]};
(* P\[Rule]V\[ScriptL]\[Nu] *)
coeffPVlnu={Abs[1+gVL]^2+Abs[gVR]^2,Re[(1+gVL)Conjugate[gVR]],Abs[gT]^2,Re[(1+gVL)Conjugate[gT]],Re[gVR Conjugate[gT]],Abs[gP]^2,Re[(1+gVL-gVR)Conjugate[gP]]};


(* Semileptonic replacement *)
RepSemilep={
gVL->WCL["VLudl\[Nu]",ind],
gVR->WCL["VRudl\[Nu]",ind],
gV->+WCL["VLudl\[Nu]",ind]+WCL["VRudl\[Nu]",ind],
gA->-WCL["VLudl\[Nu]",ind]+WCL["VRudl\[Nu]",ind],
gS->+WCL["SLudl\[Nu]",ind]+WCL["SRudl\[Nu]",ind],
gP->-WCL["SLudl\[Nu]",ind]+WCL["SRudl\[Nu]",ind],
gT->WCL["Tudl\[Nu]",ind]};


(* ::Section:: *)
(*Semileptonic*)


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
\[Delta]BrB0Dp\[Tau]\[Nu]=Abs[1+gV]^2-1+coeffPPplnu[[2;;5]] . cvBrB0Dp\[Tau]\[Nu][[2;;5]]/.RepSemilep/.ind->{2,3,3,3};
\[Delta]BrB0Dp\[Mu]\[Nu]=Abs[1+gV]^2-1+coeffPPplnu[[2;;5]] . cvBrB0Dp\[Mu]\[Nu][[2;;5]]/.RepSemilep/.ind->{2,3,2,2};
\[Delta]BrB0Dpe\[Nu]=Abs[1+gV]^2-1+coeffPPplnu[[2;;5]] . cvBrB0Dpe\[Nu][[2;;5]]/.RepSemilep/.ind->{2,3,1,1};
(* Error: *)
\[Sigma]\[Delta]BrB0Dp\[Tau]\[Nu]=Sqrt[coeffPPplnu[[2;;5]] . covBD\[Tau]\[Nu][[2;;5,2;;5]] . coeffPPplnu[[2;;5]]]/.RepSemilep/.ind->{2,3,3,3}//Expand;
\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]=Sqrt[coeffPPplnu[[2;;5]] . covBD\[Mu]\[Nu][[2;;5,2;;5]] . coeffPPplnu[[2;;5]]]/.RepSemilep/.ind->{2,3,2,2}//Expand;
\[Sigma]\[Delta]BrB0Dpe\[Nu]=Sqrt[coeffPPplnu[[2;;5]] . covBDe\[Nu][[2;;5,2;;5]] . coeffPPplnu[[2;;5]]]/.RepSemilep/.ind->{2,3,1,1}//Expand;
(**)
\[Sigma]\[Delta]BrB0Dp\[Tau]\[Nu]\[Delta]BrB0Dp\[Mu]\[Nu]=(coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{2,3,3,3}) . covBDl\[Nu][[2;;5,7;;10]] . (coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{2,3,2,2});
\[Sigma]\[Delta]BrB0Dp\[Tau]\[Nu]\[Delta]BrB0Dpe\[Nu]=(coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{2,3,3,3}) . covBDl\[Nu][[2;;5,12;;15]] . (coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{2,3,1,1});
\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]\[Delta]BrB0Dpe\[Nu]=(coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{2,3,2,2}) . covBDl\[Nu][[7;;10,12;;15]] . (coeffPPplnu[[2;;5]]/.RepSemilep/.ind->{2,3,1,1});


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
\[Delta]BrB0Dstp\[Tau]\[Nu]=Abs[1+gVL]^2+Abs[gVR]^2-1+coeffPVlnu[[2;;7]] . cvBrB0Dstp\[Tau]\[Nu][[2;;7]]/.RepSemilep/.ind->{2,3,3,3};
\[Delta]BrB0Dstp\[Mu]\[Nu]=Abs[1+gVL]^2+Abs[gVR]^2-1+coeffPVlnu[[2;;7]] . cvBrB0Dstp\[Mu]\[Nu][[2;;7]]/.RepSemilep/.ind->{2,3,2,2};
\[Delta]BrB0Dstpe\[Nu]=Abs[1+gVL]^2+Abs[gVR]^2-1+coeffPVlnu[[2;;7]] . cvBrB0Dstpe\[Nu][[2;;7]]/.RepSemilep/.ind->{2,3,1,1};
(* Error: *)
\[Sigma]\[Delta]BrB0Dstp\[Tau]\[Nu]=Sqrt[coeffPVlnu[[2;;7]] . covBDst\[Tau]\[Nu][[2;;7,2;;7]] . coeffPVlnu[[2;;7]]]/.RepSemilep/.ind->{2,3,3,3}//Expand;
\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]=Sqrt[coeffPVlnu[[2;;7]] . covBDst\[Mu]\[Nu][[2;;7,2;;7]] . coeffPVlnu[[2;;7]]]/.RepSemilep/.ind->{2,3,2,2}//Expand;
\[Sigma]\[Delta]BrB0Dstpe\[Nu]=Sqrt[coeffPVlnu[[2;;7]] . covBDste\[Nu][[2;;7,2;;7]] . coeffPVlnu[[2;;7]]]/.RepSemilep/.ind->{2,3,1,1}//Expand;
(**)
\[Sigma]\[Delta]BrB0Dstp\[Tau]\[Nu]\[Delta]BrB0Dstp\[Mu]\[Nu]=(coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{2,3,3,3}) . covBDstl\[Nu][[2;;7,9;;14]] . (coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{2,3,2,2});
\[Sigma]\[Delta]BrB0Dstp\[Tau]\[Nu]\[Delta]BrB0Dstpe\[Nu]=(coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{2,3,3,3}) . covBDstl\[Nu][[2;;7,16;;21]] . (coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{2,3,1,1});
\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]\[Delta]BrB0Dstpe\[Nu]=(coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{2,3,2,2}) . covBDstl\[Nu][[9;;14,16;;21]] . (coeffPVlnu[[2;;7]]/.RepSemilep/.ind->{2,3,1,1});


(* ::Subsection:: *)
(*Subscript[R, D]^(\[Tau]/l)*)


NPContribution["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Tau]/l)\)]\)"] := 
	(1+\[Delta]BrB0Dp\[Tau]\[Nu])/(
		(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]]^-1)^-1 (1+\[Delta]BrB0Dp\[Mu]\[Nu])+
		(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]])^-1 (1+\[Delta]BrB0Dpe\[Nu])
		)-1;


NPContributionError["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Tau]/l)\)]\)"] := 
	Abs[NPContribution["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Tau]/l)\)]\)"]+1]
	Sqrt[
		(Abs[\[Sigma]\[Delta]BrB0Dp\[Tau]\[Nu]]/(1+\[Delta]BrB0Dp\[Tau]\[Nu]))^2+
		(
			(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]]^-1)^-2 Abs[\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]]^2+
			(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]])^-2 Abs[\[Sigma]\[Delta]BrB0Dpe\[Nu]]^2
		)/
		(
			(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]]^-1)^-1 (1+\[Delta]BrB0Dp\[Mu]\[Nu])+
			(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]])^-1 (1+\[Delta]BrB0Dpe\[Nu])
		)^2-
		2(
			(
				(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]]^-1)^-1 \[Sigma]\[Delta]BrB0Dp\[Tau]\[Nu]\[Delta]BrB0Dp\[Mu]\[Nu]+
				(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]])^-1 \[Sigma]\[Delta]BrB0Dp\[Tau]\[Nu]\[Delta]BrB0Dpe\[Nu]
			)/
			(
				(1+\[Delta]BrB0Dp\[Tau]\[Nu])
					(
						(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]]^-1)^-1 (1+\[Delta]BrB0Dp\[Mu]\[Nu])+
						(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]])^-1 (1+\[Delta]BrB0Dpe\[Nu])
					)
				)
			)+
			2(
				(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]]^-1)^-1 (1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]])^-1 \[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]\[Delta]BrB0Dpe\[Nu]
			)/
			(
				(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]]^-1)^-1 (1+\[Delta]BrB0Dp\[Mu]\[Nu])+
				(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"][[1]])^-1 (1+\[Delta]BrB0Dpe\[Nu])
			)^2
		];


NPInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Tau]/l)\)]\)"] = "";


(* ::Subsection:: *)
(*Subscript[R, SuperStar[D]]^(\[Tau]/l)*)


NPContribution["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Tau]/l)\)]\)"] := 
	(1+\[Delta]BrB0Dstp\[Tau]\[Nu])/
	(
		(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]]^-1)^-1 (1+\[Delta]BrB0Dstp\[Mu]\[Nu])+
		(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]])^-1 (1+\[Delta]BrB0Dstpe\[Nu])
	)-1;


NPContributionError["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Tau]/l)\)]\)"] := 
	Abs[NPContribution["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Tau]/l)\)]\)"]+1]
	Sqrt[
		(Abs[\[Sigma]\[Delta]BrB0Dstp\[Tau]\[Nu]]/(1+\[Delta]BrB0Dstp\[Tau]\[Nu]))^2+
		(
			(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]]^-1)^-
			2 Abs[\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]]^2+
			(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]])^-
			2 Abs[\[Sigma]\[Delta]BrB0Dstpe\[Nu]]^2
		)/(
			(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]]^-1)^-1 (1+\[Delta]BrB0Dstp\[Mu]\[Nu])+
			(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]])^-1 (1+\[Delta]BrB0Dstpe\[Nu])
		)^2-
		2(
			(
				(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]]^-1)^-1 \[Sigma]\[Delta]BrB0Dstp\[Tau]\[Nu]\[Delta]BrB0Dstp\[Mu]\[Nu]+
				(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]])^-1 \[Sigma]\[Delta]BrB0Dstp\[Tau]\[Nu]\[Delta]BrB0Dstpe\[Nu]
			)/(
				(1+\[Delta]BrB0Dstp\[Tau]\[Nu])
				(
					(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]]^-1)^-1 (1+\[Delta]BrB0Dstp\[Mu]\[Nu])+
					(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]])^-1 (1+\[Delta]BrB0Dstpe\[Nu])
				)
			)
		)+
		2 (
			(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]]^-1)^-1 
			(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]])^-1 \[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]\[Delta]BrB0Dstpe\[Nu]
		)/(
			(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]]^-1)^-1 (1+\[Delta]BrB0Dstp\[Mu]\[Nu])+
			(1+SMPrediction["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"][[1]])^-1 (1+\[Delta]BrB0Dstpe\[Nu])
		)^2
	];


NPInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Tau]/l)\)]\)"] = "";


(* ::Subsection:: *)
(*Subscript[R, D]^(\[Mu]/e)*)


NPContribution["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"] := (1+\[Delta]BrB0Dp\[Mu]\[Nu])/(1+\[Delta]BrB0Dpe\[Nu])-1;


NPContributionError["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"] := 
	Abs[(1+\[Delta]BrB0Dp\[Mu]\[Nu])/(1+\[Delta]BrB0Dpe\[Nu])]
	Sqrt[
		(Abs[\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]]/(1+\[Delta]BrB0Dp\[Mu]\[Nu]))^2+
		(Abs[\[Sigma]\[Delta]BrB0Dpe\[Nu]]/(1+\[Delta]BrB0Dpe\[Nu]))^2-
		(2\[Sigma]\[Delta]BrB0Dp\[Mu]\[Nu]\[Delta]BrB0Dpe\[Nu])/((1+\[Delta]BrB0Dp\[Mu]\[Nu])(1+ \[Delta]BrB0Dpe\[Nu]))
	];


NPInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(D\)], \((\[Mu]/e)\)]\)"] = "";


(* ::Subsection:: *)
(*Subscript[R, SuperStar[D]]^(\[Mu]/e)*)


NPContribution["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"] := (1+\[Delta]BrB0Dstp\[Mu]\[Nu])/(1+\[Delta]BrB0Dstpe\[Nu])-1;


NPContributionError["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"] = 
	Abs[(1+\[Delta]BrB0Dstp\[Mu]\[Nu])/(1+\[Delta]BrB0Dstpe\[Nu])]
	Sqrt[
		(Abs[\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]]/(1+\[Delta]BrB0Dstp\[Mu]\[Nu]))^2+
		(Abs[\[Sigma]\[Delta]BrB0Dstpe\[Nu]]/(1+\[Delta]BrB0Dstpe\[Nu]))^2-
		(2\[Sigma]\[Delta]BrB0Dstp\[Mu]\[Nu]\[Delta]BrB0Dstpe\[Nu])/((1+\[Delta]BrB0Dstp\[Mu]\[Nu])(1+ \[Delta]BrB0Dstpe\[Nu]))
	];


NPInfo["\!\(\*SuperscriptBox[SubscriptBox[\(R\), SuperscriptBox[\(D\), \(*\)]], \((\[Mu]/e)\)]\)"] = "";
