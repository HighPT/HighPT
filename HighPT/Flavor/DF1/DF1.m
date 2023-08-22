(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`DF1`*)


(* ::Subtitle:: *)
(*\[CapitalDelta]F=1 observables*)


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


$\[CapitalDelta]F1Sectors={"b->sll","b->s\[Nu]\[Nu]","b->s\[Gamma]"(*,"leptonic"*)};


FlavorObservables["\[CapitalDelta]F=1"] = FlavorObservables/@$\[CapitalDelta]F1Sectors


CL90to95=3.09/2.3;


C10SM=-4.18869;
CL\[Nu]SM=2*Around[-6.32,0.07];


(* ::Section:: *)
(*Basis change (WET to LEFT)*)


DownQuarkMasses={Mass["d"],Mass["s"],Mass["b"]};


WETToLEFT={
wCL["7",{i_,j_}]:>WCL["d\[Gamma]",{i,j}]*Sqrt[4\[Pi] Param["\[Alpha]EM"]]/DownQuarkMasses[[Max[i,j]]] (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["7p",{i_,j_}]:>WCL["d\[Gamma]",{j,i}]\[Conjugate]*Sqrt[4\[Pi] Param["\[Alpha]EM"]]/DownQuarkMasses[[Max[i,j]]] (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]\[Conjugate]Vckm[3,j] Param["GF"]),
wCL["8",{i_,j_}]:>WCL["dG",{i,j}]*1/DownQuarkMasses[[Max[i,j]]] 1/Param["g3"] (4 \[Pi]^2 Sqrt[2])/(Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["9",{\[Alpha]_,\[Beta]_,i_,j_}]:>1/2 (WCL["edVLL",{\[Alpha],\[Beta],i,j}]+WCL["deVLR",{i,j,\[Alpha],\[Beta]}]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["9p",{\[Alpha]_,\[Beta]_,i_,j_}]:>1/2 (WCL["edVLR",{\[Alpha],\[Beta],i,j}]+WCL["edVRR",{\[Alpha],\[Beta],i,j}]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["10",{\[Alpha]_,\[Beta]_,i_,j_}]:>1/2 (-WCL["edVLL",{\[Alpha],\[Beta],i,j}]+WCL["deVLR",{i,j,\[Alpha],\[Beta]}]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["10p",{\[Alpha]_,\[Beta]_,i_,j_}]:>1/2 (-WCL["edVLR",{\[Alpha],\[Beta],i,j}]+WCL["edVRR",{\[Alpha],\[Beta],i,j}]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["S",{\[Alpha]_,\[Beta]_,i_,j_}]:>1/2 (WCL["edSRR",{\[Alpha],\[Beta],i,j}]+WCL["edSRL",{\[Beta],\[Alpha],j,i}]\[Conjugate]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["Sp",{\[Alpha]_,\[Beta]_,i_,j_}]:>1/2 (WCL["edSRR",{\[Beta],\[Alpha],j,i}]\[Conjugate]+WCL["edSRL",{\[Alpha],\[Beta],i,j}]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["P",{\[Alpha]_,\[Beta]_,i_,j_}]:>1/2 (WCL["edSRR",{\[Alpha],\[Beta],i,j}]-WCL["edSRL",{\[Beta],\[Alpha],j,i}]\[Conjugate]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["Pp",{\[Alpha]_,\[Beta]_,i_,j_}]:>1/2 (-WCL["edSRR",{\[Beta],\[Alpha],j,i}]\[Conjugate]+WCL["edSRL",{\[Alpha],\[Beta],i,j}]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["L\[Nu]",{\[Alpha]_,\[Beta]_,i_,j_}]:>WCL["\[Nu]dVLL",{\[Alpha],\[Beta],i,j}] (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["R\[Nu]",{\[Alpha]_,\[Beta]_,i_,j_}]:>WCL["\[Nu]dVLR",{\[Alpha],\[Beta],i,j}] (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"])
};


(* ::Section:: *)
(*b -> sll (')*)


FlavorObservables["b->sll"] = {"B+->K+\[Tau]\[Tau]","B0->K0*\[Tau]\[Tau]","Bs->\[Tau]\[Tau]"};


LowScale[Alternatives@@(FlavorObservables["b->sll"]//Flatten)] := Mass["b"]/.GetParameters[];


(* ::Text:: *)
(*Numerical inputs from 2301.06990*)


(* 
   1 -> VV
   2 -> VA
   3 -> AV
   4 -> AA
*)


(* B -> Kll *)
aK[1] = Around[0.2430,0.0001];
aK[2] = Around[-0.260,0.001];
aK[3] = Around[0,0];
aK[4] = Around[0,0];
bK[1] = Around[0.0316,0.0002];
bK[2] = Around[0.0317,0.0002];
bK[3] = Around[0,0];
bK[4] = Around[0,0];


(* B -> K*ll *)
aKst[1] = Around[0.0012,0.0048];
aKst[2] = Around[-0.038,0.008];
aKst[3] = Around[-0.191,0.010];
aKst[4] = Around[0.255,0.006];
bKst[1] = Around[0.0048,0.0010];
bKst[2] = Around[0.0047,0.0010];
bKst[3] = Around[0.0312,0.0007];
bKst[4] = Around[0.0311,0.0007];


(* VV *)
cll[1][i_] := wCL["9p",{i,i,2,3}] + wCL["9",{i,i,2,3}]
(* AV *)
cll[2][i_] := wCL["9p",{i,i,2,3}] - wCL["9",{i,i,2,3}]
(* VA *)
cll[3][i_] := wCL["10p",{i,i,2,3}] + wCL["10",{i,i,2,3}]
(* AA *)
cll[4][i_] := wCL["10p",{i,i,2,3}] - wCL["10",{i,i,2,3}] 


(* ::Subsection:: *)
(*B -> K\[Mu]\[Mu]*)


(*ExpValue$default["B0->KS\[Mu]\[Mu]"] := Around[];*)


NumericalInput["B0->KS\[Mu]\[Mu]"] := Around[0.507,0.024]*10^-4;
InputDependence["B0->KS\[Mu]\[Mu]"] := Abs[Vckm[3,3]Vckm[3,2]\[Conjugate]]^2;


NPContribution$default["B0->KS\[Mu]\[Mu]"] := (Sum[aK[i]*Re[cll[i][2]],{i,4}] + Sum[bK[i]*Abs[cll[i][2]]^2,{i,4}])/.WETToLEFT/.GetParameters[]//Chop;


(* ::Subsection:: *)
(*B -> K*\[Mu]\[Mu]*)


(*ExpValue$default["B0->K0*\[Mu]\[Mu]"] := Around[];*)


NumericalInput["B0->K0*\[Mu]\[Mu]"] := Around[1.46,0.21]*10^-4;
InputDependence["B0->K0*\[Mu]\[Mu]"] := Abs[Vckm[3,3]Vckm[3,2]\[Conjugate]]^2;


NPContribution$default["B0->K0*\[Mu]\[Mu]"] := (Sum[aKst[i]*Re[cll[i][2]],{i,4}] + Sum[bKst[i]*Abs[cll[i][2]]^2,{i,4}])/.WETToLEFT/.GetParameters[]//Chop;


(* ::Subsection:: *)
(*B -> K\[Tau]\[Tau]*)


ExpValue$default["B+->K+\[Tau]\[Tau]"] := Around[0,2.25]*10^-3*CL90to95/2;


(*BK\[Tau]\[Tau]Aux = Around[0.0001003548981731836`,2.140854596647234`*^-6];
SMPrediction$default["B+->K+\[Tau]\[Tau]"] := (Abs[Vckm[3,3]Vckm[3,2]\[Conjugate]]^2*BK\[Tau]\[Tau]Aux)/.GetParameters[Errors->True];*)


NumericalInput["B+->K+\[Tau]\[Tau]"] := Around[0.0001003548981731836`,2.140854596647234`*^-6]
InputDependence["B+->K+\[Tau]\[Tau]"] := Abs[Vckm[3,3]Vckm[3,2]\[Conjugate]]^2


vecWC["B+->K+\[Tau]\[Tau]"]={
Abs[wCL["7",ind\[Gamma]]+wCL["7p",ind\[Gamma]]]^2,
Re[(wCL["7",ind\[Gamma]]+wCL["7p",ind\[Gamma]]) (wCL["9",ind]+wCL["9p",ind])],
Re[wCL["7",ind\[Gamma]]+wCL["7p",ind\[Gamma]]],
Abs[wCL["9",ind]+wCL["9p",ind]]^2,
Re[wCL["9",ind]+wCL["9p",ind]],
Abs[wCL["10",ind]+wCL["10p",ind]]^2,
Re[(wCL["10",ind]+wCL["10p",ind])(wCL["P",ind]+wCL["Pp",ind])],
Re[wCL["10",ind]+wCL["10p",ind]],
Abs[wCL["S",ind]+wCL["Sp",ind]]^2,
Abs[wCL["P",ind]+wCL["Pp",ind]]^2,
Re[wCL["P",ind]+wCL["Pp",ind]],
1}/.ind->{3,3,2,3}/.ind\[Gamma]->{2,3};


Mlow["B+->K+\[Tau]\[Tau]"]={0.042677,0.058631,0.22883,0.020142,0.15723,0.040812,0.090806,-0.33491,0.017016,0.051602,-0.37258,1.0000};
\[Sigma]low["B+->K+\[Tau]\[Tau]"]={0.0013050,0.0010772,0.0038506,0.00024070,0.0019078,0.00023528,0.00067924,0.0019307,0.00014741,0.00039434,0.0027869,0.};


NPContribution$default["B+->K+\[Tau]\[Tau]"]:=(Mlow["B+->K+\[Tau]\[Tau]"] . vecWC["B+->K+\[Tau]\[Tau]"]-1)/.WETToLEFT/.GetParameters[]//Chop;


(* ::Subsection:: *)
(*B -> K*\[Tau]\[Tau]*)


ExpValue$default["B0->K0*\[Tau]\[Tau]"] := Around[0,3.1]*10^-3*CL90to95/2


BKst\[Tau]\[Tau]Aux = Around[0.00008685752993621607`,8.059815071523753`*^-6];
SMPrediction$default["B0->K0*\[Tau]\[Tau]"] := (Abs[Vckm[3,3]Vckm[3,2]\[Conjugate]]^2*BKst\[Tau]\[Tau]Aux)/.GetParameters[Errors->True];


vecWC["B0->K0*\[Tau]\[Tau]"]={
Abs[wCL["7",ind\[Gamma]]-wCL["7p",ind\[Gamma]]]^2,Re[(wCL["7",ind\[Gamma]]-wCL["7p",ind\[Gamma]]) (wCL["9",ind]-wCL["9p",ind])],Re[wCL["7",ind\[Gamma]]-wCL["7p",ind\[Gamma]]],
Abs[wCL["7",ind\[Gamma]]+wCL["7p",ind\[Gamma]]]^2,Re[(wCL["7",ind\[Gamma]]+wCL["7p",ind\[Gamma]]) (wCL["9",ind]+wCL["9p",ind])],Re[wCL["7",ind\[Gamma]]+wCL["7p",ind\[Gamma]]],
Abs[wCL["9",ind]-wCL["9p",ind]]^2,Re[(wCL["9",ind]-wCL["9p",ind])],Abs[wCL["9",ind]+wCL["9p",ind]]^2,Re[(wCL["9",ind]+wCL["9p",ind])],
Abs[wCL["10",ind]-wCL["10p",ind]]^2,Re[(wCL["10",ind]-wCL["10p",ind])(wCL["P",ind]-wCL["Pp",ind])],Re[(wCL["10",ind]-wCL["10p",ind])],
Abs[wCL["10",ind]+wCL["10p",ind]]^2,Re[(wCL["10",ind]+wCL["10p",ind])],Abs[wCL["S",ind]-wCL["Sp",ind]]^2,Abs[wCL["P",ind]-wCL["Pp",ind]]^2,Re[(wCL["P",ind]-wCL["Pp",ind])],
1
}/.ind->{3,3,2,3}/.ind\[Gamma]->{2,3}


Mlow["B0->K0*\[Tau]\[Tau]"]={0.2239991353378814`,0.20166956894989807`,0.7301450070314232`,0.06253060500528816`,0.04445169134368732`,0.15152411248859088`,0.04641285151351181`,0.33762161934640134`,0.007992531113793727`,0.05460943550943313`,0.015467923729101812`,0.01708305933879282`,-0.12692978212100947`,0.0010028453100097308`,-0.008229348613939853`,0.0015180859450262697`,0.008618324891073956`,-0.07009179246706694`,1.`};
\[Sigma]low["B0->K0*\[Tau]\[Tau]"]={0.024765195431317402`,0.01264699404033608`,0.0376454389137068`,0.007816515020742971`,0.00469125626579789`,0.015445662339033873`,0.0009688822371818457`,0.006406885023162325`,0.0008060555501686212`,0.005635237531279601`,0.0005169645043084875`,0.0010798294628409462`,0.004242210722355448`,0.00010874266623710754`,0.0008923423191417047`,0.00010254491888324721`,0.0005504826464761921`,0.004430540286036402`,0.`};


NPContribution$default["B0->K0*\[Tau]\[Tau]"]:=(Mlow["B0->K0*\[Tau]\[Tau]"] . vecWC["B0->K0*\[Tau]\[Tau]"]-1)/.WETToLEFT/.GetParameters[]//Chop;


(* ::Subsection:: *)
(*B -> K\[Mu]\[Tau]*)


(* ::Subsection:: *)
(*B -> K*\[Mu]-\[Tau]+*)


(* ::Subsection:: *)
(*B -> K*\[Mu]+\[Tau]-*)


(* ::Section:: *)
(*Bs->ll*)


(* ::Subsection:: *)
(*Bs->\[Mu]\[Mu]*)


(* ::Subsection:: *)
(*Bs->\[Tau]\[Tau]*)


ExpValue$default["Bs->\[Tau]\[Tau]"] := Around[0,6.8]*10^-3/2;


Bs\[Tau]\[Tau]Aux = Mass["\[Tau]"]^2/Mass["\[Mu]"]^2 Sqrt[1-4 Mass["\[Tau]"]^2/Mass["Bs"]^2]*Around[2.1516,0.0442]*10^-6;
SMPrediction$default["Bs->\[Tau]\[Tau]"] := (Abs[Vckm[3,3]Vckm[3,2]\[Conjugate]]^2*Bs\[Tau]\[Tau]Aux)/.GetParameters[Errors->True];


NPContribution$default["Bs->\[Tau]\[Tau]"] := ((Abs[C10SM+wCL["10",ind]-wCL["10p",ind]+(wCL["P",ind]-wCL["Pp",ind]) Mass["Bs"]^2/(2 Mass["\[Tau]"] (Mass["b"]+0 Mass["s"]))]^2+(1-4 Mass["\[Tau]"]^2/Mass["Bs"]^2)Abs[(wCL["S",ind]-wCL["Sp",ind]) Mass["Bs"]^2/(2 Mass["\[Tau]"] (Mass["b"]+0 Mass["s"]))]^2)/Abs[C10SM]^2-1)/.ind->{3,3,2,3}/.WETToLEFT/.GetParameters[]//Chop;


(* ::Section:: *)
(*Bd->ll*)


(* ::Subsection:: *)
(*Subscript[B, d]->\[Mu]\[Mu]*)


(* ::Section:: *)
(*b -> s\[Nu]\[Nu]*)


FlavorObservables["b->s\[Nu]\[Nu]"] = {"B+->K+\[Nu]\[Nu]","B0->K0*\[Nu]\[Nu]"};


LowScale[Alternatives@@(FlavorObservables["b->s\[Nu]\[Nu]"]//Flatten)] := Mass["b"]/.GetParameters[];


(* ::Subsection:: *)
(*B -> K\[Nu]\[Nu]*)


ExpValue$default["B+->K+\[Nu]\[Nu]"] := Around[0,2.6 10^-5]*CL90to95/2;


BK\[Nu]\[Nu]Aux = Around[2.87,0.10]*10^-3;
SMPrediction$default["B+->K+\[Nu]\[Nu]"] := (Abs[Vckm[3,3]Vckm[3,2]\[Conjugate]]^2*BK\[Nu]\[Nu]Aux)/.GetParameters[Errors->True];


NPContribution$default["B+->K+\[Nu]\[Nu]"] := (Sum[Boole[i<=j]Abs[CL\[Nu]SM["Value"] KroneckerDelta[i,j]+wCL["L\[Nu]",{i,j,2,3}]+wCL["R\[Nu]",{i,j,2,3}]]^2,{i,1,3},{j,1,3}]/(3Abs[CL\[Nu]SM["Value"]]^2)-1)/.WETToLEFT/.GetParameters[]//Chop;


(* ::Subsection:: *)
(*B -> K*\[Nu]\[Nu]*)


ExpValue$default["B0->K0*\[Nu]\[Nu]"] := Around[0,1.8 10^-5]*CL90to95/2;


BKst\[Nu]\[Nu]Aux = Around[5.9,0.8]*10^-3;
SMPrediction$default["B0->K0*\[Nu]\[Nu]"] := (Abs[Vckm[3,3]Vckm[3,2]\[Conjugate]]^2*BKst\[Nu]\[Nu]Aux)/.GetParameters[Errors->True];


\[Eta]Kst = Around[3.34,0.04];
NPContribution$default["B0->K0*\[Nu]\[Nu]"] := (1/(3Abs[CL\[Nu]SM["Value"]]^2) Sum[Boole[i<=j]Abs[CL\[Nu]SM["Value"] KroneckerDelta[i,j]+wCL["L\[Nu]",{i,j,2,3}]+wCL["R\[Nu]",{i,j,2,3}]]^2,{i,1,3},{j,1,3}]-\[Eta]Kst["Value"] 1/(3Abs[CL\[Nu]SM["Value"]]^2) Sum[Boole[i<=j]Re[(CL\[Nu]SM["Value"] KroneckerDelta[i,j]+wCL["L\[Nu]",{i,j,2,3}])Conjugate[wCL["R\[Nu]",{i,j,2,3}]]],{i,1,3},{j,1,3}]-1)/.WETToLEFT/.GetParameters[]//Chop;


(* ::Section:: *)
(*b -> s\[Gamma]*)


FlavorObservables["b->s\[Gamma]"] = {"B->Xs\[Gamma]"};


LowScale["B->Xs\[Gamma]"] = 160;


ExpValue$default["B->Xs\[Gamma]"] := Around[3.32,0.15]*10^-4;


SMPrediction$default["B->Xs\[Gamma]"] := Around[3.39,0.17]*10^-4;


NPContribution$default["B->Xs\[Gamma]"] := (1/SMPrediction$default["B->Xs\[Gamma]"]["Value"] (2.1*Re[3.93*wCL["7",{2,3}]+wCL["8",{2,3}]])*10^-4)/.WETToLEFT/.GetParameters[]//Chop;
