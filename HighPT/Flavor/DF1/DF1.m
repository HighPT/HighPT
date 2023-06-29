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


$\[CapitalDelta]F1Sectors={"b->sll"(*,"b->s\[Nu]\[Nu]","leptonic"*)};


FlavorObservables["\[CapitalDelta]F=1"] = FlavorObservables/@$\[CapitalDelta]F1Sectors


(* ::Section:: *)
(*Basis change (WET to LEFT)*)


DownQuarkMasses={Mass["d"],Mass["s"],Mass["d"]}


WETToLEFT={
wCL["7",{i_,j_}]:>WCL["d\[Gamma]",{i,j}]*Sqrt[4\[Pi] Param["\[Alpha]EM"]]/DownQuarkMasses[[Max[i,j]]] (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["7p",{i_,j_}]:>WCL["d\[Gamma]",{j,i}]\[Conjugate]*Sqrt[4\[Pi] Param["\[Alpha]EM"]]/DownQuarkMasses[[Max[i,j]]] (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]\[Conjugate]Vckm[3,j] Param["GF"]),
wCL["9",{\[Alpha]_,\[Beta]_,i_,j_}]:>(WCL["edVLL",{\[Alpha],\[Beta],i,j}]+WCL["deVLR",{i,j,\[Alpha],\[Beta]}]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["9p",{\[Alpha]_,\[Beta]_,i_,j_}]:>(WCL["edVLR",{\[Alpha],\[Beta],i,j}]+WCL["edVRR",{\[Alpha],\[Beta],i,j}]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["10",{\[Alpha]_,\[Beta]_,i_,j_}]:>(-WCL["edVLL",{\[Alpha],\[Beta],i,j}]+WCL["deVLR",{i,j,\[Alpha],\[Beta]}]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["10p",{\[Alpha]_,\[Beta]_,i_,j_}]:>(-WCL["edVLR",{\[Alpha],\[Beta],i,j}]+WCL["edVRR",{\[Alpha],\[Beta],i,j}]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["S",{\[Alpha]_,\[Beta]_,i_,j_}]:>(WCL["edSRR",{\[Alpha],\[Beta],i,j}]+WCL["edSRL",{\[Beta],\[Alpha],j,i}]\[Conjugate]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["Sp",{\[Alpha]_,\[Beta]_,i_,j_}]:>(WCL["edSRR",{\[Beta],\[Alpha],j,i}]\[Conjugate]+WCL["edSRL",{\[Alpha],\[Beta],i,j}]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]\[Conjugate]Vckm[3,j] Param["GF"]),
wCL["P",{\[Alpha]_,\[Beta]_,i_,j_}]:>(WCL["edSRR",{\[Alpha],\[Beta],i,j}]-WCL["edSRL",{\[Beta],\[Alpha],j,i}]\[Conjugate]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]Vckm[3,j]\[Conjugate] Param["GF"]),
wCL["Pp",{\[Alpha]_,\[Beta]_,i_,j_}]:>(-WCL["edSRR",{\[Beta],\[Alpha],j,i}]\[Conjugate]+WCL["edSRL",{\[Alpha],\[Beta],i,j}]) (\[Pi] Sqrt[2])/(Param["\[Alpha]EM"]Vckm[3,i]\[Conjugate]Vckm[3,j] Param["GF"])
};


(* ::Section:: *)
(*b -> sll (')*)


FlavorObservables["b->sll"] = {"B+->K+\[Tau]\[Tau]"};


(* ::Subsection:: *)
(*B -> K\[Mu]\[Mu]*)


(* ::Subsection:: *)
(*B -> K\[Tau]\[Tau]*)


ExpValue$default["B+->K+\[Tau]\[Tau]"] := Around[0,2.25]*10^-3


(*{0.0001003548981731836`,2.140854596647234`*^-6};
SMPrediction["B\[Rule]K\[Tau]\[Tau]"]=(Vckm[3,3]Vckm[3,2])^2 %[[1]]{1,Sqrt[(*(((2VtbVts0[[2]])/VtbVts0[[1]])^2)+*)(%[[2]]/%[[1]])^2]}*)


BK\[Tau]\[Tau]Aux = Around[0.0001003548981731836`,2.140854596647234`*^-6];
SMPrediction$default["B+->K+\[Tau]\[Tau]"] := Abs[Vckm[3,3]Vckm[3,2]\[Conjugate]]^2*BK\[Tau]\[Tau]Aux;


vecWC["B+\[Rule]K+\[Tau]\[Tau]"]={
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
1}/.ind->{3,3,2,3}/.ind\[Gamma]->{2,3}
Mlow["B+\[Rule]K+\[Tau]\[Tau]"]=\!\(\*
TagBox[
RowBox[{"{", 
RowBox[{
InterpretationBox[
StyleBox["\<\"0.042677\"\>",
ShowStringCharacters->False],
0.04267688555318238200022953113658025411`19.69897000433602,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.058631\"\>",
ShowStringCharacters->False],
0.0586311021787147449191976257961865891`19.698970004336015,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.22883\"\>",
ShowStringCharacters->False],
0.22883079554864191036503568560228046043`19.698970004336015,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.020142\"\>",
ShowStringCharacters->False],
0.0201418412809770127640275316966008545`19.698970004336015,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.15723\"\>",
ShowStringCharacters->False],
0.15722934284976367938460593685279855053`19.698970004336026,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.040812\"\>",
ShowStringCharacters->False],
0.040812452996434663628983845616370201`19.69897000433602,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.090806\"\>",
ShowStringCharacters->False],
0.09080615906349442191734191356818099337`19.69897000433602,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"-0.33491\"\>",
ShowStringCharacters->False],
-0.33490698928874291638349692389424880605`19.698970004336015,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.017016\"\>",
ShowStringCharacters->False],
0.01701622354914658239091813647513802777`19.698970004336026,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.051602\"\>",
ShowStringCharacters->False],
0.05160167032127285354209008776872844926`19.698970004336026,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"-0.37258\"\>",
ShowStringCharacters->False],
-0.37257767063751762473583773731638132598`19.69897000433602,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"1.0000\"\>",
ShowStringCharacters->False],
1.`19.698970004336026,
AutoDelete->True]}], "}"}],
NumberForm[#, 5]& ]\);
\[Sigma]low["B+\[Rule]K+\[Tau]\[Tau]"]=\!\(\*
TagBox[
RowBox[{"{", 
RowBox[{
InterpretationBox[
StyleBox["\<\"0.0013050\"\>",
ShowStringCharacters->False],
0.00130502616649163324789831729799563938`17.969851027011703,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.0010772\"\>",
ShowStringCharacters->False],
0.00107722684697032563960045940737180353`17.744202913270925,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.0038506\"\>",
ShowStringCharacters->False],
0.00385057811831618233062158891562486667`17.70386629588272,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.00024070\"\>",
ShowStringCharacters->False],
0.00024069744852011826171671986496776749`17.56198224987683,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.0019078\"\>",
ShowStringCharacters->False],
0.00190776826731233834248967676837093559`17.574151486565775,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.00023528\"\>",
ShowStringCharacters->False],
0.00023528461389858911605903657725114646`17.257249535963265,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.00067924\"\>",
ShowStringCharacters->False],
0.00067924226299111871758096418399925685`17.36780761381762,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.0019307\"\>",
ShowStringCharacters->False],
0.00193074554165182099017430021642261133`17.25724953596326,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.00014741\"\>",
ShowStringCharacters->False],
0.00014741437328366211539715147786699679`17.434079222244538,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.00039434\"\>",
ShowStringCharacters->False],
0.00039433581669566136145172668942725011`17.378483741758966,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.0027869\"\>",
ShowStringCharacters->False],
0.0027869310050525601850700656723029184`17.36780761381762,
AutoDelete->True], ",", 
InterpretationBox[
StyleBox["\<\"0.\"\>",
ShowStringCharacters->False],
0``0.0,
AutoDelete->True]}], "}"}],
NumberForm[#, 5]& ]\);


NPContribution$default["B+\[Rule]K+\[Tau]\[Tau]"]=(Mlow["B+\[Rule]K+\[Tau]\[Tau]"] . vecWC["B+\[Rule]K+\[Tau]\[Tau]"]-1)/.WETToLEFT//Chop


(* ::Subsection:: *)
(*B -> K*\[Tau]\[Tau]*)


(* ::Subsection:: *)
(*B -> K\[Mu]\[Tau]*)


(* ::Subsection:: *)
(*B -> K*\[Mu]-\[Tau]+*)


(* ::Subsection:: *)
(*B -> K*\[Mu]+\[Tau]-*)


(* ::Section:: *)
(*Subscript[B, s]->ll*)


(* ::Subsection:: *)
(*Subscript[B, s]->\[Mu]\[Mu]*)


(* ::Subsection:: *)
(*Subscript[B, s]->\[Tau]\[Tau]*)


(* ::Section:: *)
(*Subscript[B, d]->ll*)


(* ::Subsection:: *)
(*Subscript[B, d]->\[Mu]\[Mu]*)


(* ::Section:: *)
(*b -> s\[Nu]\[Nu]*)


(* ::Subsection:: *)
(*B -> K\[Nu]\[Nu]*)


(* ::Subsection:: *)
(*B -> K*\[Nu]\[Nu]*)
