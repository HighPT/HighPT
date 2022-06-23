(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`AnomalousDimension`*)


(* ::Subtitle:: *)
(*1-loop anomalous dimension for the SMEFT in the Warsaw basis*)


(* ::Chapter:: *)
(*Public:*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Exported*)


PackageExport["SMEFTAnomalousDimension"]


(* ::Subsection:: *)
(*Internal*)


(* ::Chapter:: *)
(*Private:*)


(* ::Section:: *)
(*Definitions*)


(* ::Subsection:: *)
(*Hypercharges*)


Hypercharge["q"]=1/6;
Hypercharge["u"]=2/3;
Hypercharge["d"]=-(1/3);
Hypercharge["l"]=-(1/2);
Hypercharge["e"]=-1;
Hypercharge["h"]=1/2;


(* ::Subsection:: *)
(*Yukawa matrices*)


(* down - alignment *)


YukawaMatrix["u"]=
DiagonalMatrix[{YukawaCoupling["u"],YukawaCoupling["c"],YukawaCoupling["t"]}] . CKM


YukawaMatrix["d"]=DiagonalMatrix[{YukawaCoupling["d"],YukawaCoupling["s"],YukawaCoupling["b"]}]


\[Gamma]H = Nc*Tr[ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"] + ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"]];
\[Gamma]q[i_,j_]:=1/2 (ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"] + ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"])[[i,j]]
\[Gamma]u[i_,j_]:=(ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"])[[i,j]]
\[Gamma]d[i_,j_]:=(ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"])[[i,j]]


(* ::Subsection:: *)
(*WC combinations*)


\[Xi]e[\[Alpha]_,\[Beta]_]:=Nc(Sum[YukawaMatrix["u"][[i,j]]WC["lequ1",{\[Alpha],\[Beta],j,i}],{i,3},{j,3}]-Sum[ConjugateTranspose[YukawaMatrix["d"]][[i,j]]WC["ledq",{\[Alpha],\[Beta],j,i}],{i,3},{j,3}])


\[Xi]B=8/3 (2Hypercharge["l"]Sum[WC["Hl1",{\[Rho],\[Rho]}],{\[Rho],3}]+2Hypercharge["q"]Nc Sum[WC["Hq1",{k,k}],{k,3}]+Hypercharge["e"]Sum[WC["He",{\[Rho],\[Rho]}],{\[Rho],3}]+Hypercharge["u"]Nc Sum[WC["Hu",{k,k}],{k,3}]+Hypercharge["d"]Nc Sum[WC["Hd",{k,k}],{k,3}]);


(* ::Section:: *)
(*4-quark operators*)


SMEFTAnomalousDimension[WC["qq1",{i_,j_,i_,j_}]] :=  (ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"]-ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"])[[i,j]]WC["Hq1",{i,j}]


SMEFTAnomalousDimension[WC["qq3",{i_,j_,i_,j_}]] :=  -(ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"]-ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"])[[i,j]]WC["Hq3",{i,j}]


SMEFTAnomalousDimension[WC["qd1",{i_,j_,i_,j_}]] :=  (ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"]-ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"])[[i,j]]WC["Hd",{i,j}]+
	2(YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[i,j]]WC["Hq1",{i,j}]


SMEFTAnomalousDimension[WC["qu1",{i_,j_,i_,j_}]] :=(ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"]-ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"])[[i,j]]WC["Hu",{i,j}]-
	2(YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]])[[i,j]]WC["Hq1",{i,j}]


SMEFTAnomalousDimension[WC["qd8",{i_,j_,i_,j_}]]:=0


SMEFTAnomalousDimension[WC["qu8",{i_,j_,i_,j_}]]:=0


SMEFTAnomalousDimension[WC["dd",{i_,j_,i_,j_}]]:=2(YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[i,j]]WC["Hd",{i,j}]


SMEFTAnomalousDimension[WC["uu",{i_,j_,i_,j_}]]:=-2(YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]])[[i,j]]WC["Hu",{i,j}]


(* ::Section:: *)
(*Semileptonic operators*)


SMEFTAnomalousDimension[WC["lq1",{\[Alpha]_,\[Beta]_,i_,j_}]]:=
	4/3 GaugeCoupling["g1"]^2 Hypercharge["h"]Hypercharge["l"]WC["Hq1",{i,j}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["h"]Hypercharge["q"]WC["Hl1",{\[Alpha],\[Beta]}]KroneckerDelta[i,j]+
	8/3 GaugeCoupling["g1"]^2 Nc Hypercharge["q"]^2 Sum[WC["lq1",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	8/3 GaugeCoupling["g1"]^2 Hypercharge["l"]^2 Sum[WC["lq1",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["e"]Hypercharge["l"]Sum[WC["eq",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["q"]Hypercharge["u"]Sum[WC["lu",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["q"]Sum[WC["ld",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	12Hypercharge["l"]Hypercharge["q"]GaugeCoupling["g1"]^2 WC["lq1",{\[Alpha],\[Beta],i,j}]+
	9GaugeCoupling["g2"]^2 WC["lq3",{\[Alpha],\[Beta],i,j}]+
	(ConjugateTranspose[YukawaMatrix["u"]] . Yu-ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"])[[i,j]]WC["Hl1",{\[Alpha],\[Beta]}]-
	Sum[ConjugateTranspose[YukawaMatrix["u"]][[i,k]]Yu[[l,j]]WC["lu",{\[Alpha],\[Beta],k,l}],{k,3},{l,3}]+
	Sum[\[Gamma]q[i,k]WC["lq1",{\[Alpha],\[Beta],k,j}],{k,3}]+
	Sum[WC["lq1",{\[Alpha],\[Beta],i,k}]\[Gamma]q[k,j],{k,3}]


SMEFTAnomalousDimension[WC["lq3",{\[Alpha]_,\[Beta]_,i_,j_}]]:=
	1/3 GaugeCoupling["g2"]^2 WC["Hq3",{i,j}]KroneckerDelta[\[Alpha],\[Beta]]+
	1/3 GaugeCoupling["g2"]^2 WC["Hl3",{\[Alpha],\[Beta]}]KroneckerDelta[i,j]+
	2/3 GaugeCoupling["g2"]^2 Nc Sum[WC["lq3",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	2/3 GaugeCoupling["g2"]^2 Nc Sum[WC["lq3",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]+
	3GaugeCoupling["g2"]^2 WC["lq1",{\[Alpha],\[Beta],i,j}]-
	6(GaugeCoupling["g2"]^2-2Hypercharge["l"]Hypercharge["q"]GaugeCoupling["g1"]^2)WC["lq3",{\[Alpha],\[Beta],i,j}]-
	(ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"] + ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"])[[i,j]]WC["Hl3",{\[Alpha],\[Beta]}]+
	Sum[\[Gamma]q[i,k]WC["lq3",{\[Alpha],\[Beta],k,j}],{k,3}]+
	Sum[WC["lq3",{\[Alpha],\[Beta],i,k}]\[Gamma]q[k,j],{k,3}]


SMEFTAnomalousDimension[WC["eq",{\[Alpha]_,\[Beta]_,i_,j_}]]:=
	4/3 GaugeCoupling["g1"]^2 Hypercharge["h"]Hypercharge["q"]WC["He",{\[Alpha],\[Beta]}]KroneckerDelta[i,j]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["e"]Hypercharge["h"]WC["Hq1",{i,j}]KroneckerDelta[\[Alpha],\[Beta]]+
	8/3 GaugeCoupling["g1"]^2 Hypercharge["e"]Hypercharge["l"]Sum[WC["lq1",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["e"]^2 Sum[WC["eq",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]+
	8/3 GaugeCoupling["g1"]^2 Nc Hypercharge["q"]^2 Sum[WC["eq",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["q"]Hypercharge["u"]Sum[WC["eu",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["q"]Sum[WC["ed",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]-
	12Hypercharge["q"]Hypercharge["e"]GaugeCoupling["g1"]^2 WC["eq",{\[Alpha],\[Beta],i,j}]+
	(ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"]-ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"])[[i,j]]WC["He",{\[Alpha],\[Beta]}]-
	Sum[YukawaMatrix["d"][[l,j]]ConjugateTranspose[YukawaMatrix["d"]][[i,k]]WC["ed",{\[Alpha],\[Beta],k,l}],{k,3},{l,3}]-
	Sum[YukawaMatrix["u"][[l,j]]ConjugateTranspose[YukawaMatrix["u"]][[i,k]]WC["eu",{\[Alpha],\[Beta],k,l}],{k,3},{l,3}]+
	Sum[\[Gamma]q[i,k]WC["eq",{\[Alpha],\[Beta],k,j}],{k,3}]+
	Sum[WC["eq",{\[Alpha],\[Beta],i,k}]\[Gamma]q[k,j],{k,3}]


SMEFTAnomalousDimension[WC["lu",{\[Alpha]_,\[Beta]_,i_,j_}]]:=
	4/3 GaugeCoupling["g1"]^2 Hypercharge["h"]Hypercharge["l"]WC["Hu",{i,j}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["h"]Hypercharge["u"]WC["Hl1",{\[Alpha],\[Beta]}]KroneckerDelta[i,j]+
	8/3 GaugeCoupling["g1"]^2 Nc Hypercharge["q"]Hypercharge["u"]Sum[WC["lq1",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["u"]^2 Sum[WC["lu",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	8/3 GaugeCoupling["g1"]^2 Hypercharge["l"]^2 Sum[WC["lu",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["u"]Sum[WC["ld",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["e"]Hypercharge["l"]Sum[WC["eu",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]-
	12Hypercharge["l"]Hypercharge["u"]GaugeCoupling["g1"]^2 WC["lu",{\[Alpha],\[Beta],i,j}]-
	2(YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]])[[i,j]]WC["Hl1",{\[Alpha],\[Beta]}]-
	2Sum[YukawaMatrix["u"][[i,k]]ConjugateTranspose[YukawaMatrix["u"]][[l,j]]WC["lq1",{\[Alpha],\[Beta],k,l}],{k,3},{l,3}]+
	Sum[\[Gamma]u[i,k]WC["lu",{\[Alpha],\[Beta],k,j}],{k,3}]+
	Sum[WC["lu",{\[Alpha],\[Beta],i,k}]\[Gamma]u[k,j],{k,3}]


SMEFTAnomalousDimension[WC["ld",{\[Alpha]_,\[Beta]_,i_,j_}]]:=
	4/3 GaugeCoupling["g1"]^2 Hypercharge["h"]Hypercharge["l"]WC["Hd",{i,j}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["d"]Hypercharge["h"]WC["Hl1",{\[Alpha],\[Beta]}]KroneckerDelta[i,j]+
	8/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["q"]Sum[WC["lq1",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]^2 Sum[WC["ld",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	8/3 GaugeCoupling["g1"]^2 Hypercharge["l"]^2 Sum[WC["ld",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["u"]Sum[WC["lu",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["e"]Hypercharge["l"]Sum[WC["ed",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]-
	12Hypercharge["l"]Hypercharge["d"]GaugeCoupling["g1"]^2 WC["ld",{\[Alpha],\[Beta],i,j}]-
	2(YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[i,j]]WC["Hl1",{\[Alpha],\[Beta]}]-
	2Sum[YukawaMatrix["d"][[i,k]]ConjugateTranspose[YukawaMatrix["d"]][[l,j]]WC["lq1",{\[Alpha],\[Beta],k,l}],{k,3},{l,3}]+
	Sum[\[Gamma]d[i,k]WC["ld",{\[Alpha],\[Beta],k,j}],{k,3}]+
	Sum[WC["ld",{\[Alpha],\[Beta],i,k}]\[Gamma]d[k,j],{k,3}]


SMEFTAnomalousDimension[WC["eu",{\[Alpha]_,\[Beta]_,i_,j_}]]:=
	4/3 GaugeCoupling["g1"]^2 Hypercharge["e"]Hypercharge["h"]WC["Hu",{i,j}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["h"]Hypercharge["u"]WC["He",{\[Alpha],\[Beta]}]KroneckerDelta[i,j]+
	8/3 GaugeCoupling["g1"]^2 Hypercharge["e"]Hypercharge["l"]Sum[WC["lu",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]+
	8/3 GaugeCoupling["g1"]^2 Nc Hypercharge["q"]Hypercharge["u"]Sum[WC["eq",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["u"]^2 Sum[WC["eu",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["e"]^2 Sum[WC["eu",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["u"]Sum[WC["ed",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	12Hypercharge["e"]Hypercharge["u"]WC["eu",{\[Alpha],\[Beta],i,j}]-
	2(YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]])[[i,j]]WC["He",{\[Alpha],\[Beta]}]-
	2Sum[YukawaMatrix["u"][[i,k]]ConjugateTranspose[YukawaMatrix["u"]][[l,j]]WC["eq",{\[Alpha],\[Beta],k,l}],{k,3},{l,3}]+
	Sum[\[Gamma]u[i,k]WC["eu",{\[Alpha],\[Beta],k,j}],{k,3}]+
	Sum[WC["eu",{\[Alpha],\[Beta],i,k}]\[Gamma]u[k,j],{k,3}]


SMEFTAnomalousDimension[WC["ed",{\[Alpha]_,\[Beta]_,i_,j_}]]:=
	4/3 GaugeCoupling["g1"]^2 Hypercharge["e"]Hypercharge["h"]WC["Hd",{i,j}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["d"]Hypercharge["h"]WC["He",{\[Alpha],\[Beta]}]KroneckerDelta[i,j]+
	8/3 GaugeCoupling["g1"]^2 Hypercharge["e"]Hypercharge["l"]Sum[WC["ld",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]+
	8/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["q"]Sum[WC["eq",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["u"]Sum[WC["eu",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["e"]^2 Sum[WC["ed",{\[Rho],\[Rho],i,j}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]^2 Sum[WC["ed",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[i,j]+
	12Hypercharge["e"]Hypercharge["d"]WC["ed",{\[Alpha],\[Beta],i,j}]+
	2(YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[i,j]]WC["He",{\[Alpha],\[Beta]}]-
	2Sum[YukawaMatrix["d"][[i,k]]ConjugateTranspose[YukawaMatrix["d"]][[l,j]]WC["eq",{\[Alpha],\[Beta],k,l}],{k,3},{l,3}]+
	Sum[\[Gamma]d[i,k]WC["ed",{\[Alpha],\[Beta],k,j}],{k,3}]+
	Sum[WC["ed",{\[Alpha],\[Beta],i,k}]\[Gamma]d[k,j],{k,3}]


SMEFTAnomalousDimension[WC["lequ1",{\[Alpha]_,\[Beta]_,i_,j_}]]:=
	-(6(Hypercharge["e"]^2+Hypercharge["e"](Hypercharge["u"]-Hypercharge["q"])+Hypercharge["q"]Hypercharge["u"])GaugeCoupling["g1"]^2+3(Nc-1/Nc)GaugeCoupling["g3"]^2)WC["lequ1",{\[Alpha],\[Beta],i,j}]-
	(24(Hypercharge["q"]+Hypercharge["u"])(2Hypercharge["e"]-Hypercharge["q"]+Hypercharge["u"])GaugeCoupling["g1"]^2-18GaugeCoupling["g2"]^2)WC["lequ1",{\[Alpha],\[Beta],i,j}]+
	2ConjugateTranspose[YukawaMatrix["u"]][[i,j]]\[Xi]e[\[Alpha],\[Beta]]+
	2Sum[ConjugateTranspose[YukawaMatrix["d"]][[i,k]]YukawaMatrix["u"][[l,j]]WC["ledq",{\[Alpha],\[Beta],k,l}],{k,3},{l,3}]+
	Sum[\[Gamma]q[i,k]WC["lequ1",{\[Alpha],\[Beta],k,j}],{k,3}]+
	Sum[WC["lequ1",{\[Alpha],\[Beta],i,k}]\[Gamma]u[k,j],{k,3}]


SMEFTAnomalousDimension[WC["lequ3",{\[Alpha]_,\[Beta]_,i_,j_}]]:=
	((2(Hypercharge["e"]^2-Hypercharge["e"]Hypercharge["q"]+Hypercharge["e"]Hypercharge["u"]-2Hypercharge["q"]^2+5Hypercharge["q"]Hypercharge["u"]-2Hypercharge["u"]^2)GaugeCoupling["g1"]^2-3GaugeCoupling["g2"]^2)+(Nc-1/Nc)GaugeCoupling["g3"]^2)WC["lequ3",{\[Alpha],\[Beta],i,j}]+
	1/8 (-4(Hypercharge["q"]+Hypercharge["u"])(2Hypercharge["e"]-Hypercharge["q"]+Hypercharge["u"])GaugeCoupling["g1"]^2+3GaugeCoupling["g2"]^2)WC["lequ1",{\[Alpha],\[Beta],i,j}]+
	Sum[\[Gamma]q[i,k]WC["lequ3",{\[Alpha],\[Beta],k,j}],{k,3}]+
	Sum[WC["lequ3",{\[Alpha],\[Beta],i,k}]\[Gamma]u[k,j],{k,3}]


SMEFTAnomalousDimension[WC["ledq",{\[Alpha]_,\[Beta]_,i_,j_}]]:=
	-(6(Hypercharge["d"](Hypercharge["q"]-Hypercharge["e"])+Hypercharge["e"](Hypercharge["e"]+Hypercharge["q"]))GaugeCoupling["g1"]^2+3(Nc-1/Nc)GaugeCoupling["g3"]^2)WC["ledq",{\[Alpha],\[Beta],i,j}]-
	2YukawaMatrix["d"][[i,j]]\[Xi]e[\[Alpha],\[Beta]]+2Sum[YukawaMatrix["d"][[i,k]]YukawaMatrix["u"][[l,j]]WC["lequ1",{\[Alpha],\[Beta],k,l}],{k,3},{l,3}]+
	Sum[\[Gamma]d[i,k]WC["ledq",{\[Alpha],\[Beta],k,j}],{k,3}]+
	Sum[WC["ledq",{\[Alpha],\[Beta],i,k}]\[Gamma]q[k,j],{k,3}]


(* ::Section:: *)
(*4-lepton operators*)


SMEFTAnomalousDimension[WC["ll",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}]]:=
	2/3 GaugeCoupling["g1"]^2 Hypercharge["h"]Hypercharge["l"]WC["Hl1",{\[Gamma],\[Delta]}]KroneckerDelta[\[Alpha],\[Beta]]-
	1/6 GaugeCoupling["g2"]^2 WC["Hl3",{\[Gamma],\[Delta]}]KroneckerDelta[\[Alpha],\[Beta]]+
	1/3 GaugeCoupling["g2"]^2 WC["Hl3",{\[Alpha],\[Delta]}]KroneckerDelta[\[Gamma],\[Beta]]+
	1/3 GaugeCoupling["g2"]^2 WC["Hl3",{\[Gamma],\[Beta]}]KroneckerDelta[\[Alpha],\[Delta]]+
	2/3 GaugeCoupling["g1"]^2 Hypercharge["h"]Hypercharge["l"]WC["Hl1",{\[Alpha],\[Beta]}]KroneckerDelta[\[Gamma],\[Delta]]-
	1/6 GaugeCoupling["g2"]^2 WC["Hl3",{\[Alpha],\[Beta]}]KroneckerDelta[\[Gamma],\[Delta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["l"]Hypercharge["q"]Sum[WC["lq1",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Gamma],\[Delta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["l"]Hypercharge["q"]Sum[WC["lq1",{\[Gamma],\[Delta],k,k}],{k,3}]KroneckerDelta[\[Alpha],\[Beta]]-
	1/3 GaugeCoupling["g2"]^2 Nc Sum[WC["lq3",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Gamma],\[Delta]]-
	1/3 GaugeCoupling["g2"]^2 Nc Sum[WC["lq3",{\[Gamma],\[Delta],k,k}],{k,3}]KroneckerDelta[\[Alpha],\[Beta]]+
	2/3 GaugeCoupling["g2"]^2 Nc Sum[WC["lq3",{\[Gamma],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Alpha],\[Delta]]+
	2/3 GaugeCoupling["g2"]^2 Nc Sum[WC["lq3",{\[Alpha],\[Delta],k,k}],{k,3}]KroneckerDelta[\[Gamma],\[Beta]]+
	2/3 GaugeCoupling["g1"]^2 Nc Hypercharge["l"]Hypercharge["u"]Sum[WC["lu",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Gamma],\[Delta]]+
	2/3 GaugeCoupling["g1"]^2 Nc Hypercharge["l"]Hypercharge["u"]Sum[WC["lu",{\[Gamma],\[Delta],k,k}],{k,3}]KroneckerDelta[\[Alpha],\[Beta]]+
	2/3 GaugeCoupling["g1"]^2 Nc Hypercharge["l"]Hypercharge["d"]Sum[WC["ld",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Gamma],\[Delta]]+
	2/3 GaugeCoupling["g1"]^2 Nc Hypercharge["l"]Hypercharge["d"]Sum[WC["ld",{\[Gamma],\[Delta],k,k}],{k,3}]KroneckerDelta[\[Alpha],\[Beta]]


SMEFTAnomalousDimension[WC["le",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}]]:=
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["h"]Hypercharge["l"]WC["He",{\[Gamma],\[Delta]}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["e"]Hypercharge["h"]WC["Hl1",{\[Alpha],\[Beta]}]KroneckerDelta[\[Gamma],\[Delta]]+
	8/3 GaugeCoupling["g1"]^2 Nc Hypercharge["e"]Hypercharge["q"]Sum[WC["lq1",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Gamma],\[Delta]]+
	8/3 GaugeCoupling["g1"]^2 Nc Hypercharge["l"]Hypercharge["q"]Sum[WC["eq",{\[Gamma],\[Delta],k,k}],{k,3}]KroneckerDelta[\[Alpha],\[Beta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["e"]Hypercharge["u"]Sum[WC["lu",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Gamma],\[Delta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["e"]Sum[WC["ld",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Gamma],\[Delta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["l"]Hypercharge["u"]Sum[WC["eu",{\[Gamma],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Alpha],\[Delta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["l"] Sum[WC["ed",{\[Gamma],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Alpha],\[Delta]]


SMEFTAnomalousDimension[WC["ee",{\[Alpha]_,\[Beta]_,\[Gamma]_,\[Delta]_}]]:=
	2/3 GaugeCoupling["g1"]^2 Nc Hypercharge["e"]Hypercharge["h"]WC["He",{\[Gamma],\[Delta]}]KroneckerDelta[\[Alpha],\[Beta]]+
	2/3 GaugeCoupling["g1"]^2 Nc Hypercharge["e"]Hypercharge["h"]WC["He",{\[Alpha],\[Beta]}]KroneckerDelta[\[Gamma],\[Delta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["e"]Hypercharge["q"]Sum[WC["eq",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Gamma],\[Delta]]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["e"]Hypercharge["q"]Sum[WC["eq",{\[Gamma],\[Delta],k,k}],{k,3}]KroneckerDelta[\[Alpha],\[Beta]]+
	2/3 GaugeCoupling["g1"]^2 Nc Hypercharge["e"]Hypercharge["u"]Sum[WC["eu",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Gamma],\[Delta]]+
	2/3 GaugeCoupling["g1"]^2 Nc Hypercharge["e"]Hypercharge["u"]Sum[WC["eu",{\[Gamma],\[Delta],k,k}],{k,3}]KroneckerDelta[\[Alpha],\[Beta]]+
	2/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["e"]Sum[WC["ed",{\[Alpha],\[Beta],k,k}],{k,3}]KroneckerDelta[\[Gamma],\[Delta]]+
	2/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["e"]Sum[WC["ed",{\[Gamma],\[Delta],k,k}],{k,3}]KroneckerDelta[\[Alpha],\[Beta]]


(* ::Section:: *)
(*Higgs-current operators*)


SMEFTAnomalousDimension[WC["Hq1",{i_,j_}]]:=
	1/2 \[Xi]B GaugeCoupling["g1"]^2 Hypercharge["q"]KroneckerDelta[i,j]+
	(4/3 GaugeCoupling["g1"]^2 Hypercharge["h"]^2+2\[Gamma]H)WC["Hq1",{i,j}]+
	8/3 GaugeCoupling["g1"]^2 Hypercharge["h"]Hypercharge["l"]Sum[WC["lq1",{\[Rho],\[Rho],i,j}],{\[Rho],3}]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["e"]Hypercharge["h"]Sum[WC["eq",{\[Rho],\[Rho],i,j}],{\[Rho],3}]-
	Sum[ConjugateTranspose[YukawaMatrix["u"]][[i,k]]WC["Hu",{k,l}]YukawaMatrix["u"][[l,j]],{k,3},{l,3}]-
	Sum[ConjugateTranspose[YukawaMatrix["d"]][[i,k]]WC["Hd",{k,l}]YukawaMatrix["d"][[l,j]],{k,3},{l,3}]+
	3/2 Sum[(ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"]+ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"])[[i,k]]WC["Hq1",{k,j}],{k,3}]+
	3/2 Sum[WC["Hq1",{i,k}](ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"]+ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"])[[k,j]],{k,3}]+
	9/2 Sum[(ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"]-ConjugateTranspose[YukawaMatrix["u"]] . Yu)[[i,k]]WC["Hq3",{k,j}],{k,3}]+
	9/2 Sum[WC["Hq3",{i,k}](ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"]-ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"])[[k,j]],{k,3}]+
	Sum[\[Gamma]q[i,k]WC["Hq1",{k,j}],{k,3}]+
	Sum[WC["Hq1",{i,k}]\[Gamma]q[k,j],{k,3}]


SMEFTAnomalousDimension[WC["Hq3",{i_,j_}]]:=
	2/3 GaugeCoupling["g2"]^2 Sum[WC["Hl3",{\[Rho],\[Rho]}],{\[Rho],3}]KroneckerDelta[i,j]+
	2/3 GaugeCoupling["g2"]^2 Nc Sum[WC["Hq3",{k,k}],{k,3}]KroneckerDelta[i,j]+
	(2\[Gamma]H-17/3 GaugeCoupling["g2"]^2)WC["Hq3",{i,j}]+
	2/3 GaugeCoupling["g2"]^2 Sum[WC["lq3",{\[Rho],\[Rho],i,j}],{\[Rho],3}]+
	3/2 Sum[(ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"]-ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"])[[i,k]]WC["Hq1",{k,j}],{k,3}]+
	3/2 Sum[WC["Hq1",{i,k}](ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"]-ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"])[[k,j]],{k,3}]+
	1/2 Sum[(ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"]+ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"])[[i,k]]WC["Hq3",{k,j}],{k,3}]+
	1/2 Sum[WC["Hq3",{i,k}](ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"]+ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"])[[k,j]],{k,3}]+
	Sum[\[Gamma]q[i,k]WC["Hq3",{k,j}],{k,3}]+
	Sum[WC["Hq3",{i,k}]\[Gamma]q[k,j],{k,3}]


SMEFTAnomalousDimension[WC["Hu",{i_,j_}]]:=
	1/2 \[Xi]B GaugeCoupling["g1"]^2 Hypercharge["u"]KroneckerDelta[i,j]+
	(4/3 GaugeCoupling["g1"]^2 Hypercharge["h"]^2+2\[Gamma]H)WC["Hu",{i,j}]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["e"]Hypercharge["h"]Sum[WC["eu",{\[Rho],\[Rho],i,j}],{\[Rho],3}]+
	8/3 GaugeCoupling["g1"]^2 Hypercharge["h"]Hypercharge["l"]Sum[WC["lu",{\[Rho],\[Rho],i,j}],{\[Rho],3}]-
	2Sum[YukawaMatrix["u"][[i,k]]WC["Hq1",{k,l}]ConjugateTranspose[YukawaMatrix["u"]][[l,j]],{k,3},{l,3}]+
	3Sum[(YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]])[[i,k]]WC["Hu",{k,j}],{k,3}]+
	3Sum[WC["Hu",{i,k}](YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]])[[k,j]],{k,3}]+
	Sum[(YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["d"]])[[i,k]]WC["Hud",{k,j}]\[Conjugate],{k,3}]+
	Sum[WC["Hud",{i,k}](YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["u"]])[[k,j]],{k,3}]+
	Sum[\[Gamma]u[i,k]WC["Hu",{k,j}],{k,3}]+
	Sum[WC["Hu",{i,k}]\[Gamma]u[k,j],{k,3}]


SMEFTAnomalousDimension[WC["Hd",{i_,j_}]]:=
	1/2 \[Xi]B GaugeCoupling["g1"]^2 Hypercharge["d"]KroneckerDelta[i,j]+
	(4/3 GaugeCoupling["g1"]^2 Hypercharge["h"]^2+2\[Gamma]H)WC["Hd",{i,j}]+
	4/3 GaugeCoupling["g1"]^2 Hypercharge["e"]Hypercharge["h"]Sum[WC["ed",{\[Rho],\[Rho],i,j}],{\[Rho],3}]+
	8/3 GaugeCoupling["g1"]^2 Hypercharge["h"]Hypercharge["l"]Sum[WC["ld",{\[Rho],\[Rho],i,j}],{\[Rho],3}]-
	2Sum[YukawaMatrix["d"][[i,k]]WC["Hq1",{k,l}]ConjugateTranspose[YukawaMatrix["d"]][[l,j]],{k,3},{l,3}]+
	3Sum[(YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[i,k]]WC["Hd",{k,j}],{k,3}]+
	3Sum[WC["Hd",{i,k}](YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[k,j]],{k,3}]-
	Sum[(YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["u"]])[[i,k]]WC["Hud",{k,j}],{k,3}]-
	Sum[WC["Hud",{i,k}]\[Conjugate](YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["d"]])[[k,j]],{k,3}]+
	Sum[\[Gamma]d[i,k]WC["Hd",{k,j}],{k,3}]+
	Sum[WC["Hd",{i,k}]\[Gamma]d[k,j],{k,3}]


SMEFTAnomalousDimension[WC["Hud",{i_,j_}]]:=
	(2\[Gamma]H-3GaugeCoupling["g1"]^2 (Hypercharge["u"]-Hypercharge["d"])^2)WC["Hud",{i,j}]-
	2Sum[(YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["d"]])[[i,k]]WC["Hd",{k,j}],{k,3}]+
	2Sum[WC["Hu",{i,k}](YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["d"]])[[k,j]],{k,3}]+
	2Sum[(YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]])[[i,k]]WC["Hud",{k,j}],{k,3}]+
	2Sum[WC["Hud",{i,k}](YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[k,j]],{k,3}]+
	Sum[\[Gamma]u[i,k]WC["Hud",{k,j}],{k,3}]+
	Sum[WC["Hud",{i,k}]\[Gamma]d[k,j],{k,3}]


SMEFTAnomalousDimension[WC["Hl1",{\[Alpha]_,\[Beta]_}]]:=
	1/2 \[Xi]B GaugeCoupling["g1"]^2 Hypercharge["l"]KroneckerDelta[\[Alpha],\[Beta]]+
	(4/3 GaugeCoupling["g1"]^2 Hypercharge["h"]^2+2\[Gamma]H)WC["Hl1",{\[Alpha],\[Beta]}]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["h"]Sum[WC["ld",{\[Alpha],\[Beta],k,k}],{k,3}]+
	8/3 GaugeCoupling["g1"]^2 Nc Hypercharge["h"]Hypercharge["q"]Sum[WC["lq1",{\[Alpha],\[Beta],k,k}],{k,3}]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["h"]Hypercharge["u"]Sum[WC["lu",{\[Alpha],\[Beta],k,k}],{k,3}]-
	2Nc Sum[WC["lq1",{\[Alpha],\[Beta],k,l}](ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"]-ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"])[[l,k]],{k,3},{l,3}]-
	2Nc Sum[WC["lu",{\[Alpha],\[Beta],k,l}](YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]])[[l,k]],{k,3},{l,3}]+
	2Nc Sum[WC["ld",{\[Alpha],\[Beta],k,l}](YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[l,k]],{k,3},{l,3}]


SMEFTAnomalousDimension[WC["Hl3",{\[Alpha]_,\[Beta]_}]]:=
	2/3 GaugeCoupling["g2"]^2 Sum[WC["Hl3",{\[Rho],\[Rho]}],{\[Rho],3}]KroneckerDelta[\[Alpha],\[Beta]]+
	2/3 GaugeCoupling["g2"]^2 Nc Sum[WC["Hq3",{k,k}],{k,3}]KroneckerDelta[\[Alpha],\[Beta]]+
	2/3 GaugeCoupling["g2"]^2 Nc Sum[WC["lq3",{\[Alpha],\[Beta],k,k}],{k,3}]+
	(2\[Gamma]H-17/3 GaugeCoupling["g2"]^2)WC["Hl3",{\[Alpha],\[Beta]}]-
	2Nc Sum[WC["lq3",{\[Alpha],\[Beta],k,l}](ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"]+ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"])[[l,k]],{k,3},{l,3}]


SMEFTAnomalousDimension[WC["He",{\[Alpha]_,\[Beta]_}]]:=
	1/2 \[Xi]B GaugeCoupling["g1"]^2 Hypercharge["e"]KroneckerDelta[\[Alpha],\[Beta]]+
	(4/3 GaugeCoupling["g1"]^2 Hypercharge["h"]^2+2\[Gamma]H)WC["He",{\[Alpha],\[Beta]}]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["d"]Hypercharge["h"]Sum[WC["ed",{\[Alpha],\[Beta],k,k}],{k,3}]+
	4/3 GaugeCoupling["g1"]^2 Nc Hypercharge["h"]Hypercharge["u"]Sum[WC["eu",{\[Alpha],\[Beta],k,k}],{k,3}]+
	8/3 GaugeCoupling["g1"]^2 Nc Hypercharge["h"]Hypercharge["q"]Sum[WC["eq",{\[Alpha],\[Beta],k,k}],{k,3}]-
	2Nc Sum[WC["eu",{\[Alpha],\[Beta],k,l}](YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]])[[l,k]],{k,3},{l,3}]+
	2Nc Sum[WC["ed",{\[Alpha],\[Beta],k,l}](YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[l,k]],{k,3},{l,3}]-
	2Nc Sum[WC["eq",{\[Alpha],\[Beta],k,l}](ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"]-ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"])[[l,k]],{k,3},{l,3}]


(* ::Section:: *)
(*Dipole operators*)


SMEFTAnomalousDimension[WC["eB",{\[Alpha]_,\[Beta]_}]]:=
	4GaugeCoupling["g1"] Nc(Hypercharge["u"]+Hypercharge["q"])Sum[WC["lequ3",{\[Alpha],\[Beta],k,l}]YukawaMatrix["u"][[l,k]],{k,3},{l,3}]


SMEFTAnomalousDimension[WC["eW",{\[Alpha]_,\[Beta]_}]]:=
	-2GaugeCoupling["g2"] Nc Sum[WC["lequ3",{\[Alpha],\[Beta],k,l}]YukawaMatrix["u"][[l,k]],{k,3},{l,3}]


(* ::Section:: *)
(*Higgs - Yukawa operators*)


\[Eta]2=Sum[
-2Nc WC["Hq1",{r,s}](ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"]+ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"])[[s,r]]+
Nc WC["Hud",{r,s}](YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["u"]])[[s,r]]+
Nc WC["Hud",{s,r}]\[Conjugate](YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["d"]])[[s,r]]
,{r,3},{s,3}]


SMEFTAnomalousDimension[WC["eH",{i_,j_}]]:=
	4Nc Coupling["\[Lambda]"] Sum[WC["ledq",{i,j,p,t}]ConjugateTranspose[YukawaMatrix["d"]][[t,p]],{t,3},{p,3}]-
	4Nc Coupling["\[Lambda]"] Sum[WC["lequ1",{i,j,p,t}]YukawaMatrix["u"][[t,p]],{t,3},{p,3}]-
	4Nc Sum[(ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[t,p]]*WC["ledq",{i,j,p,t}],{p,3},{t,3}]+
	4Nc Sum[(YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"])[[t,p]]*WC["lequ1",{i,j,p,t}],{p,3},{t,3}]


SMEFTAnomalousDimension[WC["uH",{i_,j_}]]:=
	-4Coupling["\[Lambda]"] Sum[WC["Hq1",{i,t}]ConjugateTranspose[YukawaMatrix["u"]][[t,j]],{t,3}]+
	12Coupling["\[Lambda]"] Sum[WC["Hq3",{i,t}]ConjugateTranspose[YukawaMatrix["u"]][[t,j]],{t,3}]+
	4Coupling["\[Lambda]"] Sum[ConjugateTranspose[YukawaMatrix["u"]][[i,t]]WC["Hu",{t,j}],{t,3}]-
	4Coupling["\[Lambda]"] Sum[ConjugateTranspose[YukawaMatrix["d"]][[i,t]]WC["Hud",{j,t}]\[Conjugate],{t,3}]-
	2\[Eta]2 ConjugateTranspose[YukawaMatrix["u"]][[i,j]]-
	2Sum[WC["Hq1",{i,t}]*(ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]])[[t,j]],{t,3}]+
	6Sum[WC["Hq3",{i,t}]*(ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["u"]])[[t,j]],{t,3}]-
	2Sum[(ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]])[[i,t]]WC["Hu",{t,j}],{t,3}]-
	2Sum[(ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[i,t]]WC["Hud",{t,j}]\[Conjugate],{t,3}]-
	(3GaugeCoupling["g2"]^2-12GaugeCoupling["g1"]^2 Hypercharge["q"]Hypercharge["h"])Sum[ConjugateTranspose[YukawaMatrix["u"]][[i,t]]WC["Hu",{t,j}],{t,3}]+
	3GaugeCoupling["g2"]^2 Sum[ConjugateTranspose[YukawaMatrix["d"]][[i,t]]WC["Hud",{t,j}]\[Conjugate],{t,3}]+
	12GaugeCoupling["g1"]^2 Hypercharge["u"]Hypercharge["h"]Sum[WC["Hq1",{i,t}]ConjugateTranspose[YukawaMatrix["u"]][[t,j]],{t,3}]-
	12GaugeCoupling["g1"]^2 Hypercharge["u"]Hypercharge["h"]Sum[WC["Hq3",{i,t}]ConjugateTranspose[YukawaMatrix["u"]][[t,j]],{t,3}]+
	4/3 GaugeCoupling["g2"]^2 ConjugateTranspose[YukawaMatrix["u"]][[i,j]]Sum[WC["Hl3",{t,t}]+Nc WC["Hq3",{t,t}],{t,3}]


SMEFTAnomalousDimension[WC["dH",{i_,j_}]]:=
	4Coupling["\[Lambda]"] Sum[WC["Hq1",{i,t}]ConjugateTranspose[YukawaMatrix["d"]][[t,j]],{t,3}]+
	12Coupling["\[Lambda]"] Sum[WC["Hq3",{i,t}]ConjugateTranspose[YukawaMatrix["d"]][[t,j]],{t,3}]-
	4Coupling["\[Lambda]"] Sum[ConjugateTranspose[YukawaMatrix["d"]][[i,t]]WC["Hd",{t,j}],{t,3}]-
	4Coupling["\[Lambda]"] Sum[ConjugateTranspose[YukawaMatrix["u"]][[i,t]]WC["Hud",{t,j}],{t,3}]+
	2\[Eta]2 ConjugateTranspose[YukawaMatrix["d"]][[i,j]]+
	2Sum[WC["Hq1",{i,t}]*(ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[t,j]],{t,3}]+
	6Sum[WC["Hq3",{i,t}]*(ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["d"]])[[t,j]],{t,3}]-
	2Sum[(ConjugateTranspose[YukawaMatrix["d"]] . YukawaMatrix["d"] . ConjugateTranspose[YukawaMatrix["d"]])[[i,t]]WC["Hd",{t,j}],{t,3}]-
	2Sum[(ConjugateTranspose[YukawaMatrix["u"]] . YukawaMatrix["u"] . ConjugateTranspose[YukawaMatrix["u"]])[[i,t]]WC["Hud",{t,j}],{t,3}]-
	(3GaugeCoupling["g2"]^2-12GaugeCoupling["g1"]^2 Hypercharge["q"]Hypercharge["h"])Sum[ConjugateTranspose[YukawaMatrix["u"]][[i,t]]WC["Hd",{t,j}],{t,3}]+
	3GaugeCoupling["g2"]^2 Sum[ConjugateTranspose[YukawaMatrix["u"]][[i,t]]WC["Hud",{t,j}],{t,3}]+
	12GaugeCoupling["g1"]^2 Hypercharge["d"]Hypercharge["h"]Sum[WC["Hq1",{i,t}]ConjugateTranspose[YukawaMatrix["d"]][[t,j]],{t,3}]-
	12GaugeCoupling["g1"]^2 Hypercharge["d"]Hypercharge["h"]Sum[WC["Hq3",{i,t}]ConjugateTranspose[YukawaMatrix["d"]][[t,j]],{t,3}]+
	4/3 GaugeCoupling["g2"]^2 ConjugateTranspose[YukawaMatrix["d"]][[i,j]]Sum[WC["Hl3",{t,t}]+Nc WC["Hq3",{t,t}],{t,3}]
