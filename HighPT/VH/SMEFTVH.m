(* ::Package:: *)

Package["HighPT`"]


(* ::Title:: *)
(*HighPT`SMEFT`for VH*)


(* ::Subtitle:: *)
(*Matching of the form-factors for VH production in the SMEFT*)


(* ::Section:: *)
(*Scoping*)


(* ::Subsection:: *)
(*Internal*)


PackageScope["SubstituteRulesSMEFTVH"]


(* ::Section:: *)
(*Matching the form factors to the SMEFT*)


(* Matching of the form-factors in the SMEFT to the d <= 6 SMEFT *)
SubstituteRulesSMEFTVH[\[Epsilon]_] := Module[{list},
	list = {
		(* Overline[d]_i d_j -> Zh *)
		
		(* Vector FFs *)
		ff[{Vector, 1}, {"regular", {0, 0}}, Left,  {d[i_], d[j_]}] :> -Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / (Param["sW"] * Param["cW"]) * \[Epsilon] * (WC["Hq1", {i, j}] + WC["Hq3", {i, j}]),
		ff[{Vector, 1}, {"regular", {0, 0}}, Right, {d[i_], d[j_]}] :> -Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / (Param["sW"] * Param["cW"]) * \[Epsilon] * WC["Hd", {i, j}],
		ff[{Vector, 1}, {"ZBoson", 0}, Left, {d[i_], d[j_]}] :> Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]^3/(Param["sW"]^3*Param["cW"]^3)* \[Epsilon] * (gZSM[d, Left] WC["HD"] KroneckerDelta[i, j] - (1/2) (WC["Hq1", {i, j}] + WC["Hq3", {i, j}])),
		ff[{Vector, 1}, {"ZBoson", 0}, Right, {d[i_], d[j_]}] :> Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]^3/(Param["sW"]^3*Param["cW"]^3)* \[Epsilon] * (gZSM[d, Right] WC["HD"] KroneckerDelta[i, j] - (1/2) WC["Hd", {i, j}]),
				
		(* u_i u_j* -> Zh *)
		(* Vector FFs *)
		ff[{Vector, 1}, {"regular", {0, 0}}, Left,  {u[i_], u[j_]}] :> -Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / (Param["sW"] * Param["cW"]) * \[Epsilon] * (WC["Hq1", {i, j}] - WC["Hq3", {i, j}]),
		ff[{Vector, 1}, {"regular", {0, 0}}, Right, {u[i_], u[j_]}] :> -Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / (Param["sW"] * Param["cW"]) * \[Epsilon] * WC["Hu", {i, j}],
		ff[{Vector, 1}, {"ZBoson", 0}, Left, {u[i_], u[j_]}] :> Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]^3/(Param["sW"]^3*Param["cW"]^3)* \[Epsilon] * (gZSM[u, Left] WC["HD"] KroneckerDelta[i, j] - (1/2) (WC["Hq1", {i, j}] - WC["Hq3", {i, j}])),
		ff[{Vector, 1}, {"ZBoson", 0}, Right, {u[i_], u[j_]}] :> Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]^3/(Param["sW"]^3*Param["cW"]^3)* \[Epsilon] * (gZSM[u, Right] WC["HD"] KroneckerDelta[i, j] - (1/2) WC["Hu", {i, j}])
	};
	
	Return[list]
];


(* ::Subsubsection:: *)
(*Z couplings*)


gZSM[\[Psi]_, X_] := WeakIsospin3[\[Psi], X] - Param["sW"]^2 Charge[\[Psi]];
