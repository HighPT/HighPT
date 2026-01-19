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
PackageScope["CanonizeFFVH"]


(* ::Section:: *)
(*Matching the form factors to the SMEFT*)


(* Matching of the form-factors in the SMEFT to the d <= 6 SMEFT *)
SubstituteRulesSMEFTVH[\[Epsilon]_] := Module[{list},
	list = {
		(* Overline[d]_i d_j -> Zh *)
		
		(* Vector FFs *)
		ff[{Vector, 1}, {"regular", {0, 0}}, Left,  {d[i_], d[j_]}] :> -Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / (Param["sW"] * Param["cW"]) * \[Epsilon] * (WC["Hq1", {i, j}] + WC["Hq3", {i, j}]),
		ff[{Vector, 1}, {"regular", {0, 0}}, Right, {d[i_], d[j_]}] :> -Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / (Param["sW"] * Param["cW"]) * \[Epsilon] * WC["Hd", {i, j}],
		ff[{Vector, 1}, {"ZBoson", 0}, Left, {d[i_], d[j_]}] :> (1/2) * Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]^3/(Param["sW"]^3*Param["cW"]^3)* \[Epsilon] * (gZSM[d, Left] WC["HD"] KroneckerDelta[i, j] - (1/2) (WC["Hq1", {i, j}] + WC["Hq3", {i, j}])),
		ff[{Vector, 1}, {"ZBoson", 0}, Right, {d[i_], d[j_]}] :> (1/2) * Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]^3/(Param["sW"]^3*Param["cW"]^3)* \[Epsilon] * (gZSM[d, Right] WC["HD"] KroneckerDelta[i, j] - (1/2) WC["Hd", {i, j}]),
		
		(* Overline[u]_i u_j -> Zh *)
		(* Vector FFs *)
		ff[{Vector, 1}, {"regular", {0, 0}}, Left,  {u[i_], u[j_]}] :> -Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / (Param["sW"] * Param["cW"]) * \[Epsilon] * (WC["Hq1", {i, j}] - WC["Hq3", {i, j}]),
		ff[{Vector, 1}, {"regular", {0, 0}}, Right, {u[i_], u[j_]}] :> -Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / (Param["sW"] * Param["cW"]) * \[Epsilon] * WC["Hu", {i, j}],
		ff[{Vector, 1}, {"ZBoson", 0}, Left, {u[i_], u[j_]}] :> (1/2) * Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]^3/(Param["sW"]^3*Param["cW"]^3) * \[Epsilon] * (gZSM[u, Left] WC["HD"] KroneckerDelta[i, j] - (1/2) (WC["Hq1", {i, j}] - WC["Hq3", {i, j}])),
		ff[{Vector, 1}, {"ZBoson", 0}, Right, {u[i_], u[j_]}] :> (1/2) * Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]^3/(Param["sW"]^3*Param["cW"]^3)* \[Epsilon] * (gZSM[u, Right] WC["HD"] KroneckerDelta[i, j] - (1/2) WC["Hu", {i, j}]),
		
		(* Vector, 2 *)		
		ff[{Vector, 2}, {"regular", {0, 0}}, X_, {\[Psi]_[i_], \[Psi]_[j_]}] :>  \[Epsilon] * (4 * Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / (Param["sW"]*Param["cW"]) * gZSM[\[Psi], X] * \[CapitalDelta]\[Kappa]hZZ + 2 * Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] * Charge[\[Psi]] * \[CapitalDelta]\[Kappa]hZ\[Gamma]) * KroneckerDelta[i, j],
		ff[{Vector, 2}, {"ZBoson", 0}, X_, {\[Psi]_[i_], \[Psi]_[j_]}] :>  \[Epsilon] * ( Sqrt[4*\[Pi]*Param["\[Alpha]EM"]]^3 / (Param["sW"]^3*Param["cW"]^3)) * gZSM[\[Psi], X] * \[CapitalDelta]\[Kappa]hZZ * KroneckerDelta[i, j],
		
		(* Scalar, 2 *)
		ff[{Scalar, 2}, {"regular", {0, 0}}, X_, {\[Psi]_[i_], \[Psi]_[j_]}] :> \[Epsilon] * (4*\[Pi]*Param["\[Alpha]EM"] / (Param["sW"]^2 * Param["cW"]^2)) * \[Delta]d[X, {i, j}, \[Psi]],
		ff[{Scalar, 2}, {"ZBoson", 0}, X_, {\[Psi]_[i_], \[Psi]_[j_]}] :> \[Epsilon] * (1/4) (4*\[Pi]*Param["\[Alpha]EM"] / (Param["sW"]^2 * Param["cW"]^2))^2 * \[Delta]d[X, {i, j}, \[Psi]],
		
		(* Tensor *)
		ff[{Tensor, 1}, {"regular", {1, 0}}, X_, {\[Psi]_[i_], \[Psi]_[j_]}] :> \[Epsilon] * \[Delta]d[X, {i, j}, \[Psi]],
		
		(* Overline[u]_i d_j -> W- h *)
		(* The hermitian conjugates relations are defined in the CanonizeFFVH list *)
		
		(* Vector *)
		ff[{Vector, 1}, {"WBoson", 0}, Left, {u[i_], d[j_]}] :> \[Epsilon] * (1/(2 * Sqrt[2]))*(Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / Param["sW"])^3 WC["Hq3", {i, j}],
		ff[{Vector, 1}, {"regular", {0, 0}}, Left, {u[i_], d[j_]}] :> \[Epsilon] * Sqrt[2] *(Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / Param["sW"]) WC["Hq3", {i, j}], 
		ff[{Vector, 1}, {"WBoson", 0}, Right, {u[i_], d[j_]}] :> \[Epsilon] * (1/(4 * Sqrt[2]))*(Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / Param["sW"])^3 WC["Hud", {i, j}],
		ff[{Vector, 1}, {"regular", {0, 0}}, Right, {u[i_], d[j_]}] :> \[Epsilon] * (1/Sqrt[2])*(Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / Param["sW"]) WC["Hud", {i, j}],
		ff[{Vector, 2}, {"WBoson", 0}, Left, {u[i_], d[j_]}] :> \[Epsilon] * (1/Sqrt[2]) * (Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / Param["sW"])^3 WC["HW"] KroneckerDelta[i, j],
		ff[{Vector, 2}, {"regular", {0, 0}}, Left, {u[i_], d[j_]}] :> \[Epsilon] * (2*Sqrt[2]) * (Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / Param["sW"]) WC["HW"] KroneckerDelta[i, j] ,
		
		(* Scalar *)
		ff[{Scalar, 2}, {"WBoson", 0}, Left, {u[i_], d[j_]}] :> \[Epsilon] * (1/4)*(Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / Param["sW"])^4 WC["uW", {j, i}]\[Conjugate],
		ff[{Scalar, 2}, {"regular", {0, 0}}, Left, {u[i_], d[j_]}] :> \[Epsilon] * (Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / Param["sW"])^2 WC["uW", {j, i}]\[Conjugate], 
		ff[{Scalar, 2}, {"WBoson", 0}, Right, {u[i_], d[j_]}] :> \[Epsilon] * (1/4)*(Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / Param["sW"])^4 WC["dW", {i, j}],
		ff[{Scalar, 2}, {"regular", {0, 0}}, Right, {u[i_], d[j_]}] :> \[Epsilon] * (Sqrt[4*\[Pi]*Param["\[Alpha]EM"]] / Param["sW"])^2 WC["dW", {i, j}],
		
		(* Tensor *)
		ff[{Tensor, 1}, {"regular", {1, 0}}, Left, {u[i_], d[j_]}] :> \[Epsilon] * WC["uW", {j, i}]\[Conjugate],
		ff[{Tensor, 1}, {"regular", {1, 0}}, Right, {u[i_], d[j_]}] :> \[Epsilon] * WC["dW", {i, j}],
		
		(* Remaining FFs to zero *)
		ff[{Scalar, 1}, ___] :> 0,
		ff[{Tensor, 1}, {"regular", {0, 0}}, ___] :> 0,
		ff[{Tensor, 1}, {"ZBoson", 0}, ___] :> 0,
		ff[{Tensor, 1}, {"WBoson", 0}, ___] :> 0,
		ff[{Vector, 2}, __, Right, {u[i_], d[j_]}] :> 0
	};
	
	Return[list]
];


(* ::Subsubsection:: *)
(*Z couplings*)


gZSM[\[Psi]_, X_] := WeakIsospin3[\[Psi], X] - Param["sW"]^2 Charge[\[Psi]];


\[CapitalDelta]\[Kappa]hZZ = Param["cW"]^2 WC["HW"] + Param["sW"]^2 WC["HB"] + Param["sW"] * Param["cW"] * WC["HWB"];


\[CapitalDelta]\[Kappa]hZ\[Gamma] = 2 * Param["sW"] * Param["cW"] * (WC["HW"] - WC["HB"]) - (Param["cW"]^2 - Param["sW"]^2) * WC["HWB"];


(* Dipole couplings *)
\[Delta]d[X:Right, {i_, j_}, \[Psi]:u] := (1/Sqrt[2]) * (Param["cW"] * WC["uW", {i, j}] - Param["sW"] * WC["uB", {i, j}]);
\[Delta]d[X:Right, {i_, j_}, \[Psi]:d] := -(1/Sqrt[2]) * (Param["cW"] * WC["dW", {i, j}] + Param["sW"] * WC["dB", {i, j}]);
\[Delta]d[X:Left,  {i_, j_}, \[Psi]_] := Conjugate[\[Delta]d[Right, {j, i}, \[Psi]]];


(* ::Section:: *)
(*Canonize FF*)


(* Rule to remove redundancies in the form-factors due to hermiticity *)


CanonizeFFVH = Dispatch[
	{
	    (* WH *)
        (* Vector *)
		ff[{Vector, 1}, {"WBoson", 0}, X_, {qbar_d, q_u}] :> ff[{Vector, 1}, {"WBoson", 0}, X, {q, qbar}]\[Conjugate],
		ff[{Vector, 1}, {"regular", {0,0}}, X_, {qbar_d, q_u}] :> ff[{Vector, 1}, {"regular", {0,0}}, X, {q, qbar}]\[Conjugate],
	    ff[{Vector, 2}, {"WBoson", 0}, X_, {qbar_d, q_u}] :> ff[{Vector, 2}, {"WBoson", 0}, X, {q, qbar}]\[Conjugate],
		ff[{Vector, 2}, {"regular", {0, 0}}, X_, {qbar_d, q_u}] :> ff[{Vector, 2}, {"regular", {0, 0}}, X, {q, qbar}]\[Conjugate],
		(* Scalar *)
        ff[{Scalar, 2}, {"WBoson", 0}, Right, {qbar_d, q_u}] :> ff[{Scalar, 2}, {"WBoson", 0}, Left, {q, qbar}]\[Conjugate],
		ff[{Scalar, 2}, {"regular", {0, 0}}, Right, {qbar_d, q_u}] :> ff[{Scalar, 2}, {"regular", {0, 0}}, Left, {q, qbar}]\[Conjugate],
        ff[{Scalar, 2}, {"WBoson", 0}, Left, {qbar_d, q_u}] :> ff[{Scalar, 2}, {"WBoson", 0}, Right, {q, qbar}]\[Conjugate],
        ff[{Scalar, 2}, {"regular", {0, 0}}, Left, {qbar_d, q_u}] :> ff[{Scalar, 2}, {"regular", {0, 0}}, Right, {q, qbar}]\[Conjugate],
		(* Tensor *)
		ff[{Tensor, 1}, {"regular", {1, 0}}, Right, {qbar_d, q_u}] :> ff[{Tensor, 1}, {"regular", {1, 0}}, Left, {q, qbar}]\[Conjugate],
		ff[{Tensor, 1}, {"regular", {1, 0}}, Left, {qbar_d, q_u}] :> ff[{Tensor, 1}, {"regular", {1, 0}}, Right, {q, qbar}]\[Conjugate]
	}
];
