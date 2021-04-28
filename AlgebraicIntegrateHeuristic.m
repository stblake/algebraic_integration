(* ::Package:: *)

(* ::Title:: *)
(*A collection of simple methods for solving some pseudo-elliptic integrals*)


(* ::Subtitle:: *)
(*Sam Blake, 2020*)


(* ::Text:: *)
(*samuel.thomas.blake@gmail.com*)


(* ::Text:: *)
(*Started on 16 March 2020.*)


(* ::Text:: *)
(*This package implements a suite of heuristics for solving some pseudo-elliptic integrals using a combination of integration by substitution and the method of undetermined coefficients. We also try generalised Gunther substitutions, and attempt to set up systems of equations to solve some Abelian integrals. *)


(* ::Subsection::Closed:: *)
(*BeginPackage*)


BeginPackage["AlgebraicIntegrateHeuristic`"];

IntegrateAlgebraic::usage = "IntegrateAlgebraic[f, x] is a heuristic for computing an elementary solution to a \
pseudo-elliptic integral.";

solveAlgebraicIntegral::usage = "solveAlgebraicIntegral[f, x] is a heuristic for computing an elementary solution to a \
pseudo-elliptic integral. solveAlgebraicIntegral returns {rp, up, ip}, where rp is the (unintegrated) rational part, \
up is the unintegrated part, and ip is the integrated part.";

directRationalise::usage = "directRationalise[f, x] is an interface to the generalised \
Gunther code."

$verboseLevel::usage = "Controls how much information is shown when calling solveAlgebraicIntegral. With \
$verboseLevel = 0; no information is shown, $verboseLevel = 1; only top level information is shown, \
$verboseLevel = 2; intermediate-level information is shown, and $verboseLevel = 3; shows all information.";

Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*debugPrint*)


$verboseLevel = 0;


debugPrint1[e__] /; TrueQ[$verboseLevel > 0] := Block[{Internal`$ContextMarks = False},
	If[$Profile // TrueQ, 
		Print @ Style[Row @ {"Elapsed time: ", NumberForm[AbsoluteTime[] - $ProfileStartTime, {Infinity, 3}]}, Darker @ Blue]
	];
	Print @ Style[Row @ {e} /. 
		{A[l_] :> Symbol["A"][l], V[l_] :> Symbol["V"][l], B[l_] :> Symbol["B"][l]}, Darker @ Green]
]


debugPrint2[e__] /; TrueQ[$verboseLevel > 1] := Block[{Internal`$ContextMarks = False},
	If[$Profile // TrueQ, 
		Print @ Style[Row @ {"Elapsed time: ", NumberForm[AbsoluteTime[] - $ProfileStartTime, {Infinity, 3}]}, Darker @ Blue]
	];
	Print @ Style[Row @ {e} /. 
		{A[l_] :> Symbol["A"][l], V[l_] :> Symbol["V"][l], B[l_] :> Symbol["B"][l]}, Brown]
]


debugPrint3[e__] /; TrueQ[$verboseLevel > 2] := Block[{Internal`$ContextMarks = False},
	If[$Profile // TrueQ, 
		Print @ Style[Row @ {"Elapsed time: ", NumberForm[AbsoluteTime[] - $ProfileStartTime, {Infinity, 3}]}, Darker @ Blue]
	];
	Print @ Style[Row @ {e} /. 
		{A[l_] :> Symbol["A"][l], V[l_] :> Symbol["V"][l], B[l_] :> Symbol["B"][l]}, Gray]
]


(* ::Subsection::Closed:: *)
(*rationalQ*)


rationalQ[e_, x_] := With[
    {te = Together[e]}, 
    Denominator[te] =!= 1 && PolynomialQ[Numerator[te], x] && PolynomialQ[Denominator[te], x]
]


(* ::Input:: *)
(*rationalQ[1,x]*)


(* ::Subsection::Closed:: *)
(*singleHyperEllipticRadicalQ*)


singleHyperEllipticRadicalQ[e_, x_] := 
	Length[Union[Cases[e, Power[p_, r_Rational] /; (! FreeQ[p, x] && PolynomialQ[p, x]), {0, Infinity}]]] === 1 && 
	Length[Union[
		Cases[e, Power[p_, r_Rational] /; PolynomialQ[p, x] && Exponent[p, x] > 2 :> {p, Abs[r]}, {0, Infinity}], 
		SameTest -> (#1[[2]] == #2[[2]] && PossibleZeroQ[#1[[1]] - #2[[1]]]&)]] === 1


(* ::Input:: *)
(*singleHyperEllipticRadicalQ[Sqrt[-1+x^3],x]*)


(* ::Input:: *)
(*singleHyperEllipticRadicalQ[Sqrt[x]/Sqrt[-1+x^3],x]*)


(* ::Input:: *)
(*singleHyperEllipticRadicalQ[((2+x^3)+Sqrt[(-1+x) (1+x+x^2)])/(x^2 (-2-4 x^2+2 x^3)Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*singleHyperEllipticRadicalQ[x/Sqrt[-(-11+8 Sqrt[3]+2 Sqrt[3] x-x^2) (11+8 Sqrt[3]+2 Sqrt[3] x+x^2)],x]*)


(* ::Subsection::Closed:: *)
(*singleRadicalQ*)


singleRadicalQ[e_, x_] := 
	Length[Union[Cases[e, Power[p_, r_Rational] /; (! FreeQ[p, x] && (PolynomialQ[p, x] || rationalQ[p, x])), {0, Infinity}]]] === 1


(* ::Subsection::Closed:: *)
(*algebraicQ*)


algebraicQ[e_, x_Symbol] := Complement[
Cases[e, s_Symbol /; (Context[s] === "System`" && !NumericQ[s]), {-1}, Heads -> True],
{Plus, Times, Power, C, x}] === {} && 
Cases[e, Power[p_, q_] /; (! FreeQ[p,x] && ! MatchQ[Head[q], Integer|Rational]), {0, Infinity}] === {}


(* ::Input:: *)
(*algebraicQ[1/(C[0] x^3+C[1])^(1/3),x]*)


(* ::Input:: *)
(*algebraicQ[x/Sqrt[-(-11+8 Sqrt[3]+2 Sqrt[3] x-x^2) (11+8 Sqrt[3]+2 Sqrt[3] x+x^2)],x]*)


(* ::Input:: *)
(*algebraicQ[((2+x^3)+x^2+Sqrt[(-1+x) (1+x+x^2)])/(x^2 (-2-4 x^2+2 x^3)Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*algebraicQ[((2+x^3)+x^Sqrt[2]+Sqrt[(-1+x) (1+x+x^2)])/(x^2 (-2-4 x^2+2 x^3)Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*algebraicQ[((2+x^3)+x^Pi+Sqrt[(-1+x) (1+x+x^2)])/(x^2 (-2-4 x^2+2 x^3)Sqrt[-1+x^3]),x]*)


(* ::Subsection::Closed:: *)
(*simpleRadicalQ*)


simpleRadicalQ[_, x_] := {False, 0, 0, 0}
simpleRadicalQ[r_^(-1/2), x_] /; PolynomialQ[r, x] := {True, 1, 1, r}
simpleRadicalQ[p_ r_^(-1/2), x_] /; PolynomialQ[p, x] && PolynomialQ[r, x] := {True, p, 1, r}
simpleRadicalQ[q_^-1 r_^(-1/2), x_] /; PolynomialQ[q, x] && PolynomialQ[r, x] := {True, 1, q, r}
simpleRadicalQ[p_/q_ r_^(-1/2), x_] /; PolynomialQ[p, x] && PolynomialQ[q, x] && PolynomialQ[r, x] := {True, p, q, r}


(* ::Input:: *)
(*simpleRadicalQ[(x^2+1)/((x^2-1) Sqrt[x^3+x]),x]*)


(* ::Input:: *)
(*simpleRadicalQ[1/Sqrt[x^4-x^3-2 x^2+3 x+3],x]*)


(* ::Input:: *)
(*simpleRadicalQ[x/Sqrt[x^4-x^3-2 x^2+3 x+3],x]*)


(* ::Input:: *)
(*simpleRadicalQ[1/(x Sqrt[x^4-x^3-2 x^2+3 x+3]),x]*)


(* ::Subsection::Closed:: *)
(*normalise*)


(* ::Text:: *)
(*This can be improved. See pp. 562 of "Algorithms for Computer Algebra", Geddes et al. *)


ClearAll[apartSquareFreeList];

apartSquareFreeList[e_] := Module[{apartList},
	apartList = ApartSquareFree[e];
	If[Head[apartList] === Plus,
		List @@ apartList,
		{apartList}
	]
]


ClearAll[normalise];

normalise[e_, {x_, y_}] := Module[
	{radical, p, r, num, den, numY, denY, exy, 
		y0, y1, nonalgNum, algNum, nonAlgPart, 
		terms, algterms},

	radical = Cases[e, p_^r_Rational :> p^Abs[r] /; (! FreeQ[p, x] && (PolynomialQ[p, x] || rationalQ[p, x])), {0, Infinity}][[1]];
	{p, r} = {radical[[1]], 1/radical[[2]]};
	
	nonAlgPart = 0;
	exy = e /. {radical -> y, radical^-1 -> y^-1};
	terms = apartSquareFreeList[exy];
	nonAlgPart = Total @ Cases[terms, s_ /; FreeQ[s, y], {1}];
	algterms = Cancel @ Together[exy - nonAlgPart];
	
	num = Numerator[algterms];
	den = Denominator[algterms];

	numY = num;
	denY = den;

	exy = Cancel[numY/denY];
	numY = Numerator[exy];
	denY = Denominator[exy];

	If[FreeQ[denY, y], 
		nonalgNum = Coefficient[numY, y, 0];
		algNum = Coefficient[numY, y, 1] y;
		Return[ {algNum, denY, {p, r}, y -> radical, nonAlgPart + Cancel[nonalgNum/denY]} ]
	];

	y0 = Coefficient[denY, y, 0];
	y1 = Coefficient[denY, y, 1] y;

	If[y0 === 0,
		{numY, denY} = {numY*y^(Numerator[r] - 1), denY y^(Numerator[r] - 1) /. y^k_ :> p^(k/r)},
		denY = y0^2 - y1^2;
		{numY, denY} = {numY (y0 - y1), denY} /. y^k_ :> p^(k/r)
	];

	radical = radical^Exponent[numY, y];
	r /= Exponent[numY, y];

	nonalgNum = Coefficient[numY, y, 0];
	algNum = Coefficient[numY, y, Exponent[numY, y]] y;

	nonAlgPart += nonalgNum/denY;

	{algNum, denY, {p, r}, y -> radical, nonAlgPart // Cancel}
]


(* ::Input:: *)
(*normalise[1/(x^3 C[0]+C[1])^(1/3),{x,y}]*)


(* ::Input:: *)
(*normalise[1/((x^3 C[0]+C[1])/(C[2]x^2+C[3]))^(1/3),{x,y}]*)


(* ::Input:: *)
(*f=Together[1/(1+x^4)^(1/4)];*)
(*normalise[%,{x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]*)
(*%%[[5]]+%%[[1]]/%%[[2]]-f /. %%[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=Together[1/(1+x+x^2)+(-2+3 x^5)/((1+x^5) Sqrt[1+x^2+x^5])];*)
(*normalise[%,{x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]*)
(*%%[[5]]+%%[[1]]/%%[[2]]-f /. %%[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=Together[1/(x^4+1)^(1/4)+x/(1+x^2)];*)
(*normalise[%,{x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]*)
(*%%[[5]]+%%[[1]]/%%[[2]]-f /. %%[[4]]//Together//Simplify*)


(* ::Input:: *)
(*(* Bug - we don't split the rational and algebraic parts correctly. *)*)
(*f=(-2-2 x+x^4+x^5+x Sqrt[x^4+2])/(x^2 (1+x) Sqrt[x^4+2]);*)
(*normalise[%,{x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=1/(x^4+1)^(1/4);*)
(*normalise[%,{x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=(x-1)/((1+x) Sqrt[x+x^2+x^3]);*)
(*normalise[%,{x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=(2x^2-x)/(2x^3+4Sqrt[1-x^3]);*)
(*normalise[f,{x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=(2 Sqrt[2](x^2-1) Sqrt[x^4+1])/(x^2 (x^2+1));*)
(*normalise[f,{x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=-(4/(x^3 Sqrt[1-x^4]));*)
(*normalise[f, {x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=(1+x^2)/((1-x^2)Sqrt[1+x^4]);*)
(*normalise[f, {x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=Sqrt[-1+x^3]/(1-x);*)
(*normalise[f, {x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=x Sqrt[-1+x^3];*)
(*normalise[f, {x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=(2+x^3)/(x^2 (-2-4 x^2+x^3)Sqrt[-1+x^3]);*)
(*normalise[f, {x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=(2+x^3-Sqrt[-1+x^3])/(x^2 (-2-4 x^2+x^3)Sqrt[-1+x^3]);*)
(*normalise[f, {x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=(2+x^3)/(2x^4+x^3-1-x^2 (-2-4 x^2+x^3)Sqrt[-1+x^3]);*)
(*normalise[f, {x,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Input:: *)
(*f=-4 (-1+u^2) (-1+u Sqrt[-1+3 u^2-u^4]);*)
(*normalise[f,{u,y}]*)
(*%[[5]]+%[[1]]/%[[2]]-f /. %[[4]]//Together//Simplify*)


(* ::Subsection::Closed:: *)
(*polynomialsUndetermined*)


reasonableSize = 8;
largerSize = 10;
crazySize = 12;
ludicrousSize = 16;
tableSize = reasonableSize;

ClearAll[polynomialsUndetermined];

Options[polynomialsUndetermined] = {
	"MaxNumeratorDegree" -> reasonableSize, 
	"SubstitutionSize" -> "Small"};

polynomialsUndetermined[x_Symbol, opts:OptionsPattern[]] := 
	polynomialsUndetermined[x, opts] = 
	Switch[OptionValue["SubstitutionSize"],
		"Small",
			SortBy[
				Join[
					polynomialsUndetermined0[x, OptionValue["MaxNumeratorDegree"]], 
					polynomialsUndetermined1[x, OptionValue["MaxNumeratorDegree"]]
				],
			Exponent[#, x]&],
		"Medium",
			SortBy[
				Join[
					polynomialsUndetermined0[x, OptionValue["MaxNumeratorDegree"]], 
					polynomialsUndetermined1[x, OptionValue["MaxNumeratorDegree"]], 
					polynomialsUndetermined2[x, OptionValue["MaxNumeratorDegree"]]
				],
			Exponent[#, x]&],
		"Large",
			SortBy[
				Join[
					polynomialsUndetermined0[x, OptionValue["MaxNumeratorDegree"]], 
					polynomialsUndetermined1[x, OptionValue["MaxNumeratorDegree"]], 
					polynomialsUndetermined2[x, OptionValue["MaxNumeratorDegree"]], 
					polynomialsUndetermined3[x, OptionValue["MaxNumeratorDegree"]]
				],
			Exponent[#, x]&]
	]


polynomialsUndetermined0[x_Symbol, maxDegree_Integer] := 
	Flatten @ Table[B[0] + B[1] x^n, {n, maxDegree}];

polynomialsUndetermined1[x_Symbol, maxDegree_Integer] := 
	Flatten @ Table[
		If[n > m, 
			B[0] + B[1] x^n + B[2] x^m, 
			Sequence @@ {}
		], 
	{n, maxDegree}, {m, maxDegree}];

polynomialsUndetermined2[x_Symbol, maxDegree_Integer] := 
	Flatten @ Table[
		If[n > m > k, 
			B[0] + B[1] x^n + B[2] x^m + B[3] x^k, 
			Sequence @@ {}
		], 
	{n, maxDegree}, {m, maxDegree}, {k, maxDegree}];

polynomialsUndetermined3[x_Symbol, maxDegree_Integer] := 
	Flatten @ Table[
		If[n > m > k > l, 
			B[0] + B[1] x^n + B[2] x^m + B[3] x^k + B[3] x^l, 
			Sequence @@ {}
		], 
	{n, maxDegree}, {m, maxDegree}, {k, maxDegree}, {l, maxDegree}];


(* ::Subsection::Closed:: *)
(*IntegrateAlgebraic*)


ClearAll[IntegrateAlgebraic];

Options[IntegrateAlgebraic] = {
	VerifySolutions -> True, 
	"MaxRationalDegree" -> 8,
	"SubstitutionSize" -> "Small",
	"DegreeBound" -> 8,
	"LinearRational" -> True,
	"Expansion" -> False,
	"SingleStepTimeConstraint" -> 0.25,
	"RationalUndeterminedOnly" -> False,
	"FactorComplete" -> False
}; 

IntegrateAlgebraic[e_, x_, opts:OptionsPattern[]] := Module[
	{integratedRationalPart, unintegratedPart, integratedPart, integral},

	$timeConstraint = OptionValue["SingleStepTimeConstraint"];
	$ProfileStartTime = AbsoluteTime[];

	{integratedRationalPart, unintegratedPart, integratedPart} = solveAlgebraicIntegral[e /. C -> internalC, x, opts];
	
	If[unintegratedPart === 0, 
			integral = integratedPart + integratedRationalPart,
			integral = integratedPart + integratedRationalPart + Defer[IntegrateAlgebraic][unintegratedPart, x]
	];
	
	integral = Collect[integral, (Power[p_,_Rational] /; !FreeQ[p,x]) | _Log | _ArcTan | _ArcTanh | _RootSum, Simplify];
	stripConst[integral, x] /. internalC -> C
]


(* ::Subsection::Closed:: *)
(*solveAlgebraicIntegral*)


$Testing = False;
$Profile = True;


ClearAll[solveAlgebraicIntegral];

Options[solveAlgebraicIntegral] = Options[IntegrateAlgebraic];

solveAlgebraicIntegral[integrand_, x_, opts : OptionsPattern[]] := 
	(* solveAlgebraicIntegral[integrand, x, opts] = *) Module[
{start, u, dd, rationalPart, unintegratedPart, integratedPart, 
rationalIntegrand, substitution, integral, linRat, result, 
goursat, simplified},

start = AbsoluteTime[];

{rationalPart, unintegratedPart, integratedPart} = {0, integrand, 0};

(* Integrand is a rational function of x (needed for recursive integration). *)

If[PolynomialQ[unintegratedPart, x] || rationalQ[unintegratedPart, x], 
	debugPrint1["Integrand is in Q(x): ", unintegratedPart];
	integral = Integrate[unintegratedPart, x];
	Return[ {0, 0, postProcess[integral, x]}, Module ]
];

(* Simple derivative divides. *)

dd = derivdivides[unintegratedPart, x, u]; (* TODO: add time constraint. However, there is already a fixed 
											time constraint in the routine. This should be modified to be a 
											fraction of $timeConstraint. SAMB 0421 *)

If[ListQ[dd], 
	debugPrint1["Derivative-divides produced a simplification: ", dd];
	simplified = dd[[1]];
	substitution = dd[[2]];
	result = solveAlgebraicIntegral[simplified, u, opts];
	(* TODO: what do we do if we couldn't integrate the simplified integrand? Do we 
		give up now, or try other methods on the original integrand? SAMB 0421 *)
	result = postProcess[result /. substitution, x];
	debugPrint1["Substituting back for "substitution, " gives ", result];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]]],
		Return[ result, Module ],
		Return[ {0, unintegratedPart, 0}, Module ]
	]
];

(* Integrand is in Q(x, (a*x + b)^(m[1]/n[1]), (a*x + b)^(m[2]/n[2]), \[Ellipsis]) *)

If[ListQ @ linearRadicalToRational[unintegratedPart, x, u],
	debugPrint1["Integrand is in Q(x, (a*x + b)^(m[1]/n[1]), (a*x + b)^(m[2]/n[2]), \[Ellipsis]): ", unintegratedPart];
	integral = integrateLinearRadical[unintegratedPart, x];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[integral, unintegratedPart],
		Return[ {0, 0, postProcess[integral, x]}, Module ],
		Return[ {0, unintegratedPart, 0}, Module ]
	]
];

(* Integrand is in Q(x, (a x^2 + b x + c)^(n[1]/2), (a x^2 + b x + c)^(n[2]/2), \[Ellipsis]) *)

If[ListQ @ quadraticRadicalToRational[unintegratedPart, x, u],
	debugPrint1["Integrand is in Q(x, (a x^2 + b x + c)^(n[1]/2), (a x^2 + b x + c)^(n[2]/2), \[Ellipsis]): ", unintegratedPart];
	integral = integrateQuadraticRadical[unintegratedPart, x];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[integral, unintegratedPart],
		Return[ {0, 0, postProcess[integral, x]}, Module ],
		Return[ {0, unintegratedPart, 0}, Module ]
	]
];

(* Integrand is in Q(x, (a*x + b)^(1/2), (c*x + d)^(1/2)) *)

If[ListQ @ multipleLinearRadicalToRational[unintegratedPart, x, u],
	debugPrint1["Integrand is in Q(x, (a*x + b)^(1/2), (c*x + d)^(1/2)): ", unintegratedPart];
	integral = integrateMultipleLinearRadical[unintegratedPart, x];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[integral, unintegratedPart],
		Return[ {0, 0, postProcess[integral, x]}, Module ],
		Return[ {0, unintegratedPart, 0}, Module ]
	]
];

(* Integrand is in Q(x,((a x + b)/(c x + d))^(m[1]/n[1]), ((a x + b)/(c x + d))^(m[2]/n[2]), \[Ellipsis]) *)

If[ListQ @ linearRatioRadicalToRational[unintegratedPart, x, u],
	debugPrint1["Integrand is in Q(x,((a x + b)/(c x + d))^(m[1]/n[1]), ((a x + b)/(c x + d))^(m[2]/n[2]), \[Ellipsis]): ", unintegratedPart];
	integral = integrateLinearRatioRadical[unintegratedPart, x];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[integral, unintegratedPart],
		Return[ {0, 0, postProcess[integral, x]}, Module ],
		Return[ {0, unintegratedPart, 0}, Module ]
	]
];

(* Multiple distinct radicals? See if we can split them and integrate term-by-term using existing methods. 
	For example: IntegrateAlgebraic[((x^4 + 1)*Sqrt[x^4 - 1] + (x^4 - 1)*Sqrt[x^4 + 1])/(x^8 + 1), x] SAMB 0421 *)

If[multipleRadicalsQ[unintegratedPart, x] && nestedCount[unintegratedPart, x] == 0,
	debugPrint1["Integrand contains multiple radicals: ", unintegratedPart];
	result = integrateMultipleRadicals[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]]],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]],
		Return[{0, integrand, 0}, Module](* As no further methods deal with multiple radicals. *)
	]
];

If[multipleRadicalsQ[unintegratedPart, x] && nestedCount[unintegratedPart, x] == 0,
	Return[{Integrate[rationalPart, x], unintegratedPart, integratedPart}, Module] (* As no further methods deal with multiple radicals. *)
];

(* Nested radicals. *)

If[nestedCount[unintegratedPart, x] > 0,
	If[ListQ @ decreaseNestedRadicals[unintegratedPart, x, u],
		debugPrint1["Integrand contains nested radicals: ", unintegratedPart];
		result = integrateNestedRadicals[unintegratedPart, x, u];
		If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]]],
			rationalPart    += result[[1]]; 
			unintegratedPart = result[[2]];
			integratedPart  += result[[3]],
			Return[{0, integrand, 0}, Module]
		],
		Return[{0, integrand, 0}, Module] (* As no further methods deal with nested radicals. *)
	] 
];

If[nestedCount[unintegratedPart, x] > 0, 
	Return[{Integrate[rationalPart, x], unintegratedPart, integratedPart}, Module](* As no further methods deal with nested radicals. *)
];

(* Goursat pseudo-elliptic integral. *)

If[! OptionValue["RationalUndeterminedOnly"],
debugPrint1["Trying Goursat method on ", unintegratedPart];
goursat = TimeConstrained[goursatIntegrate[unintegratedPart, x, u], $timeConstraint, False];
If[ListQ @ goursat,
	integral = goursat // First;
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[integral, unintegratedPart],
		unintegratedPart = 0;
		integratedPart  += integral
	];
	debugPrint1["Goursat returned : ", {rationalPart, unintegratedPart, integratedPart}];
]
];

(* Rational substitution with undetermined coefficients. *)

debugPrint1["Trying rational undetermined on ", unintegratedPart];
result = rationalUndeterminedIntegrate[unintegratedPart, x, opts];
If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]]],
	rationalPart    += result[[1]]; 
	unintegratedPart = result[[2]];
	integratedPart  += result[[3]]
];
debugPrint1["Rational undetermined returned : ", {rationalPart, unintegratedPart, integratedPart}];

If[! OptionValue["RationalUndeterminedOnly"],

(* Direct rationalisation. *)

debugPrint1["Trying direct rationalisation on ", unintegratedPart];
result = directRationalise[unintegratedPart, x, opts];
If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]]],
	rationalPart    += result[[1]]; 
	unintegratedPart = result[[2]];
	integratedPart  += result[[3]]
];
debugPrint1["direct rationalisation returned : ", {rationalPart, unintegratedPart, integratedPart}];

(* Linear rational substitution. *)

If[TrueQ[OptionValue["LinearRational"]],
	debugPrint1["Trying linear rational on ", unintegratedPart];
	result = linearRationalIntegrate[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]]],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["LinearRational returned : ", {rationalPart, unintegratedPart, integratedPart}];
];

(* Solving for the logarithmic part with undetermined coefficients. *)

debugPrint1["Trying log part undetermined on ", unintegratedPart];
result = logPartIntegrate[unintegratedPart, x, opts];
If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]]],
	rationalPart    += result[[1]]; 
	unintegratedPart = result[[2]];
	integratedPart  += result[[3]]
];
debugPrint1["logPart returned : ", {rationalPart, unintegratedPart, integratedPart}];

];

(* Partial fraction expansion and integrate term-by-term. *)

If[OptionValue["Expansion"],
	
	(* Use partial fractions with a factorisation over Q. *)
	
	debugPrint1["Trying partial fractions over Q on ", unintegratedPart];
	result = apartIntegrate[unintegratedPart, x, opts, "FactorComplete" -> False];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]]],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["partialFractionIntegrate returned : ", {rationalPart, unintegratedPart, integratedPart}];
		
	(* Use partial fractions with a factorisation over the splitting field of the integrand. *)

	debugPrint1["Trying partial fractions over K on ", unintegratedPart];
	result = apartIntegrate[unintegratedPart, x, opts, "FactorComplete" -> True];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]]],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["partialFractionIntegrate returned : ", {rationalPart, unintegratedPart, integratedPart}];
			
	(* Expand and integrate term-by-term. *)
	
	debugPrint1["Trying expansion into sum of terms with Apart on ", unintegratedPart];
	result = expandIntegrate2[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]]],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["partialFractionIntegrate returned : ", {rationalPart, unintegratedPart, integratedPart}];

	debugPrint1["Trying expansion into sum of terms with Expand on ", unintegratedPart];
	result = expandIntegrate1[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]]],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["expandIntegrate returned : ", {rationalPart, unintegratedPart, integratedPart}];
];

{Integrate[rationalPart, x], unintegratedPart, integratedPart}
]


(* ::Subsection::Closed:: *)
(*rationalUndeterminedIntegrate*)


ClearAll[rationalUndeterminedIntegrate];

Options[rationalUndeterminedIntegrate] = Options[solveAlgebraicIntegral]; 

rationalUndeterminedIntegrate[0|0., x_, OptionsPattern[]] := {0, 0, 0}

rationalUndeterminedIntegrate[integrand_, x_, opts : OptionsPattern[]] := Module[
	{start, k = 0, y, u, radicands, pSqCan, solution, rationalPart, 
	integrandNumerator, integrandDenominator, p, r, a, b, y2radical, matched, radicandMatchRules, 
	rationalMatchRules, rationalFormU, integrandU, intU, intX, unintegratedPart, integratedPart,
	cancellingCoefficient, cancellingTerm, uform, radicandNumeratorU, radicandU, integral,
	radicandDenominatorU, usubstitutionParam, radicandNumeratorUParam, radicandDenominatorUParam, 
	exnum, lexnum, integrated, maxRationalDegree, maxNumeratorDegree, maxDenominatorDegree, 
	degreeBound},
 
	maxRationalDegree = OptionValue["MaxRationalDegree"];
	
	If[$Testing, start = AbsoluteTime[]];
 
	(* Single radical? *)

	If[! algebraicQ[integrand, x] || ! singleRadicalQ[integrand, x], 
		Return[{0, integrand, 0}]];

	(* Rewrite the integrand in the form r(x)+(q[x]/h[x]) y^r, where 
		y \[Equal] p[x], r(x) is the non-algebraic (rational) part, and 
		q[x], h[x], p[x] are polynomials in x. *)

	{integrandNumerator, integrandDenominator, {p, r}, y2radical, rationalPart} = normalise[integrand, {x, y}];
	debugPrint3["normalise returned ", {integrandNumerator, integrandDenominator, {p, r}, y2radical, rationalPart}];

	maxNumeratorDegree = maxDenominatorDegree = Max[Exponent[Numerator @ p, x], Exponent[Denominator @ p, x]];

	(* Defaults. *)
	RationalSubstitution = $Failed;
	solution = False;
	integratedPart = 0;	
	unintegratedPart = integrandNumerator/integrandDenominator /. y2radical;

	(* Check the integrand is in the right form. *)
	integrandNumerator = Cancel[integrandNumerator/y^Exponent[integrandNumerator, y] ];
	If[! FreeQ[integrandNumerator, y], 
		debugPrint3["normalise failed ", {integrandNumerator, integrandDenominator}];
		Return[ {0, integrand, 0} ]
	];

	(* Possible radicands in u. *)
	If[Numerator[r] === 2 && FreeQ[Denominator @ p, x] && Exponent[Numerator @ p, x] > 2 && EvenQ[Exponent[Numerator @ p, x]],
		radicands = {A[1] # + A[0] &, A[2] #^2 + A[1] # + A[0] &},
		radicands = {A[1] # + A[0] &}
	];
	
	If[! FreeQ[Denominator @ p, x], 
		AppendTo[radicands, (A[1] # + A[0])/(A[3] # + A[2]) &]]; (* TODO: Optimise when to use this form relative
																			to others. SAMB 0421 *)

	Catch @ 
	(* Loop over substitution denominators 1, x, x^2, ... *)
	Do[
		++k;
		(* Radicands of the form a u + b, u^2 + b u + c, (a u + b)/(c u + d). *)		
		Do[
			(* Loop over substitution numerators. *)
			Do[
				(* Form the substitution. *)
				uform = usubstitutionNumerators/x^denP;
				radicandU = Together @ radicand[uform];
				radicandNumeratorU = Numerator @ radicandU;
				radicandDenominatorU = Denominator @ radicandU;

				(* Check the denominator can be removed from the radical. *)
				If[FreeQ[Denominator @ p, x] && Mod[Exponent[radicandDenominatorU, x], Numerator[r]] != 0, 
					Continue[]];

				(* Check the numerator degree matches the radicand degree. *)
				
				If[Exponent[radicandNumeratorU, x] != Exponent[Numerator @ p, x] || 
					(! FreeQ[Denominator @ p, x] && Exponent[radicandDenominatorU, x] != Exponent[Denominator @ p, x]),
					Continue[]
				];

				(* Find solution to the numerator of the radical part. *)
				{matched, radicandMatchRules} = solveRadicand[p, radicandNumeratorU/radicandDenominatorU, x];

				If[matched,
					(* Loop over solutions to the radical part. *)
					debugPrint3["radicand of the form ", radicand[u]];
					debugPrint3["u substitution = ", uform];
					debugPrint3["radicand numerator/denominator = ", radicandNumeratorU, ", ", radicandDenominatorU];
			
					Do[
						debugPrint["radicand matches ", Style[radicand[u], Darker @ Green], ", ", radicandNumeratorU, radicandMatchRule];
						
						(* Parameterise. *)
						radicandDenominatorUParam = Quiet @ Expand[ radicandDenominatorU /. radicandMatchRule /. {A[1]|A[2]|A[3]|B[1]|B[2] -> 1, A[0]|B[0] -> 0} ];
						radicandNumeratorUParam = Quiet @ Expand[ radicandNumeratorU /. radicandMatchRule /. {A[1]|A[2]|A[3]|B[1]|B[2] -> 1, A[0]|B[0] -> 0} ];

						If[! FreeQ[{radicandDenominatorUParam, radicandDenominatorUParam}, C[_]|A[_]] || 
							PossibleZeroQ[radicandNumeratorUParam] || PossibleZeroQ[radicandNumeratorUParam], 
							Continue[]];

						debugPrint3["radicand numerator = ", radicandNumeratorUParam];
						debugPrint3["radicand denominator = ", radicandDenominatorUParam];
						
						usubstitutionParam = Together[ uform /. radicandMatchRule /. {A[1]|A[2]|B[1]|B[2] -> 1, A[0]|B[0] -> 0} ];

						If[FreeQ[usubstitutionParam, x] || ! FreeQ[usubstitutionParam, B[_]|A[_]] || 
							! FreeQ[radicandNumeratorUParam, B[_]|A[_]], 
								Continue[]];

						(* Exclude linear substitutions. *)
						If[Denominator[usubstitutionParam] == 1 && Exponent[Numerator[usubstitutionParam],x] == 1, 
							Continue[]];

						debugPrint3["u substitution = ", usubstitutionNumerators, ", ", Style[usubstitutionParam, Red]];
						
						(* Solve for the rational part of the integral. *)
						{matched, rationalFormU, rationalMatchRules} = solveRational[
								integrandNumerator, integrandDenominator, p, r, 
								usubstitutionParam, radicandDenominatorUParam, x, u, 
								"MaxRationalDegree" -> maxRationalDegree, 
								"RadicandPart" -> FreeQ[Denominator @ p, x]];

						If[matched, 
							(* We have found a match. *)
							debugPrint3["solution to rational part is ", rationalFormU];
	
							integrandU = rationalFormU (radicand[u]^(1/r) /. radicandMatchRule /. {A[1]|A[2]|B[1]|B[2] -> 1, A[0]|B[0] -> 0});
							debugPrint2["recursively callling IntegrateAlgebraic on ", integrandU];
	
							intU = IntegrateAlgebraic[integrandU, u];
							debugPrint3["IntegrateAlgebraic returned ", intU];
		
							If[FreeQ[intU, IntegrateAlgebraic(* | RootSum | Root *)],
								debugPrint2["Substitution is ", usubstitutionParam];
								intX = intU /. u -> usubstitutionParam;
								debugPrint2["integral is ", intX];
								intX = postProcess[intX, x, "Integrand" -> integrand];
								debugPrint2["post processed integral is ", intX];
								(* Sanity check. *)
								If[TrueQ[! OptionValue[VerifySolutions]] || TrueQ[(
										(* Order tests from fastest to slowest. *)
										numericZeroQ[Together[D[intX, x] - unintegratedPart /. internalC -> C]] || 
										PossibleZeroQ[Together[D[intX, x] - unintegratedPart /. internalC -> C]] || 
										PossibleZeroQ[D[Simplify[D[intX, x] - unintegratedPart /. internalC -> C], x]])],
									integratedPart = intX;
									unintegratedPart = 0;
									uform = RationalSubstitution = usubstitutionParam; (* Keep the u-substitution. *)
									Throw[{}],
									(* else *)
									debugPrint3["we got something wrong!"];
								],
								(* else *)
								debugPrint3["could not integrate wrt u"];
							]
						],
					{radicandMatchRule, radicandMatchRules}]
				],
			{usubstitutionNumerators, polynomialsUndetermined[x, 
						"MaxNumeratorDegree" -> maxNumeratorDegree, 
						"SubstitutionSize" -> OptionValue["SubstitutionSize"]]}],
		{radicand, radicands}], 
	{denP, 0, maxDenominatorDegree}];

	debugPrint[k];

	If[integratedPart === 0 && rationalPart === 0,
		{0, integrand, 0},
		{rationalPart, unintegratedPart, integratedPart}
	]
]


(* ::Subsection::Closed:: *)
(*solveRational*)


ClearAll[solveRationalUndetermined];

Options[solveRationalUndetermined] = {"MaxRationalDegree" -> 8};

solveRationalUndetermined[num_, den_, p_, r_, usubstitution_, radicandDenominator_, x_, u_, OptionsPattern[]] := Catch @ Module[
	{degreeBound, du, ddu, ratU, ratX, radrat, ratSolution, parameterisedFormU, eqn, vars},

	maxRationalDegree = OptionValue["MaxRationalDegree"];

	radrat = PowerExpand[radicandDenominator^(-1/r)]; (* Radical contribution to the rational part of the integrand. *)
	ddu = Together[D[usubstitution, x]];

	(* + 1 added to degreeBound below for (x^3 + x^2)^(1/3). *)

	degreeBound = 1 + Max[Exponent[num, x], Exponent[den, x], 
			Exponent[Numerator @ usubstitution, x] + Exponent[Denominator @ usubstitution, x], 
			Exponent[Numerator @ ddu, x] + Exponent[Denominator @ ddu, x]];

	debugPrint3["degree bound on the rational part is ", degreeBound];
	degreeBound = Min[degreeBound, maxRationalDegree];

	Do[
		ratU = rationalUndetermined[u, maxDegree];
		debugPrint3[{ratU, Together[ratU /. u -> usubstitution], radrat, usubstitution, ddu}];
		ratX = Cancel @ Together[ ratU du radrat /. {u -> usubstitution, du -> ddu} ];

		debugPrint3[num/den == ratX];	

		(* Previously SolveAlways was used to solve for the undetermined coefficients, however 
		   this approach doesn't work if there are parameters in the integrand.  *)
		(* ratSolution = SolveAlways[Collect[Expand[num Denominator[ratX] - Numerator[ratX] den], x] == 0, x]; *)

		eqn = Collect[Expand[num Denominator[ratX] - Numerator[ratX] den], x] == 0;
		vars = Union @ Cases[eqn, (V|A|B)[_], Infinity];
		ratSolution = Quiet[ Solve[! Eliminate[! eqn, {x}], vars], {Solve::"svars"}];

		If[TrueQ[ratSolution =!= {} && ! MatchQ[ratSolution, _Solve] && ! MatchQ[ratSolution, {{(_ -> _?PossibleZeroQ) ..}..}]],
			ratSolution = ratSolution[[1]];
			debugPrint3["solution to the rational part is ", ratSolution];
			parameterisedFormU = Cancel[ratU /. ratSolution];
			debugPrint3["parameterised forms are ", parameterisedFormU];
			Throw[ {True, parameterisedFormU, ratSolution} ],
			debugPrint3["no solution to the rational part"];
		],
	{maxDegree, degreeBound}];

	{False, 0, 0}
]


ClearAll[solveRational];

Options[solveRational] = {"MaxRationalDegree" -> 8, "RadicandPart" -> True};

solveRational[num_, den_, p_, r_, usubstitution_, radicandDenominator_, x_, u_, OptionsPattern[]] := Module[
	{y = Symbol["y"], radrat, eqns, solns, sol, pv},

	If[OptionValue["RadicandPart"],
		radrat = PowerExpand[radicandDenominator^(1/r)], (* Radical contribution to the rational part of the integrand. *)
		radrat = 1
	];
	
	eqns = {
		Dt[y] == num/den radrat Dt[x], 
		u == usubstitution, 
		Dt[u == usubstitution] // Together // Cancel // PowerExpand
	};
	eqns = eqns /. HoldPattern[Dt][Except[y|x|u]] -> 0;
	debugPrint3[eqns];

(*
	eqns = TimeConstrained[
		Eliminate[eqns, {Dt[x], x}] // Factor // PowerExpand,
		$timeConstraint/2,
		$TimedOut];
*)
	
	pv = Complement[Union @ Cases[eqns, (internalC|C)[_] | _Symbol, {0, Infinity}], {y, x, u}];
	
	eqns = TimeConstrained[
		GroebnerBasis[eqns, {u, Dt[u]}, {Dt[x], x}, 
			MonomialOrder -> EliminationOrder, 
			Method -> "Buchberger", ParameterVariables -> pv] // Factor // PowerExpand,
		4. $timeConstraint,
		debugPrint3[Style["GroebnerBasis timed-out in solveRational!", Orange]];
		$TimedOut];

	If[MatchQ[eqns, {}| $TimedOut], 
		Return[ {False, 0, 0} ]
	];
	debugPrint3[eqns];

	solns = TimeConstrained[
		Quiet[ Solve[eqns[[1]] == 0, Dt[y]] ] // Factor // PowerExpand, 
		$timeConstraint/2,
		debugPrint3[Style["Solve timed-out in solveRational!", Orange]];
		$TimedOut];

	If[solns === $TimedOut, 
		Return[ {False, 0, 0} ]
	];
	debugPrint3[solns];

	If[! MatchQ[solns, {} | {{}} | _Solve], 
		sol = Cancel[ (Dt[y] /. solns[[-1]])/Dt[u] ];
		If[FreeQ[sol, Dt[u]] && (PolynomialQ[sol, u] || rationalQ[sol, u]), 
			{True, sol, {}},
			{False, 0, 0}
		],
		{False, 0, 0}
	]
]


(* ::Subsection::Closed:: *)
(*solveRadicand*)


ClearAll[solveRadicand];

solveRadicand[radicand_, form_, x_] := Module[{rules},
	
	If[FreeQ[Denominator[radicand], x],
		solvePolynomialRadicand[Numerator @ radicand, Numerator @ form, x],
		solveRationalRadicand[radicand, form, x]
	]
]


ClearAll[solvePolynomialRadicand];

solvePolynomialRadicand[poly_, form_, x_] := Module[{rules},
	(* rules = SolveAlways[poly == form, x]; *)
	
	If[Exponent[poly, x] === Exponent[form, x],
		rules = Quiet[Solve[! Eliminate[!(poly == form), {x}], Reverse @ Union @ Cases[form, (A|B)[_], Infinity]], {Solve::"svars"}];
		{! MatchQ[rules, {} | {{}} | _Solve], rules},
		{False, {}}
	]
]


ClearAll[solveRationalRadicand];

solveRationalRadicand[rat_, form_, x_] := Module[
	{num, den, formNumerator, formDenominator, vars, eqn, rules},
	(* rules = SolveAlways[rat == form, x]; *)
	
	num = Numerator[rat];
	den = Denominator[rat];
	formNumerator = Numerator[form];
	formDenominator = Denominator[form];
	
	If[Exponent[num, x] === Exponent[formNumerator, x] && Exponent[den, x] === Exponent[formDenominator, x],
		vars = Reverse @ Union @ Cases[{formNumerator, formDenominator}, (A|B)[_], Infinity];
		eqn = num formDenominator == den formNumerator;
		rules = Quiet[Solve[! Eliminate[! eqn, {x}], vars], {Solve::"svars"}];
		{! MatchQ[rules, {} | {{}} | _Solve], rules},
		{False, {}}
	]
]


(* ::Subsection::Closed:: *)
(*rationalUndetermined*)


Clear[rationalUndetermined];

rationalUndetermined[x_Symbol, max_Integer] := 
(Sum[V[k] x^k, {k, 0, max}]/Sum[V[max + k + 1] x^k, {k, 0, max}])


(* ::Subsection::Closed:: *)
(*postProcess*)


Clear[togetherAll];

togetherAll[e_] /; FreeQ[e, RootSum] := Map[Together, e, {2, Infinity}]
togetherAll[e_] := e


Clear[powerExpand];

powerExpand[e_] := e /. Power[a_ b_^n_Integer, r_Rational] /; 
	(*! NumericQ[a] && ! NumericQ[b] && n < 0 && *) Mod[n, Denominator[r]] == 0 :> (a^r) (b^(n r)) 


ClearAll[continuousQ];

continuousQ[e_, x_] := Quiet @ TimeConstrained[
	TrueQ[! Reduce[1/e == 0, x, Reals]], 
	$timeConstraint, 
	True
]


ClearAll[nicerQ];

nicerQ[a_, b_, x_] /; FreeQ[a, x] && ! FreeQ[b, x] := True
nicerQ[a_, b_, x_] /; ! FreeQ[a, x] && FreeQ[b, x] := False
nicerQ[a_, b_, x_] := continuousQ[1/a, x] && ! continuousQ[1/b, x]
nicerQ[a_, b_, x_] := !(! continuousQ[1/a, x] && continuousQ[1/b, x])


zeroQ[e_] := TimeConstrained[PossibleZeroQ[e], $timeConstraint, False]


(* A simplification based on
	 D[(Log[a[x] + b[x]] - Log[-a[x] + b[x]]) - 2*ArcTanh[a[x]/b[x]], x] \[Equal] 0 *)
ClearAll[log2ArcTanh];

log2ArcTanh = c1_. Log[p_] + c2_. Log[q_] /;
		! zeroQ[p - q] && 
		zeroQ[c1 + c2] && 
		zeroQ[(p + q)/2 + (p - q)/2 - p] && 
		zeroQ[(p + q)/2 - (p - q)/2 - q] && 
		LeafCount[collectnumden @ Cancel[(p + q)/(q - p)]] < LeafCount[p/q] :> 
	(c2 - c1) ArcTanh[collectnumden @ Cancel[(p + q)/(q - p)]];


canonic[e_] := Module[{pf, simp},

	pf = Apart[Together @ e];
	If[Head[pf] =!= Plus, 
		simp = pf,
		simp = e
	];

	Cancel[simp]
]


(* ::Input:: *)
(*(x+x^3+x Sqrt[1-x^2+x^4])/(1-x^2+x^4+(1+x^2) Sqrt[1-x^2+x^4]) // canonic*)


(* ::Text:: *)
(*A simplification based on A&S 4.4.34:    ArcTan[x] \[PlusMinus] ArcTan[y] == ArcTan[(x \[PlusMinus] y)/(1 \[MinusPlus] x y)]*)


ClearAll[arcTanDiff, arcTanSum];

arcTanDiff = a_. ArcTan[z1_] + b_. ArcTan[z2_] /; zeroQ[a + b] && 
	LeafCount[collectnumden @ canonic[(z1 - z2)/(1 + z1 z2)]] < LeafCount[z1/z2] :> 
	(a - b)/2 ArcTan[collectnumden @ canonic[(z1 - z2)/(1 + z1 z2)]];

arcTanSum = a_. ArcTan[z1_] + b_. ArcTan[z2_] /; zeroQ[a - b] && 
	LeafCount[collectnumden @ canonic[(z1 + z2)/(1 - z1 z2)]] < LeafCount[z1/z2] :> 
	a ArcTan[collectnumden @ canonic[(z1 + z2)/(1 - z1 z2)]];


(* ::Text:: *)
(*ArcTanh[x] \[PlusMinus] ArcTanh[y] == ArcTanh[(x \[PlusMinus] y)/(1 \[PlusMinus] x y)]*)


ClearAll[arcTanhDiff, arcTanhSum];

arcTanhDiff = a_. ArcTanh[z1_] + b_. ArcTanh[z2_] /; zeroQ[a + b]  && 
	LeafCount[collectnumden @ canonic[(z1 - z2)/(1 - z1 z2)]] < LeafCount[z1/z2] :> 
	(a - b)/2 ArcTanh[collectnumden @ canonic[(z1 - z2)/(1 - z1 z2)]];

arcTanhSum = a_. ArcTanh[z1_] + b_. ArcTanh[z2_] /; zeroQ[a - b]  && 
	LeafCount[collectnumden @ canonic[(z1 + z2)/(1 + z1 z2)]] < LeafCount[z1/z2] :> 
	a ArcTanh[collectnumden @ canonic[(z1 + z2)/(1 + z1 z2)]];


ClearAll[collect];

collect[e_, x_] := Module[{permutations, simp},
	permutations = Table[Select[Tuples[{Power[_, _Rational], _Log, _ArcTan, _ArcTanh, _$rootSum},{k}], Length[Union[#]] == k&], {k, 4}];
	simp = SortBy[Table[Fold[Collect[#1, #2, Together]&, e, permutations[[k]]], {k, Length @ permutations}], LeafCount][[1]];
	simp
]

ClearAll[collect];

collect[e_, x_] := Collect[e, 
	(r_^_Rational /; !FreeQ[r,x])|_Log|_ArcTan|_ArcTanh|_$rootSum, 
	Together[RootReduce[#] // ToRadicals]&]


ClearAll[collectnumden];

collectnumden[e_] := collectnumden[e] = Collect[Numerator[e], Power[_, _Rational]]/Collect[Denominator[e], Power[_, _Rational]]


ClearAll[partialExpand];

partialExpand[e_] /; MatchQ[e, c_?NumericQ p_Plus] := e /. c_?NumericQ (p_Plus) :> Distribute[c p]
partialExpand[e_] := e


ClearAll[matchRadicals];

matchRadicals[e_, None, x_] := e

matchRadicals[e_, integrand_, x_] := Module[{intrad, erad, reps, rem, quot},

erad = Cases[e, Power[p_,r_Rational] /; PolynomialQ[p, x] :> {p, r//Abs}, {0,\[Infinity]}];
intrad = Cases[integrand, Power[p_,r_Rational] /; PolynomialQ[p, x] :> {p, r//Abs}, {0,\[Infinity]}];

reps = {};
Do[
	Do[
		If[Denominator[Last[rad]] == Denominator[Last[match]],
			rem = PolynomialRemainder[First[match],First[rad],x];
			If[rem // zeroQ,
				quot = PolynomialQuotient[First[match],First[rad],x];
				If[MatchQ[quot, x^n_ /; Mod[n,1/Denominator[Last[match]]] == 0],
					AppendTo[reps, {
						Apply[Power,rad] -> Expand[quot First[rad]]^Last[rad]/x^(Exponent[quot,x]Last[rad]),
						Apply[Power,{1,-1}rad] -> Expand[quot First[rad]]^-Last[rad] x^(Exponent[quot,x]Last[rad])}]
				]
			]
	],
	{match, intrad}],
{rad, erad}];

reps = Flatten[reps];
e /. reps
]


(* ::Input:: *)
(*matchRadicals[ArcTanh[Sqrt[x+x^2]/(1+x)], x/Sqrt[x^3+x^4],x]*)


ClearAll[stripConst];

stripConst[e_, x_] := Module[{simp, const},

	simp = e;
	simp = partialExpand[simp];
	If[Head[simp] === Plus, 
		const = Cases[simp, n_ /; FreeQ[n, x], {1}];
		simp -= Total[const];
	];
	
	simp
]


(* ::Text:: *)
(*postProcess is not only for asthetic reasons. We also attempt to correct for the substitution taking a branch of the radical. *)
(**)
(*TODO: make logands monic.*)


Clear[postProcess];

Options[postProcess] = {"Integrand" -> None, "CancelRadicalDenominators" -> True};

postProcess[l_List, x_, opts:OptionsPattern[]] := Map[postProcess[#, x, opts]&, l]

postProcess[e_, x_, OptionsPattern[]] := Module[
	{$function, simp, permutations, denomP, rad, rationalTerms, nonRationalTerms, 
	rationalTermsMerged},

	(* Remove constants. *)
	simp = stripConst[simp, x];
	
	simp = e /. {RootSum -> $rootSum, Function -> $function};

	simp = simp /. (h:Sin|Cos|Tan|Cot|Sec|Csc)[Pi r_Rational] :> FunctionExpand[h[Pi r]];
	simp = simp /. (h : ArcSinh | ArcCosh | ArcSin | ArcCos)[a_] :> TrigToExp[h[a]];
			
	(* The order of the next three lines is important, for example 
		int[(x Sqrt[x^4 - x^2])/(-3 + 2 x^2), x]
    we don't want to write Sqrt[x^4 - x^2] as x Sqrt[x^2 - 1]. *)
	simp = simp // togetherAll;
	simp = simp /. Power[p_, r_Rational] /; PolynomialQ[p, x] :> Expand[p]^r;

	If[OptionValue["CancelRadicalDenominators"],
		simp = simp // powerExpand // togetherAll,
		simp = simp // togetherAll
	];

	(* Some examples for the following rule:
		int[((1 + x^6)*Sqrt[-x - x^4 + x^7])/(1 + 2*x^3 - 2*x^9 + x^12), x]
		int[((-x + x^3)^(1/3)*(-2 + x^4))/(x^4*(1 + x^2)), x] *)

	If[OptionValue["CancelRadicalDenominators"],
		denomP = Cases[simp, (p_ x^m_Integer)^n_Rational /; PolynomialQ[p, x] :> Denominator[n], {0,\[Infinity]}] /. {} -> {1};
		denomP = LCM @@ denomP;
		simp = simp /. (p_ x^m_Integer)^n_Rational /; PolynomialQ[p, x] && 
			m < 0 :> Expand[p x^(Ceiling[Abs[m], denomP] + m)]^n /x^(n (Ceiling[Abs[m], denomP]));
	];

	(* Match radicals to those in the integrand. eg. int[x/Sqrt[x^3 + x^4], x] *)
	simp = matchRadicals[simp, OptionValue["Integrand"], x];

	(* Collect and partially simplify terms. *)
	simp = collect[simp, x];	

	(* This can often result in a simplification as denominators of sums of logs often cancel. *)
	simp = simp /. (h:Log|ArcTan|ArcTanh)[arg_] :> h[Cancel @ Together @ arg];
	simp = simp /. c_. Log[ex_] /; Denominator[ex] =!= 1 :> c Log[Numerator[ex]] - c Log[Denominator[ex]];

	simp = partialExpand[simp];

	(* Another simplification to cancel logarithms. *)
	simp = simp /. Log[ex_^n_Integer] :> n Log[ex];
	simp = collect[simp, x];

	simp = simp /. (h:Log|ArcTan|ArcTanh)[arg_] :> h[collectnumden @ canonic @ arg]; (* Yes, we have to do this twice. *)
	simp = simp /. Log[ex_] :> Log[Collect[ex, Power[_, _Rational]]];

	(* Pick the nicer of ArcTan[a/b] or -ArcTan[b/a] *)
	simp = simp /. ArcTan[a_] /; nicerQ[Numerator[a], Denominator[a], x] :> -ArcTan[collectnumden @ canonic[Denominator[a]/Numerator[a]]];

	(* Remove constant multiples in logands. *)
	simp = simp /. Log[logand_] /; FactorSquareFreeList[logand][[1]] =!= {1,1} :> 
		Log[ Apply[Times, Power @@@ Rest[FactorSquareFreeList[logand]]] ];

	simp = simp /. Log[ex_^n_Integer] :> n Log[ex]; (* Yes, using this one twice as well. *)
	simp = collect[simp, x];

	simp = simp //. log2ArcTanh;
	simp = simp //. {arcTanDiff, arcTanSum, arcTanhDiff, arcTanhSum};

	simp = simp /. ArcTan[a_] :> ArcTan[collectnumden @ canonic[a]];

	(* Pick the nicer of ArcTan[a/b] or -ArcTan[b/a]. *)
	simp = simp /. ArcTan[a_] /; nicerQ[Numerator[a], Denominator[a], x] :> -ArcTan[collectnumden @ canonic[Denominator[a]/Numerator[a]]];

	(* Pick the nicer of ArcTanh[a/b] or ArcTan[b/a]. *)
	simp = simp /. ArcTanh[a_] /; nicerQ[Numerator[a], Denominator[a], x] :> ArcTanh[collectnumden @ canonic[Denominator[a]/Numerator[a]]];

	(* Remove constants. *)
	simp = stripConst[simp, x];

	simp = collect[simp, x] /. Power[p_, r_Rational] /; PolynomialQ[p, x] :> Expand[p]^r;
	simp = simp /. p_ /; PolynomialQ[p,x] :> Collect[p, x];
	simp = simp /. Log[ex_] :> Log[Collect[ex, Power[_, _Rational]]];
	simp = simp /. Log[ex_^n_] :> n Log[ex];

	(* Remove constants. *)
	simp = stripConst[simp, x];
	
	simp = simp /. {$rootSum -> RootSum, $function -> Function};

	(* Convert to radicals, if the resulting expression is reasonable. *)
	rad = ToRadicals[simp];
	If[LeafCount[rad] < LeafCount[simp], 
		simp = rad
	];
	
	(* Try merging the rational part of the integral. *)

	If[Head[simp] === Plus, 
		simp = List @@ simp;
		rationalTerms = Cases[simp, expr_ /; FreeQ[expr, Log | ArcTan | ArcTanh], {1}];
		nonRationalTerms = Plus @@ Complement[simp, rationalTerms];
		rationalTerms = Plus @@ rationalTerms;
		rationalTermsMerged = Factor @ Together[rationalTerms];
		simp = nonRationalTerms + If[LeafCount[rationalTermsMerged] < LeafCount[rationalTerms],
			rationalTermsMerged,
			rationalTerms
		]
	];

	simp
]


(* ::Input:: *)
(*postProcess[2 (-((x^2 Sqrt[-1+(1-x^3+x^6)/x^2])/(2 (1-x^3+x^6)))+3/2 ArcTan[Sqrt[-1+(1-x^3+x^6)/x^2]]-ArcTan[Sqrt[-1+(1-x^3+x^6)/x^2]/Sqrt[2]]/Sqrt[2]),x]//Timing*)


(* ::Input:: *)
(*postProcess[Log[(1+x^8)/x]-Log[2-(1+x^8)/x+2 Sqrt[1-(1+x^8)/x+(3 (1+x^8)^2)/x^2]],x]*)


(* ::Input:: *)
(*postProcess[-(ArcTan[(-((-1-x^2)/x)+Sqrt[-3+(-1-x^2)^2/x^2])/Sqrt[3]]/(2 Sqrt[3]))+1/2 ArcTan[(2 (-((-1-x^2)/x)+Sqrt[-3+(-1-x^2)^2/x^2]))/(-3+(-((-1-x^2)/x)+Sqrt[-3+(-1-x^2)^2/x^2])^2)]+1/8 Log[1+(-1-x^2)/x-Sqrt[-3+(-1-x^2)^2/x^2]]-1/8 Log[3+(-1-x^2)/x-Sqrt[-3+(-1-x^2)^2/x^2]]-1/8 Log[1-(-1-x^2)/x+Sqrt[-3+(-1-x^2)^2/x^2]]+1/8 Log[3-(-1-x^2)/x+Sqrt[-3+(-1-x^2)^2/x^2]],x]*)


(* ::Input:: *)
(*postProcess[(4 (-1+x^4)^(1/4)-4 x^4 (-1+x^4)^(1/4)+5 x^5 RootSum[2-2 #1^4+#1^8&,(-Log[x] #1+Log[(-1+x^4)^(1/4)-x #1] #1)/(-1+#1^4)&])/(20 x^5),x]*)


(* ::Input:: *)
(*postProcess[*)
(*Log[1-3 (-x+Sqrt[-1+x^2])-(-x+Sqrt[-1+x^2])^2-(-x+Sqrt[-1+x^2])^3]-Log[1-x+Sqrt[-1+x^2]+3 (-x+Sqrt[-1+x^2])^2-(-x+Sqrt[-1+x^2])^3],x]*)


(* ::Input:: *)
(*2 Log[2+Sqrt[6] u]-Log[4-2 Sqrt[6] u+6 u^2]/.u->1/(x (x+x^6)^(1/3))*)
(*postProcess[%,x]*)


(* ::Input:: *)
(*postProcess[1/120 (180 ((1+x)/x)^(2/3)-144 ((1+x)/x)^(5/3)+45 ((1+x)/x)^(8/3)+20 2^(2/3) Sqrt[3] ArcTan[(1+2^(2/3) ((1+x)/x)^(1/3))/Sqrt[3]]+20 2^(2/3) Log[2-2^(2/3) ((1+x)/x)^(1/3)]-10 2^(2/3) Log[2+2^(2/3) ((1+x)/x)^(1/3)+2^(1/3) ((1+x)/x)^(2/3)]+40 RootSum[1-#1^3+#1^6&,Log[((1+x)/x)^(1/3)-#1]/#1&]),x]*)


(* ::Input:: *)
(*postProcess[1/3 (Sqrt[-1+(-1-x^2)^2/x^2] (2+(-1-x^2)^2/x^2)-3 Sqrt[2] ArcTanh[Sqrt[-1+(-1-x^2)^2/x^2]/Sqrt[2]]),x]*)


(* ::Input:: *)
(*postProcess[Sqrt[(1+x^2)/x]/(1+(1+x^2)/x)+ArcTan[Sqrt[(1+x^2)/x]],x]*)


(* ::Input:: *)
(*postProcess[(Sqrt[2] Sqrt[1-x^6])/(5 x^5)-(Sqrt[2] Sqrt[1-x^6])/(3 x^3)-2/5 Sqrt[2] x Sqrt[1-x^6]+1/3 Sqrt[2] x^3 Sqrt[1-x^6]+1/5 Sqrt[2] x^7 Sqrt[1-x^6],x]*)


(* ::Input:: *)
(*postProcess[-((4 Sqrt[-2+2 x^5-x^7+x^8])/(3 x^6))+(6 Sqrt[-2+2 x^5-x^7+x^8])/x^2+(4 Sqrt[-2+2 x^5-x^7+x^8])/(3 x)-2/3 x Sqrt[-2+2 x^5-x^7+x^8]+2/3 x^2 Sqrt[-2+2 x^5-x^7+x^8]+3 Log[1-Sqrt[-2+2 x^5-x^7+x^8]/x^2]-3 Log[1+Sqrt[-2+2 x^5-x^7+x^8]/x^2],x]*)


(* ::Subsection::Closed:: *)
(*verifySolution*)


verifySolution[integral_, integrand_] := Module[
	{dd, tdd},
	dd = D[integral, x];
	tdd = Together[dd - integrand];
	(* Order tests from fastest to slowest. *)
	TrueQ[
		numericZeroQ[tdd] ||
		PossibleZeroQ[tdd] || 
		PossibleZeroQ[D[Simplify[tdd], x]]
	]
]


(* ::Subsection::Closed:: *)
(*numericZeroQ*)


(* ::Text:: *)
(*TODO: Perhaps this should be clever and look at $Assumptions? Or if assumptions are made then we just (possibly) fail and rely on PossibleZeroQ and Simplify in subsequent (more time intensive) testing. *)


ClearAll[numericZeroQ];

Options[numericZeroQ] = {Precision -> $MachinePrecision, Tolerance -> 1.0*^-6};

numericZeroQ[e_, OptionsPattern[]] := Module[
	{ee, vpre, v, ef, lower, upper, step, numericeval, $c},

	If[NumericQ[e] && e == 0., Return[ True ]];
	
	vpre = Union[Flatten[{ 
		Cases[e, c:(C|internalC)[_Integer] :> (c -> Unique[$c]), {0,\[Infinity]}],
		Cases[e, s_Symbol[_Integer] /; Context[s] == "Global`" :> (s -> Unique[$c]), {0,\[Infinity]}]}]];
	ee = e /. vpre;
	
	v = Union[Flatten[Variables /@ Level[ee, {-1}]]];
	ef = Function @@ {v, ee};

	lower = -10.0 - (119. E)/(121. Pi); (* Some crazy numbers to hopefully not hit a pole. *)
	upper = 10.0 + (119. E)/(121. Pi);
	step  = (1999. Sqrt[2])/(4003.);

	(* TODO: test using Compile? *)
	
	numericeval = Table[
					Quiet[
						ef @@ SetPrecision[Table[r Random[] Exp[2.0 Pi I k/12], {Length @ v}], OptionValue[Precision]]
						],
					{k, 0, 11},
					{r, lower, upper, step}
				] // Flatten;

	Mean[Norm /@ Select[numericeval, NumericQ]] < SetPrecision[OptionValue[Tolerance], OptionValue[Precision]] // TrueQ
]


(* ::Text:: *)
(*The following two examples compare machine precision with a precision of 30 digits. *)


(* ::Input:: *)
(*Table[numericZeroQ[D[(2 Sqrt[-4+x^2+3 x^3] (-4+4 x^2+3 x^3))/(3 x^3)-3/2 Sqrt[6+2 Sqrt[5]] ArcTanh[(Sqrt[2/(3-Sqrt[5])] Sqrt[-4+x^2+3 x^3])/x]-3 Sqrt[2/(3+Sqrt[5])] ArcTanh[(Sqrt[2/(3+Sqrt[5])] Sqrt[-4+x^2+3 x^3])/x]+Sqrt[5] (7/10 Sqrt[6+2 Sqrt[5]] ArcTanh[(Sqrt[2/(3-Sqrt[5])] Sqrt[-4+x^2+3 x^3])/x]-7/5 Sqrt[2/(3+Sqrt[5])] ArcTanh[(Sqrt[2/(3+Sqrt[5])] Sqrt[-4+x^2+3 x^3])/x]),x]-((-4+3 x^3)^2 (8+3 x^3) Sqrt[-4+x^2+3 x^3])/(x^4 (16+4 x^2-24 x^3-x^4-3 x^5+9 x^6))],{100}]*)


(* ::Input:: *)
(*Table[numericZeroQ[D[(2 Sqrt[-4+x^2+3 x^3] (-4+4 x^2+3 x^3))/(3 x^3)-3/2 Sqrt[6+2 Sqrt[5]] ArcTanh[(Sqrt[2/(3-Sqrt[5])] Sqrt[-4+x^2+3 x^3])/x]-3 Sqrt[2/(3+Sqrt[5])] ArcTanh[(Sqrt[2/(3+Sqrt[5])] Sqrt[-4+x^2+3 x^3])/x]+Sqrt[5] (7/10 Sqrt[6+2 Sqrt[5]] ArcTanh[(Sqrt[2/(3-Sqrt[5])] Sqrt[-4+x^2+3 x^3])/x]-7/5 Sqrt[2/(3+Sqrt[5])] ArcTanh[(Sqrt[2/(3+Sqrt[5])] Sqrt[-4+x^2+3 x^3])/x]),x]-((-4+3 x^3)^2 (8+3 x^3) Sqrt[-4+x^2+3 x^3])/(x^4 (16+4 x^2-24 x^3-x^4-3 x^5+9 x^6)), Precision->30],{100}]*)


(* ::Text:: *)
(*Compare the timings of the following*)


(* ::Input:: *)
(*D[-(1/(10 Sqrt[-1+Sqrt[5]]))(10 Sqrt[-1+Sqrt[5]] ArcTan[x/Sqrt[1+x^2+x^4]]-2 Sqrt[10] ArcTan[(Sqrt[-2+2 Sqrt[5]] Sqrt[1+x^2+x^4])/(-1+Sqrt[5]-2 x-x^2+Sqrt[5] x^2)]+I Sqrt[10] Log[2]-2 Sqrt[15-5 Sqrt[5]] Log[(2+x+Sqrt[5] x+2 x^2)/x]+2 Sqrt[15-5 Sqrt[5]] Log[(1+Sqrt[5]+2 x+x^2+Sqrt[5] x^2-Sqrt[2+2 Sqrt[5]] Sqrt[1+x^2+x^4])/x]),x]-((-1+x^2) Sqrt[1+x^2+x^4])/((1+x^2) (1+x+x^2+x^3+x^4))//Together//PossibleZeroQ//Timing*)


(* ::Input:: *)
(*D[-(1/(10 Sqrt[-1+Sqrt[5]]))(10 Sqrt[-1+Sqrt[5]] ArcTan[x/Sqrt[1+x^2+x^4]]-2 Sqrt[10] ArcTan[(Sqrt[-2+2 Sqrt[5]] Sqrt[1+x^2+x^4])/(-1+Sqrt[5]-2 x-x^2+Sqrt[5] x^2)]+I Sqrt[10] Log[2]-2 Sqrt[15-5 Sqrt[5]] Log[(2+x+Sqrt[5] x+2 x^2)/x]+2 Sqrt[15-5 Sqrt[5]] Log[(1+Sqrt[5]+2 x+x^2+Sqrt[5] x^2-Sqrt[2+2 Sqrt[5]] Sqrt[1+x^2+x^4])/x]),x]-((-1+x^2) Sqrt[1+x^2+x^4])/((1+x^2) (1+x+x^2+x^3+x^4))//numericZeroQ//Timing*)


(* ::Subsection::Closed:: *)
(*seriesZeroQ*)


Options[seriesZeroQ] = {Tolerance -> 1.0*^-6};

seriesZeroQ[integrand_, integral_, x_, n_:16, OptionsPattern[]] := Module[{integralS, integrandS},
	Print[{integrand, integral}];
	integralS = Series[integral,{x, 0, n}] // Normal // N;
	integrandS = Series[integrand,{x, 0, n - 1}] // Normal // N;
	Mean[CoefficientList[D[integralS,x] - integrandS, x]] < OptionValue[Tolerance]
]


(* ::Input:: *)
(*seriesZeroQ[(1+6 x^8)/((-1+2 x^8) (1-x^2-x^4-4 x^8+2 x^10+4 x^16)^(1/4)),-((x ((-2+x^2-Sqrt[5] x^2+4 x^8)/(-1+2 x^8))^(1/4) ((-2+x^2+Sqrt[5] x^2+4 x^8)/(-1+2 x^8))^(1/4) AppellF1[1/2,1/4,1/4,3/2,((-(1/2)-Sqrt[5]/2) x^2)/(-1+2 x^8),((-(1/2)+Sqrt[5]/2) x^2)/(-1+2 x^8)])/(Sqrt[2] (1-x^2-x^4-4 x^8+2 x^10+4 x^16)^(1/4))),x]//Timing*)


(* ::Subsection::Closed:: *)
(*elementaryQ*)


elementaryQ[expr_] := Complement[
	Cases[Level[expr // TrigToExp, {-1}, Heads -> True], s_Symbol /; Context[s] === "System`"] // Union, 
	{Log, Exp, Plus, Times, Power, RootSum, Root, List, Function, Slot, C, Pi, E}
] === {}


(* ::Subsection::Closed:: *)
(*Integrating linear and quadratic radicals *)


(* ::Text:: *)
(*Mathematica 9 has trouble with some linear and quadratic radical rationals which have elementary forms. eg. ((1 + x)^(1/3)/x^3).*)


(* ::Input:: *)
(*Integrate[(1+u)^(1/3)/u^3,u]*)


rational2dQ[e_, {x_, y_}] := With[{te = Together[e]},
Denominator[te] =!= 1 && PolynomialQ[Numerator[te],{x,y}] && PolynomialQ[Denominator[te],{x,y}]]


ClearAll[integrate];

integrate[e_, x_] := Integrate[e, x]


integrate[e_, x_] /; ListQ[ linearRadicalToRational[e, x, $u] ] := 
	integrateLinearRadical[e, x]


ClearAll[integrateLinearRadical];

integrateLinearRadical[e_, x_] := Module[{integrand, substitution, integral},

	{integrand, substitution} = linearRadicalToRational[e, x, $u];
	debugPrint3["Rationalised integrand and substitution is ", {integrand, substitution}];
	integral = Quiet[ Integrate[integrand, $u] ] /. substitution;
	integral	
]


(* ::Input:: *)
(*integrate[(1+u)^(1/3)/u^3,u]*)


integrate[e_, x_] /; ListQ[ quadraticRadicalToRational[e, x, $u] ] := 
	integrateQuadraticRadical[e,x]


ClearAll[integrateQuadraticRadical];

integrateQuadraticRadical[e_, x_] := Module[
{t, u, mmaInt, intt, integrand, substitution, integral, numerics, result},

	(* Euler's substitution for Sqrt[quadratic]. *)
	
	result = quadraticRadicalToRational[e, x, u];
	If[ListQ[result],
		{integrand, substitution} = result;
		debugPrint3["Rationalised integrand and substitution is ", {integrand, substitution}];
		integral = Quiet @ Integrate[integrand, u] /. substitution;
		integral = integral // Apart // Expand // Together,
		integral = Quiet @ Integrate[e, x] (* eg. Integrate[Sqrt[2I x^2-3I x+1],x] *)	
	];

	(* Remove constants. *)
	If[Head[integral] === Plus, 
		numerics = Cases[integral, n_ /; FreeQ[n, x], {1}];
		integral -= Total[numerics];
	];

	integral
]


(* ::Input:: *)
(*integrateQuadraticRadical[(x+Sqrt[1-4 x+x^2])/(x-(1-4 x+x^2)^(3/2)),x]*)


(* ::Input:: *)
(*integrate[(u Sqrt[-2+u^2])/(2-4 u^2+u^4),u]*)


(* ::Input:: *)
(*integrate[(u (-2+u^2) (-1+u^2)^(5/2))/((-1+u)^5 (1+u)^5),u]*)


(* ::Input:: *)
(*integrate[1/(u Sqrt[1-u+3 u^2]),u]*)


(* ::Input:: *)
(*integrate[Sqrt[1+u^2]/((-1+u) (1+u)^2),u]*)


(* ::Subsubsection::Closed:: *)
(*linearRadicalToRational*)


(* ::Text:: *)
(*This routine converts integrals of the form *)
(**)
(*Integrate[R[x, (a*x + b)^(m[1]/n[1]), (a*x + b)^(m[2]/n[2]), \[Ellipsis]], x]*)
(**)
(*to a rational function of u*)
(**)
(*(n/a)*Integrate[R[(u^n - b)/a, u^M[1], u^M[2], \[Ellipsis]]*u^(n - 1), u],*)
(**)
(*where n = LCM[n[1], n[2], ...] and M[1] = m[1]*n/n[1], M[2] = m[2]*n/n[2], ... *)


ClearAll[linearRadicalToRational];

linearRadicalToRational[e_, x_, u_] := linearRadicalToRational[e, x, u] = Module[
{y, radicals, a, b, n, reps, exy},

(* Find radicals of the form (a x + b)^(n/m). *)
radicals = Cases[e, y:(a_. x + b_.)^n_ /; 
	FreeQ[{a,b}, x] && Head[n] == Rational :> {y, a x + b, Numerator[n], Denominator[n], a, b}, {0, Infinity}];

If[radicals === {} || Length[Union[ radicals[[All,2]] ]] > 1, 
Return[ False ]];

(* Common denominator. *)
n = LCM @@ radicals[[All,4]];
{a,b} = radicals[[1,{5,6}]];
radicals[[All,3]] *= n/radicals[[All,4]];
radicals[[All,4]] = n;

(* Convert to R[x,y], where y^n = a x + b. *)
reps = Table[radicals[[k,1]] -> y^radicals[[k,3]],{k, Length[radicals]}];
exy = Cancel @ Together[e /. reps];

(* We should now have a rational function of x and y. *)
If[!(PolynomialQ[exy, {x, y}] || rational2dQ[exy, {x, y}]), 
	Return[ False ]];

(* Substitute. *)
exy = n/a u^(n-1) exy /. {x -> (u^n - b)/a, y -> u};

{exy // Cancel, u -> (a x + b)^(1/n)}
]


(* ::Input:: *)
(*linearRadicalToRational[1/(14 (-1+u) (-1+a+u)^(1/14)),u,t]*)


(* ::Input:: *)
(*linearRadicalToRational[((1+u)^(2/3) (1-4 u+2 u^2))/(3 (-1+u)^2),u,t]*)
(*\[Integral]First[%]\[DifferentialD]t/. Last[%]*)
(*\[Integral]((1+u)^(2/3) (1-4 u+2 u^2))/(3 (-1+u)^2) \[DifferentialD]u*)


(* ::Subsubsection::Closed:: *)
(*quadraticRadicalToRational*)


ClearAll[quadraticRadicalToRational];

quadraticRadicalToRational[e_, x_, u_] := quadraticRadicalToRational[e, x, u] = Module[
{y, radicals, radicalRules, a, b, c, exy, dx, U, X, \[Alpha], \[Beta], euler1, euler2, euler3, transformed},

(* Find radicals of the form (a x + b x + c)^(n/2) *)
radicals = Cases[e, y:r_^n_/;
PolynomialQ[r,x] && Exponent[r,x] == 2 && Head[n] == Rational :>
	{Expand[r]^Abs[n], Collect[Expand[r],x], Numerator[n // Abs], Denominator[n // Abs]},
{0, Infinity}];

(* Check we have some radicals with a common radicand. *)
If[radicals === {} || Length[Union[ radicals[[All,2]] ]] > 1 || Union[ radicals[[All,-1]] ] =!= {2}, 
	Return[ False ]];

(* Convert to R(x,y), where y^2 = a x^2 + b x + c. *)
radicalRules = Flatten[ {#[[1]] -> y^#[[3]], #[[1]]^-1 -> y^-#[[3]]}& /@ radicals ];
exy = Cancel @ Together[e /. radicalRules];

(* We should now have a rational function of x and y. *)
If[!(PolynomialQ[exy, {x, y}] || rational2dQ[exy, {x, y}]), 
	Return[ False ]];

a = Coefficient[radicals[[1,2]], x, 2];
b = Coefficient[radicals[[1,2]], x, 1];
c = Coefficient[radicals[[1,2]], x, 0];

(* This is here for compact forms for solveAlgebraicIntegral. *)

transformed = {};

If[Im[a] == 0 && a > 0,
	(* Euler's first substitution. *)
	X = (u^2 - c)/(b - 2 Sqrt[a] u);
	U = Sqrt[radicals[[1,2]]] - Sqrt[a] x;
	dx = 2(-Sqrt[a] u^2 + b u - Sqrt[a]c)/(-2Sqrt[a]u + b)^2;
	euler1 = {Cancel[exy dx /. {y -> Sqrt[a]X + u, x -> X}], u -> U};
	AppendTo[transformed, euler1]
];

If[Im[a] == 0 && Im[c] == 0 && a < 0 && c > 0,
	(* Euler's second substitution. *)
	X = Cancel @ Together[(2 Sqrt[c] u - b)/(a - u^2)];
	U = (Sqrt[radicals[[1,2]]] - Sqrt[c])/x;	
	dx = 2(Sqrt[c] u^2 - b u + a Sqrt[c])/(u^2 - a)^2;
	euler2 = {Cancel[exy dx /. {y -> X u + Sqrt[c], x -> X}], u -> U};
	AppendTo[transformed, euler2]
];

If[Im[b^2 - 4 a c] == 0 && b^2 - 4 a c > 0, 
	(* Euler's third substitution. *)
	\[Alpha] = -b/(2 a) - Sqrt[b^2 - 4 a c]/(2 a);
	\[Beta] = -b/(2 a) + Sqrt[b^2 - 4 a c]/(2 a);
	X = Cancel @ Together[(a \[Beta] - \[Alpha] u^2)/(a - u^2)];
	U = Sqrt[radicals[[1,2]]]/(x - \[Alpha]);
	dx = 2 u a (\[Beta] - \[Alpha])/(u^2 - a)^2;
	euler3 = {Cancel[exy dx /. {y -> (X - \[Alpha])u, x -> X}], u -> U};
	AppendTo[transformed, euler3]
];

If[transformed === {},
	False,
	SortBy[transformed, LeafCount] // First
]
]


(* ::InheritFromParent:: *)
(*quadraticRadicalToRational[(u Sqrt[-1-2 I Sqrt[2]+u^2] (-24+60 I Sqrt[2]+62 u^2+16 I Sqrt[2] u^2-3 u^4-8 I Sqrt[2] u^4+4 I Sqrt[2] u^6+u^8))/((16 I-14 Sqrt[2]+23 I u^2+8 Sqrt[2] u^2-2 I u^4+6 Sqrt[2] u^4-I u^6) (-16 I-14 Sqrt[2]-23 I u^2+8 Sqrt[2] u^2+2 I u^4+6 Sqrt[2] u^4+I u^6)),u,t]*)


(* ::Input:: *)
(*quadraticRadicalToRational[(Sqrt[b] (Sqrt[b] c+I d u) Sqrt[a+u^2])/(4 u^3), u, t]*)
(*Integrate[%//First,t] /. Last[%]//Simplify*)
(*D[%,u]-(Sqrt[b] (Sqrt[b] c+I d u) Sqrt[a+u^2])/(4 u^3)//Simplify*)


(* ::Input:: *)
(*quadraticRadicalToRational[(x (-2+x^2) (-1+x^2)^(5/2))/((-1+x)^5 (1+x)^5), x, u]*)
(*Integrate[%//First,u] /. Last[%]//Simplify*)
(*D[%,x]-(x (-2+x^2) (-1+x^2)^(5/2))/((-1+x)^5 (1+x)^5)//Simplify*)


(* ::Input:: *)
(*f=((-1-4 x-x^2) Sqrt[-1+x^2])/(1+2 x^3+x^4);*)
(*quadraticRadicalToRational[f,x,u]*)
(*Integrate[%//First,u]*)
(*postProcess[%/. Last[%%],x]*)
(*D[%,x] - f // Simplify*)
(*Clear[f]*)


(* ::Input:: *)
(*Integrate[((-1-4 x-x^2) Sqrt[-1+x^2])/(1+2 x^3+x^4),x]*)


(* ::Subsection::Closed:: *)
(*Integrating multiple linear radicals*)


ClearAll[integrateMultipleLinearRadical];

integrateMultipleLinearRadical[e_, x_] := Module[{u, integrand, subst, integral},

	{integrand, subst} = multipleLinearRadicalToRational[e, x, u];
	integral = postProcess[Integrate[integrand, u] /. subst // Apart // Together, x];
	integral
]


(* ::Input:: *)
(*integrateMultipleLinearRadical[1/(2 Sqrt[x]+Sqrt[1+x])^2,x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[Sqrt[1-x]/(8 (1+x)^(7/2)),x]*)


(* ::Text:: *)
(*\[Integral]R(x,Sqrt[a x+b],Sqrt[c x+d])\[DifferentialD]x=\[Integral]R(\[Alpha]-(4 \[Beta] \[Gamma] u)/(c u^2-a)+(4 (2\[Alpha] a c+a d+b c)u^2-8\[Beta] \[Gamma] a u)/(c u^2-a)^2, (-\[Beta] c u^2+2\[Gamma] a u-\[Beta] a)/(c u^2-a), (\[Gamma] c u^2-2\[Beta] c u+\[Gamma] a)/(c u^2-a))((4\[Beta] \[Gamma] c^2 u^4-8(2\[Alpha] a c+a d+b c)c u^3+24\[Beta] \[Gamma] a c u^2-8(2 \[Alpha] a c+a d+b c) a u+4 \[Beta] \[Gamma] a^2)/(c u^2-a)^3)\[DifferentialD]u,*)


(* ::Text:: *)
(*where u=((Sqrt[a x+b]-Sqrt[a \[Alpha]+b])/(Sqrt[c x+d]-Sqrt[c \[Alpha]+d])).*)


(* ::Text:: *)
(*Ref: equation 221.1, pp. 32		Gr\[ODoubleDot]bner, W. Hofreiter, N. (1949). "Integraltafel Erster Teil: Unbestimmte Integrale", Springer-Verlag Wien. *)
(**)
(*Note that this reduction is far from optimal. For example, for the integrand x^3/(Sqrt[2*x - 1] + Sqrt[3*x + 4]), we end up with a difficult rational function. Rubi and Prudnikov et al no. 14. pp. 54 use a nice formulae. *)


(* ::Input::Initialization::Plain:: *)
ClearAll[multipleLinearRadicalToRational];

multipleLinearRadicalToRational[e_, x_, u_] := 
	multipleLinearRadicalToRational[e, x, u] = Module[
{radicals, ys, exy,y1, y2, dx, a, b, c, d, \[Alpha], \[Beta], \[Gamma], rat, result},

(* Find radicals of the form (a x + b)^(1/2). *)
radicals = Cases[e, y:(a_. x + b_.)^m_ /; 
	Denominator[m]==2 && FreeQ[{a,b},x] :> {y, a, b}, {0, Infinity}];

radicals = Union[radicals];

(* This transform is for two Sqrt terms only. *)
If[Length[radicals] != 2, 
	Return[ False ]];

ys = Thread[radicals[[All,1]]->{y1,y2}];
exy=e /. ys;

(* We should now have a rational function of x and y. *)
If[!(PolynomialQ[exy, {x, y1, y2}] || rationalQ[exy, {x, y1, y2}]), 
	Return[ False ]];

{{a,b},{c,d}}=radicals[[All,{2,3}]];

\[Alpha] = pickAlpha[a,b,c,d];

\[Beta]=Sqrt[a \[Alpha]+b];
\[Gamma]=Sqrt[c \[Alpha]+d];

rat=e dx/. {(a x+b)^m_/;Denominator[m]==2 :> ((-\[Beta] c u^2+2\[Gamma] a u-\[Beta] a)/(c u^2-a))^(2m),(c x+d)^m_/;Denominator[m]==2:> ((\[Gamma] c u^2-2\[Beta] c u+\[Gamma] a)/(c u^2-a))^(2m),x->\[Alpha]-(4 \[Beta] \[Gamma] u)/(c u^2-a)+(4 (2\[Alpha] a c+a d+b c)u^2-8\[Beta] \[Gamma] a u)/(c u^2-a)^2, 
dx-> (4\[Beta] \[Gamma] c^2 u^4-8(2\[Alpha] a c+a d+b c)c u^3+24\[Beta] \[Gamma] a c u^2-8(2 \[Alpha] a c+a d+b c) a u+4 \[Beta] \[Gamma] a^2)/(c u^2-a)^3};

result = {rat//Together//Cancel, u -> (Sqrt[a x+b]-\[Beta])/(Sqrt[c x+d]-\[Gamma])};
debugPrint2[result];
result
]


(* ::Input:: *)
(*integrand=1/((1+x)Sqrt[4x-3]+(4+7x)Sqrt[2x-1]);*)
(*multipleLinearRadicalToRational[integrand,x,u]*)
(*Integrate[%//First, u] /. Last[%]//Apart//Together//Cancel*)
(*D[%,x]-integrand // Simplify*)
(*Clear[integrand];*)


(* ::Input:: *)
(*integrand=(Sqrt[4x+1]+Sqrt[5x+1])/((1-x^2)Sqrt[4x+1]+(x^2+1)Sqrt[5x+1]);*)
(*multipleLinearRadicalToRational[integrand,x,u]*)
(*Integrate[%//First, u] /. Last[%]//Apart//Together//Simplify*)
(*D[%,x]-integrand // Simplify*)
(*Clear[integrand]*)


(* ::Text:: *)
(*Ideally \[Alpha] should be chosen such that Sqrt[a \[Alpha] + b] and Sqrt[c \[Alpha] + d] are both integers or rationals. *)


(* ::Input::Initialization::Plain:: *)
ClearAll[pickAlpha];

pickAlpha[a_,b_,c_,d_] := Module[
{inst, \[Alpha], p, q, n, m},

inst=TimeConstrained[
Quiet @ FindInstance[a \[Alpha]+b==(p/q)^2&&c \[Alpha]+d==(n/m)^2,{\[Alpha],n,m,p,q},Integers],
$timeConstraint, 
{}];

If[!MatchQ[inst,{}|_FindInstance],
Return[ \[Alpha] /. inst /. {e_} :> e ]
];

inst=TimeConstrained[
Quiet @ FindInstance[a \[Alpha]+b==(p/q)^2&&c \[Alpha]+d==(n/m)^2,{\[Alpha],n,m,p,q},Rationals],
$timeConstraint, 
{}];

If[!MatchQ[inst,{}|_FindInstance],
Return[ \[Alpha] /. inst /. {e_} :> e ]
];

inst=TimeConstrained[
Quiet @ FindInstance[a \[Alpha]+b==(p/q)^2&&c \[Alpha]+d==n/m && n m>0,{\[Alpha],n,m,p,q},Integers],$timeConstraint, 
{}];

If[!MatchQ[inst,{}|_FindInstance],
Return[ \[Alpha] /. inst /. {e_} :> e ]
];

inst=TimeConstrained[
Quiet @ FindInstance[a \[Alpha]+b==p/q&&c \[Alpha]+d==(n/m)^2 && p q>0,{\[Alpha],n,m,p,q},Rationals],$timeConstraint, 
{}];

If[!MatchQ[inst,{}|_FindInstance],
Return[ \[Alpha] /. inst /. {e_} :> e ]
];

(* Perhaps do something clever? *)
1
]


(* ::Input:: *)
(*Table[Sqrt[{2a/b-1,3a/b+4}],{a,500},{b,500}];*)
(*Cases[%,{_Integer|_Rational,_Integer|_Rational},{2}]*)


(* ::Subsection::Closed:: *)
(*Integrating multiple ratios of linear radicals*)


ClearAll[integrateLinearRatioRadical];

integrateLinearRatioRadical[e_, x_] := Module[{u, integrand, subst, integral},

	{integrand, subst} = linearRatioRadicalToRational[e, x, u];
	integral = Integrate[integrand, u] /. subst;
	integral
]


ClearAll[linearRatioRadicalToRational];

linearRatioRadicalToRational[e_, x_, u_] := Module[
{y, radicals,a, b, c, d, n, reps, exy, result},

(* Find radicals of the form ((a x + b)/(c x + d))^(n/m) *)
radicals = Cases[e, y:r_^n_ /; linearRatioQ[r,x] && Head[n] == Rational :> 
	{y, r // Together // Cancel, Numerator[n], Denominator[n]}, {0, Infinity}];

If[radicals === {} || Length[Union[ radicals[[All,2]] ]] > 1, 
Return[ False ]];

a = Coefficient[Numerator[radicals[[1,2]]],x,1];
b = Coefficient[Numerator[radicals[[1,2]]],x,0];
c = Coefficient[Denominator[radicals[[1,2]]],x,1];
d = Coefficient[Denominator[radicals[[1,2]]],x,0];

(* Common denominator. *)
n = LCM @@ radicals[[All,4]];
radicals[[All,3]] *= n/radicals[[All,4]];
radicals[[All,4]] = n;

(* Convert to R[x,y], where y^n = ((a x + b)/(c x + d)). *)
reps = Table[radicals[[k,1]] -> y^radicals[[k,3]],{k, Length[radicals]}];
exy = Cancel @ Together[e /. reps];

(* We should now have a rational function of x and y. *)
If[!(PolynomialQ[exy, {x, y}] || rationalQ[exy, {x, y}]), 
Return[ False ]];

(* Substitute. *)
exy = Cancel[ ((n(a d - b c) u^(n - 1))/(a - c u^n)^2) exy /. {x -> (d u^n - b)/(a - c u^n), y -> u} ];
result = {exy, u -> ((a x + b)/(c x + d))^(1/n)};
debugPrint2["Rationalised integrand and substitution are: ", result];
result
]


(* ::Input:: *)
(*f=(((x-1)/(2 x+1))^(1/4)-3 ((x-1)/(2 x+1))^(3/4))/((x-1) (2 x+1));*)
(*linearRatioRadicalToRational[f,x,u]*)
(*Integrate[%[[1]],u]*)
(*% /. %%[[2]]*)
(*D[%,x]-f//Simplify*)
(*Clear[f];*)


(* ::Input:: *)
(*f=D[(((x-1)/(2x+3))^(1/4)+((x-1)/(2x+3))^(3/2))/(1+((x-1)/(2x+3))^(1/6)),x]//Together*)
(*linearRatioRadicalToRational[f,x,u]*)
(*Integrate[%[[1]],u]*)
(*% /. %%[[2]] // Simplify*)
(*D[%,x]-f//Simplify*)
(*Clear[f];*)


(* ::Code::Initialization::Plain:: *)
ClearAll[linearRatioQ];

linearRatioQ[e_,x_] := Module[{te,num,den,nex,dex},
(* (a x + b)/(c x + d) ? *)
te=Cancel[Together[e]];
num = Numerator[te];
den=Denominator[te];
nex=Exponent[num,x];
dex=Exponent[den,x];
PolynomialQ[num,x] && PolynomialQ[den,x] && nex<2 && dex<2 && nex+dex>0
]


(* ::Input:: *)
(*linearRatioQ[(x-1)/(2x+3),x]*)
(*linearRatioQ[(3x-7)/4,x]*)
(*linearRatioQ[1/(2x+3),x]*)
(*linearRatioQ[1/(2x^2+3),x]*)
(*linearRatioQ[(2x^2+3)/7,x]*)
(*linearRatioQ[4/5,x]*)


(* ::Subsection::Closed:: *)
(*Goursat pseudo-elliptic rationalisations*)


goursatIntegrate[integrand_, x_, u_] := Module[
{quartic, integrandu, sub, integralu},

quartic = goursatQuartic[integrand, x, u];
If[ListQ[quartic],
	{integrandu, sub} = quartic;
	integralu = integrate[integrandu, u];
	Return[ List @ postProcess[integralu /. sub, x] ]
];

False
]


(* ::Subsubsection::Closed:: *)
(*goursatQuartic*)


goursatQuartic[e_, x_, u_] := Module[
{integrand, radicands, Px, Rx, roots, perms, a, b, c, d, \[ScriptCapitalL], \[ScriptCapitalM], \[ScriptCapitalN], \[Alpha], \[Beta], dx, intU},

integrand = Together[e];

If[Not[algebraicQ[integrand, x] && singleRadicalQ[integrand, x]], 
	Return[ False ]
];

radicands = Cases[integrand, 
	Power[p_, n : (1/2)|(-1/2)] /; (! FreeQ[p, x] && PolynomialQ[p, x] && Exponent[p, x] == 4) :> p, 
	{0, Infinity}];

If[radicands === {}, Return[ False ]];

Px = radicands[[1]];
Rx = Cancel @ Together[ integrand Sqrt[Px] ];

If[Length[radicands] =!= 1 && (PolynomialQ[Rx, x] || rationalQ[Rx, x]),
	Return[ False ]
];

roots = Union[ x /. Solve[Px == 0, x] ];
perms = Permutations[roots] // Sort // Reverse;
perms = Cases[perms, {a_,b_,c_,d_} /; TrueQ[ Positive[(a-c)(a-d)(b-c)(b-d)] ]];

Do[
	{a,b,c,d} = perm;
	\[ScriptCapitalL] = a + b - c - d;
	\[ScriptCapitalM] = c d - a b;
	\[ScriptCapitalN] = a b (c + d) - c d (a + b);
	If[Rx + (Rx /. x -> -((\[ScriptCapitalM] x + \[ScriptCapitalN])/(\[ScriptCapitalL] x + \[ScriptCapitalM]))) // Together // PossibleZeroQ, 
		(* Integral is a Goursat pseudo-elliptic *)
		roots = u /. Solve[\[ScriptCapitalL] u^2 + 2 \[ScriptCapitalM] u + \[ScriptCapitalN] == 0, u];
		If[Length[roots] == 2,  
			{\[Alpha], \[Beta]} = roots, 
			Continue[]];
		dx = D[(\[Beta] u - \[Alpha])/(u - 1),u];
		intU = Rx/Sqrt[Px] dx /. x -> (\[Beta] u - \[Alpha])/(u - 1) // Together // Factor // PowerExpand;
		Return[{intU, u -> (x - \[Alpha])/(x - \[Beta])}, Module]
	],
{perm,perms}];

False
]


(* ::Input:: *)
(*goursatQuartic[(-7+x)/((-11+5 x)Sqrt[-60+83 x-21 x^2-3 x^3+x^4]), x,u]*)


(* ::Input:: *)
(*goursatQuartic[(77-46 x+5 x^2)/((-23+82 x-23 x^2) Sqrt[-60+83 x-21 x^2-3 x^3+x^4]), x,u]*)


(* ::Subsection::Closed:: *)
(*Linear rational substitutions*)


(* ::Text:: *)
(*This routine tries a substitution of the form u == (x + a)/(x + b).*)


ClearAll[linearRationalIntegrate];

Options[linearRationalIntegrate] = Options[solveAlgebraicIntegral];

linearRationalIntegrate[0|0., _, OptionsPattern[]] := {0, 0, 0}

linearRationalIntegrate[e_, x_, opts : OptionsPattern[]] := Module[
	{u, linRat, options, result, const, integral},

	linRat = linearRationalSubstitution1[e, x, u];
	debugPrint3["linearRationalSubstitution1 returned ", linRat];

	If[!ListQ[linRat] || LeafCount[linRat // First] > 2 LeafCount[e], 
		linRat = linearRationalSubstitution2[e, x, u];
		debugPrint3["linearRationalSubstitution2 returned ", linRat];
	];

	If[!ListQ[linRat] || LeafCount[linRat // First] > 2 LeafCount[e], 
		linRat = linearRationalSubstitution3[e, x, u];
		debugPrint3["linearRationalSubstitution3 returned ", linRat];	
	];

	If[!ListQ[linRat] || LeafCount[linRat // First] > 2 LeafCount[e], 
		Return[ {0, e, 0} ]
	];

	debugPrint2["Linear rational substitution is ", linRat];

	(* Disable linear rational substitutions for the recursive 
	call to prevent infinite loops. SB *)
	options = Append[
		DeleteCases[{opts}, HoldPattern["LinearRational" -> True]], 
		"LinearRational" -> False];
	
	result = expandIntegrate1[linRat // First, u, Sequence @@ options];
	
	If[! MatchQ[result, {_, 0, _}],
		result = solveAlgebraicIntegral[linRat // First, u, Sequence @@ options]];
	
	If[MatchQ[result, {_, 0, _}],
	
		(* Fix for int[(3 - x^2)/((1 - x^2)*(1 - 6*x^2 + x^4)^(1/4)), x, "Expansion" -> True] *)
		integral = postProcess[powerReduce1[MapAll[Together,result /. Last[linRat]], x], x];
		const = Simplify[e/(integral[[1]] + D[integral[[3]], x])];
		If[Cancel[D[const, x]] == 0, integral[[3]] *= const];

		debugPrint2["Integral of ", linRat // First, " is ", result];
		integral,
		debugPrint2["Recursive call could not find an antiderivative of ", linRat // First];
		{0, e, 0}
	]
]


(* ::Input:: *)
(*linearRationalIntegrate[1/((x+1) (x^4+6 x^2+1)^(1/4)),x]*)


ClearAll[semiRationalise];

semiRationalise[e_, x_] := Module[{flden, conj, num, den}, 

flden = FactorSquareFree[Denominator[e]];
conj = Times @@ Cases[flden, a_. x + b_ :> b - a x];

If[conj === 1, 
	e, 
	den = conj Denominator[e] // Expand // FactorSquareFree;
	num = conj Numerator[e] // Expand // FactorSquareFree;
	num/den
]
]


(* ::Input:: *)
(*semiRationalise[-((2 2^(3/4) u^2)/((1+u) (1+u^2) (1+u^4)^(3/4))),u]*)


linearFactorQ[p_,x_] := If[PolynomialQ[p,x],
	!FreeQ[Exponent[#,x]& /@ FactorList[p][[All,1]],1],
	False
]


(* ::Input:: *)
(*linearFactorQ[-27+82 x-68 x^2+24 x^3,x]*)


powerReduce1[e_, x_] := e //. (p_ q_^n_Integer)^m_Rational /; 
	PolynomialQ[p,x] && PolynomialQ[q,x] && n < 0 && -n <= Denominator[m] :> (p q^(Denominator[m]+n))^m/q^Numerator[m]


powerReduce2[e_, x_] := e //. (p_ q_^n_Integer)^m_Rational /;
	PolynomialQ[p,x] && PolynomialQ[q,x] && n < 0 && -n <= Denominator[m] :> -(-p q^(Denominator[m]+n))^m/q^Numerator[m]


(* ::Input:: *)
(*powerReduce1[((p x^2 \[ScriptCapitalA]^2+2 p x \[ScriptCapitalA] \[ScriptCapitalB]+p \[ScriptCapitalB]^2+q x^2 \[ScriptCapitalC]^2+2 q x \[ScriptCapitalC] \[ScriptCapitalD]+q \[ScriptCapitalD]^2)/(x \[ScriptCapitalC]+\[ScriptCapitalD])^2)^(7/3),x]*)


(* ::Input:: *)
(*powerReduce1[((-27+28 x-12 x^2)/(-1+2 x)^2)^(1/3),x]*)


(* ::Subsubsection::Closed:: *)
(*linearRationalSubstitution1*)


ClearAll[linearRationalSubstitution1];

linearRationalSubstitution1[e_, x_, u_] := Module[
{radicals, radicand, deg, a, b, d, p, q, r, s, n, radU,
 eqn, soln, solns, subX, subU, intU, dx, subs, goodsubs, 
 best, uradicals, subs1, subs2},

radicals = Cases[e, Power[p_ /; PolynomialQ[p,x], r_Rational] :> {p, r}, {0,Infinity}];

If[radicals === {} || Length[Union[radicals]] > 1,
	Return[False]
];

radicand = radicals[[1,1]];
n = Denominator[radicals[[1,2]]];
deg = Exponent[radicand, x];
If[n != deg, Return[ False ]];

radU = Collect[radicand /. {x -> (a - b u)/(u - 1)} // Together // Cancel, u];
eqn = Numerator[radU] == (p u^deg + q);

solns = TimeConstrained[
		Quiet @ Solve[!Eliminate[!eqn, {u}], {a,b,p,q}],
		$timeConstraint,
		$TimedOut];

If[solns === $TimedOut, 
	Return[ False ]];

solns = DeleteCases[solns, s_ /; !FreeQ[s, Power[_Integer, _Rational]]];

If[MatchQ[solns, {}|{{}} | _Solve | {{(_ -> _?PossibleZeroQ) ..}..}], 
	Return[ False ]];

subs1 = Table[
	subX = (a - b u)/(u - 1) /. soln // Cancel;
	subX = subX /. a|b|p|q -> 1 // Cancel;
	intU = Quiet[ MapAll[Together, e dx /. {x -> subX, dx -> D[subX, u]}] // Cancel ];
	subU = (x + a)/(x + b) /. soln // Cancel;
	subU = subU /. a|b|p|q -> 1 // Cancel;
	{intU, u -> subU},
{soln, solns}];

subs2 = Table[
	subX = (a - b u)/(u - 1) /. soln // Cancel;
	subX = subX /. a|b|p|q -> 1 // Cancel;
	intU = Quiet[ MapAll[Together, e dx /. {x -> subX, dx -> D[subX, u]}] // Cancel // PowerExpand // Cancel ];
	subU = (x + a)/(x + b) /. soln // Cancel;
	subU = subU /. a|b|p|q -> 1 // Cancel;
	{intU, u -> subU},
{soln, solns}];

subs = Join[subs1, subs2];

goodsubs = Cases[subs, 
	{integrand_, u -> usub_} /; 
	Quiet[ PossibleZeroQ[e - Cancel @ Together[integrand D[usub, x] /. u -> usub]] ]];

If[goodsubs === {}, 
	Return[ False ]
];

best = SortBy[goodsubs, LeafCount] // First // PowerExpand;
best[[1]] = semiRationalise[best[[1]], u];

uradicals = Cases[best // First, Power[p_ /; !FreeQ[p, u] && PolynomialQ[p, u], r_Rational], {0,Infinity}];
If[LeafCount[uradicals] < LeafCount[radicals], 
	best, 
	False
]
]


(* ::Input:: *)
(*linearRationalSubstitution1[(-1+x^2)^2/((1+x^2) (1+6 x^2+x^4)^(3/4)),x,u]*)


(* ::Input:: *)
(*linearRationalSubstitution1[(3-x^2)/((1-x^2) (1-6 x^2+x^4)^(1/4)),x,u]*)


(* ::Input:: *)
(*linearRationalSubstitution1[1/((1+x) (x^2-x+1)^(1/3)),x,u]*)


(* ::Input:: *)
(*linearRationalSubstitution1[(-x+x^2)/Sqrt[-2 x+4 x^2-2 x^3+x^4-2 x^5+x^6],x,u]*)


(* ::Input:: *)
(*linearRationalSubstitution1[(1-x^2)^2/((x^2+1) (x^4+6 x^2+1)^(3/4)),x,u]*)


(* ::Input:: *)
(*linearRationalSubstitution1[1/((x+1) (x^4+6 x^2+1)^(1/4)),x,u]*)


(* ::Input:: *)
(*linearRationalSubstitution1[(1+15 x^2+15 x^4+x^6)^(1/6)/((-1+x) (1+x)^2),x,u]*)


(* ::Subsubsection::Closed:: *)
(*linearRationalSubstitution2*)


ClearAll[linearRationalSubstitution2];

linearRationalSubstitution2[e_, x_, u_] := Module[
{radicals, radicand, deg, a, b, c, d, p, q, r, s, t, radU,
 eqn, soln, solns, subX, subU, intU, dx, subs, goodsubs, best, 
 uradicals},

radicals = Cases[e, Power[p_ /; PolynomialQ[p,x], r_Rational] :> {p, r}, {0,Infinity}];

If[radicals === {} || Length[Union[radicals[[All,1]]]] > 1,
	Return[False]
];

radicand = radicals[[1,1]];
deg = Exponent[radicand, x];
If[OddQ[deg] || deg < 2, Return[False]];

radU = Collect[radicand /. {x -> (a - b u)/(u - 1)} // Together // Cancel, u];
eqn = Numerator[radU] == (p u^(deg) + q u^(deg/2) + r);

solns = TimeConstrained[
		Quiet @ Solve[!Eliminate[!eqn, {u}], {a,b,p,q,r}],
		$timeConstraint,
		$TimedOut];
		
If[solns === $TimedOut, 
	Return[ False ]];

solns = DeleteCases[solns, s_ /; !FreeQ[s, Power[_Integer, _Rational]]];

If[MatchQ[solns, {}|{{}} | _Solve | {{(_ -> _?PossibleZeroQ) ..}..}], 
	Return[ False ]];

subs = Table[
	subX = (a - b u)/(u - 1) /. soln // Cancel;
	subX = subX /. a|b|p|q|r -> 1 // Cancel;
	intU = Quiet[ MapAll[Together, e dx /. {x -> subX, dx -> D[subX, u]}] // Cancel // PowerExpand // Cancel ];
	subU = (x + a)/(x + b) /. soln // Cancel;
	subU = subU /. a|b|p|q|r -> 1 // Cancel;
	{semiRationalise[intU, u], u -> subU},
{soln, solns}];

goodsubs = Cases[subs, 
	{integrand_, u -> usub_} /; 
	Quiet[ PossibleZeroQ[e - PowerExpand @ Cancel @ Together[integrand D[usub, x] /. u -> usub]] ]];

If[goodsubs === {}, 
	Return[ False ]
];

best = SortBy[goodsubs, LeafCount] // First;

uradicals = Cases[best // First, Power[p_ /; !FreeQ[p, u] && PolynomialQ[p, u], r_Rational], {0,Infinity}];
If[LeafCount[uradicals] < LeafCount[radicals], 
	best, 
	False
]
]


(* ::Input:: *)
(*Timing[linearRationalSubstitution2[(-7+x)/((-11+5 x) Sqrt[-60+83 x-21 x^2-3 x^3+x^4]),x,u]]*)


(* ::Input:: *)
(*linearRationalSubstitution2[(x^2+1)/((1+x) Sqrt[x^4+x^2+1]),x,u]*)


(* ::Subsubsection::Closed:: *)
(*linearRationalSubstitution3*)


ClearAll[linearRationalSubstitution3];

linearRationalSubstitution3[e_, x_, u_] := Module[
{radicals, radicand, deg, a, b, p, q, r, n, radU,
 eqn, soln, solns, subX, subU, intU, dx, subs, goodsubs, 
 expt, best, uradicals},

radicals = Cases[e, Power[p_ /; PolynomialQ[p,x], r_Rational] :> {p, r}, {0,Infinity}];

If[radicals === {} || Length[Union[radicals[[All,1]]]] > 1,
	Return[False]
];

radicand = radicals[[1,1]];
n = Denominator[radicals[[1,2]]];
deg = Exponent[radicand, x];
If[n != deg || ! linearFactorQ[radicand, x], Return[ False ]];

radU = radicand /. {x -> (a - b u)/(u - 1)} // Together // Cancel;
eqn = Numerator[radU] == p u^(deg-1) + q;

solns = TimeConstrained[
		Quiet @ Solve[!Eliminate[!eqn, {u}], {a,b,p,q}],
		$timeConstraint,
		$TimedOut];

solns = DeleteCases[solns, s_ /; ! FreeQ[s, Complex]];

If[solns === $TimedOut, 
	Return[ False ]];

If[MatchQ[solns, {}|{{}} | _Solve | {{(_ -> _?PossibleZeroQ) ..}..}], 
	Return[ False ]];

subs = Table[
	subX = (a - b u)/(u - 1) /. soln;
	subX = subX /. a|b|p|q|r -> 1 // Cancel;
	intU = Quiet[ MapAll[Together, e dx /. {x -> subX, dx -> D[subX, u]}] // Cancel ];
	subU = (x + a)/(x + b) /. soln;
	subU = subU /. a|b|p|q|r -> 1 // Cancel;
	Sequence @@ {
		{semiRationalise[powerReduce1[intU, u], u], u -> subU}, 
		{semiRationalise[powerReduce2[intU, u], u], u -> subU}},
{soln, solns}];

goodsubs = Cases[subs, 
	{integrand_, u -> usub_} /; 
	Quiet[ PossibleZeroQ[e - powerReduce1[MapAll[Together, integrand D[usub, x] /. u -> usub],x]] ]];

If[goodsubs === {}, 
	Return[ False ]
];

best = SortBy[goodsubs, LeafCount] // First // PowerExpand;

uradicals = Cases[best // First, Power[p_ /; !FreeQ[p, u] && PolynomialQ[p, u], r_Rational], {0,Infinity}];
If[LeafCount[uradicals] < LeafCount[radicals], 
	best, 
	False
]
]


(* ::Input:: *)
(*linearRationalSubstitution3[((2 x^2+x-1) (x^4-x^3)^(1/4))/(x^2-x-1),x,u]*)


(* ::Subsection::Closed:: *)
(*Direct rationalisation*)


(* ::Text:: *)
(*Here we try to compute a substitution which rationalises the integrand. The methods here are a direct generalisation of the method of Gunther. *)
(**)
(*In the future it would be nice to see if some of this can be improved by better bounds on the degrees of the numerator and denominator of the rationalised integrand. *)
(**)
(*Ref: S. Gunther, "Sur l'\[EAcute]valuation de certaines int\[EAcute]grales pseudo-elliptiques", Bulletin de la S. M. F., tome 10 (1882), p. 88-97*)


Clear[directRationalise];

Options[directRationalise] = Options[solveAlgebraicIntegral];

directRationalise[0|0., OptionsPattern[]] := {0, 0, 0}

directRationaliseMaxRational = 2;

directRationalise[e_, x_, OptionsPattern[]] := Module[{u,r, n, p, q, result, ratIntegral},

If[Not[algebraicQ[e, x] && singleRadicalQ[e, x]], 
	Return[ {0, e, 0} ]
];

{{r,n}} = Cases[e, Power[p_, n_Rational] /; (! FreeQ[p, x] && (PolynomialQ[p, x] || rationalQ[p, x])) :> {p,n}, {0, Infinity}];

Which[
PolynomialQ[Numerator[e], x] && PolynomialQ[Denominator[e]/r^Abs[n], x],
	p = Numerator[e];
	q = Denominator[e]/r^Abs[n],
PolynomialQ[Denominator[e], x] && PolynomialQ[Numerator[e]/r^Abs[n], x],
	p = Numerator[e]/r^Abs[n];
	q = Denominator[e],
True, 
	Return[ {0, e, 0} ]	
];

(* Substitution containing r[x]^n/s[x] or s[x]/r[x]^n *)

result = directRationaliseSolve[p, q, r, n, n,-Sign[n],x,u];

If[result =!= $Failed, 
	debugPrint2["directRationalise -- substitution of the form s[x]^(-Sign[n])*r[x]^n: ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, postProcess[ratIntegral /. u -> Last[result], x]} ]
];

result = directRationaliseSolve[p, q, r, n, -n,Sign[n],x,u];

If[result =!= $Failed, 
	debugPrint2["directRationalise -- substitution of the form s[x]^(-Sign[n])*r[x]^n: ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, postProcess[ratIntegral /. u -> Last[result], x]} ]
];

(* Substitution containing r[x]^(n+1)/s[x] or s[x]/r[x]^(n+1) *)

result = directRationaliseSolve[p, q, r, n, n+1,-Sign[n+1],x,u];

If[result =!= $Failed, 
	debugPrint2["directRationalise -- substitution of the form s[x]^(-Sign[n+1])*r[x]^(n+1): ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, postProcess[ratIntegral /. u -> Last[result], x]} ]
];

result = directRationaliseSolve[p, q, r, n, -n-1,Sign[n+1],x,u];

If[result =!= $Failed, 
	debugPrint2["directRationalise -- substitution of the form s[x]^(-Sign[n+1])*r[x]^(n+1): ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, postProcess[ratIntegral /. u -> Last[result], x]} ]
];

(* Substitution containing s[x]*r[x]^n or 1/(s[x]*r[x]^n) *)
(*
result = directRationaliseSolve[p, q, r, n, n,Sign[n],x,u];

If[result =!= $Failed, 
	debugPrint2["directRationalise -- substitution of the form s[x]^(Sign[n])*r[x]^n: ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, postProcess[ratIntegral /. u -> Last[result], x]} ]
];

(* Substitution containing s[x]*r[x]^(n+1) or 1/(s[x]*r[x]^(n+1)) *)

result = directRationaliseSolve[p, q, r, n, n+1,Sign[n+1],x,u];

If[result =!= $Failed, 
	debugPrint2["directRationalise -- substitution of the form s[x]^(Sign[n+1])*r[x]^(n+1): ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, postProcess[ratIntegral /. u -> Last[result], x]} ]
];
*)

(* Quadratic rational substitutions. *)

result = directRationaliseQuadraticRationalSolve[p, q, r, n, n, x, u];

If[result =!= $Failed, 
	debugPrint2["directRationaliseQuadraticRationalSolve -- substitution of the form s[x]/t[x]*r[x]^n: ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, postProcess[ratIntegral /. u -> Last[result], x]} ]
];

result = directRationaliseQuadraticRationalSolve[p, q, r, n, -n, x, u];

If[result =!= $Failed, 
	debugPrint2["directRationaliseQuadraticRationalSolve -- substitution of the form s[x]/t[x]*r[x]^(-n): ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, postProcess[ratIntegral /. u -> Last[result], x]} ]
];

result = directRationaliseQuadraticRationalSolve[p, q, r, n, n+1, x, u];

If[result =!= $Failed, 
	debugPrint2["directRationaliseQuadraticRationalSolve -- substitution of the form s[x]/t[x]*r[x]^(n+1): ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, postProcess[ratIntegral /. u -> Last[result], x]} ]
];

result = directRationaliseQuadraticRationalSolve[p, q, r, n, -n-1, x, u];

If[result =!= $Failed, 
	debugPrint2["directRationaliseQuadraticRationalSolve -- substitution of the form s[x]/t[x]*r[x]^(-n-1): ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, postProcess[ratIntegral /. u -> Last[result], x]} ]
];

{0, e, 0}
]


(* ::Subsubsection:: *)
(*directRationaliseSolve*)


ClearAll[directRationaliseSolve];

directRationaliseSolve[p_, q_, r_, l_, m_, n_, x_, u_] := Catch @ Module[
{degp, degq, degMax, y, formnum, formden, form, yform, 
unparameterised, eqn, vars, soln, solnre, A, V},

debugPrint2["Trying directRationaliseSolve ", {p,q,r,l,m,n,x,u}];

degp = Exponent[p,x];
degq = Exponent[q,x];

degMax = Min[Max[3, degq - degp + Max[Exponent[Numerator @ r, x], Exponent[Denominator @ r, x]] - 1], 8];

debugPrint2["Degree bound = ", degMax];

Do[
	If[ndeg == 0 && ddeg == 0(* || ndeg > ddeg *), 
		Continue[]];

	debugPrint3["numerator/denominator degree = ", ndeg, ", ", ddeg];

	y = Sum[A[k] x^k, {k, 0, deg}]^n r^m;
	(*y = y /. A[deg] \[Rule] 1;*)
	debugPrint3["Substitution form is ", y];

	formnum = c Sum[V[k] u^(Denominator[m] k), {k, 0, ndeg}];
	formden = Sum[V[ndeg + 1 + k] u^(Denominator[m] k), {k, 0, ddeg}];
	form = formnum/formden;
	(* form = form /. V[ndeg|ndeg + 1 + ddeg] -> 1; *)
	form = form /. V[ndeg + 1 + ddeg] -> 1;
	debugPrint3["Rational integrand form is ", form];
	yform = form /. u -> y;

	TimeConstrained[
		unparameterised = Cancel[Together[yform D[y,x]]/r^l],
		$timeConstraint/8.0, 
		Continue[]];
	
	If[Cases[unparameterised, pp_^nn_Rational /; (PolynomialQ[pp, x] || rationalQ[pp,x]) && !FreeQ[pp, x], {0, \[Infinity]}] =!= {}, 
		Continue[]];

	(*
	Print[{{degp, Exponent[Numerator @ unparameterised, x]}, 
		{degq, Exponent[Denominator @ unparameterised, x]}}]; *)

	If[degp > Exponent[Numerator @ unparameterised, x] || 
		degq > Exponent[Denominator @ unparameterised, x] || 
		Exponent[Denominator @ unparameterised, x] - degq > 4 (* hack! *),
		Continue[]];
	
	eqn = Numerator[unparameterised] q - Denominator[unparameterised] p == 0;
	vars = Union @ Cases[eqn, (A|V)[_], Infinity];

	soln = TimeConstrained[
		Quiet @ Solve[! Eliminate[! eqn, {x}], vars],
		$timeConstraint, 
		{}
	];
	
	(* Favour solutions which do not introduce complex numbers. *)
	soln = DeleteCases[soln, {Rule[_,0]..}];
	solnre = Cases[soln, s_ /; FreeQ[N[s], _Complex]];
	soln = Join[solnre, Complement[soln, solnre]];

	Do[
		{form, y} = {form, y} /. s /. {V[_] -> 1, A[_] -> 1};
		If[!MatchQ[form, Indeterminate|0] && PossibleZeroQ[Cancel[Together[p/q r^l - (form D[y, x] /. u -> y)]]],
			RationalSubstitution = y;
			Throw @ Cancel[ {form, y} ]
		], 
	{s, soln}], 
	{ndeg, 0, directRationaliseMaxRational}, 
	{ddeg, 1, directRationaliseMaxRational},
	{c, {1, u, 1/u}}, 
	{deg, 1, degMax}];

$Failed
]


(* ::Input:: *)
(*int[(b^2 (b^3+a^3 x^3)^(1/3))/(-b^3+a^3 x^3),x]*)


(* ::Subsubsection::Closed:: *)
(*directRationaliseQuadraticRationalSolve*)


ClearAll[directRationaliseQuadraticRationalSolve];

directRationaliseQuadraticRationalSolve[p_, q_, r_, l_, m_, x_, u_] := Catch @ Module[
{degp, degq, y, formnum, formden, form, yform, unparameterised, eqn, vars, 
soln, solnre, A, V},

debugPrint2["Trying directRationaliseQuadraticRationalSolve ", {p,q,r,l,m,x,u}];

degp = Exponent[p,x];
degq = Exponent[q,x];

Do[
	If[ndeg == 0 && ddeg == 0(* || ndeg > ddeg *), 
		Continue[]];

	debugPrint3["numerator/denominator degree = ", ndeg, ", ", ddeg];

	y = (1 + A[0] x + A[1] x^2)/(A[2] + A[3] x + A[4] x^2) r^m;
	debugPrint3["Substitution form is ", y];

	formnum = c Sum[V[k] u^(Denominator[m] k), {k, 0, ndeg}];
	formden = Sum[V[ndeg + 1 + k] u^(Denominator[m] k), {k, 0, ddeg}];
	form = formnum/formden;
	form = form /. V[ndeg|ndeg + 1 + ddeg] -> 1;
	debugPrint3["Rational integrand form is ", form];
	yform = form /. u -> y;

	TimeConstrained[
		unparameterised = Cancel[Together[yform D[y,x]]/r^l],
		$timeConstraint/8.0, 
		Continue[]];
	
	(* We should now have an unparameterised rational function of x.  *)
	If[Cases[unparameterised, pp_^nn_Rational /; (PolynomialQ[pp, x] || rationalQ[pp, x]) && !FreeQ[pp, x], {0, \[Infinity]}] =!= {}, 
		Continue[]];

	(*
	Print[{{degp, Exponent[Numerator @ unparameterised, x]}, 
		{degq, Exponent[Denominator @ unparameterised, x]}}]; *)

	If[degp > Exponent[Numerator @ unparameterised, x] || 
		degq > Exponent[Denominator @ unparameterised, x] || 
		Exponent[Denominator @ unparameterised, x] - degq > 4 (* hack! *),
		Continue[]];
	
	eqn = Numerator[unparameterised] q - Denominator[unparameterised] p == 0;
	vars = Union @ Cases[eqn, (A|V)[_], Infinity];

	soln = TimeConstrained[
		Quiet @ Solve[! Eliminate[! eqn, {x}], vars],
		$timeConstraint, 
		{}
	];
	
	(* Favour solutions which do not introduce complex numbers. *)
	soln = DeleteCases[soln, {Rule[_,0]..}];
	solnre = Cases[soln, s_ /; FreeQ[N[s], _Complex]];
	soln = Join[solnre, Complement[soln, solnre]];

	Do[
		{form, y} = {form, y} /. s /. {V[_] -> 1, A[_] -> 1};
		If[!MatchQ[form, Indeterminate|0] && PossibleZeroQ[Cancel[Together[p/q r^l - (form D[y, x] /. u -> y)]]],
			RationalSubstitution = y;
			Throw @ Cancel[ {form, y} ]
		], 
	{s, soln}], 
	{ndeg, 0, 0}, 
	{ddeg, 1, 1},
	{c, {1, u, 1/u}}];

$Failed
]


(* ::Subsection::Closed:: *)
(*Specific forms solved with undetermined coefficients*)


(* ::Text:: *)
(*Some specific methods for solving otherwise difficult integrals. These methods were motivated by an integral posted by Henri Cohen in sci.math.symbolic in 1993: *)


(* ::Input:: *)
(*Integrate[x/Sqrt[-71-96 x+10 x^2+x^4],x]*)


ClearAll[logPartIntegrate];

Options[logPartIntegrate] = Options[solveAlgebraicIntegral];

logPartIntegrate[0|0., _, OptionsPattern[]] := {0, 0, 0}

logPartIntegrate[e_, x_, opts:OptionsPattern[]] := Module[
{degreeBound, te, a, b, r, n, sol},

degreeBound = OptionValue["DegreeBound"];

te = Together[e];
a = Numerator[te];
b = Denominator[te];

If[Not[algebraicQ[te, x] && singleHyperEllipticRadicalQ[te, x] && PolynomialQ[a, x]], 
	Return[ {0, e, 0} ]
];

{{r,n}} = Union @ Cases[te, Power[p_, n_Rational] /; (! FreeQ[p, x] && PolynomialQ[p, x]) :> {p, Abs[n]}, {0, Infinity}];

If[! PolynomialQ[b/r^n, x] || TrueQ[n != 1/2],
	Return[ {0, e, 0} ]
];

sol = logPartSolve1[a, b/r^n, r, x, degreeBound];
If[sol =!= False, 
	Return[ {0, 0, sol} ]
];

sol = logPartSolve2[a, b/r^n, r, x, degreeBound];
If[sol =!= False, 
	Return[ {0, 0, sol} ]
];

(* Many more to come... *)

{0, e, 0}]


(* ::Input:: *)
(*logPartIntegrate[x/Sqrt[-71-96 x+10 x^2+x^4],x]*)


(* ::Input:: *)
(*logPartIntegrate[x/Sqrt[1+4 x+3 x^2-2 x^3+x^4],x]*)


(* ::Subsubsection::Closed:: *)
(*logPartSolve1	\[LongDash]	Integrate[a[x]/(b[x] Sqrt[r[x]]), x] == c Log[p[x] + q[x] Sqrt[r[x]]]*)


(* ::Text:: *)
(*Here we require a[x], b[x], r[x] to be polynomials in x. Given*)
(**)
(*Integrate[a[x]/(b[x] Sqrt[r[x]])] == c Log[p[x] + q[x] r[x]]*)
(**)
(*we differentiate both sides and equate coefficients to give the system of (polynomial) differential equations with undetermined coefficients in the polynomials p[x] and q[x]; and a single coefficient c:*)
(**)
(*                     -2*a[x]*q[x]*r[x] + 2*c*b[x]*r[x]*p'[x] == 0*)
(*-2*a[x]*p[x] + 2*c*b[x]*r[x]*q'[x] + c*b[x]*q[x]*r'[x] == 0*)
(**)
(*The derivation of these equations is as follows:*)


(* ::Input:: *)
(*Clear[a,b,c,p,q,r,R];*)
(*D[c Log[p[x]+q [x]Sqrt[r[x]]],x]//Together//Cancel//ExpandDenominator;*)
(*%==a[x]/(b[x]Sqrt[r[x]]) /. {r[x]^(1/2) -> R,r[x]^(-1/2) -> 1/R};*)
(*Numerator[%[[1]]]Denominator[%[[2]]]==Denominator[%[[1]]]Numerator[%[[2]]] /. {r[x]^(1/2) -> R,r[x]^(-1/2) -> 1/R};*)
(*Expand[Subtract @@ %] /. R^2->r[x];*)
(*Coefficient[Collect[%,R],R,0]==0*)
(*Coefficient[Collect[%%,R],R,1]==0*)


(* ::Text:: *)
(*It is important to note that this method can fail as we do not have the a good method to bound the degrees of p[x], q[x]. This is a difficult problem which was solved by Abel, Davenport and Trager.*)


undetermined[maxDegree_, x_, offset_] := Sum[V[offset + n] x^n, {n, 0, maxDegree}]


logPartSolve1[a_, b_, r_, x_, degreeBound_:12] := 
	logPartSolve1[Function @@ {x,a}, Function @@ {x,b}, Function @@ {x,r}, x, degreeBound]


logPartSolve1[a_Function, b_Function, r_Function, x_, degreeBound_:12] := Module[
{p, q, eqns, vars, c, sol, l, logs},

Do[
	p = Function @@ {x, undetermined[bound, x, 0]};
	q = Function @@ {x, undetermined[bound, x, bound + 1]};

	eqns = And @@ {
		-2 a[x] q[x] r[x] + 2 c b[x] r[x] Derivative[1][p][x] == 0,
		-2 a[x] p[x] + 2 c b[x] r[x] Derivative[1][q][x] + c b[x] q[x] Derivative[1][r][x] == 0
	};

	vars = Append[Union @ Cases[eqns, V[_], Infinity], c];

	sol = TimeConstrained[
		Quiet @ Solve[! Eliminate[!eqns, {x}], vars],
		$timeConstraint bound,
		{}];

	sol = sol /. ConditionalExpression[e_,___] :> e /. C[1] -> 1;
	sol = DeleteCases[sol, {(_ -> _?PossibleZeroQ) ..}];

	If[! MatchQ[sol, {} | {{}} | _Solve | False], 
		Break[]
	],
	{bound, degreeBound}];

	If[MatchQ[sol, {} | {{}} | _Solve | False], 
		Return[ False ]
	];

	logs = Quiet @ Table[
		c Log[p[x] + q[x]Sqrt[r[x]]] /. sol[[k]] /. V[_] -> Apply[LCM, Denominator /@ Most[sol[[k]][[All,-1]]]],
		{k, Length[sol]}];

	logs = logs /. LCM -> Times; (* Incase LCM doesn't simplify. eg. LCM[1, Sqrt[2]] *)

	Select[logs, Quiet[ PossibleZeroQ[Together[D[#, x] - a[x]/(b[x] Sqrt[r[x]])]] ] &, 1] /. {{e_} :> e, {} -> False}
]


(* ::Text:: *)
(*Ref: Henri Cohen on sci.math.symbolic in 1993, solved by Bronstein using AXIOM:*)


(* ::Input:: *)
(*a=x;*)
(*b=1;*)
(*r=-71-96 x+10 x^2+x^4;*)
(*logPartSolve1[a,b,r,x]//Timing*)
(*D[%//Last,x]-a/(b Sqrt[r])//Simplify*)
(*Clear[a,b,r]*)


(* ::Text:: *)
(*Ref: Schultz, 2015*)


(* ::Input:: *)
(*a=29x^2+18x-3;*)
(*b=1;*)
(*r=x^6+4x^5+6x^4-12x^3+33x^2-16x;*)
(*logPartSolve1[a,b,r,x,30]//Timing*)
(*D[%//Last,x]-a/(b Sqrt[r])//Simplify*)
(*Clear[a,b,r]*)


(* ::Text:: *)
(*Ref: Deconinck & Patterson "Computing the Abel Map"*)


(* ::Input:: *)
(*a=39x^2+9x-1;*)
(*b=1;*)
(*r=x^6+4x^4+10x^3+4x^2-4x+1;*)
(*logPartSolve1[a,b,r,x,40]//Timing*)
(*D[%//Last,x]-a/(b Sqrt[r])//Simplify*)
(*Clear[a,b,r]*)


(* ::Text:: *)
(*Algebraic integral from the Facebook AI paper.*)


(* ::Input:: *)
(*a=16 x^3-42 x^2+2 x;*)
(*b=1;*)
(*r=-16 x^8+112 x^7-204 x^6+28 x^5-x^4+1;*)
(*logPartSolve1[a,b,r,x]//Timing//ExpToTrig*)
(*D[%//Last,x]-a/(b Sqrt[r])//Simplify*)
(*Clear[a,b,r]*)


(* ::Text:: *)
(*Some more nice examples I have put together:*)


(* ::Input:: *)
(*a=x;*)
(*b=1;*)
(*r=1+4 x+3 x^2-2 x^3+x^4;*)
(*Timing[logPartSolve1[a,b,r,x]]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]\(Last[%]\)\)-a/(b Sqrt[r])]*)
(*Clear[a,b,r]*)


(* ::Input:: *)
(*a=x+x^2;*)
(*b=1;*)
(*r=2 x+4 x^2+2 x^3+x^4+2 x^5+x^6;*)
(*Timing[logPartSolve1[a,b,r,x]]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]\(Last[%]\)\)-a/(b Sqrt[r])]*)
(*Clear[a,b,r]*)


(* ::Input:: *)
(*a=-x+x^2;*)
(*b=1;*)
(*r=-2 x+4 x^2-2 x^3+x^4-2 x^5+x^6;*)
(*Timing[logPartSolve1[a,b,r,x]]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]\(Last[%]\)\)-a/(b Sqrt[r])]*)
(*Clear[a,b,r]*)


(* ::Input:: *)
(*a=1+3x;*)
(*b=1;*)
(*r=-1-4 x-5 x^2-2 x^3+x^4;*)
(*Timing[logPartSolve1[a,b,r,x]]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]\(Last[%]\)\)-a/(b Sqrt[r])]*)
(*Clear[a,b,r]*)


(* ::Subsubsection::Closed:: *)
(*logPartSolve2	\[LongDash]	Integrate[a[x]/(b[x] Sqrt[r[x]]), x] == c Log[(p[x] + q[x] Sqrt[r[x]])/(p[x] - q[x] Sqrt[r[x]])]*)


(* ::Input:: *)
(*Clear[a,b,c,p,q,r,R]*)
(*D[c Log[(p[x]+q [x]Sqrt[r[x]])/(p[x]-q [x]Sqrt[r[x]])],x]//Together//Cancel//ExpandDenominator*)
(*%==a[x]/(b[x]Sqrt[r[x]]) /. {r[x]^(1/2) -> R,r[x]^(-1/2) -> 1/R}*)
(*Numerator[%[[1]]]Denominator[%[[2]]]==Denominator[%[[1]]]Numerator[%[[2]]] /. {r[x]^(1/2) -> R,r[x]^(3/2) -> R r[x],r[x]^(-1/2) -> 1/R}*)
(*Expand[Subtract @@ %] /. R^2->r[x]*)
(*Coefficient[Collect[%,R]/R,R,0]==0*)


logPartSolve2[a_, b_, r_, x_, degreeBound_:12] := 
	logPartSolve2[Function @@ {x,a}, Function @@ {x,b}, Function @@ {x,r}, x, degreeBound]


logPartSolve2[a_Function, b_Function, r_Function, x_, degreeBound_:12] := Module[
{c, p, q, eqns, vars, sol, logs},

Do[
	p = Function @@ {x,undetermined[bound, x, 0]};
	q = Function @@ {x,undetermined[bound, x, bound+1]};

	eqns = a[x] p[x]^2 - a[x] q[x]^2 r[x] + 2 c b[x] q[x] r[x] Derivative[1][p][x] - 2 c b[x] p[x] r[x] Derivative[1][q][x] - c b[x] p[x] q[x] Derivative[1][r][x] == 0;

	vars = Append[Union @ Cases[eqns, V[_], Infinity], c];

	sol = TimeConstrained[
		Quiet @ Solve[! Eliminate[!eqns, {x}], vars],
		$timeConstraint bound,
		{}];

	sol = sol /. ConditionalExpression[e_,___] :> e /. C[1] -> 1;
	sol = DeleteCases[sol, {(_ -> _?PossibleZeroQ) ..}];

	If[! MatchQ[sol, {} | {{}} | _Solve | False], 
		Break[]
	],
	{bound, degreeBound}];

	If[MatchQ[sol, {} | {{}} | _Solve | False], 
		Return[ False ]
	];

	logs = Quiet @ Table[
		c Log[(p[x] + q [x]Sqrt[r[x]])/(p[x] - q [x]Sqrt[r[x]])] /. sol[[k]] /. V[_] -> Apply[LCM, Denominator /@ Most[sol[[k]][[All,-1]]]],
		{k, Length[sol]}];
	logs = logs /. LCM -> Times; (* Incase LCM doesn't simplify. eg. LCM[1, Sqrt[2]] *)

	Select[logs, Quiet[ PossibleZeroQ[Together[D[#,x]-a[x]/(b[x] Sqrt[r[x]])]] ]&, 1] /. {{e_} :> e, {} -> False}
]


(* ::Input:: *)
(*a=2+x-x^3;*)
(*b=1+x-x^2+x^3;*)
(*r=1+x+x^3;*)
(*logPartSolve2[a,b,r,x]//Timing; $MessageList*)


(* ::Input:: *)
(*a=2+x-x^3;*)
(*b=1+x-x^2+x^3;*)
(*r=1+x+x^3;*)
(*logPartSolve2[a,b,r,x]//Timing*)
(*D[%//Last,x]-a/(b Sqrt[r])//Simplify*)
(*Clear[a,b,r]*)


(* ::Input:: *)
(*a=1-2*x;*)
(*b=1;*)
(*r=5+5*x-4*x^2-2*x^3+x^4;*)
(*logPartSolve2[a,b,r,x]//Timing*)
(*D[%//Last,x]-a/(b Sqrt[r])//Simplify*)
(*Clear[a,b,r]*)


(* ::Input:: *)
(*a=1-x;*)
(*b=1;*)
(*r=4 - 4*x^2 - 4*x^3 + x^4 + 2*x^5 + x^6;*)
(*logPartSolve2[a,b,r,x]//Timing*)
(*D[%//Last,x]-a/(b Sqrt[r])//Simplify*)
(*Clear[a,b,r]*)


(* ::Input:: *)
(*a=1+2x;*)
(*b=1;*)
(*r=-4 - 3*x - 2*x^2 + 2*x^3 + x^4;*)
(*logPartSolve2[a,b,r,x]//Timing*)
(*D[%//Last,x]-a/(b Sqrt[r])//Simplify*)
(*Clear[a,b,r]*)


(* ::Input:: *)
(*a=1;*)
(*b=1;*)
(*r=2+5*x+5*x^2+3*x^3+x^4;*)
(*logPartSolve2[a,b,r,x]//Timing*)
(*D[%//Last,x]-a/(b Sqrt[r])//Simplify*)
(*Clear[a,b,r]*)


(* ::Subsection::Closed:: *)
(*Expand integrate*)


ClearAll[expandIntegrate1];

Options[expandIntegrate1] = Options[solveAlgebraicIntegral];

expandIntegrate1[0|0., x_, opts:OptionsPattern[]] := {0, 0, 0}


expandIntegrate1[e_, x_, opts:OptionsPattern[]] := Module[
{exnum, lexnum, unintegratedPart, integratedPart, term, integrated, rationalPart},

exnum = Expand @ Numerator[e];

If[Head[exnum] === Plus, 
	debugPrint2["Expanding integrand and integrating term-by-term: ", exnum];
	lexnum = List @@ exnum;
	rationalPart = 0;
	unintegratedPart = 0;
	integratedPart = 0;
	Do[
		term = numterm/Denominator[e];
		If[rationalQ[term, x], 
			rationalPart += term,
			debugPrint2["Integrating ", term, " wrt ", x];
			integrated = solveAlgebraicIntegral[term, x, opts];
			debugPrint2["Recursive call to solveAlgebraicIntegral returned ", integrated];
			rationalPart += integrated[[1]];
			unintegratedPart += integrated[[2]];
			integratedPart += integrated[[3]]
		], 
	{numterm, lexnum}];

	If[integratedPart === 0,
		unintegratedPart = e;
		integratedPart = 0;
		rationalPart = 0;
	];
	{rationalPart, unintegratedPart, integratedPart}, 
	{0, e, 0}
]
]


(* ::Input:: *)
(*expandIntegrate1[-((2 2^(3/4) (-1+u) u^2)/((-1+u^4) (1+u^4)^(3/4))),u]*)


ClearAll[apartList];

apartList[e_, x_] := Module[{pf},

pf = Apart[e, x];

If[Head[pf] === Plus,
	pf = List @@ pf,
	Return[{e}]
];

pf = GatherBy[pf, Union[Cases[#, Power[p_, n_Rational] /; (! FreeQ[p, x] && (PolynomialQ[p, x] || rationalQ[p, x])) :> p^Abs[n],{0,Infinity}]]&];

Simplify[ Cancel[Together[Total[#]]]& /@ pf ]
]


(* ::Input:: *)
(*apartList[Sqrt[1-x]/(8 (1+x)^(7/2)),x]*)


(* ::Input:: *)
(*apartList[(2 x^(5/2))/((-1+x)^4 (1+x)^(7/2)),x]*)


ClearAll[expandIntegrate2];

Options[expandIntegrate2] = Options[solveAlgebraicIntegral];

expandIntegrate2[0|0., x_, opts:OptionsPattern[]] := {0, 0, 0}


expandIntegrate2[e_, x_, opts:OptionsPattern[]] := Module[
{exy, pf, unintegratedPart, integratedPart, integrated, rationalPart},

If[MatchQ[e, 0|0.], 
	Return[ {0, 0, 0} ]];

If[! algebraicQ[e, x], 
	Return[ {0, e, 0} ]
];

pf = apartList[e, x];

If[Length[pf] > 1, 
	debugPrint2["Expanding integrand and integrating like terms, term-by-term."];
	rationalPart = 0;
	unintegratedPart = 0;
	integratedPart = 0;
	Do[
		If[rationalQ[term, x], 
			rationalPart += term,
			debugPrint2["Integrating ", term, " wrt ", x];
			integrated = solveAlgebraicIntegral[term, x, opts];
			debugPrint2["Recursive call to solveAlgebraicIntegral returned ", integrated];
			rationalPart += integrated[[1]];
			unintegratedPart += integrated[[2]];
			integratedPart += integrated[[3]]
		], 
	{term, pf}];

	If[integratedPart === 0,
		unintegratedPart = e;
		integratedPart = 0;
		rationalPart = 0;
	];
	{rationalPart, unintegratedPart, postProcess[integratedPart // Expand, x]}, 
	{0, e, 0}
]
]


(* ::Subsection::Closed:: *)
(*Partial fraction integrate*)


ClearAll[apartIntegrate];

Options[apartIntegrate] = Options[solveAlgebraicIntegral];

apartIntegrate[0|0., x_, opts:OptionsPattern[]] := {0, 0, 0}


apartIntegrate[e_, x_, opts:OptionsPattern[]] := apartIntegrate[e, x, opts] = Module[
	{radicals, pf, y, exy, sf, rationalPart, unintegratedPart, integratedPart, integrated, den, realsf, recuropts},

radicals = Cases[e, Power[p_, n_Rational] /; (! FreeQ[p, x] && PolynomialQ[p, x]), {0, Infinity}];	

If[Length[radicals] > 1, Return[ {0, e, 0} ]];

exy = e /. radicals[[1]] -> y;

If[FreeQ[Denominator[exy], x] || Complement[Flatten[Variables /@ Level[Denominator[exy], {-1}]], {x, y}] =!= {}, Return[ {0, e, 0} ]];

If[OptionValue["FactorComplete"], 
	sf = x /. Solve[Denominator[exy] == 0 /. y -> 1, x];
	realsf = Re[sf];

	If[FreeQ[realsf, Re],
		den = Factor[Denominator[exy], Extension -> realsf],
		den = Denominator[exy]];
	If[den === Denominator[exy], 
		den = Factor[Denominator[exy], Extension -> sf]];
	exy = Numerator[exy]/den
];

pf = Apart[exy, x] /. y -> radicals[[1]];
If[Head[pf] === Plus,
	pf = List @@ pf,
	Return[{0, e, 0}]
];

debugPrint2["Using partial fractions and integrating term-by-term."];
debugPrint2["Partial fraction expansion is: ", Plus @@ pf];
rationalPart = 0;
unintegratedPart = 0;
integratedPart = 0;

Do[
	If[rationalQ[term, x], 
		rationalPart += term,
		debugPrint2["Integrating ", term, " wrt ", x];
		integrated = solveAlgebraicIntegral[term, x, opts];
		debugPrint2["Recursive call to solveAlgebraicIntegral returned ", integrated];
		rationalPart += integrated[[1]];
		unintegratedPart += integrated[[2]];
		integratedPart += integrated[[3]]
	], 
{term, pf}];

recuropts = Append[
		DeleteCases[{opts}, HoldPattern["Expansion" -> True]], 
		"Expansion" -> False];

If[integratedPart === 0,
	unintegratedPart = e;
	integratedPart = 0;
	rationalPart = 0,
	(* Recursively try to integrate all the unintegrated terms together. *)
	unintegratedPart = Cancel @ Together @ unintegratedPart;
	debugPrint2["Recursively calling solveAlgebraicIntegral on all unintegrated terms: ", unintegratedPart];
	integrated = solveAlgebraicIntegral[unintegratedPart, x, Sequence @@ recuropts];
	rationalPart += integrated[[1]];
	unintegratedPart = integrated[[2]];
	integratedPart += integrated[[3]]
];

{rationalPart, unintegratedPart, postProcess[integratedPart // Expand, x]}
]


(* ::Input:: *)
(*apartIntegrate[1/((x^2-3)Power[1-3 x^2, (3)^-1]),x,"FactorComplete"->True]*)


(* ::Subsection::Closed:: *)
(*Integrating nested radicals*)


ClearAll[integrateNestedRadicals];

integrateNestedRadicals[e_, x_, u_] := Module[{integrand, subst, result},

	{integrand, subst} = decreaseNestedRadicals[e, x, u];
	debug1["Using the substitution ", subst, " reduces the integral to ", integrand];
	result = solveAlgebraicIntegral[integrand, u];
	
	result = {0, result[[2]], Integrate[result[[1]], u] + result[[3]]} /. subst;
	result = MapAll[Factor, ExpandAll[result]] /. {RootSum -> $rootSum, Function -> $function};
	result = result /. Factor -> Identity;
	result = result /. {$rootSum -> RootSum, $function -> Function};

	rewriteNestedRadicals[
		postProcess[result // Apart // Expand, x, "CancelRadicalDenominators" -> False], 
		Last @ subst, 
		x]
]


ClearAll[decreaseNestedRadicals];

decreaseNestedRadicals[e_, x_, u_] := decreaseNestedRadicals[e, x, u] = Module[
{terms, substitution, simp},

If[nestedCount[e, x] == 0, 
	Return[False]];

terms = Union @ DeleteCases[
   Flatten[Level[e, {0, \[Infinity]}]], _Symbol | _?NumericQ | _?NumericQ _Symbol];

terms = Cases[terms, s_Power /; !FreeQ[s, Power[p_, _Rational] /; !FreeQ[p, x]]];
terms = terms /. p_^n_Rational /; n < 0 && ! PolynomialQ[p, x] :> p^(-n);
terms = Reverse @ SortBy[terms, LeafCount];
debugPrint2[terms];

If[terms === {}, 
	Return[ False ]];

simp = False;

Do[
	substitution = TimeConstrained[
	    subst[e, u -> sub, x], 
	    $timeConstraint, 
	    $TimedOut];

	If[MatchQ[substitution, {} | $TimedOut | False], 
		Continue[]];

	debugPrint2[substitution];

	If[nestedCount[substitution // First, u] < nestedCount[e, x], 
		simp = substitution;
		Break[]
	],
{sub, terms}];

simp
]


(* ::Input:: *)
(*decreaseNestedRadicals[((1+x)Sqrt[1-Sqrt[1-x^2]])/(1-x),x,u]*)


(* ::Input:: *)
(*decreaseNestedRadicals[Sqrt[x-Sqrt[x-Sqrt[1-x^2]]],x,u]*)


(* ::Input:: *)
(*decreaseNestedRadicals[x Sqrt[x^2+1] Sqrt[1+Sqrt[x^2+1]],x,u]//Timing*)


(* ::Input:: *)
(*decreaseNestedRadicals[Sqrt[x+Sqrt[2+x^2]]/((1+x^2) Sqrt[2+x^2]),x,u]//Timing*)


(* ::Code::Initialization::Plain:: *)
ClearAll[nestedCount];

nestedCount[e_, x_] := Total[ Cases[e, Power[r_, _Rational] /; 
				! FreeQ[r, x] && ! FreeQ[r, Power[_, _Rational]] :> 1 + nestedCount[r, x], {0, \[Infinity]}] /. {} -> {0} ]


(* ::Code::Initialization::Plain:: *)
ClearAll[radicalCount];

radicalCount[e_, x_] := Total[ Cases[e, Power[r_, _Rational] /; 
				! FreeQ[r, x] :> 1, {0, \[Infinity]}] /. {} -> {0} ]


(* ::Input:: *)
(*nestedCount[Sqrt[x^2+1],x]*)


(* ::Input:: *)
(*nestedCount[x Sqrt[x^2+1] Sqrt[1+Sqrt[x^2+1]],x]*)


(* ::Input:: *)
(*nestedCount[Sqrt[1+Sqrt[1-Sqrt[x]]],x]*)


(* ::Input:: *)
(*nestedCount[Sqrt[1-Sqrt[x]],x]*)


(* ::Input:: *)
(*nestedCount[Sqrt[1-Sqrt[1-Sqrt[x]]],x]*)


(* ::Input:: *)
(*nestedCount[u^2 Sqrt[1+u],u]*)


Clear[rewriteNestedRadicals];

rewriteNestedRadicals[integral_, sub_, x_] := 
((integral /. Power[e_Plus|e_Times, r_Rational] :> Factor[Expand[e]]^r) //. 
Power[a_ b_, r_Rational] /; FreeQ[a, x] || (PolynomialQ[a, x] && Denominator[b] == 1) :> PowerExpand[a^r] b^r) /.{
Power[a_+b:Power[c_,_Rational],r_Rational] /; Denominator[sub[[1]]] == 1 && Simplify[(a+b+sub[[1]])-2b] == 0 :> PowerExpand[Power[Expand[(a+b)(-a+b)], r]]/(-a+b)^r,
Power[a_-b:Power[c_,_Rational],r_Rational] /; Denominator[sub[[1]]] == 1 && Simplify[(a-b+sub[[1]])-2a] == 0 :> PowerExpand[Power[Expand[(a+b)(a-b)], r]]/(a+b)^r
}


(* ::Input:: *)
(*rewriteNestedRadicals[(Sqrt[1-Sqrt[1-Sqrt[(-1+x)/x]]] (-1+Sqrt[1-Sqrt[(-1+x)/x]]) (1+Sqrt[1-Sqrt[(-1+x)/x]]))/((-1+Sqrt[1-Sqrt[1-Sqrt[(-1+x)/x]]]) (1+Sqrt[1-Sqrt[1-Sqrt[(-1+x)/x]]]) Sqrt[1-Sqrt[(-1+x)/x]] (1+Sqrt[(-1+x)/x]) Sqrt[(-1+x)/x] x^2),Sqrt[1-Sqrt[1-Sqrt[(-1+x)/x]]],x]*)


(* ::Input:: *)
(*rewriteNestedRadicals[(I a x)/Sqrt[-a (-b+Sqrt[b^2+a x^2])],Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*rewriteNestedRadicals[(Sqrt[-b+(a x^2+Sqrt[b+a^2 x^4])^2]+Sqrt[b] ArcTan[Sqrt[-b+(a x^2+Sqrt[b+a^2 x^4])^2]/Sqrt[b]])/(2 Sqrt[2] Sqrt[a]),Sqrt[a x^2+Sqrt[a^2 x^4+b]],x]*)


(* ::Input:: *)
(*rewriteNestedRadicals[1/Sqrt[-1+Sqrt[1+x^2]]+ArcTan[Sqrt[-1+Sqrt[1+x^2]]/Sqrt[2]]/Sqrt[2],Sqrt[1+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*rewriteNestedRadicals[(Sqrt[a] Sqrt[-b+Sqrt[b^2+a x^2]])/(b-Sqrt[b^2+a x^2])+(Sqrt[2] Sqrt[a] ArcTan[(Sqrt[-b+Sqrt[b^2+a x^2]]-Sqrt[b+Sqrt[b^2+a x^2]])/(Sqrt[2] Sqrt[b])])/Sqrt[b],Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*rewriteNestedRadicals[(2 I ArcTanh[Sqrt[b-Sqrt[b^2+a x^2]]/Sqrt[b]])/(Sqrt[a] Sqrt[b]),Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Subsection::Closed:: *)
(*Integration by substitution*)


(* ::Input::Initialization::Plain:: *)
ClearAll[subst];

subst[integrand_, u_-> sub_, x_] := Module[
{y, eqns, usub, uintegrands, gooduintegrands, facuintegrands},

If[sub==x // TrueQ, 
Return[ {integrand /. u -> x, u -> x} ]];

eqns = {
Dt[y]==integrand Dt[x],
u==sub(* // PowerExpand *),
Dt[u==sub]//Together//Cancel(* // PowerExpand *)
};

eqns = eqns /. HoldPattern[Dt][Except[y|x|u]] -> 0; 

debugPrint2[eqns];

(*
eqns = TimeConstrained[
	Eliminate[eqns,{Dt[x],x}],(* Express Dt[y] in terms of Dt[u] and u. *)
	$timeConstraint, 
	$TimedOut];
*)

(* Using GroebnerBasis with the "Buchberger" method appears to be much 
faster for algebraic functions than Eliminate. *)

eqns = GroebnerBasis[eqns, {Dt[u],u}, {Dt[x],x}, 
	MonomialOrder -> EliminationOrder, Method -> "Buchberger"] // Factor;

debugPrint2[eqns];

eqns = PowerExpand[Factor[eqns]] //. Power[a_,n_Rational]Power[b_,n_Rational] :> a^IntegerPart[n] b^IntegerPart[n] Power[a b, FractionalPart[n]];

debugPrint2[eqns];

uintegrands=Quiet @ Solve[eqns[[1]] == 0,{Dt[y]}];
debugPrint2[uintegrands];

If[MatchQ[uintegrands, {}|{{}}|_Solve],
	Return[ False ]];

uintegrands = uintegrands /. Dt[u] -> 1;

uintegrands = PowerExpand[Factor[uintegrands]] //. Power[a_,n_Rational]Power[b_,n_Rational] :> a^IntegerPart[n] b^IntegerPart[n] Power[a b, FractionalPart[n]];
debugPrint2[uintegrands];

(* Pick the correct substitution. *)
gooduintegrands = Cases[uintegrands, {_ -> intU_} /; (PossibleZeroQ[integrand - rewriteNestedRadicals[Cancel @ Together[intU D[sub, x] /. u -> sub], sub, x]]), 1, 1];

If[gooduintegrands==={},
gooduintegrands = Join[gooduintegrands,
Cases[uintegrands, {dd_ -> intU_} /; PossibleZeroQ[integrand - PowerExpand @ Cancel @ Together[PowerExpand[intU] D[sub, x] /. u -> sub]] :> {dd -> intU // PowerExpand}, 1, 1]
	]
];

If[gooduintegrands === {}, 
	Return[ False ]
];

(* Pick the real solution, where possible. *)
gooduintegrands = Join[gooduintegrands, Cases[gooduintegrands, s_ /;FreeQ[s // N, _Complex]]]; 
debugPrint2[gooduintegrands];

If[usub === {},
	If[! OptionValue[PowerExpand],
	subst[integrand, u -> sub, x, PowerExpand -> True], 
	False],
	gooduintegrands = Dt[y]/.gooduintegrands[[-1]] //FactorSquareFree;
{gooduintegrands, u -> sub}
]
]


(* ::Input:: *)
(*subst[Sqrt[b+Sqrt[b^2+a x^2]],u->Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*subst[Sqrt[1+Sqrt[1+x^2]],u->Sqrt[1+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*subst[Sqrt[1-Sqrt[1-Sqrt[1-1/x]]]/x^2,u->1/x,x]*)


(* ::Input:: *)
(*subst[x^5 Sqrt[x^6+1],u->x^6+1,x]*)


(* ::Input:: *)
(*subst[Sqrt[1+Sqrt[x]],u->Sqrt[x],x]*)


(* ::Input:: *)
(*subst[x Sqrt[1+Sqrt[x]],u->Sqrt[x],x]*)


(* ::Input:: *)
(*subst[Sqrt[Tan[x]],u->Sqrt[Tan[x]],x]*)


(* ::Input:: *)
(*subst[Sqrt[Tan[x]^2-2Tan[x]+2],u->Tan[x],x]*)


(* ::Input:: *)
(*subst[(x^2-1)/((1+x^2)Sqrt[1+x^4]),u->x+1/x,x]*)


(* ::Input:: *)
(*subst[1/((x^4+1) (x^4-1)^(1/4)),u->(x^4-1)/x^4,x]*)


(* ::Input:: *)
(*subst[1/((x^4+1) (x^4-1)^(1/4)),u->((x^4-1)/x^4)^(1/4),x]*)


(* ::Input:: *)
(*subst[1/((x^8+1) (x^8-1)^(1/8)),u->((x^8-1)/x^8)^(1/8),x]//Timing*)


(* ::Input:: *)
(*subst[((-1+x^2)^2 (x+x^3))/(Sqrt[1+x^4] (1-2 x^2+4 x^4-2 x^6+x^8)),u->(1-x^2)/Sqrt[1+x^4],x]*)


(* ::Input:: *)
(*subst[Sec[x]Tan[x]Sqrt[Sec[x]^2+1],u->Sec[x],x]//Timing*)


(* ::Input:: *)
(*subst[(x^3 Exp[ArcSin[x]])/Sqrt[1-x^2],u->ArcSin[x],x]//Timing*)


(* ::Subsection::Closed:: *)
(*Derivative divides pre-processing*)


linearPolynomialQ[p_, x_] := PolynomialQ[p, x] && Exponent[p,x] == 1


linearRationalPolynomialQ[p_, x_] := 
	linearPolynomialQ[Numerator[p], x] && linearPolynomialQ[Denominator[p], x]


(* Simple derivative-divides heuristic. *)
ClearAll[derivdivides];

Options[derivdivides] = {"SingleStepTimeConstraint" -> 0.25};

derivdivides[e_, x_, u_, OptionsPattern[]] := Module[
{candidates, diff, eu, ratio, y, sys, eus, subs1, subs2, 
	a, b, eqns, special1, special2, special3, special, subx},

(* Create a list of candidate substitutions. *)

candidates = Level[e,{0,\[Infinity]}];
candidates = If[MatchQ[#, a_ b_ /; FreeQ[a,x]], Last[#], #]& /@ candidates;
candidates = If[MatchQ[#, x^n_Rational /; n < 0], 1/#, #]& /@ candidates;
candidates = Flatten[ If[MatchQ[#, x^_Rational], {#, # /. x^n_Rational :> x^(1/Denominator[n])}, #]& /@ candidates ];
candidates = DeleteCases[candidates, x^n_Integer /; n < 0];
candidates = Cases[candidates, s_ /; !FreeQ[s,x]];
candidates = SortBy[DeleteDuplicates[candidates], LeafCount];
candidates = DeleteCases[candidates, x];
candidates = Select[candidates, LeafCount[#] < 2/3 LeafCount[e]&]; (* Only try _small_ substitution (relative to the integrand) *)

(* Computationally cheap checks first. *)

Do[
	subs1 = Table[sub^n -> u^n, {n, -16, 16}]; (* This is a hack, but speedy compared to Eliminate/Solve below. *)
	subs2 = Table[n sub -> n u, {n, -16, 16}];
	diff = D[sub,x];
	eu = e //. subs1;
	eu = eu //. subs2; 
	eu = Cancel[eu/diff] //. subs1 //. subs2;
	ratio = Cancel[Together[D[eu,x]]];
	If[(LeafCount[eu] < If[linearPolynomialQ[sub, x], 1.0, 1.25] LeafCount[e] || 
			nestedCount[eu, u] < nestedCount[e, x] || 
			(PolynomialQ[eu, u] && ! PolynomialQ[e, x]) || 
			(rationalQ[eu, u] && ! rationalQ[e, x])) && (FreeQ[eu, x] || PossibleZeroQ[ratio]),
		debugPrint2["derivdivides level 1 returned: ", {eu, u -> sub}];
		Return[{eu, u -> sub}, Module]
	],
{sub, candidates}];

(* Some special cases that we handle separately: u \[Rule] a x + b, u \[Rule] (a x + b)/(c x + d), 
	u \[Rule] (a x + b)^(1/n), u \[Rule] ((a x + b)/(c x + d))^(1/n). *)

special1 = Cases[candidates, p_ /; (linearPolynomialQ[p,x] || linearRationalPolynomialQ[p,x])];
special2 = Cases[candidates, p_^n_Rational /; (Numerator[n] == 1 && (linearPolynomialQ[p,x] || linearRationalPolynomialQ[p,x]))];
special3 = Cases[candidates, p_^n_Rational q_^m_Rational /; (Numerator[n] == 1 && n + m == 0 && linearPolynomialQ[p,x] && linearPolynomialQ[q,x])];
special  = Join[special3, special2, special1];

Do[
	subs1 = Table[sub^n -> u^n, {n, -16, 16}]; 
	subs2 = {}; (* Table[n sub -> n u, {n, -16, 16}]; *)
	diff = D[sub,x];
	eu = e //. subs1 //. subs2;
	subx = Solve[u == sub, x]; (* Inverse function for the candidate substitution. *)
	If[Length[subx] > 1, Continue[]]; (* Something went wrong. *)
	eu = eu //. subx[[1]];
	eu = (Cancel[eu/diff] //. subs1 //. subs2 //. subx[[1]]);
	ratio = Cancel[Together[D[eu,x]]];
	If[(LeafCount[eu] < LeafCount[e] || 
			nestedCount[eu, u] < nestedCount[e, x] || 
			(PolynomialQ[eu, u] && ! PolynomialQ[e, x]) || 
			(rationalQ[eu, u] && ! rationalQ[e, x])) && 
			(FreeQ[eu, x] || PossibleZeroQ[ratio]),
		debugPrint2["derivdivides level 2 returned: ", {eu, u -> sub}];
		Return[{eu, u -> sub}, Module]
	],
{sub, special}];

(* Try using Eliminate, with a tight time constraint. *)

Do[
	TimeConstrained[
		subs1 = Table[sub^n -> u^n, {n, -16, 16}]; 
		subs2 = Table[n sub -> n u, {n, -16, 16}];
		eu = e //. subs1 //. subs2;
		eqns = {Dt[y] == eu Dt[x], u == sub, Dt[u == sub]} /. HoldPattern[Dt][Except[y|x|u]] -> 0;
		(* sys = Eliminate[eqns, {x, Dt[x]}] /. HoldPattern[Unequal][_,_] -> True; *)
		sys = GroebnerBasis[eqns, {Dt[u],u}, {Dt[x],x}, MonomialOrder -> EliminationOrder, Method -> "Buchberger"] // Factor;
		If[sys === {}, 
			Continue[]];
		eus = Solve[sys[[1]] == 0, Dt[y]];
		eus = Factor[eus /. Dt[u] -> 1];
		eus = Cancel[(Dt[y] /. eus)];
		Do[
			If[(LeafCount[eu] < 1.25 LeafCount[e] || 
					nestedCount[eu, u] < nestedCount[e, x] || 
					(PolynomialQ[eu, u] && ! PolynomialQ[e, x]) || 
					(rationalQ[eu, u] && ! rationalQ[e, x])) && 
					PossibleZeroQ[Cancel[Together[e - (eu D[sub,x] /. u -> sub)]]],
				debugPrint2["derivdivides level 3 returned: ", {eu, u -> sub}];
				Return[{eu, u -> sub}, Module]
		],{eu, eus}],
		OptionValue["SingleStepTimeConstraint"]
	],
{sub, candidates}];

False
]


(* ::Input:: *)
(*derivdivides[x^2/((1-2x^3)^(1/3) (1+x^3)),x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[Sqrt[1+Sqrt[a x+b]]/(a x+b),x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[Sqrt[1+Sqrt[a x+b]]/(a^2 x^2-b^2),x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[x/(1-a x^2),x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[(Log[x]/x)/(1-a Log[x]^2),x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[((-27+x^3) (27+x^3)^(1/3))/(81 x),x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[1/(x (1-2 x^4)^(1/4) (1+x^4)),x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[Sqrt[x]/Sqrt[Sqrt[x]+1],x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[((-2+x^3) Sqrt[1-x^2+x^3])/(1+x^3)^2,x,u]//Timing(* This one should fail (quickly). *)*)


(* ::Input:: *)
(*derivdivides[(3-9 x^4+2 x^6)/(x (1+x^2)^2 (-1+2 x^2) Sqrt[(1-2 x^2)/(1+2 x^2)] (1+2 x^2)),x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[1/((C[2] x^2+C[3]) Sqrt[(C[4] x+C[5])/(C[6] x+C[7])]),x,u]//Timing(* Should fail. *)*)


(* ::Input:: *)
(*derivdivides[(x (-1+x^2))/((1+x^2)^3 Sqrt[1+4/3 (x/(x^2+1))^2+Sqrt[1+4/3 (x/(x^2+1))^2]]),x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[3/(8 Sqrt[Sqrt[u]+u]),u,t] //Timing*)


(* ::Input:: *)
(*derivdivides[Sqrt[1+Sqrt[a x+b]]/(a x-b),x,u] //Timing*)


(* ::Input:: *)
(*derivdivides[Sqrt[b - a x+Sqrt[a x+b]]/(a x-b),x,u] //Timing*)


(* ::Input:: *)
(*derivdivides[1/Sqrt[Sqrt[(a x+b)/(c x+d)]-(a x+b)/(c x+d)] (-b c+a d)/(d+c x)^2,x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[1/((a x+b)^2 Sqrt[1-Power[a x+b, (3)^-1]]) a/(3 (b+a x)^(2/3)),x,u]//Timing*)


(* ::Input:: *)
(*(* A nice example where we call derivdivides multiple times. *)*)
(*derivdivides[Sqrt[1+Sqrt[a x^2+b]]/(a x^2-b)/x,x,u]*)
(*derivdivides[%[[1]], u, t]*)


(* ::Input:: *)
(*derivdivides[Sqrt[1+Sqrt[a x+b]]/(a^2 x^2-b^2),x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[((Sqrt[-1+x+Sqrt[x-Log[x]]]+Sqrt[1+x+Sqrt[x-Log[x]]]) (-1+x+2 x Sqrt[x-Log[x]]))/(x Sqrt[-1+x+Sqrt[x-Log[x]]] Sqrt[1+x+Sqrt[x-Log[x]]] Sqrt[x-Log[x]]),x,u]//Timing*)


(* ::Input:: *)
(*derivdivides[(1+x^2)/(2 x^(5/2)),x,u]*)


(* ::Input:: *)
(*derivdivides[(Sqrt[x-Sqrt[Log[x]]]-Sqrt[1-x-Sqrt[x-Sqrt[Log[x]]]+Sqrt[Log[x]]]) (1-1/(2 x Sqrt[Log[x]])),x,u]*)


(* ::Input:: *)
(*derivdivides[(1/Sqrt[1-Sqrt[x Cos[x]-Sqrt[Sin[x]]]]-Sqrt[1-x Cos[x]-Sqrt[x Cos[x]-Sqrt[Sin[x]]]+Sqrt[Sin[x]]]) (Cos[x]-Cos[x]/(2 Sqrt[Sin[x]])-x Sin[x]),x,u]*)


(* ::Subsection::Closed:: *)
(*Multiple radicals*)


multipleRadicalsQ[e_, x_] := Length[Union[Cases[e, Power[r_, _Rational] /; ! FreeQ[r, x], {0, \[Infinity]}]]] > 1


(* ::Input:: *)
(*multipleRadicalsQ[((x^4+1) Sqrt[x^4-1]+(x^4-1) Sqrt[x^4+1])/(x^8+1),x]*)


ClearAll[integrateMultipleRadicals];

Options[integrateMultipleRadicals] = Options[solveAlgebraicIntegral];

integrateMultipleRadicals[0|0., x_, opts:OptionsPattern[]] := {0, 0, 0}

integrateMultipleRadicals[e_, x_, opts:OptionsPattern[]] := Module[
	{expanded, rationalPart, unintegratedPart, integratedPart, integrated},

	expanded = apartList[e, x];

	If[Length[expanded] == 1 || Apply[Or, multipleRadicalsQ[#, x]& /@ expanded], 
		Return[{0, e, 0}, Module]
	];
	
	rationalPart = 0;
	unintegratedPart = 0;
	integratedPart = 0;
		
	Do[
		If[rationalQ[term, x], 
			rationalPart += term,
			debugPrint2["Integrating ", term, " wrt ", x];
			integrated = solveAlgebraicIntegral[term, x, opts];
			debugPrint2["Recursive call to solveAlgebraicIntegral returned ", integrated];
			rationalPart += integrated[[1]];
			unintegratedPart += integrated[[2]];
			integratedPart += integrated[[3]]		
		],
	{term, expanded}];
	
	If[integratedPart === 0,
		unintegratedPart = e;
		integratedPart = 0;
		rationalPart = 0;
	];
	
	{rationalPart, unintegratedPart, postProcess[integratedPart // Expand, x]}
]


(* ::Subsection::Closed:: *)
(*End Package*)


End[];
EndPackage[];


(* ::Section:: *)
(*Examples, wish list, bugs/deficiencies, regression tests, ...*)


(* ::Input:: *)
(*$verboseLevel=0;*)
(*int=IntegrateAlgebraic;*)


(* ::Text:: *)
(*Examples from the paper (https://arxiv.org/abs/2004.04910): *)


(* ::Input:: *)
(*int[((-2+x^3) Sqrt[1-x^2+x^3])/(1+x^3)^2,x]*)
(*AlgebraicIntegrateHeuristic`Private`RationalSubstitution*)


(* ::Input:: *)
(*int[((x^4-1) Sqrt[x^4+1])/(x^8+1),x]*)


(* ::Text:: *)
(*My favourite examples thus far:*)


(* ::Input:: *)
(*int[((x^2-1) Sqrt[x^4+x^2+1])/((x^2+1) (x^4+x^3+x^2+x+1)),x]*)


(* ::Input:: *)
(*int[((x^4-1) Sqrt[1+x^4])/(1+3 x^2+x^4)^2,x]*)


(* ::Input:: *)
(*int[(-2+3 x^5)/((1+x^5) Sqrt[1+x^2+x^5]),x]*)


(* ::Input:: *)
(*int[(1+x^8)/((1-x^8) Sqrt[1-x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(x^6-x^4)^(1/6),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/((1+x)^2 Sqrt[1+x^4]),x]*)


(* ::Text:: *)
(*Two integrals with enormous solutions:*)


(* ::Input:: *)
(*int[(-2+x)/((x+2) (x^3+x^2-x-1)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x-1)/((-3+x) (x^3-x^2-x+1)^(1/3)),x]*)


(* ::Text:: *)
(*We now allow symbolic summation over the roots of a polynomial in the results.*)


(* ::Input:: *)
(*int[((x^8-1) (x^4-1)^(1/4))/(x^6 (x^8+1)),x]*)


(* ::Text:: *)
(*A difficult integral which was solved by Euler in 1777.*)


(* ::Input:: *)
(*int[(1-x^2)^2/((x^2+1) (x^4+6 x^2+1)^(3/4)),x,"Expansion"->True]*)


(* ::Text:: *)
(*Some integrals with parameters which cause Risch-Trager-Bronstein to hang can be handled.*)


(* ::Input:: *)
(*int[(2 a^2 x^4-b^2)/((a^2 x^8-b^2) (a^2 x^4-b^2)^(1/4)),x]*)


(* ::Text:: *)
(*An example which AXIOM and Maple can compute, but FriCAS claims is not elementary.*)


(* ::Input:: *)
(*int[((-1+3 x^4) Sqrt[1+x+2 x^4+x^5+x^8])/(x^2 (4+x+4 x^4)),x]*)


(* ::Text:: *)
(*An example which Maple (with RootOf conversion) cannot compute as the radicand factors.*)


(* ::Input:: *)
(*int[((x^3+1) Sqrt[x^6-x^3-2])/(x^4 (x^6-2 x^3-1)),x]*)


(* ::Text:: *)
(*An example that AXIOM (August, 2014) claims is not elementary.*)


(* ::Input:: *)
(*int[(x^12-1)/((x^12+1) Sqrt[x^4+1]),x]*)


(* ::Text:: *)
(*We can now handle some integrals with rational radicands. The following example is computed with the generalised Gunther substitution method:*)


(* ::Input:: *)
(*Timing[int[(t^2+1)/((t^4-m t^2+1) Sqrt[(2 t^2-t-2)/(t^2+t-1)]),t]]*)


(* ::Text:: *)
(*And this example is computed using the Laurent polynomial substitution code*)


(* ::Input:: *)
(*Timing[IntegrateAlgebraic[((1+3 x^4) (-a-b x+a x^4) ((-c-d x+c x^4)/(-e-f x+e x^4))^(1/3))/(x^2 (-a+b x+a x^4)),x]]*)


(* ::Text:: *)
(*We can now handle some integrals with multiple distinct radicals (providing we can split them up and integrate term-by-term). For example*)


(* ::Input:: *)
(*IntegrateAlgebraic[((x^4+1) Sqrt[x^4-1]+(x^4-1) Sqrt[x^4+1])/(x^8+1),x]*)


(* ::Subsection::Closed:: *)
(*current bugs and deficiencies*)


(* ::Text:: *)
(*Why does postProcess hang here? This should be fixed!*)


(* ::Input:: *)
(*Timing[postProcess[1/3 ArcTan[(1-x^2)/((1+x+x^2) (1-x^4)^(1/3))]+1/6 (-1+Sqrt[3]) ArcTan[Sqrt[3]-(2 (1-x^2))/((1+x+x^2) (1-x^4)^(1/3))]+1/6 (1+Sqrt[3]) ArcTan[Sqrt[3]+(2 (1-x^2))/((1+x+x^2) (1-x^4)^(1/3))]+1/6 Log[1+(1-x^2)^2/((1+x+x^2)^2 (1-x^4)^(2/3))]-1/12 (1+Sqrt[3]) Log[1+(1-x^2)^2/((1+x+x^2)^2 (1-x^4)^(2/3))-(Sqrt[3] (1-x^2))/((1+x+x^2) (1-x^4)^(1/3))]+1/12 (-1+Sqrt[3]) Log[1+(1-x^2)^2/((1+x+x^2)^2 (1-x^4)^(2/3))+(Sqrt[3] (1-x^2))/((1+x+x^2) (1-x^4)^(1/3))],x]]*)


(* ::Text:: *)
(*What on Earth has gone on here?*)


(* ::InheritFromParent:: *)
(*int[x Sqrt[1+Sqrt[2]+Sqrt[2] x+x^2],x]*)


(* ::Text:: *)
(*This returns an incorrect result and should be fixed asap! *)


(* ::Input:: *)
(*IntegrateAlgebraic[(b^2+a x)/((-b^2+a x) Sqrt[b+Sqrt[b^2+a x^2]]),x,"SingleStepTimeConstraint"->1.0]*)


(* ::Text:: *)
(*Something strange happening here:*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x) Sqrt[1-x^2+x^4]),x,"Expansion"->True]*)
(*IntegrateAlgebraic[(1+x)/((-1+x) Sqrt[1-x^2+x^4]),x]*)


(* ::Text:: *)
(*This should return the partial answer it found (if not the complete integral):*)


(* ::Input:: *)
(*int[(-1+x^3)^(1/3)/(1+x),x,"Expansion"->True]*)


(* ::Text:: *)
(*The integral below should work without Expand[]:*)


(* ::Input:: *)
(*int[(2 x-1)/((x+1) Sqrt[Expand[(x+1)^6-a^2 x^2]]),x]*)


(* ::Text:: *)
(*It should be easy enough to implement a general method for integrals like*)


(* ::Input:: *)
(*int[Sqrt[x]/(-2+x^2)^(3/4),x]*)
(*subst[Sqrt[x]/(-2+x^2)^(3/4),u->Sqrt[x],x]*)
(*int[First[%],u]/. Last[%]*)


(* ::Text:: *)
(*This solution to this integral is terrible. (Ok, its seemingly better now. 04032021)*)


(* ::Input:: *)
(*int[Sqrt[x^4-1]/(x^4+1),x]*)


(* ::Input:: *)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]\((\(-*)
(*\*FractionBox[\(1\), \(2\)]\)\ ArcTan[*)
(*\*FractionBox[\(x\ \((1 - *)
(*\*SuperscriptBox[\(x\), \(2\)])\)\), *)
(*SqrtBox[\(\(-1\) + *)
(*\*SuperscriptBox[\(x\), \(4\)]\)]]] - *)
(*\*FractionBox[\(1\), \(2\)]\ ArcTanh[*)
(*\*FractionBox[\(x\ \((1 + *)
(*\*SuperscriptBox[\(x\), \(2\)])\)\), *)
(*SqrtBox[\(\(-1\) + *)
(*\*SuperscriptBox[\(x\), \(4\)]\)]]])\)\)-Sqrt[x^4-1]/(x^4+1)]*)


(* ::Text:: *)
(*Is there anything we can do for these integrals?*)


(* ::Input:: *)
(*int[x/((4-x^3) Sqrt[1-x^3]),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[-((2 2^(1/3)-3 x^2+2^(1/3) x^3)/(6 2^(2/3) Sqrt[1-x^3] (-4+6 2^(1/3) x-3 2^(2/3) x^2+x^3))),x]+int[(2 2^(1/3)+3 x^2+2^(1/3) x^3)/(6 2^(2/3) x (2 2^(1/3)+2^(2/3) x+x^2) Sqrt[1-x^3]),x]+int[x^2/(2 2^(2/3) Sqrt[1-x^3] (-4+x^3)),x]+int[-(1/(6 2^(2/3) x Sqrt[1-x^3])),x]*)
(*FullSimplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)]*)


(* ::Input:: *)
(*int[1/((x+1) (x^3+2)^(1/3)),x,"Expansion"->True]*)


(* ::Input:: *)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]\(( *)
(*\*FractionBox[\(1\), \(12\)]\ \((2\ *)
(*\*SqrtBox[\(3\)]\ ArcTan[\(( *)
(*\*SqrtBox[\(3\)]\ *)
(*\*SuperscriptBox[\((2 + *)
(*\*SuperscriptBox[\(x\), \(3\)])\), \(2/3\)]\ \((\(-1072244631963565627440642667696\) - 1764382450892402509391037276448\ x - 642339750020464731448133545632\ *)
(*\*SuperscriptBox[\(x\), \(2\)] + 190053406517364372745124029472\ *)
(*\*SuperscriptBox[\(x\), \(3\)] + 98966744593197647869364591874\ *)
(*\*SuperscriptBox[\(x\), \(4\)])\) + *)
(*\*SqrtBox[\(3\)]\ *)
(*\*SuperscriptBox[\((2 + *)
(*\*SuperscriptBox[\(x\), \(3\)])\), \(1/3\)]\ \((1190118508012558386973005239952 + 2230842809300000322439227290544\ x + 735314591615271415729365586328\ *)
(*\*SuperscriptBox[\(x\), \(2\)] - 726175722499147186465445363320\ *)
(*\*SuperscriptBox[\(x\), \(3\)] - 453545129950193664973324584892\ *)
(*\*SuperscriptBox[\(x\), \(4\)] - 45228634350310035870300951616\ *)
(*\*SuperscriptBox[\(x\), \(5\)])\) + *)
(*\*SqrtBox[\(3\)]\ \((\(-47674000995597211057816884304\) + 351260598258508240019971964880\ x + 888876515195959220955879945824\ *)
(*\*SuperscriptBox[\(x\), \(2\)] + 889426563183087468015580290048\ *)
(*\*SuperscriptBox[\(x\), \(3\)] + 673924074224408772959625384792\ *)
(*\*SuperscriptBox[\(x\), \(4\)] + 382151535711085278859235047618\ *)
(*\*SuperscriptBox[\(x\), \(5\)] + 93292570833559435663132301885\ *)
(*\*SuperscriptBox[\(x\), \(6\)])\))\)/\((4664445860470002276943457906640 + 7625406903034897531937916271008\ x + 1085003586721431086608600126056\ *)
(*\*SuperscriptBox[\(x\), \(2\)] - 2686291575945300326054363894472\ *)
(*\*SuperscriptBox[\(x\), \(3\)] + 46796858328175763683008212928\ *)
(*\*SuperscriptBox[\(x\), \(4\)] + 1013240117509374668590043803350\ *)
(*\*SuperscriptBox[\(x\), \(5\)] + 236716304443694165237125394649\ *)
(*\*SuperscriptBox[\(x\), \(6\)])\)] + Log[\((\(-140\) - 192\ x + 24\ *)
(*\*SuperscriptBox[\(x\), \(2\)] + 44\ *)
(*\*SuperscriptBox[\(x\), \(3\)] - 48\ *)
(*\*SuperscriptBox[\(x\), \(4\)] + 6\ *)
(*\*SuperscriptBox[\(x\), \(5\)] + 22\ *)
(*\*SuperscriptBox[\(x\), \(6\)] + *)
(*\*SuperscriptBox[\((2 + *)
(*\*SuperscriptBox[\(x\), \(3\)])\), \(2/3\)]\ \((12 - 60\ x - 96\ *)
(*\*SuperscriptBox[\(x\), \(2\)] - 6\ *)
(*\*SuperscriptBox[\(x\), \(3\)] + 21\ *)
(*\*SuperscriptBox[\(x\), \(4\)])\) + *)
(*\*SuperscriptBox[\((2 + *)
(*\*SuperscriptBox[\(x\), \(3\)])\), \(1/3\)]\ \((96 + 228\ x + 102\ *)
(*\*SuperscriptBox[\(x\), \(2\)] - 48\ *)
(*\*SuperscriptBox[\(x\), \(3\)] + 21\ *)
(*\*SuperscriptBox[\(x\), \(5\)])\))\)/\((1 + 6\ x + 15\ *)
(*\*SuperscriptBox[\(x\), \(2\)] + 20\ *)
(*\*SuperscriptBox[\(x\), \(3\)] + 15\ *)
(*\*SuperscriptBox[\(x\), \(4\)] + 6\ *)
(*\*SuperscriptBox[\(x\), \(5\)] + *)
(*\*SuperscriptBox[\(x\), \(6\)])\)])\))\)\)-1/((x+1) (x^3+2)^(1/3))]*)


(* ::Subsection::Closed:: *)
(*wish list*)


(* ::Text:: *)
(*We would like to do this with the substitution u == (x^2 - 1)/x, then the integral is reduced to 1/((u^2 + 1)*((2*u - 1)/(u + 1))^(1/2))*)


(* ::Input:: *)
(*int[(1+x^2)/(Sqrt[(-2-x+2 x^2)/(-1+x+x^2)] (1-x^2+x^4)),x]*)


(* ::Text:: *)
(*Euler's substitution for this integral gives a result that is much larger than necessary:*)


(* ::Input:: *)
(*int[Sqrt[2+u^2]/(Sqrt[3] (3-u+u^2)),u]*)


(* ::Input:: *)
(*int[Sqrt[(Sqrt[x^4+1]-x^2)/(x^4+1)],x]*)


(* ::Input:: *)
(*int[1/((1+x) (-2+3 x-2 x^2+3 x^3-2 x^4)^(3/2)),x]*)


(* ::Input:: *)
(*int[1/((1+x) (-2+2 x+x^2-x^4)^(3/2)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (-1-x^2+x^4+x^6)^(1/3))/x,x]*)


(* ::Input:: *)
(*int[(1+2 x+x^2)^(1/3)/(3+x^2),x]*)


(* ::Input:: *)
(*int[(1+2 x+x^2)^(1/3)/(4+x+x^2+x^3),x]*)


(* ::Input:: *)
(*int[(1-2 x+x^2)^(1/3)/(2+x^2),x]*)


(* ::Input:: *)
(*int[Sqrt[b+a x]/Sqrt[a b x+Sqrt[b+a x]],x]*)


(* ::Input:: *)
(*int[x/((x^3+1) Sqrt[4 x^3+1]),x]*)


(* ::Input:: *)
(*int[(1-x^3)^(1/3)/(1+x),x]*)


(* ::Input:: *)
(*int[(1-x^3)^(1/3)/(1-x+x^2),x]*)


(* ::Input:: *)
(*int[x/((3 x^2+2 Sqrt[2]-3) Sqrt[x^3-x]),x]*)


(* ::Input:: *)
(*int[x/((3 x^2-2 Sqrt[2]-3) Sqrt[x^3-x]),x]*)


(* ::Input:: *)
(*int[x/((3 x^2+2 Sqrt[2]+3) Sqrt[x^3-x]),x]*)


(* ::Input:: *)
(*int[x/((3 x^2-2 Sqrt[2]+3) Sqrt[x^3-x]),x]*)


(* ::Input:: *)
(*int[x/((x^2+2 Sqrt[2]-3) Sqrt[x^3-x]),x]*)


(* ::Input:: *)
(*int[x/((x^2-2 Sqrt[2]-3) Sqrt[x^3-x]),x]*)


(* ::Input:: *)
(*int[(1+x)/((1+x+x^2) (a+b x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((1-x^2) Sqrt[1-x^4-x^8]),x]*)


(* ::Input:: *)
(*int[(-2+x)/((-1+x^2) (x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-5+x^4)/((x+x^3)^(1/4) (-1+x^4)),x]*)


(* ::Input:: *)
(*int[((-2+x) (-x^2+x^4)^(1/4))/(-1+2 x^2),x]*)


(* ::Text:: *)
(*This one requires the rational substitution u == (1+2 x^2)/(-1+2 x)*)


(* ::Input:: *)
(*int[((-1-2 x+2 x^2) Sqrt[x+x^4])/(-1+2 x)^3,x]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]\(( *)
(*\*FractionBox[\(\(( *)
(*\*FractionBox[\(1\), \(4\)] + *)
(*\*FractionBox[*)
(*SuperscriptBox[\(x\), \(2\)], \(2\)])\)\ *)
(*\*SqrtBox[\(x + *)
(*\*SuperscriptBox[\(x\), \(4\)]\)]\), *)
(*SuperscriptBox[\((\(-1\) + 2\ x)\), \(2\)]] + *)
(*\*FractionBox[\(1\), \(16\)]\ Log[\(-1\) - 2\ *)
(*\*SuperscriptBox[\(x\), \(2\)] + 2\ *)
(*\*SqrtBox[\(x\ \((1 + *)
(*\*SuperscriptBox[\(x\), \(3\)])\)\)]] - *)
(*\*FractionBox[\(1\), \(16\)]\ Log[1 + 2\ *)
(*\*SuperscriptBox[\(x\), \(2\)] + 2\ *)
(*\*SqrtBox[\(x + *)
(*\*SuperscriptBox[\(x\), \(4\)]\)]])\)\)-((-1-2 x+2 x^2) Sqrt[x+x^4])/(-1+2 x)^3]*)


(* ::Subsection:: *)
(*previously bugs, deficiencies or edge cases*)


(* ::Input:: *)
(*int[1/Sqrt[1-Sqrt[x]]-Sqrt[1-Sqrt[x]-x],x]*)
(*D[%,x]-(1/Sqrt[1-Sqrt[x]]-Sqrt[1-Sqrt[x]-x])//Together*)


(* ::Input:: *)
(*int[(-1+x^2) (-1+x Sqrt[-1+3 x^2-x^4]),x]*)
(*D[%,x]-(-1+x^2) (-1+x Sqrt[-1+3 x^2-x^4])//Simplify*)


(* ::Input:: *)
(*IntegrateAlgebraic[((x^4+1) Sqrt[x^4-1]+(x^4-1) Sqrt[x^4+1])/(x^8+1),x]*)


(* ::Input:: *)
(*int[u (-3+2 u^2) Sqrt[(1-2 u^2)/(4+3 u^2)],u]*)


(* ::Input:: *)
(*int[1/Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x-1)/(x (x^3+2 x^2+2 x+1)^(1/3)),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(a x^2-2 b)/((a x^2+c x^4-b) (a x^2-b)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x) Sqrt[1-x^2+x^4]),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[1/(2 Sqrt[x]+Sqrt[x+1])^2,x]*)


(* ::Input:: *)
(*int[(x^4 (x^3+x^4)^(1/4))/(1+x),x]*)


(* ::Input:: *)
(*int[(x^2 (x^3+x^4)^(1/4))/(-1+x),x]*)


(* ::Input:: *)
(*int[((x^4+x-1) (x^4-x^3)^(1/4))/(x+1),x]*)


(* ::Input:: *)
(*int[((x^2-x) (x^4-x^3)^(1/4))/(x^2-x-1),x]*)


(* ::Input:: *)
(*int[((2 x^2+x-1) (x^4-x^3)^(1/4))/(x^2-x-1),x]*)


(* ::Input:: *)
(*int[1/((1+x) (1-x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1+x)/((1+4 x+x^2) (1-x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[Sqrt[1-Sqrt[1-Sqrt[1-1/x]]]/x,x,"SingleStepTimeConstraint"->2]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)-Sqrt[1-Sqrt[1-Sqrt[1-1/x]]]/x]*)


(* ::Input:: *)
(*int[Sqrt[1-Sqrt[1-Sqrt[1-1/x]]]/x^2,x,"SingleStepTimeConstraint"->2]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)-Sqrt[1-Sqrt[1-Sqrt[1-1/x]]]/x^2]*)


(* ::Input:: *)
(*int[Sqrt[1-Sqrt[1-Sqrt[1-1/x^2]]]/x,x,"SingleStepTimeConstraint"->2]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)-Sqrt[1-Sqrt[1-Sqrt[1-1/x^2]]]/x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[1-Sqrt[1+1/x^2]]]/x,x,"SingleStepTimeConstraint"->2]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)-Sqrt[1+Sqrt[1-Sqrt[1+1/x^2]]]/x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[x^3+x^4]),x]*)


(* ::Input:: *)
(*int[x/Sqrt[x^3+x^4],x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+2 x^2+x^4])/((1+x^2) (1+x^4)),x]*)


(* ::Input:: *)
(*int[((2 x^2+3) Sqrt[2 x^3+x])/(2 x^2+1)^2,x]*)


(* ::Input:: *)
(*int[((2 x^5+3) Sqrt[-x^6-2 x^4+x])/(x^5-1)^2,x]*)


(* ::Input:: *)
(*int[((x^4+3) Sqrt[x-x^5])/(x^8-x^6-2 x^4+1),x]*)


(* ::Input:: *)
(*int[((1+x) (x^3+x^5)^(1/4))/(x (-1+x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (-x^3+x^4)^(1/4))/(-1+x+2 x^2),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (x^2+x^6)^(1/4))/(x^2 (1+x^2)),x]*)


(* ::Input:: *)
(*int[(-1+x^8)/((x^2+x^6)^(1/4) (1+x^8)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/((1+x^2+x^4) (x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) (x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+2 x+2 x^2) (-x^2+x^4)^(1/4))/(-1+2 x^2),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[1/((-1+x) (x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[((x+2 x^2) (x^2+x^4)^(1/4))/(1+2 x^2),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[((1+2 x+2 x^2) (x^2+x^4)^(1/4))/(1+2 x^2),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[1/((-2+x) (-x^2+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (1+x^4))/(1+x^2+x^4)^(5/2),x]*)


(* ::Input:: *)
(*int[(-1+7 x^8)/((1+x^8) Sqrt[3-x+x^2+6 x^8-x^9+3 x^16]),x]*)


(* ::Input:: *)
(*int[(x-1)/(x (1+x^4)^(1/4)),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[(x-1)/((1+x) Sqrt[x+x^2+x^3]),x]*)


(* ::Input:: *)
(*int[x/Sqrt[-71-96 x+10 x^2+x^4],x]*)


(* ::Input:: *)
(*int[x/Sqrt[1+4 x+3 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(1+3 x)/Sqrt[-1-4 x-5 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(x^2-x)/Sqrt[-2 x+4 x^2-2 x^3+x^4-2 x^5+x^6],x]*)


(* ::Input:: *)
(*int[(-1-2 x+3 x^2)/Sqrt[-3-2 x-x^2+4 x^3-x^4-2 x^5+x^6],x]*)


(* ::Input:: *)
(*int[(x Sqrt[-x^2+x^4])/(-3+2 x^2),x]*)


(* ::Input:: *)
(*int[((-1-x^4)^(1/4) (-1+x^4))/(x^6 (1+2 x^4)),x]*)


(* ::Input:: *)
(*int[(x (2-x^3+x^8)^(1/3) (-6+5 x^8))/(4+x^6+4 x^8+x^16),x]*)


(* ::Input:: *)
(*int[((x+2 x^3)^(1/3) (-1+x^4))/(x^4 (2-x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (-x+2 x^3)^(1/3))/(x^2 (1+x^4)),x]*)


(* ::Input:: *)
(*int[((-3+x^2) (-1+x^2)^(2/3))/(-1+2 x^2-x^4+x^6),x]*)


(* ::Input:: *)
(*int[((1+2 x^2) (x+2 x^3)^(1/3))/(x^4 (1+2 x^4)),x]*)


(* ::Input:: *)
(*int[1/(x^3+x^2)^(1/3),x]*)


(* ::Input:: *)
(*int[(x^3+x^2)^(1/3),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[x+x^3])/((1+x^2) (1+x+x^2)^2),x]*)


(* ::Input:: *)
(*int[((-x+x^3)^(1/3) (-2+x^4))/(x^4 (1+x^2)),x]*)


(* ::Input:: *)
(*int[((-1+2 x) (2-x+x^2) Sqrt[-2+x^2-2 x^3+x^4])/(3-2 x+2 x^2),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-2 x^8] (-1+2 x^8) (1+2 x^8))/(x^7 (-1+x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^8) Sqrt[1+x^8])/x^7,x]*)


(* ::Input:: *)
(*int[((1+x^8) Sqrt[-1-2 x^4+x^8])/x^7,x]*)


(* ::Input:: *)
(*int[((4+x^5) Sqrt[1-2 x^5+x^8+x^10])/x^9,x]*)


(* ::Input:: *)
(*int[(-1+4 x^3)/Sqrt[-1-2 x+2 x^4],x]*)


(* ::Input:: *)
(*int[((x^2-1) Sqrt[x^4+1])/(x^2 (x^2+1)),x]*)


(* ::Input:: *)
(*int[((x^8+2) Sqrt[x^16-2 x^8+4])/x^9,x]*)


(* ::Input:: *)
(*int[x (1+2 x^2) Sqrt[-1+2 x^2+2 x^4],x]*)


(* ::Input:: *)
(*int[x^5 Sqrt[1-2 x^3],x]*)


(* ::Input:: *)
(*int[((1+2 x^4) Sqrt[1+2 x^8])/x,x]*)


(* ::Input:: *)
(*int[(3 x^2+1)/Sqrt[x^3+x-1],x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^6] (1+2 x^6))/(x^2 (-1-x^2+x^6)),x]*)


(* ::Input:: *)
(*int[((x^2-1) Sqrt[x^4+1])/(x^2 (x^2+1)),x]*)


(* ::Input:: *)
(*int[(x^2 (-2+x^3) Sqrt[1+x^3])/(4+12 x^3+13 x^6+4 x^9),x]*)


(* ::Input:: *)
(*int[(x^2 (-2+x^3) Sqrt[1+x^3])/(1+3 x^3+x^9),x]*)


(* ::Input:: *)
(*int[((-2+x^3) (1+x^3)^(3/2))/x^6,x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+x^4])/x^3,x]*)


(* ::Input:: *)
(*int[((-2+x^3) Sqrt[1+x^3] (2-x^2+2 x^3))/(x^4 (1-3 x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(1+x^4)/((1-x^4) Sqrt[-1+x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(x^2-1)/((x^2+1) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[((x^2-1) (x^2+1) Sqrt[x^4+1])/x^4,x]*)


(* ::Input:: *)
(*int[((x^2-1) (x^2+1) Sqrt[x^4+1])/x^4,x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^3] (2+x^3))/(x^2 (-2-4 x^2+2 x^3)),x]*)


(* ::Input:: *)
(*int[((4 x^3+1) (2 x^4+2 x+1))/Sqrt[x^4+x],x]*)


(* ::Input:: *)
(*int[((x^2+1) Sqrt[1-2 x^4])/x^5,x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+x^4])/x^5,x]*)


(* ::Input:: *)
(*int[((-2+3 x^5) Sqrt[1+x^5])/(1+x^4+2 x^5+x^10),x]*)


(* ::Subsection::Closed:: *)
(*Examples from documentation*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x^2-1)/((x^2+1) Sqrt[x^4+1]),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[((x^3-1) (x^3+x^2)^(2/3))/(x^6 (x^3+x^2)),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x^3-x^2)^(1/3)/x,x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(3 x+1)/Sqrt[x^4-2 x^3-5 x^2-4 x-1],x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(1-x^3 (4 x+5)^(2/3)-x^3 (4 x+5)^(1/3))/(1-x Sqrt[4 x+5]),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[1/((1+x) Sqrt[4 x-3]+(4+3 x) Sqrt[2 x-1]),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(((x-1)/(2 x+1))^(1/4)-3 ((x-1)/(2 x+1))^(3/4))/((x-1) (x+1)^2 (2 x-1)),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(Sqrt[x^2-4 x+1]+(x^2-4 x+1)^(3/2))/(Sqrt[x^2-4 x+1]-(x^2-4 x+1)^(5/2)+(x^2-4 x+1)^(3/2)),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[((x^3-2) Sqrt[1-x^2+x^3])/(x^3+1)^2,x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(3 x+2)/((9 x^2+52 x-12) (3 x^2+4)^(1/3)),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[((-3+2 x+2 x^5) Sqrt[x-x^2+x^6])/(1-2 x+x^2-x^3+x^4+2 x^5-3 x^6-x^8+x^10),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x^3-x^2)^(1/3),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[((-1+x^8) (-1+x^4)^(1/4))/(x^6 (1+x^8)),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[((2 c-a x^3) Sqrt[a x^3+b x^2+c])/((a x^3+(b-3) x^2+c) (a x^3+(b-2) x^2+c)),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[Sqrt[x-Sqrt[x^2-1]]/x^2,x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(6 x^8+1)/((2 x^8-1) (4 x^16+2 x^10-4 x^8-x^4-x^2+1)^(1/4)),x,"Elementary"->False,VerifySolutions->False]*)


(* ::Input:: *)
(*IntegrateAlgebraic[x/Sqrt[-71-96 x+10 x^2+x^4],x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(29 x^2+18 x-3)/Sqrt[x^6+4 x^5+6 x^4-12 x^3+33 x^2-16 x],x,"DegreeBound"->30]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(39 x^2+9 x-1)/Sqrt[x^6+4 x^4+10 x^3+4 x^2-4 x+1],x,"DegreeBound"->40]*)


(* ::Input:: *)
(*IntegrateAlgebraic[((x^4-1) Sqrt[1+x^4])/(1+3 x^2+x^4)^2,x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x^2-x+1)/((x^2-1) Sqrt[x^3+x]),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(2 x^2+x+2)/((2 x-1) Sqrt[x^4+x]),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(1-x^2)^2/((x^2+1) (x^4+6 x^2+1)^(3/4)),x,"Expansion"->True]*)


(* ::Input:: *)
(*IntegrateAlgebraic[1/((x+1) (x^4+6 x^2+1)^(1/4)),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x^6-x^3+1)/((x^6-1) (x^4+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(30 x^3-24 x^2-16)/((9 x^10-18 x^9+9 x^8-24 x^7+24 x^6+16 x^4+24) Sqrt[3 x^3-3 x^2-4]),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x^4-1)/((x^4+1) (x^9-x^7)^(1/8)),x,"MaxNumeratorDegree"->9,"MaxDenominatorDegree"->9]*)


(* ::Subsection::Closed:: *)
(*Examples from github documentation*)


(* ::Input:: *)
(*int[Sqrt[x^4+6 x^2+1]/(x (x^2+1)),x]*)


(* ::Input:: *)
(*int[Sqrt[x^4+6 x^2+1]/((x-1) (x+1)^3),x]*)


(* ::Input:: *)
(*int[1/((x+1) (x^4+6 x^2+1)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1-2 x+3 x^2)/Sqrt[-3-2 x-x^2+4 x^3-x^4-2 x^5+x^6],x]*)


(* ::Input:: *)
(*int[(x^2-x)/Sqrt[-2 x+4 x^2-2 x^3+x^4-2 x^5+x^6],x]*)


(* ::Input:: *)
(*int[(1+3 x)/Sqrt[-1-4 x-5 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[x/Sqrt[1+4 x+3 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[x/Sqrt[-71-96 x+10 x^2+x^4],x]*)


(* ::Input:: *)
(*int[((2+x-x^3-x^4)^(2/3) (6+2 x+x^4) (-2-x+x^3+x^4))/(x^6 (-2-x+2 x^3+x^4)),x]*)
(*AlgebraicIntegrateHeuristic`Private`RationalSubstitution*)


(* ::Input:: *)
(*int[((-1+x^3-x^5-2 x^7)^(2/3) (1-x^3+x^5+2 x^7) (-3+2 x^5+8 x^7))/x^9,x]*)
(*AlgebraicIntegrateHeuristic`Private`RationalSubstitution*)


(* ::Input:: *)
(*int[((1-x^3)^(2/3) (-1+4 x^3))/(x^6 (-2+3 x^3)),x]*)
(*AlgebraicIntegrateHeuristic`Private`RationalSubstitution*)


(* ::Input:: *)
(*int[((x^2-1) Sqrt[x^4+x^2+1])/((x^2+1) (x^4+x^3+x^2+x+1)),x]*)
(*AlgebraicIntegrateHeuristic`Private`RationalSubstitution*)


(* ::Input:: *)
(*int[(1-x^2)^2/((x^2+1) (x^4+6 x^2+1)^(3/4)),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[((3+2 x) (1+x+x^3)^(1/3))/(x^2 (1+x)),x]*)


(* ::Input:: *)
(*int[((-1+x^4-x^5)^(1/4) (-4+x^5))/x^6,x]*)


(* ::Input:: *)
(*int[1/(x^4+1)^(1/4),x]*)


(* ::Input:: *)
(*int[(1+x^6)/((1-x^6) Sqrt[1-x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((1-x^2) Sqrt[1+x^4]),x]*)


(* ::Subsection::Closed:: *)
(*regression testing*)


(* ::Input:: *)
(*int[(Sqrt[x-Sqrt[Log[x]]]-Sqrt[1-x-Sqrt[x-Sqrt[Log[x]]]+Sqrt[Log[x]]]) (1-1/(2 x Sqrt[Log[x]])),x]//Timing*)


(* ::Input:: *)
(*int[((Sqrt[-1+x+Sqrt[x-Log[x]]]+Sqrt[1+x+Sqrt[x-Log[x]]]) (-1+x+2 x Sqrt[x-Log[x]]))/(x Sqrt[-1+x+Sqrt[x-Log[x]]] Sqrt[1+x+Sqrt[x-Log[x]]] Sqrt[x-Log[x]]),x]//Timing*)


(* ::Input:: *)
(*int[1/ Sqrt[1+4/3 (x/(x^2+1))^2+Sqrt[1+4/3 (x/(x^2+1))^2]] (x (-1+x^2))/(1+x^2)^3,x]//Timing*)


(* ::Input:: *)
(*int[((x^2 C[3]-C[4]) (x+3 x^2 C[3]+3 C[4]))/(x (-x^2+x^4 C[3]^2+2 x^2 C[3] C[4]+C[4]^2)Sqrt[(x C[0]+x^2 C[3]+C[4])/(x C[1]+x^2 C[3]+C[4])]),x]//Timing*)


(* ::Input:: *)
(*int[(x (x^2 C[3]-C[4]))/((x+3 x^2 C[3]+3 C[4]) (-x^2+x^4 C[3]^2+2 x^2 C[3] C[4]+C[4]^2)Sqrt[(x C[0]+x^2 C[3]+C[4])/(x C[1]+x^2 C[3]+C[4])]),x]//Timing*)


(* ::Input:: *)
(*int[((x^2 C[3]-C[4]) (x+3 x^2 C[3]+3 C[4]))/((-x^3+x^6 C[3]^3+3 x^4 C[3]^2 C[4]+3 x^2 C[3] C[4]^2+C[4]^3)Sqrt[(x C[0]+x^2 C[3]+C[4])/(x C[1]+x^2 C[3]+C[4])]),x,"SingleStepTimeConstraint"->2]//Timing*)


(* ::Input:: *)
(*int[(x^5 (x^2 C[3]+3 C[4]))/((x^3+3 x^2 C[3]+3 C[4]) (x^6-x^4 C[3]^2-2 x^2 C[3] C[4]-C[4]^2)Sqrt[(x^3 C[0]+x^2 C[3]+C[4])/(x^3 C[1]+x^2 C[3]+C[4])]),x,"SingleStepTimeConstraint"->10]//Timing(* This one takes over 30 seconds. *)*)


(* ::Input:: *)
(*int[(x^3 (x^3 C[3]-2 C[4]) Sqrt[(x^2 C[0]+x^3 C[3]+C[4])/(x^2 C[1]+x^3 C[3]+C[4])])/((x^2+2 x^3 C[3]+2 C[4]) (-x^4+x^6 C[3]^2+2 x^3 C[3] C[4]+C[4]^2)),x]//Timing*)


(* ::Input:: *)
(*int[(x^5 (2 x^5 C[3]-3 C[4]) Sqrt[(x^3 C[0]+x^5 C[3]+C[4])/(x^3 C[1]+x^5 C[3]+C[4])])/((x^3+2 x^5 C[3]+2 C[4]) (-x^6+x^10 C[3]^2+2 x^5 C[3] C[4]+C[4]^2)),x,"SingleStepTimeConstraint"->2.5]//Timing*)


(* ::Input:: *)
(*int[(x^3 (x^3 C[3]-2 C[4]))/((x^2+2 x^3 C[3]+2 C[4]) (-x^4+x^6 C[3]^2+2 x^3 C[3] C[4]+C[4]^2)((x^2 C[0]+x^3 C[3]+C[4])/(x^2 C[1]+x^3 C[3]+C[4]))^(1/3)),x,"SingleStepTimeConstraint"->2.5]//Timing*)


(* ::Input:: *)
(*int[(x (2 x^3 C[3]-C[4]))/((-x+x^3 C[3]+C[4]) (x^2+x^4 C[3]+x^6 C[3]^2+x C[4]+2 x^3 C[3] C[4]+C[4]^2)((x C[0]+x^3 C[3]+C[4])/(x C[1]+x^3 C[3]+C[4]))^(1/3)),x,"SingleStepTimeConstraint"->2.5]//Timing*)


(* ::Input:: *)
(*Timing[int[(x^2 (x^2 C[3]-C[4]) ((x C[0]+x^2 C[3]+C[4])/(x C[1]+x^2 C[3]+C[4]))^(1/4))/((-x+x^2 C[3]+C[4]) (x+x^2 C[3]+C[4]) (x^2+x^4 C[3]^2+2 x^2 C[3] C[4]+C[4]^2)),x]]*)


(* ::Input:: *)
(*int[((-3+x^2) (1-2 x^2+x^4+x^6))/(x^10 ((-a+a x^2+b x^3)/(-c+c x^2+d x^3))^(1/4)),x]*)


(* ::Input:: *)
(*int[((-4+3 x) ((-a+a x+b x^4)/(-c+c x+d x^4))^(1/4))/((-1+x) x),x]*)


(* ::Input:: *)
(*int[((-4+3 x) ((-c+c x+d x^4)/(-e+e x+f x^4))^(1/4))/(x (a-a x+b x^4)),x]*)


(* ::Input:: *)
(*int[((-4+3 x) (-a+a x+b x^4) ((-c+c x+d x^4)/(-e+e x+f x^4))^(1/4))/(x^5 (a-a x+b x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (-a-b x+a x^2))/(x^2 (-c+d x+c x^2) ((-1+x^2-x C[0])/(-1+x^2-x C[1]))^(1/3)),x]*)


(* ::Input:: *)
(*Timing[IntegrateAlgebraic[((2+x) (-1-x+x^2) ((1+x-2 x^2)/(1+x+4 x^2))^(1/4))/(2 x (1+2 x+x^2+x^4)),x]]*)


(* ::Input:: *)
(*Timing[IntegrateAlgebraic[((-4-3 x+2 x^2) (1+x-x^2+x^4) ((1+x-x^2+2 x^4)/(1+x-x^2+3 x^4))^(1/3))/(x^5 (-1-x+x^2+x^4)),x]]*)


(* ::Input:: *)
(*Timing[int[((-2+x) (1-x+x^2))/(x^3 (-1+x+x^2) ((1-x+2 x^2)/(1-x+3 x^2))^(1/3)),x]]*)


(* ::Input:: *)
(*Timing[IntegrateAlgebraic[((1+x^2) (1-3 x^2+x^4))/(x^2 (1-x-3 x^2+x^3+x^4) Sqrt[(-2+x+2 x^2)/(-1+x+x^2)]),x]]*)


(* ::Input:: *)
(*int[(-36-6 x^2+6 x^3+x^6)/(x (-6+x^3) (36-90 x+122 x^2-96 x^3+51 x^4-26 x^5+15 x^6-6 x^7+x^8) ((6+x^3)/(-6+x^3))^(1/6)),x]*)


(* ::Input:: *)
(*int[(-4-2 x+2 x^2+x^4)/(x (-2+x^2) (8-10 x+4 x^2+4 x^3-4 x^4+x^5) ((2+x^2)/(-2+x^2))^(1/4)),x]*)


(* ::Input:: *)
(*int[(3-8 x+8 x^2-12 x^4)/(x (1+2 x^2) (3-7 x+7 x^2-6 x^3+2 x^4) ((1-2 x^2)/(1+2 x^2))^(1/3)),x]*)


(* ::Input:: *)
(*int[((-1+x) (-3+8 x-8 x^2+12 x^4))/(x (1+2 x^2) (3-7 x+7 x^2-6 x^3+2 x^4) ((1-2 x^2)/(1+2 x^2))^(2/3)),x]*)


(* ::Input:: *)
(*int[((-1+x)^2 (-10-8 x+5 x^2+5 x^3))/((-2+x^2) (-3+7 x-11 x^2+4 x^3+4 x^4-4 x^5+x^6) ((1+x)/(-2+x^2))^(3/4)),x]*)


(* ::Input:: *)
(*int[(2+16 x-x^2-9 x^3)/((-2+x^2) (-3+2 x+7 x^2-7 x^3-9 x^4+9 x^5+5 x^6-5 x^7-x^8+x^9) ((1+x)/(-2+x^2))^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+x)^2 (x-2 x^2+2 x^3))/((-1+2 x) (-2+4 x+3 x^2-4 x^3+2 x^4) Sqrt[(1-2 x)/(1+2 x^2)]),x]*)


(* ::Input:: *)
(*int[(-1+4 x-4 x^2+4 x^4)/((1+2 x^2) (-1-4 x+12 x^2-8 x^3+4 x^4) Sqrt[(1-2 x^2)/(1+2 x^2)]),x]*)


(* ::Input:: *)
(*int[((-1+2 x^2) (-1+4 x-4 x^2+4 x^4))/((1+2 x^2) (-1-8 x+32 x^2-40 x^3+46 x^4-64 x^5+56 x^6-32 x^7+8 x^8) Sqrt[(1-2 x^2)/(1+2 x^2)]),x]*)


(* ::Input:: *)
(*int[(3-9 x^4+2 x^6)/(x (1+x^2)^2 (-1+2 x^2) (1+2 x^2) Sqrt[(1-2 x^2)/(1+2 x^2)]),x]*)


(* ::Input:: *)
(*int[((2+x)^2 (-19+66 x-30 x^2+9 x^3)^(1/3))/((-3+2 x)^2 (-5+6 x-6 x^2+x^3)),x]*)


(* ::Input:: *)
(*int[1/((-1+x) (-2 x^2-3 x^3+x^4)^(3/2)),x]*)


(* ::Input:: *)
(*int[(x^2-x+1)/((x^2-1) Sqrt[x^4-x^3+x^2-x+1]),x]*)


(* ::Input:: *)
(*int[(x^2-1)/((x^2+1) Sqrt[x^4-x^3-x^2-x+1]),x]*)


(* ::Input:: *)
(*int[x/(x^3+x^2-x-1)^(1/3),x]*)


(* ::Input:: *)
(*int[(2+x)/((x-1) Sqrt[x^3+3 x-1]),x]*)


(* ::Input:: *)
(*int[x/Sqrt[x^4+2 x^3-3 x^2+3 x-3],x]*)


(* ::Input:: *)
(*int[(-1+x)/(x^3-x^2-x+1)^(1/3),x]*)


(* ::Input:: *)
(*int[(x+2)/((x-3) (x^2+1) (1-x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((a x^2-2 b) (a x^2-b)^(1/4)),x]*)


(* ::Input:: *)
(*int[(a+b x)/((2-x^2) (x^2-1)^(1/4)),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[1/((x^2-3) (1-3 x^2)^(1/3)),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[(x+1)/((x^2-3) (x^2+1)^(1/3)),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[(x+1)/((x^2-3) (x^2+1)^(1/3)),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[(k^2 x^3+(k^2-2) x)/((x^4+(d k^2-2) x^2-d+1) ((1-x^2) (1-k^2 x^2))^(1/3)),x]*)


(* ::Input:: *)
(*int[((d+1) x^2-2 d a x+d a^2)/((a-x)^2 ((1-e) x+a e) (x^2 (x-a))^(1/3)),x]*)


(* ::Input:: *)
(*int[((d+1) x-a)/(((e-1) x^2+2 a x-a^2) (x^2 (x-a))^(1/3)),x]*)


(* ::Input:: *)
(*int[(x-a)/(((d-1) x^2-2 d a x+d a^2) (x^2 (x-a))^(1/3)),x]*)


(* ::Input:: *)
(*int[((x+a-2 b) (x-b))/((x^4-4 a x^3+(6 a^2-d) x^2-2 (2 a^3-b d) x+a^4-b^2 d) ((x-a) (x-b))^(1/3)),x]*)


(* ::Input:: *)
(*int[(a-2 b+x)/(((-a+x) (-b+x))^(1/3) (b+a^2 d+(-1-2 a d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(-Sqrt[a b]+x)/(Sqrt[x (a+x) (b+x)] (Sqrt[a b]+x)),x]*)


(* ::Input:: *)
(*int[(x (-b+x) (a b-2 a x+x^2))/((-a+x) Sqrt[x (-a+x) (-b+x)] (a d+(-b-d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(a^2 b-a (2 a+b) x+3 a x^2-x^3)/(x (-b+x) Sqrt[x (-a+x) (-b+x)] (a+(-1-b d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(-a^2 b+a (2 a+b) x-3 a x^2+x^3)/(Sqrt[x (-a+x) (-b+x)] (-a^2+2 a x+(-1+b^2 d) x^2-2 b d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(a b x-x^3)/(Sqrt[x (-a+x) (-b+x)] (a^2 b^2 d-2 a b (a+b) d x+(-1+a^2 d+4 a b d+b^2 d) x^2-2 (a+b) d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x) (-a b+x^2))/(Sqrt[x (-a+x) (-b+x)] (a^2 b^2 d-2 a b (a+b) d x+(-1+a^2 d+4 a b d+b^2 d) x^2-2 (a+b) d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(-a^2 b+a (2 a+b) x-3 a x^2+x^3)/(Sqrt[x (-a+x) (-b+x)] (-a^2-a (-2+b e) x+(-1+b^2 d+a e+b e) x^2+(-2 b d-e) x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/Sqrt[x+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[(1+x^2)^(3/2) Sqrt[x+Sqrt[1+x^2]] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^2] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]],x]*)


(* ::Input:: *)
(*int[(1+x^4)^2/((-1+x^4)^2 Sqrt[x^2+Sqrt[1+x^4]]),x,"SingleStepTimeConstraint"->2]*)


(* ::Input:: *)
(*int[(-1+x^2)^2/((1+x^2)^2 Sqrt[x^2+Sqrt[1+x^4]]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)^2/((1+x^2)^2 Sqrt[1+x^4] Sqrt[x^2+Sqrt[1+x^4]]),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/((1+x)^2 Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/((1+x) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/(1+x^2),x]*)


(* ::Input:: *)
(*int[x^2 Sqrt[a x^2+Sqrt[b+a^2 x^4]],x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]],x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[1/((-1+x^2) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[1/((d+c x) Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[(a x+Sqrt[b^2+a^2 x^2])/(b+Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[1+Sqrt[1+x^2]])/(-1+x^4),x]*)


(* ::Input:: *)
(*int[((b^2+a x^2)^2 Sqrt[b+Sqrt[b^2+a x^2]])/(-b^2+a x^2)^2,x,"SingleStepTimeConstraint"->2]*)


(* ::Input:: *)
(*int[((-b^2+a x^2) Sqrt[b+Sqrt[b^2+a x^2]])/(b^2+a x^2),x,"SingleStepTimeConstraint"->2]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/(b^2+a x^2)^4,x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/x^4,x]*)


(* ::Input:: *)
(*int[Sqrt[c+d Sqrt[b+a x^2]]/x,x]*)


(* ::Input:: *)
(*int[(b^2+a x^2) Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[(x+2)/((x-1) Sqrt[x^3-a x^2+3 x-1]),x]*)


(* ::Input:: *)
(*int[(x+2)/((x-1) Sqrt[x^3+a x^2+3 x-1]),x]*)


(* ::Input:: *)
(*int[(x-2)/((x+1) Sqrt[x^3+a x^2+3 x+1]),x]*)


(* ::Input:: *)
(*int[(x-2)/((x+1) Sqrt[x^3-a x^2+3 x+1]),x]*)


(* ::Input:: *)
(*int[1/Sqrt[x^8-x^2],x]*)


(* ::Input:: *)
(*int[(3-x^2)/((1-x^2) (1-6 x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^2)^2/((1-x^2) (1-6 x^2+x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[(x^2+1)/((1+x) Sqrt[x^4+x^2+1]),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[1/((x^2-3) (1-3 x^2)^(1/3)),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[1/(x (2-3 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-1+x) (2-2 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-5+x)/((-2-x+x^2)^(1/3) (-3+4 x+x^2)),x]*)


(* ::Input:: *)
(*int[(6+2 x+x^2)/((2+x) (2+x^2) (2+x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(6+2 x+x^2)/((1+x) (2+x+x^2)^(1/3) (2-x+2 x^2)),x]*)


(* ::Input:: *)
(*int[(k x+1)/((k x-1) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(k x-1)/((k x+1) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[x/((k^2 x^2-1) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(k^2 x^2-1)/((k^2 x^2+1) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(k^2 x^2+1)/((k^2 x^2-1) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(k^2 x^2+b x+1)/((k^2 x^2-1) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(k^2 x^2-1)/((a k^2 x^2+b x+a) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(k^2 x^2-2 k^2 x+1)/(((a k^2+b k^2) x^2-b x-a) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(x^2-x)/((k^2 x^2-2 x+1) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(k^3 x^3-1)/((k^3 x^3+1) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(k^4 x^4-1)/((k^4 x^4+1) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[1/(x (3 x^2-6 x+4)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1-2 x+k^2 x^2)/((-1+2 x-2 x^2+k^2 x^2) Sqrt[x-x^2-k^2 x^2+k^2 x^3]),x]*)


(* ::Input:: *)
(*int[(k x^2-1)/((a k x+b) (b x+a) Sqrt[x (1-x) (1-k x)]),x]*)


(* ::Input:: *)
(*int[1/(x ((x-1) (x^2-2 q x+q))^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x ((x+1) (x^2+2 q x+q))^(1/3)),x]*)


(* ::Input:: *)
(*int[(a+b x)/((2-x^2) (x^2-1)^(1/4)),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[1/(Sqrt[x^2+1]+2 x)^2,x]*)


(* ::Input:: *)
(*int[1/(Sqrt[x^2-1] (3 x^2-4)^2),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2-1]/(x-I)^2,x]*)


(* ::Input:: *)
(*int[1/(Sqrt[x-1] (Sqrt[x-1]+2 Sqrt[x])^2),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[1/(Sqrt[x^2-1] (Sqrt[x^2-1]+Sqrt[x])^2),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[((x-1)^(3/2)+(x+1)^(3/2))/((x+1)^(3/2) (x-1)^(3/2)),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[(x+3)/((x-1)^2 (x^2-1)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x+1)/((x+3) (2 x+1) (x^2+1)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x+1)/((x-1) (2 x+1) (3 x^2-1)^(1/3)),x]*)


(* ::Input:: *)
(*int[Sqrt[1-x]/(8 (1+x)^(7/2)),x]*)


(* ::Input:: *)
(*int[(3 x+2)/((9 x^2+52 x-12) (3 x^2+4)^(1/3)),x]*)


(* ::Input:: *)
(*int[(2+x-x^3)/(Sqrt[1+x+x^3] (1+x-x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(1-2 x)/Sqrt[5+5 x-4 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(1+2 x)/Sqrt[-4-3 x-2 x^2+2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[1/Sqrt[3-5 x+x^2+x^3],x]*)


(* ::Input:: *)
(*int[(-1+2 x)/Sqrt[-4-4 x+5 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[1/Sqrt[3+4 x+x^4],x]*)


(* ::Input:: *)
(*int[(1+x^3-(1+x^4)^(1/4)+x^3 (1+x^4)^(1/4))/((-1+x^3) Sqrt[1+x^4]),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[(1-x)/Sqrt[3+2 x-5 x^2-4 x^3+x^4+2 x^5+x^6],x]*)


(* ::Input:: *)
(*int[1/((x+1) (x^4+6 x^2+1)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x (x^6+1)^(1/3)),x]*)


(* ::Input:: *)
(*int[x/(1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[((1+x+x^2) (2 x+x^2) Sqrt[1+2 x+x^2-x^4])/(1+x)^4,x]*)


(* ::Input:: *)
(*int[(1+x^6)/((1-x^6) Sqrt[x+x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(x^2+1)/((x^2-1) (2 x^2+1)^(3/2)),x]*)


(* ::Input:: *)
(*int[1/(x^4-x^2)^(1/4),x]*)


(* ::Input:: *)
(*int[1/(x (x^3+1)^(1/3)),x]*)


(* ::Input:: *)
(*int[((x^4-1) (x^4+1)^(1/4))/x,x]*)


(* ::Input:: *)
(*int[((x^4-1) (x^4+1)^(1/4))/x^2,x]*)


(* ::Input:: *)
(*int[1/((x^4-1)^(1/4) (3 x^4+1)),x]*)


(* ::Input:: *)
(*int[1/(x (x^4-1)^(1/4)),x]*)


(* ::Input:: *)
(*int[(2 x^8-2 x^4+1)/((x^4-1)^(1/4) (2 x^8-x^4-2)),x]*)


(* ::Input:: *)
(*int[((x-1) (x^4+x^3)^(1/4))/(x (x+1)),x]*)


(* ::Input:: *)
(*int[((x^2+1) (x^4+x^3)^(1/4))/(x^2 (x^2-1)),x]*)


(* ::Input:: *)
(*int[(x^2+1)/((x^2-1) (x^5-x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x^8 (x^4+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x^4 (x^4+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x^3+1)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x (x^6+1)^(1/3)),x]*)


(* ::Input:: *)
(*int[x/(x^3+x^2)^(1/3),x]*)


(* ::Input:: *)
(*int[(x^3+x^2)^(1/3)/x,x]*)


(* ::Input:: *)
(*int[(x^2+2)/((x^2+1) (x^3-x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^3 (x^3+1) (x^3-x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[((x^3+1) (x^3-x^2)^(1/3))/(x^6+1),x]*)


(* ::Input:: *)
(*int[(x^3 (x^3-x^2)^(1/3))/(x^6+1),x]*)


(* ::Input:: *)
(*int[x^3/((x^3-x^2)^(1/3) (x^6-1)),x]*)


(* ::Input:: *)
(*int[(x^2-1)/((x^2+1) (x^4+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[((x^2-1) (x^4+x^2)^(1/3))/x^3,x]*)


(* ::Input:: *)
(*int[(x^2-1)/((x^2+x+1) (x^4+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x+1)/((x-1) (x^4+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x^6-1)/((x^4+x^2)^(1/3) (x^6+1)),x]*)


(* ::Input:: *)
(*int[(x^3+1)/((x^3-1) (x^4+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x^2+1)/((x^2-1) (x^4+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x^6+1)/((x^4+x^2)^(1/3) (x^6-1)),x]*)


(* ::Input:: *)
(*int[(x^6-x^3+1)/((x^4+x^2)^(1/3) (x^6-1)),x]*)


(* ::Input:: *)
(*int[(x^6+x^3+1)/((x^4+x^2)^(1/3) (x^6-1)),x]*)


(* ::Input:: *)
(*int[(x^2+1)/((x^2+x-1) (x^4-x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x^2-1)/((x^2+1) (x^4-x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x^6+1)/((x^4-x^2)^(1/3) (x^6-1)),x]*)


(* ::Input:: *)
(*int[1/((x^3+1) (x^4-x)^(1/4)),x]*)


(* ::Input:: *)
(*int[((x^2+1) (x^4+x^3)^(1/4))/(x^2 (x^2-1)),x]*)


(* ::Input:: *)
(*int[1/(x^4+1)^(1/4),x]*)


(* ::Input:: *)
(*int[((6+x^4) Sqrt[-2 x+x^4+x^5])/((-2+x^4) (-2-x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-1+3 x^4) Sqrt[1+x^2+2 x^4+x^8])/((1-x+x^4)^2 (1+x+x^4)),x]*)


(* ::Input:: *)
(*int[((2+x^4) (-2-x^2+x^4) Sqrt[-2-2 x^2+x^4])/(-2+x^4)^3,x]*)


(* ::Input:: *)
(*int[((-1-x-x^2+x^4) (2+x+2 x^4))/(Sqrt[-1-x+x^4] (4+8 x+3 x^2-x^3-7 x^4-8 x^5+x^6+4 x^8)),x]*)


(* ::Input:: *)
(*int[((-1+2 x^4) (1-x^2+2 x^4)^(3/2))/((1+2 x^4) (1+2 x^4+4 x^8)),x]*)


(* ::Input:: *)
(*int[((1+x^3) Sqrt[x-x^4])/(2+4 x^3+3 x^6),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (1-x^2+x^4) Sqrt[4-x^2+4 x^4])/((1+x^4) (4+7 x^4+4 x^8)),x]*)


(* ::Input:: *)
(*int[(Sqrt[2-x^3-x^4] (4+x^3+2 x^4))/((-2-3 x^2+x^3+x^4) (-2-x^2+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-1+6 x^4) Sqrt[x+2 x^5])/((1+2 x^4) (1-x^2+4 x^4+4 x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (1+x^4) Sqrt[-1-x^2+x^4])/((-2-x^2+2 x^4)^2 (-2+x^2+2 x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^4] (-1+x^4))/((1+x^4) (1-x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (1+x^2)^3 Sqrt[1+2 x^2+x^4])/((1+x^4) (1-x^2+x^4-x^6+x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[-1-4 x-5 x^2-4 x^3-x^4])/((1+x+x^2) (1+3 x+x^2)^2),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^4] (1+x^4))/(-4-3 x^2+4 x^4)^2,x]*)


(* ::Input:: *)
(*int[((8+x^3) Sqrt[-4+3 x^2+x^3])/((-4+x^3) (-4+2 x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-3+2 x^4) Sqrt[x+2 x^4+2 x^5])/(1+x^3+2 x^4)^2,x]*)


(* ::Input:: *)
(*int[((3+x) Sqrt[-2 x-x^2+x^4])/(2+x+x^3)^2,x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1-x^2+x^4])/((1+x^2) (1+x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^4) (-1+x^2+x^4)^(3/2))/((-1+x^4) (1+x^2-x^4-x^6+x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[x+x^3])/(1+x+x^2)^3,x]*)


(* ::Input:: *)
(*int[(-2+3 x^5)/((1+x^5) Sqrt[1+x^2+x^5]),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (1+x^4))/(1+x^2+x^4)^(5/2),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2+x^3-x^4] (1+x^2-x^3+x^4) (-2-x^3+2 x^4))/((1-x^3+x^4) (1+x^2-2 x^3+x^4-x^5+2 x^6-2 x^7+x^8)),x]*)


(* ::Input:: *)
(*int[((-2+x^3) (1-x^2+x^3))/((1+x^3)^2 Sqrt[1+x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+2 x^2-x^4] (-1+x^4) (1+x^4))/((-1-x^2+x^4) (1+3 x^2-x^4-3 x^6+x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^2+x^4])/(1+x^4+x^8),x]*)


(* ::Input:: *)
(*int[((2+x^3) Sqrt[-1+x^2+x^3])/(-1+x^3)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2-x^3] (-2+x^3))/((1+x^3) (1-x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^4])/(1-x^2+x^4)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^4] (1+x^4))/((-1+x^4) (-1-x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((3+x^4) Sqrt[x+x^4-x^5])/((-1+x^4) (-1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^6) Sqrt[-x+x^4+x^7])/(-1+x^6)^2,x]*)


(* ::Input:: *)
(*int[((-3+x^4+3 x^6) Sqrt[x+x^5+x^7])/((1+x^4+x^6) (1-x^3+x^4+x^6)),x]*)


(* ::Input:: *)
(*int[((3+2 x^5) Sqrt[-x+2 x^4+x^6])/((-1+x^5) (-1+x^3+x^5)),x]*)


(* ::Input:: *)
(*int[((-3+2 x+2 x^5) Sqrt[x-x^2+x^6])/(1-2 x+x^2-x^3+x^4+2 x^5-3 x^6-x^8+x^10),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[-1-x^2-x^4])/((1+x^2) (1+x^4)),x]*)


(* ::Input:: *)
(*int[((1+2 x^3)^(4/3) (1+3 x^3))/(x^8 (1+4 x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^6) (-1+2 x^6) (-1+x^4+2 x^6)^(5/4))/(x^10 (-1-x^4+2 x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^4-x^5)^(1/4) (-4+x^5))/x^6,x]*)


(* ::Input:: *)
(*int[((2+3 x^4)^(1/4) (4+6 x^4+x^8))/(x^6 (1+x^4) (1+2 x^4)),x]*)


(* ::Input:: *)
(*int[((2+x^4-2 x^8)^(1/4) (1+x^8))/(x^2 (-1+x^8)),x]*)


(* ::Input:: *)
(*int[(-1+2 x^4)^(1/4)/(x^6 (-1+x^4)),x]*)


(* ::Input:: *)
(*int[((-1-x^6)^(1/4) (-2+x^6) (1+x^6))/x^10,x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(1/4) (-1+x^8))/(x^6 (1+x^8)),x]*)


(* ::Input:: *)
(*int[((1+x^6) Sqrt[-x-x^4+x^7])/(1+2 x^3-2 x^9+x^12),x]*)


(* ::Input:: *)
(*int[((-1+x^6) Sqrt[x+x^4+x^7])/(1-x^3+x^6-x^9+x^12),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^3] (2+x^3))/((-1+x^3) (-1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[-1-x^2+x^4])/(-1+x^4)^2,x]*)


(* ::Input:: *)
(*int[((1+2 x^4) Sqrt[2-x^2-4 x^4])/((-1+2 x^4) (-1-x^2+2 x^4)),x]*)


(* ::Input:: *)
(*int[((-2+x^3) Sqrt[-1+2 x^2-x^3])/((1+x^3) (1-x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^4] (1+x^4))/(4-7 x^4+4 x^8),x]*)


(* ::Input:: *)
(*int[((-2+x^4) Sqrt[2+x^4])/(4+3 x^4+x^8),x]*)


(* ::Input:: *)
(*int[((1-x+x^4) Sqrt[-1+2 x-2 x^2+x^4])/((-1+2 x+x^4) (-1+2 x-x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[2+2 x^2+x^5] (-4+3 x^5))/((2+x^5) (2-x^2+x^5)),x]*)


(* ::Input:: *)
(*int[((-2+x^3) Sqrt[1-2 x^2+x^3])/(1+x^2+x^3)^2,x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[-1-x^2+x^4])/(-1+x^2+x^4)^2,x]*)


(* ::Input:: *)
(*int[(x Sqrt[-x^2+x^4])/(-3+2 x^2),x]*)


(* ::Input:: *)
(*int[(1+x^6)/((1-x^6) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+x^4])/(1+x^2)^3,x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+x^4])/((1+x^2) (1+x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+x^4])/((1+x^2)^2 (1-x+x^2)),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+x^4])/((1-x+x^2) (1+x+x^2)^2),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (1+x+x^2) Sqrt[1+3 x^2+x^4])/(1+x+x^2+x^3+x^4)^2,x]*)


(* ::Input:: *)
(*int[x/((-1+x^4)^(3/2) (1+x^4)),x]*)


(* ::Input:: *)
(*int[x/((1+x^4) Sqrt[1+2 x^4]),x]*)


(* ::Input:: *)
(*int[x/(Sqrt[1+x^4] (1+3 x^4+3 x^8)),x]*)


(* ::Input:: *)
(*int[((2+x^3) Sqrt[-1-x^2+x^3])/(1-x^2-2 x^3-x^4+x^5+x^6),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^3] (2+x^3))/(1-2 x^3-x^4+x^6),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1-x^2-x^3] (-2+x^3))/((1+x^3) (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^3] (2+x^3))/(-1+x^3)^2,x]*)


(* ::Input:: *)
(*int[((-2+x^3) Sqrt[1-x^2+x^3])/(1+x^3)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2-x^3] (2+x^3))/(-1+x^3)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^3] (-2+x^3))/((1+x^3) (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^3] (2+x^3))/((-1+x^3) (-1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((2+x^3) Sqrt[-1-x^2+x^3])/((-1+x^3) (-1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^3] (-2+x^3))/((1+x^3) (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^4] (1+x^4))/(1-x^4+x^8),x]*)


(* ::Input:: *)
(*int[((x^4-1) (1+x^2+3 x^4+x^6+x^8) Sqrt[1+x^2+x^4])/((1+x^4)^3 (1-x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1-x^2-x^4] (-1+x^4))/(1+x^4+x^8),x]*)


(* ::Input:: *)
(*int[((1+x^4) (-1-x^2+x^4))/(Sqrt[-1+x^4] (1+x^2-x^4-x^6+x^8)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2-x^4] (-1+x^4) (1+x^2+x^4))/(1+x^4)^3,x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2-x^4] (-1+x^4) (1-x^2+x^4-x^6+x^8))/(1+x^4)^4,x]*)


(* ::Input:: *)
(*int[(Sqrt[2-x^4] (2+x^4))/(-2+x^4)^2,x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[-1+2 x^2+x^4])/((-1+x^4) (-1+x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[-1-3 x^2+x^4])/(-1-x^2+x^4)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[2+x^2-x^4] (2+x^4))/((-2-2 x^2+x^4) (-2-x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^4])/(1+x^4+x^8),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+x^4])/((1+x^2) (1+x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^4] (1+x^4) (1-2 x^2+2 x^6+x^8))/((-1+x^4) (-1+x^2+x^4)^3),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^4])/(1+x^2+3 x^4+x^6+x^8),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^4] (-1+x^4) (1+x^2+3 x^4+x^6+x^8))/((1+x^2+x^4)^2 (1+3 x^2+5 x^4+3 x^6+x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^2)^3 (1+x^2))/(1-x^2+x^4)^(5/2),x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[-2-x^2+2 x^4])/((-1+x^4) (-2+x^2+2 x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^4] (-1+x^4))/(1+x^8),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^4] (-1+x^4))/(1+3 x^2+x^4)^2,x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[-2-x^2+2 x^4])/((-1+x^4) (-2+x^2+2 x^4)),x]*)


(* ::Input:: *)
(*int[((-2+x^4) Sqrt[2+x^4])/((2+x^2+x^4) (2+2 x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-3 x^2-2 x^4] (1+2 x^4))/((-1+x^2+2 x^4) (-1+2 x^2+2 x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2+x^5] (2+3 x^5))/(-1+x^5)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^5] (-2+3 x^5))/(1+x^4+2 x^5+x^10),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^5] (2+3 x^5))/(-1-x^2+x^5)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[-1-2 x^2+x^5] (2+3 x^5))/(-1-x^2+x^5)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^2-x^5] (2+3 x^5))/((-1+x^5) (-1+x^2+x^5)),x]*)


(* ::Input:: *)
(*int[(-2+3 x^5)/(Sqrt[1+x^5] (1-x^2+x^5)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2-x^5] (-2+3 x^5))/((1-2 x^2+x^5) (1-x^2+x^5)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^6] (-1+2 x^6))/(1-x^2+x^6)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^2+x^6] (-1+2 x^6))/(1+x^6)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^6] (1+2 x^6))/(1-x^4-2 x^6+x^12),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+2 x^2+x^6] (1+2 x^6))/(-1+x^2+x^6)^2,x]*)


(* ::Input:: *)
(*int[(-1+2 x^6)/(Sqrt[1+x^6] (1-x^2+x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^2+x^6] (-1+2 x^6))/((1+x^6) (1+x^2+x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^6] (1+2 x^6))/(1+x^4-2 x^6+x^12),x]*)


(* ::Input:: *)
(*int[(Sqrt[-2+x^2-2 x^3+2 x^4] (2-x^3+2 x^4))/((-1-x^3+x^4) (-2-x^2-2 x^3+2 x^4)),x]*)


(* ::Input:: *)
(*int[((-4+x^3) Sqrt[2-x^2+x^3])/((2+x^3) (2+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-2+x^3) Sqrt[2+x^2+2 x^3])/((1+x^3) (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^2+x^4])/((1+x^4) (2+x^2+2 x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[2+2 x^2-x^3] (4+x^3))/((-2+x^3) (-2-2 x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-2 x^3-x^4] (1+x^3+x^4))/((-1+2 x^3+x^4) (-1+x^2+2 x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2-x^3+x^5] (2-x^3+3 x^5))/(1+2 x^3+x^4-2 x^5+x^6-2 x^8+x^10),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^5] (-2+3 x^5))/(1+x^5)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^5] (-2+3 x^5))/(1+x^4+2 x^5+x^10),x]*)


(* ::Input:: *)
(*int[((2+x^3) Sqrt[-1-x^2+x^3])/(1-2 x^3-x^4+x^6),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2+x^5] (2+3 x^5))/(1+x^4-2 x^5+x^10),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^2-x^5] (2+3 x^5))/((-1+x^5) (-1+x^2+x^5)),x]*)


(* ::Input:: *)
(*int[(-2+3 x^5)/(Sqrt[1+x^5] (1-x^2+x^5)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1-x^2+x^6] (-1+x^2+x^6) (1+2 x^6))/(-1+x^6)^3,x]*)


(* ::Input:: *)
(*int[((-1-x^2+x^6)^(3/2) (1+2 x^6))/(-1+x^6)^3,x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2-x^6] (-1+2 x^6))/((1+x^6) (1-x^2+x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^3-x^6] (-2-x^3+4 x^6))/((1-x^3+x^6) (1+x^2-x^3+x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2-x^3-x^6] (-1-x^2+x^3+x^6) (2+x^3+4 x^6))/(-1+x^3+x^6)^3,x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^6] (1+2 x^6) (1+x^2-x^4-2 x^6-x^8+x^12))/((-1+x^6) (-1+2 x^6-3 x^12+x^18)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2-x^3-x^6] (2+x^3+4 x^6))/(1-2 x^3-x^4-x^6+2 x^9+x^12),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2+x^4+x^5] (2+2 x^4+3 x^5))/(-1+x^4+x^5)^2,x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1-x^2+x^4])/((1+x^4) (1+x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1-x^2+x^5] (2+3 x^5))/(1-x^4-2 x^5+x^10),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^5] (-2+3 x^5))/((1+x^5) (1-x^2+x^5)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^2+x^5] (-2+3 x^5))/((1+x^5) (1+x^2+x^5)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^2+x^4+x^5] (-2+2 x^4+3 x^5))/((1+x^4+x^5) (1+x^2+x^4+x^5)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^4])/(1+x^4+x^8),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^4])/(1+3 x^4+x^8),x]*)


(* ::Input:: *)
(*int[((1-x^2+2 x^4) Sqrt[-1+x^2+x^4+x^6])/((-1+x^2) (1+x^2) (-1+x^4+x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[-1+x^2+x^4])/(-1+x^4)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^6] (-1+2 x^6))/((1+x^6) (1+x^2+x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2+x^4+x^6] (1+x^4+2 x^6))/(-1+x^4+x^6)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2+x^4+x^6] (1+x^4+2 x^6))/(1-x^4-2 x^6+x^8+2 x^10+x^12),x]*)


(* ::Input:: *)
(*int[((1-x^2+2 x^4) Sqrt[1-x^2-x^4-x^6])/((-1+x^2) (1+x^2) (-1+x^4+x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[-1-2 x^2+x^4])/(-1-x^2+x^4)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^4] (x^4-1))/(1+x^4+x^8),x]*)


(* ::Input:: *)
(*int[((-2+x^3) Sqrt[1-x^2+x^3])/((1+x^3) (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2+x^3+x^4] (2+x^3+2 x^4))/((-1+x^3+x^4) (-1-x^2+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+2 x^2-x^3-x^4] (2+x^3+2 x^4))/((-1-2 x^2+x^3+x^4) (-1-x^2+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-2+x^2) (2-2 x+x^2) Sqrt[4+2 x+2 x^2+x^3+x^4])/x^4,x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+3 x^2+x^4])/(x^2 (1+x^2)),x]*)


(* ::Input:: *)
(*int[(-1+2 x^2)/((1+2 x^2) Sqrt[x+x^2+2 x^3]),x]*)


(* ::Input:: *)
(*int[((2+x^2) Sqrt[4-5 x^2+x^4])/(x^2 (-2+2 x+x^2)),x]*)


(* ::Input:: *)
(*int[((2+x^2) (-4+x+2 x^2) Sqrt[8-7 x^2+2 x^4])/x^4,x]*)


(* ::Input:: *)
(*int[((-1+x^2) (1+x^2) Sqrt[1+3 x^2+x^4])/(x^2 (1+x+x^2)^2),x]*)


(* ::Input:: *)
(*int[((2+x^2) (-2-2 x+x^2) Sqrt[4-3 x^2+x^4])/(x^2 (-2+x^2) (-4+x+2 x^2)),x]*)


(* ::Input:: *)
(*int[((1+x^3) Sqrt[-2-x^3+x^6])/(x^4 (-1-2 x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-6+x^4) (2+x^4) Sqrt[-4-2 x^3-4 x^4+2 x^6-x^7-x^8])/x^10,x]*)


(* ::Input:: *)
(*int[((-4+x^3) (2+x^3) (2-2 x^2+x^3))/(x^6 Sqrt[2+x^2+x^3]),x]*)


(* ::Input:: *)
(*int[((1+x^3) (-1-x^2+2 x^3) Sqrt[-1+x^2+2 x^3])/x^6,x]*)


(* ::Input:: *)
(*int[((1+x^3) Sqrt[-1+x^2+2 x^3] (-2-x^2+4 x^3))/(x^2 (-1+2 x^3) (-2+x^2+4 x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[3 x^4-2] (3 x^4-1))/(x^3 (3 x^8-3 x^4+1)),x]*)


(* ::Input:: *)
(*int[(Sqrt[x^4-1] (x^8+1))/(x^7 (x^8+2)),x]*)


(* ::Input:: *)
(*int[(Sqrt[2 x^4-1] (2 x^8-1))/(x^7 (x^8+2 x^4-1)),x]*)


(* ::Input:: *)
(*int[(Sqrt[x^4+1] (x^8-x^4+2))/(x^7 (x^8-2)),x]*)


(* ::Input:: *)
(*int[((8 x^7-3) (8 x^14-2 x^10+8 x^7+x^6-x^3+2)^(3/2))/x^13,x]*)


(* ::Input:: *)
(*int[((x^3+2) Sqrt[x^6-x^4-2 x^3+1])/(x^3 (x^3-x^2-1)),x]*)


(* ::Input:: *)
(*int[(Sqrt[2-x^4] (x^8-2 x^4+2))/(x^7 (x^8-2 x^4-2)),x]*)


(* ::Input:: *)
(*int[(Sqrt[x^4-1] (x^8+2 x^4+2))/(x^7 (x^8-2)),x]*)


(* ::Input:: *)
(*int[((x^2+1) (x^8+1) Sqrt[x^8+x^6+x^4+x^2+1])/(x^7 (x^2-1)),x]*)


(* ::Input:: *)
(*int[((x^3+2) Sqrt[2 x^6+x^5+x^4-4 x^3-x^2+2])/x^5,x]*)


(* ::Input:: *)
(*int[(Sqrt[1-2 x^4] (x^4-1))/(x^7 (x^8+2 x^4-1)),x]*)


(* ::Input:: *)
(*int[((x^6+1) (x^6+x^3-1) Sqrt[x^12+1])/(x^7 (x^6-x^3-1)),x]*)


(* ::Input:: *)
(*int[((x^8+1) Sqrt[x^16-3 x^12+3 x^4+1])/x^9,x]*)


(* ::Input:: *)
(*int[((x^3-1)^3 (x^3+1) Sqrt[2 x^12+3 x^6+2])/(x^7 (x^6+1)),x]*)


(* ::Input:: *)
(*int[((3 x^4-1) Sqrt[x^8+x^5+2 x^4+x+1])/(x^2 (4 x^4+x+4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^4] (x^4-1) (2 x^4-1))/(x^7 (5 x^8-10 x^4+4)),x]*)


(* ::Input:: *)
(*int[((x^6-4) (x^6-x^4+2)^(5/2))/(x^7 (x^6+2)^2),x]*)


(* ::Input:: *)
(*int[((x^8+1) Sqrt[x^16-x^8+1])/(x^5 (x^8-1)),x]*)


(* ::Input:: *)
(*int[((x^5+4) (2 x^5+3 x^4-2)^(3/2))/(x^7 (2 x^5-x^4-2)),x]*)


(* ::Input:: *)
(*int[((x^6-2) (x^6+2 x^4+1)^(3/2))/(x^3 (x^6+1)^2),x]*)


(* ::Input:: *)
(*int[((x^4-3) (4 x^8+x^6+8 x^4+4)^(3/2))/(x^10 (x^4+1)),x]*)


(* ::Input:: *)
(*int[(2 x^5+1)/(x Sqrt[x^10+x^6-4 x^5+x^2-2 x+4]),x]*)


(* ::Input:: *)
(*int[(Sqrt[x^6+3 x^2+2] (x^6-1) (x^6+2))/(x^2 (x^6+x^2+2)^2),x]*)


(* ::Input:: *)
(*int[((x^6+1) Sqrt[x^12-2 x^9+2 x^3+1])/(x (x^6-x^3-1)^2),x]*)


(* ::Input:: *)
(*int[(x Sqrt[-x^6+x^4+2] (x^6+4))/(x^6-2 x^4-2)^2,x]*)


(* ::Input:: *)
(*int[((x^7-x^4-1)^(3/2) (3 x^7+4))/(x^3 (x^7-1) (x^7+x^4-1)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-x^6-x^4+1] (x^6-1) (x^6+2))/(x^7 (x^6-x^4-1)),x]*)


(* ::Input:: *)
(*int[(Sqrt[x^6-2] (x^6+1) (x^6+x^2-2))/(x^4 (x^6-2 x^2-2)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2-x^3] (2+x^3))/((-1+x^3) (-1-x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^5] (-2+3 x^5))/((1+x^5) (1+x^2+x^5)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^6] (1+2 x^6))/(1-x^4-2 x^6+x^12),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^6] (-1+2 x^6))/(1+x^6)^2,x]*)


(* ::Input:: *)
(*int[((2+x^3) Sqrt[-1+2 x^2+x^3])/(1+x^2-2 x^3-2 x^4-x^5+x^6),x]*)


(* ::Input:: *)
(*int[(Sqrt[-2+x^2+4 x^5-4 x^6] (-1-3 x^5+4 x^6))/((1-2 x^5+2 x^6) (2+x^2-4 x^5+4 x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+2 x^6] (-1+4 x^6))/((1+2 x^6) (2-x^2+4 x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1-2 x^2-2 x^3-x^8] (-1+x^3+3 x^8))/((1+2 x^3+x^8) (1+x^2+2 x^3+x^8)),x]*)


(* ::Input:: *)
(*int[((-1-x^2+x^4+x^5) (2+2 x^4+3 x^5))/(Sqrt[1-x^4-x^5] (-1+x^4+x^5)^2),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (1-x^2+x^4))/(Sqrt[1+x^4] (1+3 x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^8] (-1+3 x^8))/(1+x^8)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^2+x^6] (-1+2 x^6))/((1-2 x^2+x^6) (1-x^2+x^6)),x]*)


(* ::Input:: *)
(*int[((1-x^2+x^6) (-1+2 x^6))/(Sqrt[1+x^6] (1+x^6)^2),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (1-x+x^2-x^3+x^4))/((1-x+x^2)^2 (1+x+x^2) Sqrt[1+3 x^2+x^4]),x]*)


(* ::Input:: *)
(*int[((1+2 x^6) (1+x^2-x^4-2 x^6-x^8+x^12))/((-1-x^2+x^6)^(5/2) (-1+x^2+x^6)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)^(3/4)/(1+x^4),x]*)


(* ::Input:: *)
(*int[(-1+x^4)^(3/4)/(1+x^4+x^8),x]*)


(* ::Input:: *)
(*int[((-1+x^6) (1+x^6))/((x-x^4+x^7)^(1/4) (1+3 x^6+x^12)),x]*)


(* ::Input:: *)
(*int[((-2+x^2) (-1+x^2))/((-1+x^2-x^4)^(1/4) (1-2 x^2+x^4+x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^6) (1+x^6))/((-x-x^4+x^7)^(1/4) (1-x^6+x^12)),x]*)


(* ::Input:: *)
(*int[((1-x^3+x^4+x^6)^(3/4) (-4+x^3+2 x^6))/(1-x^3+x^6)^2,x]*)


(* ::Input:: *)
(*int[((-1+x^8) (1+x^8))/((-1-x^4+x^8)^(1/4) (1-3 x^8+x^16)),x]*)


(* ::Input:: *)
(*int[((2+x^6) (-1-x^4+x^6))/((1-x^4-x^6)^(1/4) (-1+x^6)^2),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (1+x^2+x^4))/(Sqrt[1+x^4] (1+3 x^4+x^8)),x]*)


(* ::Input:: *)
(*int[((1+x^8)^(3/4) (-1+x^8))/(1+x^8+x^16),x]*)


(* ::Input:: *)
(*int[((-1+x^8) (1+2 x^4+x^8)^(3/4))/(1+x^4+x^8)^2,x]*)


(* ::Input:: *)
(*int[((1+x^6)^2 (-1+2 x^6))/((1-x^2+x^6)^(3/2) (1-x^2-x^4+2 x^6-x^8+x^12)),x]*)


(* ::Input:: *)
(*int[((1+x^8) (-1+3 x^8))/(Sqrt[1-x^2+x^8] (1-x^2+x^8) (1+x^2+x^8)),x]*)


(* ::Input:: *)
(*int[((1+x+x^7) (-1+6 x^7))/(1-x+x^2+2 x^7-x^8+x^14)^(3/2),x]*)


(* ::Input:: *)
(*int[((1+x^8) (-1+3 x^8))/((1-x^2+x^8) (1+x^2+x^8)^(3/2)),x]*)


(* ::Input:: *)
(*int[((1+2 x^2+x^6) (-1+2 x^6))/((1+x^6)^(3/2) (1+x^2+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (1+x^2+x^4))/(1+x^4)^(5/2),x]*)


(* ::Input:: *)
(*int[((-1+x^7) (2+5 x^7))/(-1-x^2+x^7)^(5/2),x]*)


(* ::Input:: *)
(*int[(1+3 x^4)/(Sqrt[1+3 x^4] (-1+3 x^4)),x]*)


(* ::Input:: *)
(*int[(-2+3 x^4)/((2+x^4) Sqrt[4+x^2+4 x^4+x^8]),x]*)


(* ::Input:: *)
(*int[(-1+3 x^8)/((1+x^8) Sqrt[1+x^2+x^8]),x]*)


(* ::Input:: *)
(*int[(-1+5 x^6)/((1+x^6) Sqrt[x+x^2+x^7]),x]*)


(* ::Input:: *)
(*int[(-1+2 x^6)/((1+x^6) Sqrt[1+x^2+x^6]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[x+x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(-3+x^4)/((1+x^4) (-3 x+4 x^4-3 x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+3 x^4)/((-1+x^4) Sqrt[-x+x^5]),x]*)


(* ::Input:: *)
(*int[(-1+x^3+x^8)/((1-4 x^3+x^8) (1-4 x^3+x^4+x^8)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2+x^4)/(2+x^4)^(3/2),x]*)


(* ::Input:: *)
(*int[(-1+x^8)/((1+x^8) (1-4 x^4+x^8)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^8)/((1+x^8) (1+x^8)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^8)/((-1+x^8) (1+x^8)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((2-x+2 x^2) Sqrt[x+x^3]),x]*)
