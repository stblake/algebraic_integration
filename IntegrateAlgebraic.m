(* ::Package:: *)

(* ::Title:: *)
(*A collection of simple methods for solving some algebraic integrals*)


(* ::Subtitle:: *)
(*Sam Blake, 2020*)


(* ::Text:: *)
(*samuel.thomas.blake@gmail.com*)


(* ::Text:: *)
(*Started on 16 March 2020.*)


(* ::Text:: *)
(*This package implements a suite of heuristics for solving some pseudo-elliptic integrals using a combination of integration by substitution and the method of undetermined coefficients. We also try generalised Gunther substitutions, and attempt to set up systems of equations to solve some Abelian integrals. *)


(* ::Section:: *)
(*IntegrateAlgebraic*)


(* ::Subsection::Closed:: *)
(*BeginPackage*)


BeginPackage["AlgebraicIntegrateHeuristic`"];

IntegrateAlgebraic::usage = "IntegrateAlgebraic[f, x] is a heuristic for computing an elementary \
solution to a pseudo-elliptic integral.";

$verboseLevel::usage = "Controls how much information is shown when calling IntegrateAlgebraic. \
With $verboseLevel = 0; no information is shown, $verboseLevel = 1; only top level information is \
shown, $verboseLevel = 2; intermediate-level information is shown, and $verboseLevel = 3; shows all \
information.";

(*
solveAlgebraicIntegral::usage = "solveAlgebraicIntegral[f, x] is a heuristic for computing an \
elementary solution to a pseudo-elliptic integral. solveAlgebraicIntegral returns {rp, up, ip}, \
where rp is the (unintegrated) rational part, up is the unintegrated part, and ip is the \
integrated part.";

directRationalise::usage = "directRationalise[f, x] is an interface to the generalised \
Gunther code.";

derivdivides::usage = "derivative-divides heuristic.";
*)

Begin["`Private`"];


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

	(* integral = Collect[integral, (Power[p_,_Rational] /; !FreeQ[p,x]) | _Log | _ArcTan | _ArcTanh | _RootSum, Simplify]; *)
	integral /. internalC -> C
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
	Return[ {0, 0, simplify[integral, x, "CancelRadicalDenominators" -> False]}, Module ]
];

(* Simple derivative divides. *)

If[! algebraicQ[unintegratedPart, x],
	debugPrint1["Trying derivative-divides..."];
	dd = derivdivides[unintegratedPart, x, u];
	If[ListQ[dd], 
		debugPrint1["Derivative-divides produced a simplification: ", dd];
		simplified = dd[[1]];
		substitution = dd[[2]];
		result = solveAlgebraicIntegral[simplified, u, opts];
		(* TODO: what do we do if we couldn't integrate the simplified integrand? Do we 
			give up now, or try other methods on the original integrand? SAMB 0421 *)
		(* result = simplify[result /. substitution, x]; *)
		result = result /. substitution;
		debugPrint1["Substituting back for ", substitution, " gives ", result];
		If[(TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x]),
			rationalPart    += result[[1]]; 
			unintegratedPart = result[[2]];
			integratedPart  += result[[3]]
		]
	]
];

If[! algebraicQ[unintegratedPart, x] && ! OptionValue["Expansion"],
	Return[{Integrate[rationalPart, x], unintegratedPart, integratedPart}, Module] (* As no further methods deal with non-algebraics. *)
];

(* Integrand is in Q(x, (a*x + b)^(m[1]/n[1]), (a*x + b)^(m[2]/n[2]), \[Ellipsis]) *)

If[ListQ @ linearRadicalToRational[unintegratedPart, x, u],
	debugPrint1["Integrand is in Q(x, (a*x + b)^(m[1]/n[1]), (a*x + b)^(m[2]/n[2]), \[Ellipsis]): ", unintegratedPart];
	integral = integrateLinearRadical[unintegratedPart, x];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[integral, unintegratedPart, x],
		Return[ {0, 0, integral}, Module ],
		Return[ {0, unintegratedPart, 0}, Module ]
	]
];

(* Integrand is in Q(x, (a x^2 + b x + c)^(n[1]/2), (a x^2 + b x + c)^(n[2]/2), \[Ellipsis]) *)

If[ListQ @ quadraticRadicalToRational[unintegratedPart, x, u],
	debugPrint1["Integrand is in Q(x, (a x^2 + b x + c)^(n[1]/2), (a x^2 + b x + c)^(n[2]/2), \[Ellipsis]): ", unintegratedPart];
	integral = integrateQuadraticRadical[unintegratedPart, x];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[integral, unintegratedPart, x],
		Return[ {0, 0, integral}, Module ],
		Return[ {0, unintegratedPart, 0}, Module ]
	]
];

(* Integrand is in Q(x, (a*x + b)^(1/2), (c*x + d)^(1/2)) *)

If[ListQ @ multipleLinearRadicalToRational[unintegratedPart, x, u],
	debugPrint1["Integrand is in Q(x, (a*x + b)^(1/2), (c*x + d)^(1/2)): ", unintegratedPart];
	integral = integrateMultipleLinearRadical[unintegratedPart, x];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[integral, unintegratedPart, x],
		Return[ {0, 0, integral}, Module ],
		Return[ {0, unintegratedPart, 0}, Module ]
	]
];

(* Integrand is in Q(x,((a x + b)/(c x + d))^(m[1]/n[1]), ((a x + b)/(c x + d))^(m[2]/n[2]), \[Ellipsis]) *)

If[ListQ @ linearRatioRadicalToRational[unintegratedPart, x, u],
	debugPrint1["Integrand is in Q(x,((a x + b)/(c x + d))^(m[1]/n[1]), ((a x + b)/(c x + d))^(m[2]/n[2]), \[Ellipsis]): ", unintegratedPart];
	integral = integrateLinearRatioRadical[unintegratedPart, x];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[integral, unintegratedPart, x],
		Return[ {0, 0, integral}, Module ],
		Return[ {0, unintegratedPart, 0}, Module ]
	]
];

(* Multiple distinct radicals? See if we can split them and integrate term-by-term using existing methods. 
	For example: IntegrateAlgebraic[((x^4 + 1)*Sqrt[x^4 - 1] + (x^4 - 1)*Sqrt[x^4 + 1])/(x^8 + 1), x] SAMB 0421 *)

If[multipleRadicalsQ[unintegratedPart, x] && nestedCount[unintegratedPart, x] == 0,
	debugPrint1["Integrand contains multiple radicals: ", unintegratedPart];
	result = integrateMultipleRadicals[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	]
];

(*
If[multipleRadicalsQ[unintegratedPart, x] && nestedCount[unintegratedPart, x] == 0,
	Return[{Integrate[rationalPart, x], unintegratedPart, integratedPart}, Module] (* As no further methods deal with multiple radicals. *)
];
*)

(* Nested radicals. *)

If[nestedCount[unintegratedPart, x] > 0,
	If[ListQ @ decreaseNestedRadicals[unintegratedPart, x, u],
		debugPrint1["Integrand contains nested radicals: ", unintegratedPart];
		result = integrateNestedRadicals[unintegratedPart, x, u];
		If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x],
			rationalPart    += result[[1]]; 
			unintegratedPart = result[[2]];
			integratedPart  += result[[3]]
		]
	] 
];

(*
If[nestedCount[unintegratedPart, x] > 0, 
	Return[{Integrate[rationalPart, x], unintegratedPart, integratedPart}, Module](* As no further methods deal with nested radicals. *)
];
*)

(* Factoring the radicand (s) and taking branch cuts. *)

If[! OptionValue["RationalUndeterminedOnly"] && nestedCount[unintegratedPart, x] == 0,
	debugPrint1["Trying factorisation of the radicand and taking branch cuts on ", unintegratedPart];
	result = decreasePolynomialRadicandDegreeIntegrate[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["decreasePolynomialRadicandDegreeIntegrate returned : ", {rationalPart, unintegratedPart, integratedPart}]
];

(* Goursat pseudo-elliptic integral. *)

If[! OptionValue["RationalUndeterminedOnly"] && ! multipleRadicalsQ[unintegratedPart, x] && nestedCount[unintegratedPart, x] == 0,
debugPrint1["Trying Goursat method on ", unintegratedPart];
goursat = TimeConstrained[goursatIntegrate[unintegratedPart, x, u], $timeConstraint, False];
If[ListQ @ goursat,
	integral = goursat // First;
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[integral, unintegratedPart, x],
		unintegratedPart = 0;
		integratedPart  += integral
	];
	debugPrint1["Goursat returned : ", {rationalPart, unintegratedPart, integratedPart}];
]
];

(* Rational substitution with undetermined coefficients. *)

If[! multipleRadicalsQ[unintegratedPart, x] && nestedCount[unintegratedPart, x] == 0,
	debugPrint1["Trying rational undetermined on ", unintegratedPart];
	result = rationalUndeterminedIntegrate[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["Rational undetermined returned : ", {rationalPart, unintegratedPart, integratedPart}];
];

If[! OptionValue["RationalUndeterminedOnly"],

(* Direct rationalisation. *)
If[! multipleRadicalsQ[unintegratedPart, x] && nestedCount[unintegratedPart, x] == 0,
	debugPrint1["Trying direct rationalisation on ", unintegratedPart];
	result = directRationalise[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["direct rationalisation returned : ", {rationalPart, unintegratedPart, integratedPart}];
];

(* Linear rational substitution. *)

If[TrueQ[OptionValue["LinearRational"]] && ! multipleRadicalsQ[unintegratedPart, x] && nestedCount[unintegratedPart, x] == 0,
	debugPrint1["Trying linear rational on ", unintegratedPart];
	result = linearRationalIntegrate[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["LinearRational returned : ", {rationalPart, unintegratedPart, integratedPart}];
];

(* Solving for the logarithmic part with undetermined coefficients. *)

If[! multipleRadicalsQ[unintegratedPart, x] && nestedCount[unintegratedPart, x] == 0,
	debugPrint1["Trying log part undetermined on ", unintegratedPart];
	result = logPartIntegrate[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["logPart returned : ", {rationalPart, unintegratedPart, integratedPart}];
]
];

(* Partial fraction expansion and integrate term-by-term. *)

If[OptionValue["Expansion"],
	
	(* Use partial fractions with a factorisation over Q. *)
	
	debugPrint1["Trying partial fractions over Q on ", unintegratedPart];
	result = apartIntegrate[unintegratedPart, x, opts, "FactorComplete" -> False];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["partialFractionIntegrate returned : ", {rationalPart, unintegratedPart, integratedPart}];
		
	(* Use partial fractions with a factorisation over the splitting field of the integrand. *)

	debugPrint1["Trying partial fractions over K on ", unintegratedPart];
	result = apartIntegrate[unintegratedPart, x, opts, "FactorComplete" -> True];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["partialFractionIntegrate returned : ", {rationalPart, unintegratedPart, integratedPart}];
			
	(* Expand and integrate term-by-term. *)

	debugPrint1["Trying to integrate term-by-term on: ", unintegratedPart];
	result = expandIntegrate0[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["expandIntegrate0 returned : ", {rationalPart, unintegratedPart, integratedPart}];
		
	debugPrint1["Trying expansion into sum of terms with Apart on ", unintegratedPart];
	result = expandIntegrate2[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["partialFractionIntegrate returned : ", {rationalPart, unintegratedPart, integratedPart}];

	debugPrint1["Trying expansion into sum of terms with Expand on ", unintegratedPart];
	result = expandIntegrate1[unintegratedPart, x, opts];
	If[TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x],
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	];
	debugPrint1["expandIntegrate1 returned : ", {rationalPart, unintegratedPart, integratedPart}];
];

(* Everything else has failed, so try derivative-divides on the algebraic integral. *)

debugPrint1["Trying derivative-divides..."];
dd = derivdivides[unintegratedPart, x, u];
If[ListQ[dd], 
	debugPrint1["Derivative-divides produced a simplification: ", dd];
	simplified = dd[[1]];
	substitution = dd[[2]];
	result = solveAlgebraicIntegral[simplified, u, opts];
	(* result = simplify[result /. substitution, x]; *)
	result = result /. substitution;
	debugPrint1["Substituting back for ", substitution, " gives ", result];
	If[(TrueQ[! OptionValue[VerifySolutions]] || verifySolution[result[[3]], unintegratedPart - result[[1]] - result[[2]], x]),
		rationalPart    += result[[1]]; 
		unintegratedPart = result[[2]];
		integratedPart  += result[[3]]
	]
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
							debugPrint3["Solution to rational part is ", rationalFormU];
	
							integrandU = rationalFormU (radicand[u]^(1/r) /. radicandMatchRule /. {A[1]|A[2]|B[1]|B[2] -> 1, A[0]|B[0] -> 0});
							debugPrint2["Recursively callling IntegrateAlgebraic on ", integrandU];
	
							intU = solveAlgebraicIntegral[integrandU, u];
							debugPrint3["Recursive integration returned ", intU];

							If[intU[[2]] === 0,
								intU = intU[[1]] + intU[[3]];
								debugPrint2["Substitution is ", usubstitutionParam];
								intX = intU /. u -> usubstitutionParam;
								debugPrint2["integral is ", intX];
								intX = simplify[intX, x, "Integrand" -> integrand];
								debugPrint2["Simplified integral is ", intX];
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
									debugPrint3["We got something wrong!"];
								],
								(* else *)
								debugPrint3["Could not integrate wrt u"];
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


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
(*rationalUndetermined*)


Clear[rationalUndetermined];

rationalUndetermined[x_Symbol, max_Integer] := 
(Sum[V[k] x^k, {k, 0, max}]/Sum[V[max + k + 1] x^k, {k, 0, max}])


(* ::Subsubsection::Closed:: *)
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

	radical = Union @ Cases[e, p_^r_Rational :> p^Abs[r] /; (! FreeQ[p, x] && (PolynomialQ[p, x] || rationalQ[p, x])), {0, Infinity}];
	If[Length[radical] > 1, Return[ False ], radical = radical[[1]]];(* Fix for multiple distinct radicals. *)
	{p, r} = {radical[[1]], 1/radical[[2]]};
	
	nonAlgPart = 0;
	exy = e //. {radical -> y, radical^-1 -> y^-1};
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
(*normalise[(2 (-1-4 u^2+Sqrt[1-12 u^4+16 u^6]))/((-2+u^2) (1+4 u^2)),{u,y}]*)
(*%[[5]]+%[[1]]/%[[2]]*)


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

{{r,n}} = Union @ Cases[e, Power[p_, n_Rational] /; (! FreeQ[p, x] && (PolynomialQ[p, x] || rationalQ[p, x])) :> {p,n}, {0, Infinity}];

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
	Return[ {0, 0, simplify[ratIntegral /. u -> Last[result], x, "CancelRadicalDenominators" -> False]} ]
];

result = directRationaliseSolve[p, q, r, n, -n,Sign[n],x,u];

If[result =!= $Failed, 
	debugPrint2["directRationalise -- substitution of the form s[x]^(-Sign[n])*r[x]^n: ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, simplify[ratIntegral /. u -> Last[result], x, "CancelRadicalDenominators" -> False]} ]
];

(* Substitution containing r[x]^(n+1)/s[x] or s[x]/r[x]^(n+1) *)

result = directRationaliseSolve[p, q, r, n, n+1,-Sign[n+1],x,u];

If[result =!= $Failed, 
	debugPrint2["directRationalise -- substitution of the form s[x]^(-Sign[n+1])*r[x]^(n+1): ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, simplify[ratIntegral /. u -> Last[result], x, "CancelRadicalDenominators" -> False]} ]
];

result = directRationaliseSolve[p, q, r, n, -n-1,Sign[n+1],x,u];

If[result =!= $Failed, 
	debugPrint2["directRationalise -- substitution of the form s[x]^(-Sign[n+1])*r[x]^(n+1): ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, simplify[ratIntegral /. u -> Last[result], x, "CancelRadicalDenominators" -> False]} ]
];

(* Substitution containing s[x]*r[x]^n or 1/(s[x]*r[x]^n) *)
(*
result = directRationaliseSolve[p, q, r, n, n,Sign[n],x,u];

If[result =!= $Failed, 
	debugPrint2["directRationalise -- substitution of the form s[x]^(Sign[n])*r[x]^n: ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, simplify[ratIntegral /. u -> Last[result], x, "CancelRadicalDenominators" \[Rule] False]} ]
];

(* Substitution containing s[x]*r[x]^(n+1) or 1/(s[x]*r[x]^(n+1)) *)

result = directRationaliseSolve[p, q, r, n, n+1,Sign[n+1],x,u];

If[result =!= $Failed, 
	debugPrint2["directRationalise -- substitution of the form s[x]^(Sign[n+1])*r[x]^(n+1): ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, simplify[ratIntegral /. u -> Last[result], x, "CancelRadicalDenominators" \[Rule] False]} ]
];
*)

(* Quadratic rational substitutions. *)

result = directRationaliseQuadraticRationalSolve[p, q, r, n, n, x, u];

If[result =!= $Failed, 
	debugPrint2["directRationaliseQuadraticRationalSolve -- substitution of the form s[x]/t[x]*r[x]^n: ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, simplify[ratIntegral /. u -> Last[result], x, "CancelRadicalDenominators" -> False]} ]
];

result = directRationaliseQuadraticRationalSolve[p, q, r, n, -n, x, u];

If[result =!= $Failed, 
	debugPrint2["directRationaliseQuadraticRationalSolve -- substitution of the form s[x]/t[x]*r[x]^(-n): ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, simplify[ratIntegral /. u -> Last[result], x, "CancelRadicalDenominators" -> False]} ]
];

result = directRationaliseQuadraticRationalSolve[p, q, r, n, n+1, x, u];

If[result =!= $Failed, 
	debugPrint2["directRationaliseQuadraticRationalSolve -- substitution of the form s[x]/t[x]*r[x]^(n+1): ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, simplify[ratIntegral /. u -> Last[result], x, "CancelRadicalDenominators" -> False]} ]
];

result = directRationaliseQuadraticRationalSolve[p, q, r, n, -n-1, x, u];

If[result =!= $Failed, 
	debugPrint2["directRationaliseQuadraticRationalSolve -- substitution of the form s[x]/t[x]*r[x]^(-n-1): ", result];
	ratIntegral = Integrate[result // First, u];
	Return[ {0, 0, simplify[ratIntegral /. u -> Last[result], x, "CancelRadicalDenominators" -> False]} ]
];

{0, e, 0}
]


(* ::Subsubsection::Closed:: *)
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
	simplify[integral, x, "CancelRadicalDenominators" -> False]	
]


(* ::Input:: *)
(*integrate[(1+u)^(1/3)/u^3,u]*)


integrate[e_, x_] /; ListQ[ quadraticRadicalToRational[e, x, $u] ] := 
	integrateQuadraticRadical[e,x]


ClearAll[integrateQuadraticRadical];

integrateQuadraticRadical[e_, x_] := Module[
{t, u, mmaInt, intt, integrand, substitution, integral, numerics, result},

	(* Can we substitute u \[Equal] (x^2)? eg. (x*Sqrt[-2 + x^2])/(2 - 4*x^2 + x^4) *)

	intt = TimeConstrained[subst[e, t -> x^2, x], $timeConstraint, False];
	If[intt =!= False && ListQ[linearRadicalToRational[intt // First, t, u]],
		Return[ simplify[integrate[intt // First, t] /. Last[intt], x] ]
	];

	(* Euler's substitution for f[x, Sqrt[quadratic[x]]]. *)
	
	result = quadraticRadicalToRational[e, x, u];
	If[ListQ[result],
		{integrand, substitution} = result;
		debugPrint3["Rationalised integrand and substitution is ", {integrand, substitution}];
		integral = Quiet @ Integrate[integrand, u] /. substitution;
		integral = Apart[integral], (* We need Apart here for example IntegrateAlgebraic[(-2 + u)/(1 - u + u^2)^(3/2), u] *)
		integral = Quiet @ Integrate[e, x] (* eg. Integrate[Sqrt[2I x^2-3I x+1],x] *)	
	];

	simplify[integral, x, "CancelRadicalDenominators" -> False]
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

rationalOfLinearRadicalsQ[e_, x_] := ListQ[linearRadicalToRational[e, x, Unique["u"]]]

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

rationalOfQuadraticRadicalsQ[e_, x_] := ListQ[quadraticRadicalToRational[e, x, Unique["u"]]]

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
(*simplify[%/. Last[%%],x]*)
(*D[%,x] - f // Simplify*)
(*Clear[f]*)


(* ::Input:: *)
(*Integrate[((-1-4 x-x^2) Sqrt[-1+x^2])/(1+2 x^3+x^4),x]*)


(* ::Subsection::Closed:: *)
(*Integrating multiple linear radicals*)


ClearAll[integrateMultipleLinearRadical];

integrateMultipleLinearRadical[e_, x_] := Module[{u, integrand, subst, integral},

	{integrand, subst} = multipleLinearRadicalToRational[e, x, u];
	integral = simplify[Integrate[integrand, u] /. subst, x];
	simplify[integral, x, "CancelRadicalDenominators" -> False]
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
	integral = simplify[Integrate[integrand, u] /. subst, x, "CancelRadicalDenominators" -> False];
	simplify[integral, x, "CancelRadicalDenominators" -> False]
]


ClearAll[linearRatioRadicalQ];
linearRatioRadicalQ[e_, x_] := ListQ[linearRatioRadicalToRational[e, x, Unique["u"]]]


(* ::Input:: *)
(*linearRatioRadicalQ[(3-9 u^2+2 u^3)/(2 u (1+u)^2 (-1+2 u) (1+2 u)Sqrt[(1-2 u)/(1+2 u)]),u]*)


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


(* ::Subsection:: *)
(*Integrating by factorisation and taking branch cuts*)


(* ::Text:: *)
(*This method is motivated by solving integrals like *)


(* ::Input:: *)
(*Integrate[Sqrt[-1-11 x-36 x^2-27 x^3+16 x^4+9 x^5+x^6],x]*)


(* ::Text:: *)
(*in terms of the radicals present in the integrand and to avoid the use of piecewise constants. Here's the result from mathematica version 12.1: *)


(* ::Input:: *)
(*Simplify[D[(Sqrt[-1 - x + x^2]*(1 + 5*x + x^2)**)
(*     (2*Sqrt[-1 - x + x^2]*(-185 - 6*x + 104*x^2 + 16*x^3) + *)
(*      325*ArcTanh[(1 - 2*x)/(2*Sqrt[-1 - x + x^2])]))/*)
(*    (128*Sqrt[(-1 - x + x^2)*(1 + 5*x + x^2)^2]), x]]*)


(* ::Text:: *)
(*It is interesting that AXIOM (2014) gets these types of integrals wrong:*)


(* ::Input:: *)
(*D[(75023 + 66080*x - 646304*x^2 - 183040*x^3 + 1189248*x^4 - 163840*x^5 - *)
(*    524288*x^6 + 131072*x^7 + 32768*x^8 + Sqrt[-1 - x + x^2]**)
(*     (-7064 + 292592*x + 6336*x^2 - 839808*x^3 + 276480*x^4 + *)
(*      430080*x^5 - 147456*x^6 - 32768*x^7) + *)
(*    (18200 - 249600*x - 83200*x^2 + 665600*x^3 - 332800*x^4 + *)
(*      Sqrt[-1 - x + x^2]*(62400 + 41600*x - 499200*x^2 + 332800*x^3))**)
(*     Log[1 - 2*x + 2*Sqrt[-1 - x + x^2]])/(7168 - 98304*x - 32768*x^2 + *)
(*    262144*x^3 - 131072*x^4 + Sqrt[-1 - x + x^2]**)
(*     (24576 + 16384*x - 196608*x^2 + 131072*x^3)), x] - *)
(*Sqrt[-1 - 11*x - 36*x^2 - 27*x^3 + 16*x^4 + 9*x^5 + x^6] // Simplify*)


(* ::Text:: *)
(*This method is presently only implemented for polynomial radicands. However, it generalises easily to rational radicands. *)


partiallyRemovableQ[listOfFactors_, r_, x_] := 
	Cases[listOfFactors, {p_,n_} /; !FreeQ[p,x] && n >= Denominator[r]] =!= {}


(* ::Code::Initialization:: *)
ClearAll[decreasePolynomialRadicandDegreeIntegrate];

Options[decreasePolynomialRadicandDegreeIntegrate] = Options[IntegrateAlgebraic];

decreasePolynomialRadicandDegreeIntegrate[e_, x_, opts:OptionsPattern[]] := Module[
{fp, flist, fl, qr, fo, fi,ep,fot, rules, epint, invrules},
(* Find radicands, p^(m/n), such that the polynomial, p, factors 
into p = r*q^(v n), for integer v > 0 and polynomials q and r. *)

fp = Union @ Cases[e,Power[p_, r_Rational] /; PolynomialQ[p,x], {0, Infinity}];
If[Length[fp] != 1, Return[ {0, e, 0} ]];(* No radicals or multiple distinct radicals. *)
flist = {FactorList[#1],#1, #2}& @@@ fp;
flist = Cases[flist,{facs_, _, r_} /; partiallyRemovableQ[facs, r, x], {1}];
If[flist === {}, Return[ {0, e, 0} ]];(* Radicands do not partially factor out. *)
debugPrint2["Radicand is partially factorable ",flist];
(* Factor out terms from all radicals. *)
rules = Table[
	With[{factors = fl[[1]], p = fl[[2]], r = fl[[3]], radical = fl[[2]]^fl[[3]]},
	qr = {#1,Quotient[#2,Denominator[r]],Mod[#2,Denominator[r]]}& @@@ factors;
	fo = Select[qr, #[[2]] != 0&];(* Partially factored-out terms. *)
	fi = Select[qr, #[[2]] == 0&];(* Factored terms that stay in the radical. *)
	fi = Apply[Times, Power[#1,#3]& @@@ fi];
	fot = 1;(* Factored-out terms. *)
	Do[
		fot *= ft[[1]]^(ft[[2]] Numerator[r]); (* Quotient factors out. *)
		fi *= ft[[1]]^(ft[[3]] Numerator[r]),(* Remainder stays inside the radical. *)
	{ft, fo}];
	{
		{radical -> fot fi^r, 1/radical -> 1/(fot fi^r)}, 
		{Expand[fi]^r -> radical/fot, 1/Expand[fi]^r -> fot/radical}
	}
],
{fl, flist}];

ep = e /. Flatten[ rules[[All,1]] ];

debugPrint2["Reduced integrand for recursive integration is ", ep];
epint = solveAlgebraicIntegral[ep, x, opts];
debugPrint2["Recursive integration returned ", epint];
If[epint[[2]] =!= 0, 
	Return[ {0, e, 0} ], 
	epint = epint[[1]] + epint[[3]]
];

(* The creation of inverse rules is a hack and should be completely rewritten. SAMB 0521 *)
invrules = Join @@ Table[
	Which[
		n == 0, 
			Sequence @@ {}, 
		n == 1, 
			rule, 
		True, 
			Distribute[(rule)^n,Rule]
	],
{rule, Flatten[ rules[[All,-1]] ]},
{n, -16, 16}];

{0, 0, simplify[epint /. invrules, x]}
]


(* ::Input:: *)
(*IntegrateAlgebraic[Sqrt[1+10 x+27 x^2+10 x^3+x^4],x](* We cannot currently do this one, which is a shame. SAMB 0621 *)*)


(* ::Input:: *)
(*IntegrateAlgebraic[Sqrt[-1-11 x-36 x^2-27 x^3+16 x^4+9 x^5+x^6],x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[Sqrt[u]/Sqrt[-1-u+u^2+u^3],u]*)


(* ::Input:: *)
(*IntegrateAlgebraic[1/((1+Sqrt[x])Sqrt[x-Sqrt[x]]),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x^4+b)^2/(a b^4+4 a b^3 x^4+b^4 x^4+6 a b^2 x^8+4 b^3 x^8+4 a b x^12+6 b^2 x^12+a x^16+4 b x^16+x^20)^(1/4),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[1/(1-3 x+3 x^3-9 x^4+3 x^6-9 x^7+x^9-3 x^10)^(1/3),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(1-3 x+3 x^3-9 x^4+3 x^6-9 x^7+x^9-3 x^10)^(1/3),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(1-(1-3 x+3 x^3-9 x^4+3 x^6-9 x^7+x^9-3 x^10)^(1/3))/(x^2+(1-3 x+3 x^3-9 x^4+3 x^6-9 x^7+x^9-3 x^10)^(1/3)),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x+Sqrt[1+4 x+7 x^2+8 x^3+5 x^4+2 x^5])/(1-Sqrt[1+4 x+7 x^2+8 x^3+5 x^4+2 x^5]),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[1/(-1-3 x^4-2 x^8+2 x^12+3 x^16+x^20)^(1/4),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[((1-x^4)(1-x-4 x^2+4 x^3+6 x^4-6 x^5-4 x^6+4 x^7+x^8-x^9)^(1/4))/(1+x^4),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[1/(-8+12 x+54 x^2-135 x^3+81 x^4)^(1/3),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[Sqrt[-81+27 x+135 x^2-150 x^3+65 x^4-13 x^5+x^6]/(x-1),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[Sqrt[(-81+27 x+135 x^2-150 x^3+65 x^4-13 x^5+x^6)^3]/(x-1),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[((-3+x)^6 (-1-x+x^2)^(3/2))/(-1+x),x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[Sqrt[1-12 u^4+16 u^6]/((-2+u^2) (1+4 u^2)),u]*)


(* ::Subsection::Closed:: *)
(*Goursat pseudo-elliptic rationalisations*)


(* ::Text:: *)
(*TODO: this is mostly under construction. *)


goursatIntegrate[integrand_, x_, u_] := Module[
{quartic, integrandu, sub, integralu},

quartic = goursatQuartic[integrand, x, u];
If[ListQ[quartic],
	{integrandu, sub} = quartic;
	integralu = integrate[integrandu, u];
	Return[ List @ simplify[integralu /. sub, x] ]
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
		integral = simplify[powerReduce1[MapAll[Together,result /. Last[linRat]], x], x];
		const = Simplify[e/(integral[[1]] + D[integral[[3]], x])];
		If[Cancel[D[const, x]] == 0, integral[[3]] *= const];

		debugPrint2["Integral of ", linRat // First, " is ", result];
		simplify[integral, x],
		debugPrint2["Recursive call could not find an antiderivative of ", linRat // First];
		{0, e, 0}
	]
]


(* ::Input:: *)
(*linearRationalIntegrate[1/((x+1) (x^4+6 x^2+1)^(1/4)),x]*)


(* ::Input:: *)
(*linearRationalIntegrate[(-1+x)/(x^3-x^2-x+1)^(1/3),x]*)


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
(*Expand integrate*)


ClearAll[expandIntegrate0];

Options[expandIntegrate0] = Options[solveAlgebraicIntegral];

expandIntegrate0[0|0., x_, opts:OptionsPattern[]] := {0, 0, 0}


expandIntegrate0[e_, x_, opts:OptionsPattern[]] := Module[
{terms, unintegratedPart, integratedPart, term, integrated, rationalPart},

If[Head[e] === Plus, 
	debugPrint2["Integrating the sum term-by-term: ", e];
	terms = List @@ e;
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
	{term, terms}];

	If[integratedPart === 0,
		unintegratedPart = e;
		integratedPart = 0;
		rationalPart = 0;
	];
	{rationalPart, unintegratedPart, integratedPart}, 
	{0, e, 0}
]
]


ClearAll[expandIntegrate1];

Options[expandIntegrate1] = Options[solveAlgebraicIntegral];

expandIntegrate1[0|0., x_, opts:OptionsPattern[]] := {0, 0, 0}


expandIntegrate1[e_, x_, opts:OptionsPattern[]] := Module[
{exnum, lexnum, unintegratedPart, integratedPart, term, integrated, rationalPart},

exnum = Expand @ Numerator[e];

If[Head[exnum] === Plus && exnum =!= Numerator[e], 
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

pf = GatherBy[pf, Union[Cases[#, Power[p_, n_Rational] /; (! FreeQ[p, x] && (PolynomialQ[p, x] || rationalQ[p, x] || ! FreeQ[p, Power[r_, _Rational] /; !FreeQ[r,x]])) :> p^Abs[n],{0,Infinity}]]&];

Simplify[ Cancel[Together[Total[#]]]& /@ pf ]
]


(* ::Input:: *)
(*apartList[Sqrt[1-x]/(8 (1+x)^(7/2)),x]*)


(* ::Input:: *)
(*apartList[(2 x^(5/2))/((-1+x)^4 (1+x)^(7/2)),x]*)


(* ::Input:: *)
(*apartList[((-8 x^3+7 x^4) (-1+x^2-Sqrt[1+(Sqrt[2] x^4)/Sqrt[-1+x]]))/(2 2^(3/4) (-1+x)^(5/2) Sqrt[x^4/Sqrt[-1+x]] (1+x) Sqrt[1+(Sqrt[2] x^4)/Sqrt[-1+x]]),x](* This is suboptimal. *)*)


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
	{rationalPart, unintegratedPart, simplify[integratedPart // Expand, x]}, 
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
	pf = List @@ pf;
	pf = Together /@ pf,
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

{rationalPart, unintegratedPart, simplify[integratedPart // Expand, x]}
]


(* ::Input:: *)
(*apartIntegrate[1/((x^2-3)Power[1-3 x^2, (3)^-1]),x,"FactorComplete"->True]*)


(* ::Subsection::Closed:: *)
(*Integrating nested radicals*)


ClearAll[integrateNestedRadicals];

integrateNestedRadicals[e_, x_, u_] := Module[{integrand, subst, result},

	{integrand, subst} = decreaseNestedRadicals[e, x, u];
	debugPrint1["Using the substitution ", subst, " reduces the integral to ", integrand];
	result = solveAlgebraicIntegral[integrand, u];
	result = {0, result[[2]], Expand[ result[[1]] + result[[3]] ]} /. subst;
	result[[3]] = Collect[result[[3]], (Power[p_,_Rational] /; !FreeQ[p,x]) | _Log | _ArcTan | _ArcTanh | _RootSum, Together];

	rewriteNestedRadicals[
		simplify[result, x, "CancelRadicalDenominators" -> False], 
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
terms = ReverseSortBy[terms, LeafCount];
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
u==sub,
Dt[u==sub]//Together//Cancel
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
(*Derivative divides*)


(* ::Subsubsection::Closed:: *)
(*ranking integrands*)


linearPolynomialQ[p_, x_] := PolynomialQ[p, x] && Exponent[p,x] <= 1


linearRationalPolynomialQ[p_, x_] := 
	linearPolynomialQ[Numerator[p], x] && linearPolynomialQ[Denominator[p], x]


polynomialOrRationalQ[p_, x_] := PolynomialQ[p,x] || rationalQ[p, x]


ClearAll[distinctRadicalCount];

distinctRadicalCount[e_, x_] := distinctRadicalCount[e, x] = 
	Length[ Union[Cases[e, Power[r_, _Rational] /; ! FreeQ[r, x], {0, \[Infinity]}]] ]


(* ::Input:: *)
(*distinctRadicalCount[Sqrt[x] Sqrt[1-Sqrt[x^4-1]],x]*)


ClearAll[elementaryCount];

elementaryCount[e_, x_] := elementaryCount[e, x] = 
	Length @ Union[Cases[e // TrigToExp, Log[__]|Power[E, _], {0, Infinity}]]


ClearAll[specialCount];

specialCount[e_, x_] := specialCount[e, x] = Length[Union[
	Cases[e // TrigToExp, (h_)[__] /; 
		Context[h] === "System`" && ! MatchQ[h, Complex|Log|Exp|Plus|Times|Power|Rational], 
		{0, Infinity}]
	]]


(* ::Input:: *)
(*specialCount[BesselJ[2,x]-Sin[x]+f[x],x]*)


(* ::Input:: *)
(*specialCount[BesselJ[2,x]-AiryAi[x]-Log[x]Sin[x]+f[x],x]*)


(* ::Input:: *)
(*specialCount[Sqrt[1-x^4], x]*)


(* Estimate the difficulty of an integral. *)
ClearAll[simpleQ];
simpleQ[e_, x_] := TrueQ[rank[e, x] < 8] (* Always algorithmically integrable in elementary terms. *)

ClearAll[rank];
rank[e_, x_] /; PolynomialQ[e, x] := If[linearPolynomialQ[e, x], 1, 2]
rank[e_, x_] /; rationalQ[e, x] := If[linearRationalPolynomialQ[e, x], 3, 4]
rank[e_, x_] /; nestedCount[e, x] == 0 && rationalOfLinearRadicalsQ[e, x] := 5
rank[e_, x_] /; nestedCount[e, x] == 0 && rationalOfQuadraticRadicalsQ[e, x] := 6
rank[e_, x_] /; nestedCount[e, x] == 0 && linearRatioRadicalQ[e, x] := 7
(* TODO: Missing multiple linear radicals. *)

(* transcendental elementary. *)
rank[e_, x_] /; distinctRadicalCount[e, x] == 0 && 
	specialCount[e, x] == 0 && elementaryCount[e, x] > 0 := 8

(* transcendental special. *)
rank[e_, x_] /; distinctRadicalCount[e, x] == 0 && 
	specialCount[e, x] > 0 := 9 

(* (possibly nested) algebraic *)
rank[e_, x_] /; (distinctRadicalCount[e, x] > 0 || 
	nestedCount[e, x] > 0) && elementaryCount[e, x] == 0 && 
	specialCount[e, x] == 0 := 10 + 
								(2 distinctRadicalCount[e, x])^2 + 
								(4 nestedCount[e, x])^4

(* (possibly nested) elementary/special *)
rank[e_, x_] /; (distinctRadicalCount[e, x] > 0 || 
	nestedCount[e, x] > 0) && (elementaryCount[e, x] > 0 || 
	specialCount[e, x] > 0) := 11 + 
								(2 distinctRadicalCount[e, x])^2 + 
								(3 elementaryCount[e, x])^3 + 
								(4 nestedCount[e, x])^4 + 
								(5 specialCount[e, x])^5

(* something unforeseen?? *)
rank[e_, x_] := 10^20 LeafCount[e]


(* ::Input:: *)
(*rank[1,x]*)


(* ::Input:: *)
(*rank[3x-1,x]*)


(* ::Input:: *)
(*rank[3x^2-1,x]*)


(* ::Input:: *)
(*rank[(3x^2-1)/(1-x),x]*)


(* ::Input:: *)
(*rank[((3x^2-1)Sqrt[3+5x])/(1-x),x]*)


(* ::Input:: *)
(*rank[((3x^2-1)Sqrt[3+5x^2])/(1-x),x]*)


(* ::Input:: *)
(*rank[(Sin[x]-Cos[x])/(1+Cot[x]),x]*)


(* ::Input:: *)
(*rank[(Sin[x]-Cos[x])/BesselJ[2,x],x]*)


(* ::Input:: *)
(*rank[Sqrt[1-x^4],x]*)


(* ::Input:: *)
(*rank[Sqrt[1-Sqrt[x^4-1]],x]*)


(* ::Input:: *)
(*rank[Sqrt[x] Sqrt[1-Sqrt[x^4-1]],x]*)


(* ::Input:: *)
(*rank[(Sqrt[x] Sqrt[1-Sqrt[x^4-1]])/(1-x^4),x]*)


(* ::Input:: *)
(*rank[Sqrt[1-x^4]+Log[x],x]*)


(* ::Input:: *)
(*rank[Sqrt[1-x^4]+AiryAi[x],x]*)


(* ::Input:: *)
(*rank[Sqrt[1-x^4]+AiryAi[x]-Sin[x],x]*)


(* ::Input:: *)
(*rank[(3-9 u^2+2 u^3)/(2 u (1+u)^2 (-1+2 u) (1+2 u)Sqrt[(1-2 u)/(1+2 u)]),u]*)


(* ::Input:: *)
(*rank[(3-9 u^2+2 u^3)/(2 u (1+u)^2 (-1+2 u) (1+2 u)Sqrt[(1-2 u^4)/(1+3 u)]),u]*)


(* ::Input:: *)
(*rank[(3-9 u^2+2 u^3)/(2 u (1+u)^2 (-1+2 u) (1+2 u)Sqrt[(1-2 u^4)/(1+2 Sqrt[u])]),u]*)


(* ::Subsubsection::Closed:: *)
(*candidateSubstitutions*)


ClearAll[candidateSubstitutions];

Options[candidateSubstitutions] = {"SingleStepTimeConstraint" -> 0.25};

candidateSubstitutions[e_, x_, OptionsPattern[]] := Module[
{candidates, sorted, scores},

(* TODO. If it's cheap to Factor and Decompose the integrand, or 
subsets of the integrand, then we should probably do so and add 
these to the list of candidate substitutions. SAMB 0521 *)

candidates = Union @ Level[e, {0,\[Infinity]}];
candidates = If[MatchQ[#, a_ b_ /; FreeQ[a,x]], Last[#], #]& /@ candidates; (* Remove constant multiples. *)
candidates = If[MatchQ[#, x^n_Rational /; n < 0], 1/#, #]& /@ candidates;
candidates = Flatten[ 
		If[MatchQ[#, px_^_Rational /; polynomialOrRationalQ[px, x]], 
			{#, # /. px_^n_Rational :> px^(1/Denominator[n]),
				# /. px_^n_Rational :> px^(-1/Denominator[n])}, #]& /@ candidates ];
candidates = DeleteCases[candidates, x^n_Integer /; n < 0];
candidates = Cases[candidates, s_ /; !FreeQ[s,x]];
candidates = DeleteCases[candidates, x];
candidates = Select[candidates, LeafCount[#] < 4/5 LeafCount[e]&]; (* Only try _small_ substitutions (relative to the integrand) *)
candidates = Union[candidates, SameTest -> (FreeQ[#1/#2, x]&)];

scores = scoreCandidate[e, #, x] - 10^-3 LeafCount[#]& /@ Union[candidates]; (* Use LeafCount to break ties. *)
candidates = Transpose[{candidates, scores}];
sorted = ReverseSortBy[candidates, Last];
debugPrint3["Ranked/sorted candidates = ", sorted];
sorted[[All, 1]]
]


(* ::Input:: *)
(*candidateSubstitutions[(3-9 x^4+2 x^6)/(x (1+x^2)^2 (-1+2 x^2) Sqrt[(1-2 x^2)/(1+2 x^2)] (1+2 x^2)),x]//Timing*)


(* ::Subsubsection::Closed:: *)
(*scoreCandidate*)


(* ::Text:: *)
(*We would like a fast heuristic for sorting candidate substitutions for the derivative-divides routine so that more likely candidates are tried before others. scoreCandidate is a prototype that seems to perform reasonably. *)


scoreCandidate[e_, sub_, x_] := Module[{dd, splits, ranks},
dd = D[sub,x] // Together // Cancel;
If[FreeQ[dd, x], Return[0, Module]];(* We don't assign a score for linear polynomials. *)
splits = Union @ DeleteCases[Level[dd,{0,\[Infinity]}], x| (a_ x /; FreeQ[a,x]) |(a_ /; FreeQ[a,x])]; (* Remove x, a x and constants *)
splits = Union[splits, SameTest -> (FreeQ[#1/#2,x]&)];(* Remove copies of constant multiples. *)
If[splits === {}, Return[0, Module]];
ranks = (LeafCount[#]^2 (Count[e, #|1/#,{0,\[Infinity]}] /. 0 -> -1))& /@ splits; (* Many tests suggest this is a reasonable scoring function. *)
Mean[ranks] // N
]


(* ::Input:: *)
(*scoreCandidate[x^2/((1-2 x^3)^(1/3) (1+x^3)),x^3,x]//Timing*)


(* ::Subsubsection::Closed:: *)
(*derivdivides*)


(* Simple derivative-divides heuristic. *)
ClearAll[derivdivides];

Options[derivdivides] = {"SingleStepTimeConstraint" -> 0.25, "Candidates" -> Automatic};

derivdivides[e_, x_, u_, opts:OptionsPattern[]] := Module[
{candidates, diff, eu, euu, y, sys, eus, subs1, subs2, 
	a, b, eqns, special1, special2, special3, special, subx, gs},

(* Create a list of candidate substitutions. *)

If[OptionValue["Candidates"] === Automatic,
	candidates = candidateSubstitutions[e, x, opts],
	candidates = OptionValue["Candidates"]];
debugPrint3["Candidates for derivative-divides are ", candidates];

(* Computationally cheap checks first. *)

Do[
	TimeConstrained[
		subs1 = Table[sub^n -> u^n, {n, -16, 16}]; (* This is a hack, but speedy compared to Eliminate/Solve below. *)
		subs2 = Table[n sub -> n u, {n, -16, 16}]; (* Hack! *)
		diff = Cancel @ Together[D[sub,x]];
		eu = e //. subs1;
		eu = eu //. subs2; 
		eu = Cancel[Together[eu/diff]] //. subs1 //. subs2;
		debugPrint3["Trying sub = ", sub, ", giving ", eu, ", rank in = ", 
			rank[e, x],", rank out = ", rank[eu, u], ", size ratio = ", LeafCount[eu]/LeafCount[e] // N];
		If[FreeQ[eu, x] && (simpleQ[eu, u] || LeafCount[eu]/LeafCount[e] < 0.75 || rank[eu, u]/rank[e, x] < 0.9),
			debugPrint2["derivdivides level 1 returned: ", {eu, u -> sub}];
			Return[{eu, u -> sub}, Module]
		],
		OptionValue["SingleStepTimeConstraint"]
	],
{sub, candidates}];

(* Some special cases that we handle separately: u \[Rule] a x + b, u \[Rule] (a x + b)/(c x + d), 
	u \[Rule] (a x + b)^(1/n), u \[Rule] ((a x + b)/(c x + d))^(1/n). 

	TODO: u \[Rule] a x^(1/n) + b, u \[Rule] (a x^(1/n) + b)/(c x^(1/n) + d), 
			u \[Rule] (a x^(1/n) + b)^(1/m), u \[Rule] ((a x^(1/n) + b)/(c x^(1/n) + d))^(1/m) *)

special1 = Cases[candidates, p_ /; 
	(linearPolynomialQ[p,x] || linearRationalPolynomialQ[p,x])];
special2 = Cases[candidates, p_^n_Rational /; 
	(Numerator[n] == 1 && (linearPolynomialQ[p,x] || linearRationalPolynomialQ[p,x]))];
special3 = Cases[candidates, p_^n_Rational q_^m_Rational /; 
	(Numerator[n] == 1 && n + m == 0 && linearPolynomialQ[p,x] && linearPolynomialQ[q,x])];
special  = Join[special3, special2, special1];

Do[
	TimeConstrained[
		subs1 = Table[sub^n -> u^n, {n, -16, 16}]; 
		subs2 = Table[n sub -> n u, {n, -16, 16}];
		diff = Cancel @ Together[D[sub,x]];
		eu = e //. subs1 //. subs2;
		subx = Solve[u == sub, x]; (* Inverse function for the candidate substitution. *)
		If[Length[subx] > 1, Continue[]]; (* Something went wrong. (Special candidates should only have a single solution.) *)
		eu = eu //. subx[[1]];
		eu = (Cancel[Together[eu/diff]] //. subs1 //. subs2 //. subx[[1]]);
		debugPrint3["Trying sub = ", sub, ", giving ", eu, ", rank in = ", 
			rank[e, x],", rank out = ", rank[eu, u], ", size ratio = ", LeafCount[eu]/LeafCount[e] // N];
		If[FreeQ[eu, x] && (simpleQ[eu, u] || 
				LeafCount[eu]/LeafCount[e] < 0.75 || 
				rank[eu, u]/rank[e, x] < 0.9),
			debugPrint2["derivdivides level 2 returned: ", {eu, u -> sub}];
			Return[{eu, u -> sub}, Module]
		],
		OptionValue["SingleStepTimeConstraint"]
	],
{sub, special}];

(* TODO: Should we try computing all inverse functions of the u-substitution with Solve and 
test them one-by-one? Are there (reasonable) examples where that approach succeeds and the 
Groebner basis-based elimination methods below fails? Perhaps for functions which are not 
in $allowableFunctions, but invertible nonetheless. SAMB 0621 *)

(* Try the GroebnerBasis-based elimination method, with a tight time constraint. *)

Do[
	TimeConstrained[
		gs = groebnerSubstitute[e, u -> sub, x];
		If[ListQ[gs], 
			eu = First[gs];
			debugPrint3["sub = ", sub, ", giving ", eu, ", rank in = ", 
				rank[e, x],", rank out = ", rank[eu, u], ", size ratio = ", LeafCount[eu]/LeafCount[e] // N];
			If[simpleQ[eu, u] || LeafCount[eu]/LeafCount[e] < 0.75 || rank[eu, u]/rank[e, x] < 0.9,
				debugPrint2["derivdivides level 3 returned: ", {eu, u -> sub}];
				Return[{eu, u -> sub}, Module]
			]
		],
		OptionValue["SingleStepTimeConstraint"]
	],
{sub, candidates}];

False
]


(* ::Input:: *)
(*Timing[derivdivides[x^2/((1-2 x^3)^(1/3) (1+x^3)),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[Sqrt[1+Sqrt[a x+b]]/(a x+b),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[Sqrt[1+Sqrt[a x+b]]/(a^2 x^2-b^2),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[x/(1-a x^2),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[Log[x]/(x (1-a Log[x]^2)),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[((-27+x^3) (27+x^3)^(1/3))/(81 x),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[1/(x (1-2 x^4)^(1/4) (1+x^4)),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[Sqrt[x]/Sqrt[Sqrt[x]+1],x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[((-2+x^3) Sqrt[1-x^2+x^3])/(1+x^3)^2,x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[(3-9 x^4+2 x^6)/(x (1+x^2)^2 (-1+2 x^2) Sqrt[(1-2 x^2)/(1+2 x^2)] (1+2 x^2)),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[1/((C[2] x^2+C[3]) Sqrt[(C[4] x+C[5])/(C[6] x+C[7])]),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[(x (-1+x^2))/((1+x^2)^3 Sqrt[1+4/3 (x/(x^2+1))^2+Sqrt[1+4/3 (x/(x^2+1))^2]]),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[3/(8 Sqrt[Sqrt[u]+u]),u,t]]*)


(* ::Input:: *)
(*Timing[derivdivides[Sqrt[1+Sqrt[a x+b]]/(a x-b),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[Sqrt[b-a x+Sqrt[a x+b]]/(a x-b),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[(-b c+a d)/(Sqrt[Sqrt[(a x+b)/(c x+d)]-(a x+b)/(c x+d)] (d+c x)^2),x,u]]*)


(* ::Input:: *)
(*derivdivides[a/(((a x+b)^2 Sqrt[1-(a x+b)^(1/3)]) (3 (b+a x)^(2/3))),x,u]*)
(*derivdivides[%[[1]],u,t]*)


(* ::Input:: *)
(*derivdivides[Sqrt[1+Sqrt[a x^2+b]]/((a x^2-b) x),x,u]*)
(*derivdivides[%[[1]],u,t]*)


(* ::Input:: *)
(*Timing[derivdivides[Sqrt[1+Sqrt[a x+b]]/(a^2 x^2-b^2),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[((Sqrt[-1+x+Sqrt[x-Log[x]]]+Sqrt[1+x+Sqrt[x-Log[x]]]) (-1+x+2 x Sqrt[x-Log[x]]))/(x Sqrt[-1+x+Sqrt[x-Log[x]]] Sqrt[1+x+Sqrt[x-Log[x]]] Sqrt[x-Log[x]]),x,u]]*)


(* ::Input:: *)
(*derivdivides[(1+x^2)/(2 x^(5/2)),x,u]*)


(* ::Input:: *)
(*Timing[derivdivides[(Sqrt[x-Sqrt[Log[x]]]-Sqrt[1-x-Sqrt[x-Sqrt[Log[x]]]+Sqrt[Log[x]]]) (1-1/(2 x Sqrt[Log[x]])),x,u]]*)


(* ::Input:: *)
(*Timing[derivdivides[(1/Sqrt[1-Sqrt[x Cos[x]-Sqrt[Sin[x]]]]-Sqrt[1-x Cos[x]-Sqrt[x Cos[x]-Sqrt[Sin[x]]]+Sqrt[Sin[x]]]) (Cos[x]-Cos[x]/(2 Sqrt[Sin[x]])-x Sin[x]),x,u]]*)


(* ::Input:: *)
(*derivdivides[(1-3 x^4)^(1/3)/x,x,u]*)
(*derivdivides[%[[1]],u,t]*)


(* ::Input:: *)
(*derivdivides[((x-Sqrt[Log[1+x^4]]) (-2 x^3+Sqrt[Log[1+x^4]]+x^4 Sqrt[Log[1+x^4]]))/((1+x^4) Sqrt[Log[1+x^4]] (-1+x^3-3 x^2 Sqrt[Log[1+x^4]]+3 x Log[1+x^4]-Log[1+x^4]^(3/2))),x,u]*)


(* ::Input:: *)
(*derivdivides[x^3/((1+x^4) (-1+Sqrt[Log[1+x^4]]) (1+Sqrt[Log[1+x^4]]) Sqrt[Log[1+x^4]]),x,u]*)
(*derivdivides[First[%],u,t]*)


(* ::Input:: *)
(*derivdivides[E^(x^4/(-1+x)) (1-E^((2 x^4)/(-1+x))/Sqrt[1+E^((2 x^4)/(-1+x))]) ((4 x^3)/(-1+x)-x^4/(-1+x)^2),x,u]*)


(* ::Input:: *)
(*derivdivides[x/Sqrt[x^4-1],x,u]*)


(* ::Input:: *)
(*derivdivides[((2 r) Exp[-((Sqrt[a^2-r^2]-Sqrt[b^2-r^2])/\[Lambda])])/c^2,r,u]*)


(* ::Input:: *)
(*derivdivides[Sqrt[1+x^2]/(x Sqrt[1-ArcSinh[x]]),x,u]*)


(* ::Subsection::Closed:: *)
(*Groebner basis-based elimination*)


ClearAll[$allowableFunctions];

$allowableFunctions = {Log,Exp,ExpIntegralEi,ExpIntegralE,LogIntegral,
PolyLog,FresnelC,FresnelS,SinIntegral,CosIntegral,CoshIntegral,
SinhIntegral,Erf,Erfc,Erfi,ProductLog,EllipticK,EllipticE,EllipticPi};


ClearAll[groebnerSubstitute];

groebnerSubstitute[e_, u_ -> sub_, x_] := Module[
{k = 0, et, subt, y, fns, vs, ddvs, eqns, rules, allvars, vlist, elimvars, gb, ufns, tic},
(* TODO: investigate converting Sin and Cos to Tan instead of their exponential form. (Similarly, 
for Sinh and Cosh to Tanh) *)
et = TrigToExp[e];
subt = TrigToExp[sub]; 
fns = Union @ Cases[et,
		(h_[args__] /; ! FreeQ[{args}, x] && MemberQ[$allowableFunctions, h]) | (Power[E,arg_] /; ! FreeQ[arg, x]), 
		{0, Infinity}];(* Collect all distinct functions. *)
vs = With[{sym = Unique["V"]}, {sym, #}]& /@ fns;(* Assign each function with a unique internal variable. *)
rules = Rule @@@ Map[Reverse,vs];
ddvs = Dt[Equal @@@ vs]; (* Construct a system of differentials associated with the functions. *)
ddvs = ddvs //. {subt -> u, 1/subt -> 1/u};
eqns = {
	u == subt, 
	Dt[y] == et Dt[x] //. {subt -> u, 1/subt -> 1/u}, 
	Dt[u] == (D[subt,x] //. {subt -> u, 1/subt -> 1/u}) Dt[x]
	};
eqns = Join[eqns, ddvs];
eqns = eqns //. Join[rules];
allvars = Join[{x,u,y}, vs[[All,1]]];
eqns = eqns/. HoldPattern[Dt][Except[Alternatives @@ allvars]] -> 0; (* Constants/parameters. *)
debugPrint3["System for groebnerSubstitute is ",eqns];
vlist = Flatten[ {#,Dt[#]}& /@ vs[[All,1]] ];
elimvars = Join[{Dt[x],x},vlist];
tic = AbsoluteTime[];
gb = GroebnerBasis[eqns, {Dt[u],u}, elimvars, MonomialOrder -> EliminationOrder, Method -> "Buchberger"];
debugPrint3["Time elapsed in GroebnerBasis = ", AbsoluteTime[]-tic];
If[gb == {}, Return[ False ]];
gb = Factor[gb];
ufns = Solve[gb[[1]] == 0, Dt[y]];
If[MatchQ[ufns, {}|{{}}], Return[ False ]];
ufns = Factor[ufns /. Dt[u] -> 1]; (* This is faster than dividing by Dt[u] and cancelling. *)
ufns = Cancel @ Together[(Dt[y] /. ufns)];
debugPrint3["Candidate integrands in ", u, " are ",ufns];
Do[
If[FreeQ[ufn, x] && PossibleZeroQ[Cancel[Together[et - (ufn D[subt,x] /. u -> subt)]]],
	Return[{ufn, u -> sub}, Module]
],{ufn, ufns}];

(* Sometimes we need PowerExpand (why?). *)
ufns = PowerExpand[Factor //@ ufns];
debugPrint3["Candidate integrands in ", u, " are ",ufns];
Do[
If[FreeQ[ufn, x] && PossibleZeroQ[Cancel[Together[et - (ufn D[subt,x] /. u -> subt)]]],
	Return[{ufn, u -> sub}, Module]
],{ufn, ufns}];

(* More perplexingly, sometimes we need it twice. It would be good to 
sort this out, so we do not require 3 attempts to verify the substitution. *)
ufns = PowerExpand[Factor //@ ufns];
debugPrint3["Candidate integrands in ", u, " are ",ufns];
Do[
If[FreeQ[ufn, x] && PossibleZeroQ[Cancel[Together[et - (ufn D[subt,x] /. u -> subt)]]],
	Return[{ufn, u -> subt}, Module]
],{ufn,ufns}];

False
]


(* ::Input:: *)
(*Sqrt[Log[Exp[x]+x-Log[x]]+ProductLog[x]];*)
(*integrand=1/(u^4+1) D[%,x]/.u->%//Cancel*)
(*groebnerSubstitute[integrand,u->Sqrt[Log[Exp[x]+x-Log[x]]+ProductLog[x]],x]//Timing*)
(*Clear[integrand]*)


(* ::Input:: *)
(*Sqrt[x+ProductLog[x]];*)
(*integrand=1/(u^4-u+1) D[%,x]/.u->%//Cancel*)
(*groebnerSubstitute[integrand,u->Sqrt[x+ProductLog[x]],x]//Timing*)
(*Clear[integrand]*)


(* ::Input:: *)
(*groebnerSubstitute[(ProductLog[x]-2 Sqrt[1+ProductLog[x]]-2 ProductLog[x] Sqrt[1+ProductLog[x]])/(2 x (1+ProductLog[x])^(3/2) (-Log[x]^2-ProductLog[x]+2 Log[x] Sqrt[1+ProductLog[x]])),u->Log[x]-Sqrt[1+ProductLog[x]],x]//Timing (* But how do we guess the substitution? *)*)


(* ::Input:: *)
(*Sqrt[a Exp[x]-Log[x]]*)
(*integrand=((u^2-1)/(u^2+1)/Sqrt[1+u^4])D[%,x]/.u->%*)
(*groebnerSubstitute[integrand,u->Sqrt[a E^x-Log[x]],x]//Timing*)
(*Clear[integrand]*)


(* ::Input:: *)
(*Sqrt[1-Sqrt[Exp[x]-Log[x]]];*)
(*integrand=((u^2-1)/(u^2+1)/Sqrt[1+u^4])D[%,x]/.u->%*)
(*groebnerSubstitute[integrand,u->Sqrt[1-Sqrt[Exp[x]-Log[x]]],x]//Timing*)
(*Clear[integrand]*)


(* ::Input:: *)
(*Sqrt[Log[2x^2-1]-Sqrt[Exp[x]-Log[x]]];*)
(*integrand=(Sqrt[u-Sqrt[u^2+1]])D[%,x]/.u->%*)
(*groebnerSubstitute[integrand,u->Sqrt[Log[2x^2-1]-Sqrt[Exp[x]-Log[x]]],x]//Timing*)
(*Clear[integrand]*)


(* ::Input:: *)
(*Sqrt[Sqrt[1-Log[2x^2-1]]-Sqrt[Exp[x]-Log[x]]]*)
(*integrand=(Sqrt[u-Sqrt[u^2+1]])D[%,x]/.u->%*)
(*groebnerSubstitute[integrand,u->Sqrt[Sqrt[1-Log[2x^2-1]]-Sqrt[Exp[x]-Log[x]]],x]//Timing (* It would be nice to know why this example takes so much longer than the last example. *)*)
(*Clear[integrand]*)
(*(* If we introduce more intermediate variables, we can significantly speed things up. However, is this repeatable and always faster?  *)*)
(*{Dt[y]==Cancel[(Sqrt[u-Sqrt[1-v4+v5]] (-((v2-1/x)/(2 v4))-(2 x)/(v5 (-1+2 x^2))))/u] Dt[x],u==Sqrt[-v4+v5],Dt[u]==(Dt[v5]-Dt[v4])/(2 u),Dt[v1]==Dt[x]/x,Dt[v2]==v2 Dt[x],Dt[v3]==(4 x Dt[x])/(-1+2 x^2),Dt[v4]==(v2 Dt[x]-Dt[x]/x)/(2 v4),Dt[v5]==-((2 x Dt[x])/(v5 (-1+2 x^2)))}*)
(*GroebnerBasis[%,*)
(*{Dt[u],u},{Dt[x],x,v1,v2,v3,v4,v5,v6,Dt[v1],Dt[v2],Dt[v3],Dt[v4],Dt[v5],Dt[v6]}, MonomialOrder -> EliminationOrder, Method -> "Buchberger"]//Timing*)
(*Solve[%[[-1,1]]==0,Dt[y]]/.Dt[u]->1*)


(* ::Input:: *)
(*Sqrt[x^2-1-Sqrt[x^4-3x^2+1]]*)
(*integrand=(1/u/(u^2+1)/Sqrt[1+u])D[%,x]/.u->%//Cancel*)
(*groebnerSubstitute[integrand,u->Sqrt[x^2-1-Sqrt[x^4-3x^2+1]],x]//Timing*)
(*Clear[integrand]*)


(* ::Input:: *)
(*Power[1-Sin[x], (4)^-1]*)
(*integrand=-4(1/Power[1+u^4, (4)^-1])D[%,x]/.u->%//Cancel*)
(*groebnerSubstitute[integrand,u->Power[1-Sin[x], (4)^-1],x]//Timing*)
(*Clear[integrand]*)


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
	
	{rationalPart, unintegratedPart, simplify[integratedPart // Expand, x]}
]


(* ::Subsection::Closed:: *)
(*simplify*)


Clear[togetherAll];

togetherAll[e_] /; FreeQ[e, RootSum] := Map[Together, e, {2, Infinity}]
togetherAll[e_] := e


Clear[lessAggressivePowerExpand];

lessAggressivePowerExpand[e_] := e //. Power[a_ b_^n_Integer, r_Rational] /; 
	n < 0 && Mod[n, Denominator[r]] == 0 :> (a^r) (b^(n r)) 


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

	Cancel[Together @ simp]
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

collect[e_, x_] := Collect[e, 
	(r_^_Rational /; !FreeQ[r,x])|_Log|_ArcTan|_ArcTanh|_$rootSum, 
	Together[RootReduce[#] // ToRadicals]&]


ClearAll[collectnumden];

collectnumden[e_] := With[{te = Together[e]}, 
	Collect[Numerator[te], Power[_, _Rational]]/Collect[Denominator[te], Power[_, _Rational]]
]


ClearAll[matchRadicals];

matchRadicals[e_, None, x_] := e

matchRadicals[e_, integrand_, x_] := Module[{intrad, erad, reps, rem, quot},

erad = Cases[e, Power[p_,r_Rational] /; (! FreeQ[p, x] && PolynomialQ[p, x]) :> {p, r//Abs}, {0,\[Infinity]}];
intrad = Cases[integrand, Power[p_,r_Rational] /; (! FreeQ[p, x] && PolynomialQ[p, x]) :> {p, r//Abs}, {0,\[Infinity]}];

reps = {};
Do[
	Do[
		If[! CoprimeQ[Denominator[Last[rad]], Denominator[Last[match]]],
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
	If[Head[simp] === Plus, 
		const = Cases[simp, n_ /; FreeQ[n, x], {1}];
		simp -= Total[const];
	];
	
	simp
]


(* ::Text:: *)
(*simplify is not only for asthetic reasons. We also attempt to correct for the substitution taking a branch of the radical. *)
(**)
(*TODO: make logands monic.*)


Clear[simplify];

Options[simplify] = {"Integrand" -> None, "CancelRadicalDenominators" -> True};

simplify[l_List, x_, opts:OptionsPattern[]] := Map[simplify[#, x, opts]&, l]

simplify[e_, x_, OptionsPattern[]] := Module[
	{$function, simp, permutations, denomP, rad, rationalTerms, nonRationalTerms, 
	rationalTermsMerged},
	
	simp = e /. {RootSum -> $rootSum, Function -> $function};
	
	simp = simp /. c_ p_Plus /; FreeQ[c, x] :> Distribute[c p, Plus, Times];

	(* Remove constants. *)
	simp = stripConst[simp, x];
	
	simp = simp /. (h:Sin|Cos|Tan|Cot|Sec|Csc)[Pi r_Rational] :> FunctionExpand[h[Pi r]];
	simp = simp /. (h : ArcSinh | ArcCosh | ArcSin | ArcCos)[a_] :> TrigToExp[h[a]];
	
	(* The order of the next three lines is important, for example 
		int[(x Sqrt[x^4 - x^2])/(-3 + 2 x^2), x]
    we don't want to write Sqrt[x^4 - x^2] as x Sqrt[x^2 - 1]. *)
	simp = simp // togetherAll;
	simp = simp /. Power[p_, r_Rational] /; PolynomialQ[p, x] :> Expand[p]^r;
	simp = simp // togetherAll;
	
	If[OptionValue["CancelRadicalDenominators"],
		simp = simp // lessAggressivePowerExpand // togetherAll,
		simp = simp // togetherAll
	];
	
	(* Some examples for the following rule:
		int[((1 + x^6)*Sqrt[-x - x^4 + x^7])/(1 + 2*x^3 - 2*x^9 + x^12), x]
		int[((-x + x^3)^(1/3)*(-2 + x^4))/(x^4*(1 + x^2)), x]
		int[((-1 + x^2)*(x^2 + x^6)^(1/4))/(x^2*(1 + x^2)), x] *)

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

	(* Another simplification to cancel logarithms. *)
	simp = simp /. Log[ex_^(n_Integer|n_Rational)] :> n Log[ex];
	simp = collect[simp, x];

	simp = simp /. (h:Log|ArcTan|ArcTanh)[arg_] :> h[collectnumden @ canonic @ arg]; (* Yes, we have to do this twice. *)
	simp = simp /. Log[ex_] :> Log[Collect[ex, Power[_, _Rational]]];

	(* Remove constant multiples in logands. *)
	simp = simp /. Log[logand_] /; FactorSquareFreeList[logand][[1]] =!= {1,1} :> 
		Log[ Apply[Times, Power @@@ Rest[FactorSquareFreeList[logand]]] ];

	simp = simp /. Log[ex_^(n_Integer|n_Rational)] :> n Log[ex]; (* Yes, using this one twice as well. *)
	simp = collect[simp, x];

	simp = simp //. log2ArcTanh;
	simp = simp //. {arcTanDiff, arcTanSum, arcTanhDiff, arcTanhSum};

	simp = simp /. (h:ArcTan|ArcTanh)[a_] :> h[collectnumden @ canonic[a]];

	(* Pick the nicer of ArcTan[a/b] or -ArcTan[b/a]. *)
	simp = simp /. ArcTan[a_] /; nicerQ[Numerator[a], Denominator[a], x] :> -ArcTan[collectnumden @ canonic[Denominator[a]/Numerator[a]]];

	(* Pick the nicer of ArcTanh[a/b] or ArcTanh[b/a]. *)
	simp = simp /. ArcTanh[a_] /; nicerQ[Numerator[a], Denominator[a], x] :> ArcTanh[collectnumden @ canonic[Denominator[a]/Numerator[a]]];

	simp = collect[simp, x] /. Power[p_, r_Rational] /; PolynomialQ[p, x] :> Expand[p]^r;
	simp = simp /. p_ /; PolynomialQ[p,x] :> Collect[p, x];
	simp = simp /. Log[ex_] :> Log[Collect[ex, Power[_, _Rational]]];
	simp = simp /. Log[ex_^(n_Integer|n_Rational)] :> n Log[ex];

	(* Try merging the rational part of the integral. *)
	simp = simp /. c_ p_Plus /; FreeQ[c, x] :> Distribute[c p, Plus, Times];

	If[Head[simp] === Plus, 
		simp = List @@ simp;
		rationalTerms = Cases[simp, expr_ /; algebraicQ[expr, x], {1}];
		nonRationalTerms = Plus @@ Complement[simp, rationalTerms];
		rationalTerms = Plus @@ rationalTerms;
		rationalTermsMerged = collectnumden[ Simplify[ rationalTerms ] ]; (* Simplify added. SAMB 0521 *)
		rationalTermsMerged = rationalTermsMerged /. Power[p_, r_Rational] /; PolynomialQ[p, x] :> Expand[p]^r;
		simp = nonRationalTerms + If[LeafCount[rationalTermsMerged] < LeafCount[rationalTerms],
			rationalTermsMerged,
			rationalTerms
		]
	];

	simp = simp /. {$rootSum -> RootSum, $function -> Function};

	(* Convert to radicals, if the resulting expression is reasonable. *)
	rad = ToRadicals[simp];
	If[LeafCount[rad] < LeafCount[simp], 
		simp = rad
	];

	(* Remove constants. *)
	simp = stripConst[simp, x];
	simp
]


(* ::Input:: *)
(*simplify[((1+2 x^2) Sqrt[-1+(1+2 x^2)^2/(-1+2 x)^2])/(8 (-1+2 x))-1/4 ArcTanh[Sqrt[-1+(1+2 x^2)^2/(-1+2 x)^2]/(1+(1+2 x^2)/(-1+2 x))],x,"Integrand"->((-1-2 x+2 x^2) Sqrt[x+x^4])/(-1+2 x)^3]*)


(* ::Input:: *)
(*simplify[2 (-((x^2 Sqrt[-1+(1-x^3+x^6)/x^2])/(2 (1-x^3+x^6)))+3/2 ArcTan[Sqrt[-1+(1-x^3+x^6)/x^2]]-ArcTan[Sqrt[-1+(1-x^3+x^6)/x^2]/Sqrt[2]]/Sqrt[2]),x]//Timing*)


(* ::Input:: *)
(*simplify[Log[(1+x^8)/x]-Log[2-(1+x^8)/x+2 Sqrt[1-(1+x^8)/x+(3 (1+x^8)^2)/x^2]],x]*)


(* ::Input:: *)
(*simplify[-(ArcTan[(-((-1-x^2)/x)+Sqrt[-3+(-1-x^2)^2/x^2])/Sqrt[3]]/(2 Sqrt[3]))+1/2 ArcTan[(2 (-((-1-x^2)/x)+Sqrt[-3+(-1-x^2)^2/x^2]))/(-3+(-((-1-x^2)/x)+Sqrt[-3+(-1-x^2)^2/x^2])^2)]+1/8 Log[1+(-1-x^2)/x-Sqrt[-3+(-1-x^2)^2/x^2]]-1/8 Log[3+(-1-x^2)/x-Sqrt[-3+(-1-x^2)^2/x^2]]-1/8 Log[1-(-1-x^2)/x+Sqrt[-3+(-1-x^2)^2/x^2]]+1/8 Log[3-(-1-x^2)/x+Sqrt[-3+(-1-x^2)^2/x^2]],x]*)


(* ::Input:: *)
(*simplify[(4 (-1+x^4)^(1/4)-4 x^4 (-1+x^4)^(1/4)+5 x^5 RootSum[2-2 #1^4+#1^8&,(-Log[x] #1+Log[(-1+x^4)^(1/4)-x #1] #1)/(-1+#1^4)&])/(20 x^5),x]*)


(* ::Input:: *)
(*simplify[*)
(*Log[1-3 (-x+Sqrt[-1+x^2])-(-x+Sqrt[-1+x^2])^2-(-x+Sqrt[-1+x^2])^3]-Log[1-x+Sqrt[-1+x^2]+3 (-x+Sqrt[-1+x^2])^2-(-x+Sqrt[-1+x^2])^3],x]*)


(* ::Input:: *)
(*2 Log[2+Sqrt[6] u]-Log[4-2 Sqrt[6] u+6 u^2]/.u->1/(x (x+x^6)^(1/3))*)
(*simplify[%,x]*)


(* ::Input:: *)
(*simplify[1/120 (180 ((1+x)/x)^(2/3)-144 ((1+x)/x)^(5/3)+45 ((1+x)/x)^(8/3)+20 2^(2/3) Sqrt[3] ArcTan[(1+2^(2/3) ((1+x)/x)^(1/3))/Sqrt[3]]+20 2^(2/3) Log[2-2^(2/3) ((1+x)/x)^(1/3)]-10 2^(2/3) Log[2+2^(2/3) ((1+x)/x)^(1/3)+2^(1/3) ((1+x)/x)^(2/3)]+40 RootSum[1-#1^3+#1^6&,Log[((1+x)/x)^(1/3)-#1]/#1&]),x]*)


(* ::Input:: *)
(*simplify[1/3 (Sqrt[-1+(-1-x^2)^2/x^2] (2+(-1-x^2)^2/x^2)-3 Sqrt[2] ArcTanh[Sqrt[-1+(-1-x^2)^2/x^2]/Sqrt[2]]),x]*)


(* ::Input:: *)
(*simplify[Sqrt[(1+x^2)/x]/(1+(1+x^2)/x)+ArcTan[Sqrt[(1+x^2)/x]],x]*)


(* ::Input:: *)
(*simplify[(Sqrt[2] Sqrt[1-x^6])/(5 x^5)-(Sqrt[2] Sqrt[1-x^6])/(3 x^3)-2/5 Sqrt[2] x Sqrt[1-x^6]+1/3 Sqrt[2] x^3 Sqrt[1-x^6]+1/5 Sqrt[2] x^7 Sqrt[1-x^6],x]*)


(* ::Input:: *)
(*simplify[-((4 Sqrt[-2+2 x^5-x^7+x^8])/(3 x^6))+(6 Sqrt[-2+2 x^5-x^7+x^8])/x^2+(4 Sqrt[-2+2 x^5-x^7+x^8])/(3 x)-2/3 x Sqrt[-2+2 x^5-x^7+x^8]+2/3 x^2 Sqrt[-2+2 x^5-x^7+x^8]+3 Log[1-Sqrt[-2+2 x^5-x^7+x^8]/x^2]-3 Log[1+Sqrt[-2+2 x^5-x^7+x^8]/x^2],x]*)


(* ::Subsection::Closed:: *)
(*verifySolution*)


ClearAll[verifySolution];

verifySolution[integral_, integrand_, x_] := Module[
	{dd, tdd},
	debugPrint2["Verifying the integrand, integral: ", integrand, ",", integral];
	If[TrueQ[integral == 0] || TrueQ[integrand == 0], Return[ False ]];
	dd = D[integral, x];
	tdd = Cancel @ Together[dd - integrand];
	(* Order tests from fastest to slowest. *)
	debugPrint2["Result of verification is ", tdd];
	TrueQ[
		tdd === 0 ||
		numericZeroQ[tdd] ||
		PossibleZeroQ[tdd] || 
		PossibleZeroQ[D[Simplify[tdd], x]]
	]
]


(* ::Subsubsection::Closed:: *)
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
(*utilities*)


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
(*elementaryQ*)


elementaryQ[expr_] := Complement[
	Cases[Level[expr // TrigToExp, {-1}, Heads -> True], s_Symbol /; Context[s] === "System`"] // Union, 
	{Log, Exp, Plus, Times, Power, RootSum, Root, List, Function, Slot, C, Pi, E}
] === {}


(* ::Subsubsection::Closed:: *)
(*rationalQ*)


rationalQ[e_, x_] := With[
    {te = Together[e]}, 
    Denominator[te] =!= 1 && PolynomialQ[Numerator[te], x] && PolynomialQ[Denominator[te], x]
]


(* ::Input:: *)
(*rationalQ[1,x]*)


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
(*singleRadicalQ*)


singleRadicalQ[e_, x_] := 
	Length[Union[Cases[e, Power[p_, r_Rational] /; (! FreeQ[p, x] && (PolynomialQ[p, x] || rationalQ[p, x])), {0, Infinity}]]] === 1


(* ::Subsubsection::Closed:: *)
(*algebraicQ*)


algebraicQ[e_, x_Symbol] := Complement[
Cases[e, s_Symbol /; (Context[s] === "System`" && !NumericQ[s]), {-1}, Heads -> True],
{Plus, Times, Power, C, x}] === {} && 
Cases[e, Power[p_, q_] /; (! FreeQ[p,x] && ! MatchQ[Head[q], Integer|Rational]), {0, Infinity}] === {} && 
Cases[e, Power[p_, q_] /; (FreeQ[p,x] && !FreeQ[q,x]), {0, Infinity}] === {}


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


(* ::Input:: *)
(*algebraicQ[(E^(1/(E^x+x)+(-1+x^2)/x) (E^x+2 x-x^2))/(x^2 (E^x+x)^2),x]*)


(* ::Subsubsection::Closed:: *)
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
(*End Package*)


End[];
EndPackage[];


(* ::Section:: *)
(*Examples, wish list, bugs/deficiencies, previously bugs, edge cases, regression tests, ...*)


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


(* ::Input:: *)
(*int[(x-1)/((-3+x) (x^3-x^2-x+1)^(1/3)),x]*)


(* ::Text:: *)
(*We allow symbolic summation over the roots of a polynomial in the results.*)


(* ::Input:: *)
(*int[((x^8-1) (x^4-1)^(1/4))/(x^6 (x^8+1)),x]*)


(* ::Text:: *)
(*A difficult integral which was solved by Euler in 1777.*)


(* ::Input:: *)
(*int[(1-x^2)^2/((x^2+1) (x^4+6 x^2+1)^(3/4)),x]*)


(* ::Text:: *)
(*Some integrals with parameters which cause Risch-Trager-Bronstein to hang can be handled.*)


(* ::Input:: *)
(*int[(2 a^2 x^4-b^2)/((a^2 x^8-b^2) (a^2 x^4-b^2)^(1/4)),x]*)


(* ::Text:: *)
(*An example which AXIOM and Maple can compute, but FriCAS claims is not elementary.*)


(* ::Input:: *)
(*int[((-1+3 x^4) Sqrt[1+x+2 x^4+x^5+x^8])/(x^2 (4+x+4 x^4)),x]*)


(* ::Text:: *)
(*An example which Maple 2018 (with RootOf conversion) cannot compute as the radicand factors.*)


(* ::Input:: *)
(*int[((x^3+1) Sqrt[x^6-x^3-2])/(x^4 (x^6-2 x^3-1)),x]*)


(* ::Text:: *)
(*An example that AXIOM (August, 2014) claims is not elementary.*)


(* ::Input:: *)
(*int[(x^12-1)/((x^12+1) Sqrt[x^4+1]),x]*)


(* ::Text:: *)
(*We can now handle some integrals with rational radicands. The following example is computed with the generalised Gunther substitution method:*)


(* ::Input:: *)
(*int[(t^2+1)/((t^4-m t^2+1) Sqrt[(2 t^2-t-2)/(t^2+t-1)]),t]*)


(* ::Text:: *)
(*And this example is computed using the Laurent polynomial substitution code*)


(* ::Input:: *)
(*int[((1+3 x^4) (-a-b x+a x^4) ((-c-d x+c x^4)/(-e-f x+e x^4))^(1/3))/(x^2 (-a+b x+a x^4)),x]*)


(* ::Text:: *)
(*We can now handle some integrals with multiple distinct radicals (providing we can split them up and integrate term-by-term). For example*)


(* ::Input:: *)
(*int[((x^4+1) Sqrt[x^4-1]+(x^4-1) Sqrt[x^4+1])/(x^8+1),x]*)


(* ::Text:: *)
(*We now have a reasonable derivative-divides algorithm which can compute many otherwise difficult integrals. For example *)


(* ::Input:: *)
(*int[((-1+8 Sqrt[1-x] x^3+x^4) Sqrt[1-Sqrt[1-x]+Log[1-x^4]])/(2 Sqrt[1-x] (-1+x^4) Sqrt[1+Sqrt[1-x]-Log[1-x^4]]),x]*)


(* ::Text:: *)
(*We can now compute integrals which require factoring the radicand, integrating after introducing branch cuts, then correcting for these branch cuts without resorting to piecewise constants. For example*)


(* ::Input:: *)
(*int[Sqrt[-1-11 x-36 x^2-27 x^3+16 x^4+9 x^5+x^6],x]*)


(* ::Subsection::Closed:: *)
(*current bugs and deficiencies*)


(* ::Text:: *)
(*I'm not sure if we should be able to do this one. If we can compute the rational part of the integral, then we can do the logarithmic part:*)


(* ::Text:: *)
(*[samb@spartan - login2 ~] $ maple*)
(*    | \^ / |     Maple 2018 (X86 64 LINUX)*)
(*._ | \|   | /| _. Copyright (c) Maplesoft, a division of Waterloo Maple Inc. 2018*)
(* \  MAPLE  /  All rights reserved. Maple is a trademark of*)
(* < ___ _ ___ _ >  Waterloo Maple Inc.*)
(*        |       Type ? for help.*)
(*   > int (x/(x^3 + x^2 - x - 1)^(1/3), x);     *)
(*memory used = 4.1 MB, alloc = 40.3 MB, time = 0.14*)
(*                                                          /*)
(*                                      (x - 1) (x + 1)       |                1*)
(*                                --------------------- +  |  - ------------------------- dx*)
(*                                                2 1/3    |                      2 (1/3)*)
(*                                ((x - 1) (x + 1) )      /     3 ((x - 1) (x + 1) )*)
(**)
(*> int (convert (x/(x^3 + x^2 - x - 1)^(1/3), RootOf), x);*)
(**)


(* ::Input:: *)
(*int[x/(x^3+x^2-x-1)^(1/3),x]*)
(*int[1/(3 (x^3+x^2-x-1)^(1/3)),x]*)


(* ::Text:: *)
(*This example takes far too long in verifying solutions. *)


(* ::Input:: *)
(*Timing[int[((-(E^(-1+x^4)/(2 Sqrt[1-x]))+4 E^(-1+x^4) Sqrt[1-x] x^3) Sqrt[1-E^(-1+x^4) Sqrt[1-x]])/Sqrt[1+E^(-1+x^4) Sqrt[1-x]],x]]*)


(* ::Text:: *)
(*Why does simplify hang here? This should be fixed!*)


(* ::Input:: *)
(*Timing[simplify[1/3 ArcTan[(1-x^2)/((1+x+x^2) (1-x^4)^(1/3))]+1/6 (-1+Sqrt[3]) ArcTan[Sqrt[3]-(2 (1-x^2))/((1+x+x^2) (1-x^4)^(1/3))]+1/6 (1+Sqrt[3]) ArcTan[Sqrt[3]+(2 (1-x^2))/((1+x+x^2) (1-x^4)^(1/3))]+1/6 Log[1+(1-x^2)^2/((1+x+x^2)^2 (1-x^4)^(2/3))]-1/12 (1+Sqrt[3]) Log[1+(1-x^2)^2/((1+x+x^2)^2 (1-x^4)^(2/3))-(Sqrt[3] (1-x^2))/((1+x+x^2) (1-x^4)^(1/3))]+1/12 (-1+Sqrt[3]) Log[1+(1-x^2)^2/((1+x+x^2)^2 (1-x^4)^(2/3))+(Sqrt[3] (1-x^2))/((1+x+x^2) (1-x^4)^(1/3))],x]]*)


(* ::Text:: *)
(*What on Earth has gone on here?*)


(* ::Input:: *)
(*int[x Sqrt[1+Sqrt[2]+Sqrt[2] x+x^2],x]*)


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
(*We can compute this one using the piecewise constant method: *)


(* ::Input:: *)
(*int[x Sqrt[Sqrt[1-x^2]+x Sqrt[1/x^2-1]],x]*)


(* ::Input:: *)
(*(Sqrt[Sqrt[1-x^2]+x Sqrt[1/x^2-1]] int[x Sqrt[2 Sqrt[1-x^2]],x])/Sqrt[2 Sqrt[1-x^2]]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)-x Sqrt[Sqrt[1-x^2]+x Sqrt[1/x^2-1]]]*)


(* ::Text:: *)
(*Can we do this integral without resorting to piecewise constants or expansion? *)


(* ::Input:: *)
(*int[(2-8 x+8 x^2)^(1/3)/(3+x),x]*)


(* ::Text:: *)
(*When we expand it out manually we can do it:*)


(* ::Input:: *)
(*int[2/((3+x)(2-8 x+8 x^2)^(2/3)),x]*)
(*int[(-8x)/((3+x)(2-8 x+8 x^2)^(2/3)),x]*)
(*int[(8x^2)/((3+x)(2-8 x+8 x^2)^(2/3)),x,"Expansion"->True]*)
(*%+%%+%%%//Simplify*)


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


(* ::Text:: *)
(*An integral solved in terms of elementary functions by Chebyshev in 1857:*)


(* ::Input:: *)
(*int[(2x^6+4x^5+7x^4-3x^3-x^2-8x-8)/((2x^2+1)^2 Sqrt[x^4+4x^3+2x^2+1]),x]*)


(* ::Subsection::Closed:: *)
(*previously bugs, deficiencies or edge cases*)


(* ::Input:: *)
(*int[1/((1+Sqrt[x]) Sqrt[x-Sqrt[x]]),x]*)


(* ::Input:: *)
(*int[Sqrt[1-12 u^4+16 u^6]/((-2+u^2) (1+4 u^2)),u]*)


(* ::Input:: *)
(*int[Sqrt[u]/Sqrt[-1-u+u^2+u^3],u]*)


(* ::Input:: *)
(*int[Sqrt[-Sqrt[u]+u]/u,u]*)


(* ::Input:: *)
(*int[(x^2 (3+2 x^2) (1+x^2+2 x^6))/((1+x^2)^2 Sqrt[1+x^2+x^6]),x]*)


(* ::Input:: *)
(*int[(b^2+a x)/((-b^2+a x) Sqrt[b+Sqrt[b^2+a x^2]]),x,"SingleStepTimeConstraint"->1.0]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)-(b^2+a x)/((-b^2+a x) Sqrt[b+Sqrt[b^2+a x^2]])]*)


(* ::Input:: *)
(*int[-((Sqrt[-1+u^2] (1+u^2)^2 (-3-2 u+3 u^2))/(Sqrt[2] (-1+u) u (1+u) (-1+2 u+u^2)^3)),u]*)


(* ::Input:: *)
(*int[Sqrt[x]/(-2+x^2)^(3/4),x]*)


(* ::Input:: *)
(*int[u^(1/3)/(-1+5 u^2-4 u^4),u]*)


(* ::Input:: *)
(*int[(2 E^-x^2)/(Sqrt[\[Pi]] Erf[x])-(Erf[Sqrt[3] Tan[x]]+2 E^(-3 Tan[x]^2) Sqrt[3/\[Pi]] x Sec[x]^2)/(1+x Erf[Sqrt[3] Tan[x]]),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[1/Sqrt[1-Sqrt[x]]-Sqrt[1-Sqrt[x]-x],x]*)
(*Together[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)-(1/Sqrt[1-Sqrt[x]]-Sqrt[1-Sqrt[x]-x])]*)


(* ::Input:: *)
(*int[(-1+x^2) (-1+x Sqrt[-1+3 x^2-x^4]),x]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)-(-1+x^2) (-1+x Sqrt[-1+3 x^2-x^4])]*)


(* ::Input:: *)
(*int[((x^4+1) Sqrt[x^4-1]+(x^4-1) Sqrt[x^4+1])/(x^8+1),x]*)


(* ::Input:: *)
(*int[u (-3+2 u^2) Sqrt[(1-2 u^2)/(4+3 u^2)],u]*)


(* ::Input:: *)
(*int[1/Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*int[(x-1)/(x (x^3+2 x^2+2 x+1)^(1/3)),x]*)


(* ::Input:: *)
(*int[(a x^2-2 b)/((a x^2+c x^4-b) (a x^2-b)^(1/4)),x]*)


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
(*int[x/Sqrt[1-Sqrt[1-Sqrt[1-1/x]]],x,"SingleStepTimeConstraint"->2]*)
(*FullSimplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)-x/Sqrt[1-Sqrt[1-Sqrt[1-1/x]]]]*)


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
(*Examples from wolfram resource documentation*)


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
(*IntegrateAlgebraic[(6 x^8+1)/((2 x^8-1) (4 x^16+2 x^10-4 x^8-x^4-x^2+1)^(1/4)),x,VerifySolutions->False]*)


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
(*IntegrateAlgebraic[(x^4-1)/((x^4+1) (x^9-x^7)^(1/8)),x]*)


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
(*int[Sqrt[-1-11 x-36 x^2-27 x^3+16 x^4+9 x^5+x^6],x]*)


(* ::Input:: *)
(*int[Sqrt[u]/Sqrt[-1-u+u^2+u^3],u]*)


(* ::Input:: *)
(*int[1/((1+Sqrt[x]) Sqrt[x-Sqrt[x]]),x]*)


(* ::Input:: *)
(*int[(x^4+b)^2/(a b^4+4 a b^3 x^4+b^4 x^4+6 a b^2 x^8+4 b^3 x^8+4 a b x^12+6 b^2 x^12+a x^16+4 b x^16+x^20)^(1/4),x]*)


(* ::Input:: *)
(*int[1/(1-3 x+3 x^3-9 x^4+3 x^6-9 x^7+x^9-3 x^10)^(1/3),x]*)


(* ::Input:: *)
(*int[(1-3 x+3 x^3-9 x^4+3 x^6-9 x^7+x^9-3 x^10)^(1/3),x]*)


(* ::Input:: *)
(*int[(1-(1-3 x+3 x^3-9 x^4+3 x^6-9 x^7+x^9-3 x^10)^(1/3))/(x^2+(1-3 x+3 x^3-9 x^4+3 x^6-9 x^7+x^9-3 x^10)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x+Sqrt[1+4 x+7 x^2+8 x^3+5 x^4+2 x^5])/(1-Sqrt[1+4 x+7 x^2+8 x^3+5 x^4+2 x^5]),x]*)


(* ::Input:: *)
(*int[1/(-1-3 x^4-2 x^8+2 x^12+3 x^16+x^20)^(1/4),x]*)


(* ::Input:: *)
(*int[((1-x^4) (1-x-4 x^2+4 x^3+6 x^4-6 x^5-4 x^6+4 x^7+x^8-x^9)^(1/4))/(1+x^4),x]*)


(* ::Input:: *)
(*int[1/(-8+12 x+54 x^2-135 x^3+81 x^4)^(1/3),x]*)


(* ::Input:: *)
(*int[Sqrt[-81+27 x+135 x^2-150 x^3+65 x^4-13 x^5+x^6]/(x-1),x]*)


(* ::Input:: *)
(*int[Sqrt[(-81+27 x+135 x^2-150 x^3+65 x^4-13 x^5+x^6)^3]/(x-1),x]*)


(* ::Input:: *)
(*int[((-3+x)^6 (-1-x+x^2)^(3/2))/(-1+x),x]*)


(* ::Input:: *)
(*int[Sqrt[1-12 u^4+16 u^6]/((-2+u^2) (1+4 u^2)),u]*)


(* ::Input:: *)
(*int[1/((-1+x) Sqrt[x-Sqrt[x]]),x]*)


(* ::Input:: *)
(*int[Sqrt[x]/((-1+x) Sqrt[x-Sqrt[x]]),x]*)


(* ::Input:: *)
(*int[x^2/Sqrt[1-Sqrt[1-Sqrt[1-1/x]]],x]*)
(*FullSimplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)-x^2/Sqrt[1-Sqrt[1-Sqrt[1-1/x]]]]*)


(* ::Input:: *)
(*Timing[int[1/(x^3+x^2-x-1)^(1/3),x]]*)


(* ::Input:: *)
(*int[Sqrt[1+x^4]/((1+x)^3 Sqrt[x^2+Sqrt[1+x^4]]),x,"SingleStepTimeConstraint"->0.5]*)


(* ::Input:: *)
(*int[Sqrt[1+x^4]/((1+x)^4 Sqrt[x^2+Sqrt[1+x^4]]),x,"SingleStepTimeConstraint"->2.]*)


(* ::Input:: *)
(*int[(Sqrt[2] u Sqrt[-1+u^2] (-1-2 u+u^2))/((-1+u) (1+u) (-1+2 u+u^2)^2),u]*)


(* ::Input:: *)
(*int[(((4 x)/(-1+2 x^2)-(E^x-1/x)/(2 Sqrt[E^x-Log[x]])) Sqrt[Sqrt[-Sqrt[E^x-Log[x]]+Log[-1+2 x^2]]-Sqrt[1-Sqrt[E^x-Log[x]]+Log[-1+2 x^2]]])/(2 Sqrt[-Sqrt[E^x-Log[x]]+Log[-1+2 x^2]]),x]*)


(* ::Input:: *)
(*int[Cos[x]/((1-Sin[x])^(3/4) (2-Sin[x])^(1/4)),x]*)


(* ::Input:: *)
(*int[(E^x-1/x)/(4 Sqrt[1+(1-Sqrt[E^x-Log[x]])^2] Sqrt[1-Sqrt[E^x-Log[x]]] (2-Sqrt[E^x-Log[x]])),x]*)


(* ::Input:: *)
(*int[(-1+x+E^x x-ProductLog[x]+E^x ProductLog[x]+2 x ProductLog[x]+E^x x ProductLog[x]-Log[x] ProductLog[x])/(2 x (E^x+x-Log[x]) (1+ProductLog[x]) Sqrt[Log[E^x+x-Log[x]]+ProductLog[x]] (1+Log[E^x+x-Log[x]]^2+2 Log[E^x+x-Log[x]] ProductLog[x]+ProductLog[x]^2)),x]*)


(* ::Input:: *)
(*int[((E^x+ExpIntegralEi[x]) Sqrt[1+x ExpIntegralEi[x]-x^2 ExpIntegralEi[x]^2])/(x ExpIntegralEi[x]),x]*)


(* ::Input:: *)
(*int[(x^3-1)/(x^3+2)^(1/3),x]*)


(* ::Input:: *)
(*int[(x^3+b)/(x^3+a)^(1/3),x]*)


(* ::Input:: *)
(*int[(x^3+b)^3/(x^3+a)^(1/3),x]*)


(* ::Input:: *)
(*int[((x^3+b) (x^3+c))/(x^3+a)^(1/3),x]*)


(* ::Input:: *)
(*int[((x^3+b) (x^3-b))/(x^3+a x^2)^(1/3),x]*)


(* ::Input:: *)
(*int[((x^3+b) (x^3-b) (x^3-c))/(x^3+a x^2)^(1/3),x]*)


(* ::Input:: *)
(*int[1/((-b+a x) (b^2 x+a^2 x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x (b+a x^2)^(1/4) Sqrt[1+b+a x^2])/((1-b-a x^2) (b+a x^2)),x]*)


(* ::Input:: *)
(*Timing[int[(1/(2 Sqrt[-1+x])-((4 x^3)/Sqrt[-1+x]-x^4/(2 (-1+x)^(3/2)))/(2^(3/4) Sqrt[x^4/Sqrt[-1+x]])) (Sqrt[-1+x]-2^(1/4) Sqrt[x^4/Sqrt[-1+x]]-1/((Sqrt[-1+x]-2^(1/4) Sqrt[x^4/Sqrt[-1+x]]) Sqrt[1+(Sqrt[-1+x]-2^(1/4) Sqrt[x^4/Sqrt[-1+x]])^2])),x]]*)


(* ::Input:: *)
(*Timing[int[Sec[x Log[(Sqrt[2] x^4)/Sqrt[-1+x]]]^2 ((Sqrt[-1+x] ((4 Sqrt[2] x^3)/Sqrt[-1+x]-x^4/(Sqrt[2] (-1+x)^(3/2))))/(Sqrt[2] x^3)+Log[(Sqrt[2] x^4)/Sqrt[-1+x]]) (Tan[x Log[(Sqrt[2] x^4)/Sqrt[-1+x]]]-Cot[x Log[(Sqrt[2] x^4)/Sqrt[-1+x]]]/Sqrt[1+Tan[x Log[(Sqrt[2] x^4)/Sqrt[-1+x]]]^2]),x]]*)


(* ::Input:: *)
(*Timing[int[(E^((Sqrt[2] x^4)/Sqrt[-1+x]) (Sqrt[3]-E^((4 Sqrt[2] x^4)/Sqrt[-1+x])) ((4 Sqrt[2] x^3)/Sqrt[-1+x]-x^4/(Sqrt[2] (-1+x)^(3/2))))/Sqrt[1+E^((2 Sqrt[2] x^4)/Sqrt[-1+x])],x]]*)


(* ::Input:: *)
(*Timing[int[(E^(x^4/Sqrt[-1+x]) (1-E^((2 x^4)/Sqrt[-1+x])) ((4 x^3)/Sqrt[-1+x]-x^4/(2 (-1+x)^(3/2))))/Sqrt[1+E^((2 x^4)/Sqrt[-1+x])],x]]*)


(* ::Input:: *)
(*Timing[int[E^(x^4/(-1+x)) (1-E^((2 x^4)/(-1+x))/Sqrt[1+E^((2 x^4)/(-1+x))]) ((4 x^3)/(-1+x)-x^4/(-1+x)^2),x]]*)


(* ::Input:: *)
(*Timing[int[((-(E^(-(x^4/(-1+x)))/(2 Sqrt[1-x]))+E^(-(x^4/(-1+x))) Sqrt[1-x] (-((4 x^3)/(-1+x))+x^4/(-1+x)^2)) Sqrt[1-E^(-(x^4/(-1+x))) Sqrt[1-x]])/Sqrt[1+E^(-(x^4/(-1+x))) Sqrt[1-x]],x]]*)


(* ::Input:: *)
(*Timing[int[(Sqrt[1+E^(1-x^4)-Sqrt[1-x]] (-(1/(2 Sqrt[1-x]))+4 E^(1-x^4) x^3))/Sqrt[1-E^(1-x^4)+Sqrt[1-x]],x]]*)


(* ::Input:: *)
(*Timing[int[((-1+8 Sqrt[1-x] x^3+x^4) Sqrt[1-Sqrt[1-x]+Log[1-x^4]])/(2 Sqrt[1-x] (-1+x^4) Sqrt[1+Sqrt[1-x]-Log[1-x^4]]),x]]*)


(* ::Input:: *)
(*Timing[int[((8 x^4+(-1+x^4) Log[1-x^4]) (Sqrt[1-Sqrt[x] Log[1-x^4]]+Sqrt[1+Sqrt[x] Log[1-x^4]]))/(2 Sqrt[x] (-1+x^4)),x]]*)


(* ::Input:: *)
(*Timing[int[((-1-8 x^(7/2)+x^4) Sqrt[1-Sqrt[x]+Log[1-x^4]])/(2 Sqrt[x] (-1+x^4) Sqrt[1+Sqrt[x]-Log[1-x^4]]),x]]*)


(* ::Input:: *)
(*Timing[int[(Sqrt[x-Sqrt[Log[x]]]-Sqrt[1-x-Sqrt[x-Sqrt[Log[x]]]+Sqrt[Log[x]]]) (1-1/(2 x Sqrt[Log[x]])),x]]*)


(* ::Input:: *)
(*Timing[int[((Sqrt[-1+x+Sqrt[x-Log[x]]]+Sqrt[1+x+Sqrt[x-Log[x]]]) (-1+x+2 x Sqrt[x-Log[x]]))/(x Sqrt[-1+x+Sqrt[x-Log[x]]] Sqrt[1+x+Sqrt[x-Log[x]]] Sqrt[x-Log[x]]),x]]*)


(* ::Input:: *)
(*Timing[int[(x (-1+x^2))/(Sqrt[1+4/3 (x/(x^2+1))^2+Sqrt[1+4/3 (x/(x^2+1))^2]] (1+x^2)^3),x]]*)


(* ::Input:: *)
(*Timing[int[((x^2 C[3]-C[4]) (x+3 x^2 C[3]+3 C[4]))/(x (-x^2+x^4 C[3]^2+2 x^2 C[3] C[4]+C[4]^2) Sqrt[(x C[0]+x^2 C[3]+C[4])/(x C[1]+x^2 C[3]+C[4])]),x]]*)


(* ::Input:: *)
(*Timing[int[(x (x^2 C[3]-C[4]))/((x+3 x^2 C[3]+3 C[4]) (-x^2+x^4 C[3]^2+2 x^2 C[3] C[4]+C[4]^2) Sqrt[(x C[0]+x^2 C[3]+C[4])/(x C[1]+x^2 C[3]+C[4])]),x]]*)


(* ::Input:: *)
(*Timing[int[((x^2 C[3]-C[4]) (x+3 x^2 C[3]+3 C[4]))/((-x^3+x^6 C[3]^3+3 x^4 C[3]^2 C[4]+3 x^2 C[3] C[4]^2+C[4]^3) Sqrt[(x C[0]+x^2 C[3]+C[4])/(x C[1]+x^2 C[3]+C[4])]),x,"SingleStepTimeConstraint"->2]]*)


(* ::Input:: *)
(*Timing[int[(x^5 (x^2 C[3]+3 C[4]))/((x^3+3 x^2 C[3]+3 C[4]) (x^6-x^4 C[3]^2-2 x^2 C[3] C[4]-C[4]^2) Sqrt[(x^3 C[0]+x^2 C[3]+C[4])/(x^3 C[1]+x^2 C[3]+C[4])]),x,"SingleStepTimeConstraint"->10]]*)


(* ::Input:: *)
(*Timing[int[(x^3 (x^3 C[3]-2 C[4]) Sqrt[(x^2 C[0]+x^3 C[3]+C[4])/(x^2 C[1]+x^3 C[3]+C[4])])/((x^2+2 x^3 C[3]+2 C[4]) (-x^4+x^6 C[3]^2+2 x^3 C[3] C[4]+C[4]^2)),x]]*)


(* ::Input:: *)
(*Timing[int[(x^5 (2 x^5 C[3]-3 C[4]) Sqrt[(x^3 C[0]+x^5 C[3]+C[4])/(x^3 C[1]+x^5 C[3]+C[4])])/((x^3+2 x^5 C[3]+2 C[4]) (-x^6+x^10 C[3]^2+2 x^5 C[3] C[4]+C[4]^2)),x,"SingleStepTimeConstraint"->2.5]]*)


(* ::Input:: *)
(*Timing[int[(x^3 (x^3 C[3]-2 C[4]))/((x^2+2 x^3 C[3]+2 C[4]) (-x^4+x^6 C[3]^2+2 x^3 C[3] C[4]+C[4]^2) ((x^2 C[0]+x^3 C[3]+C[4])/(x^2 C[1]+x^3 C[3]+C[4]))^(1/3)),x,"SingleStepTimeConstraint"->5,VerifySolutions->False]]*)


(* ::Input:: *)
(*Timing[int[(x (2 x^3 C[3]-C[4]))/((-x+x^3 C[3]+C[4]) (x^2+x^4 C[3]+x^6 C[3]^2+x C[4]+2 x^3 C[3] C[4]+C[4]^2) ((x C[0]+x^3 C[3]+C[4])/(x C[1]+x^3 C[3]+C[4]))^(1/3)),x,"SingleStepTimeConstraint"->2.5]]*)


(* ::Input:: *)
(*Timing[int[(x^2 (x^2 C[3]-C[4]) ((x C[0]+x^2 C[3]+C[4])/(x C[1]+x^2 C[3]+C[4]))^(1/4))/((-x+x^2 C[3]+C[4]) (x+x^2 C[3]+C[4]) (x^2+x^4 C[3]^2+2 x^2 C[3] C[4]+C[4]^2)),x,"SingleStepTimeConstraint"->2.5]]*)


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
(*int[(1+x^4)^2/((-1+x^4)^2 Sqrt[x^2+Sqrt[1+x^4]]),x,"SingleStepTimeConstraint"->5]*)


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
(*int[Sqrt[x^2+Sqrt[1+x^4]]/(1+x),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/(1+a x),x]*)


(* ::Input:: *)
(*int[x^2 Sqrt[a x^2+Sqrt[b+a^2 x^4]],x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]],x]*)


(* ::Input:: *)
(*int[Sqrt[a x^2+Sqrt[b+a^2 x^4]],x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) Sqrt[x+Sqrt[1+x^2]]),x,"SingleStepTimeConstraint"->0.5]*)


(* ::Input:: *)
(*int[(1+a x^2)/((-1+a x^2) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(1+a x^2)/((-1+a x^2) Sqrt[a x+Sqrt[b+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[1/((-1+x^2) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[1/((d+c x) Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[1/((d+c x)^2 Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


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
(*Timing[int[(x^2+1)/((1+x) Sqrt[x^4+x^2+1]),x,"Expansion"->True]]*)


(* ::Input:: *)
(*Timing[int[1/((x^2-3) (1-3 x^2)^(1/3)),x,"Expansion"->True]]*)


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
(*int[(1+x^3-(1+x^4)^(1/4)+x^3 (1+x^4)^(1/4))/((-1+x^3) Sqrt[1+x^4]),x]*)


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
