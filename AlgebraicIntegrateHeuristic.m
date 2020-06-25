(* ::Package:: *)

(* ::Title:: *)
(*A simple method for solving some pseudo-elliptic integrals*)


(* ::Subtitle:: *)
(*Sam Blake, 2020*)


(* ::Text:: *)
(*Started on 16 March 2020.*)


(* ::Text:: *)
(*This package implements a heuristic for solving some pseudo-elliptic integrals using a combination of integration by substitution and the method of undetermined coefficients.*)


(* ::Subsection::Closed:: *)
(*BeginPackage*)


BeginPackage["AlgebraicIntegrateHeuristic`"];

solveAlgebraicIntegral::usage = "solveAlgebraicIntegral[f, x] is a heuristic for computing an elementary solution to a \
pseudo-elliptic integral. solveAlgebraicIntegral returns {rp, up, ip}, where rp is the (unintegrated) rational part, \
up is the unintegrated part, and ip is the integrated part.";

$verboseLevel::usage = "Controls how much information is shown when calling solveAlgebraicIntegral. With \
$verboseLevel = 0; no information is shown, $verboseLevel = 1; only top level information is shown, \
$verboseLevel = 2; intermediate-level information is shown, and $verboseLevel = 3; shows all information.";

Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*debugPrint*)


$verboseLevel = 0;


debugPrint1[e__] /; TrueQ[$verboseLevel > 0] := Block[{Internal`$ContextMarks = False},
	Print @ Style[Row @ {e} /. 
		{A[l_] :> Symbol["A"][l], V[l_] :> Symbol["V"][l], B[l_] :> Symbol["B"][l]}, Darker @ Green]
]


debugPrint2[e__] /; TrueQ[$verboseLevel > 1] := Block[{Internal`$ContextMarks = False},
	Print @ Style[Row @ {e} /. 
		{A[l_] :> Symbol["A"][l], V[l_] :> Symbol["V"][l], B[l_] :> Symbol["B"][l]}, Brown]
]


debugPrint3[e__] /; TrueQ[$verboseLevel > 2] := Block[{Internal`$ContextMarks = False},
	Print @ Style[Row @ {e} /. 
		{A[l_] :> Symbol["A"][l], V[l_] :> Symbol["V"][l], B[l_] :> Symbol["B"][l]}, Gray]
]


(* ::Subsection::Closed:: *)
(*singleEllipticRadicalQ*)


singleEllipticRadicalQ[e_, x_] := 
	Length[Union[Cases[e, Power[p_, r_Rational] /; (! FreeQ[p, x] && PolynomialQ[p, x]), {0, Infinity}]]] === 1 && 
	Length[Union[
		Cases[e, Power[p_, r_Rational] /; PolynomialQ[p, x] && Exponent[p, x] > 2 :> {p, Abs[r]}, {0, Infinity}], 
		SameTest -> (#1[[2]] == #2[[2]] && PossibleZeroQ[#1[[1]] - #2[[1]]]&)]] === 1


(* ::Input:: *)
(*singleEllipticRadicalQ[Sqrt[-1+x^3],x]*)


(* ::Input:: *)
(*singleEllipticRadicalQ[Sqrt[x]/Sqrt[-1+x^3],x]*)


(* ::Input:: *)
(*singleEllipticRadicalQ[((2+x^3)+Sqrt[(-1+x) (1+x+x^2)])/(x^2 (-2-4 x^2+2 x^3)Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*singleEllipticRadicalQ[x/Sqrt[-(-11+8 Sqrt[3]+2 Sqrt[3] x-x^2) (11+8 Sqrt[3]+2 Sqrt[3] x+x^2)],x]*)


(* ::Subsection::Closed:: *)
(*singleRadicalQ*)


singleRadicalQ[e_, x_] := 
	Length[Union[Cases[e, Power[p_, r_Rational] /; (! FreeQ[p, x] && PolynomialQ[p, x]), {0, Infinity}]]] === 1


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


apartSquareFreeList[e_] := Module[{apartList},
	apartList = ApartSquareFree[e];
	If[Head[apartList] === Plus,
		List @@ apartList,
		{apartList}
	]
]


Clear[normalise];

normalise[e_, {x_, y_}] := Module[
	{radical, p, r, num, den, numY, denY, exy, 
		y0, y1, nonalgNum, algNum, nonAlgPart, 
		terms, algterms},

	radical = Cases[e, p_^r_Rational :> p^Abs[r] /; (PolynomialQ[p, x] && ! FreeQ[p, x]), {0, Infinity}][[1]];
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
		Return[ {algNum, denY, {p, r}, y -> radical, nonalgNum/denY} ]
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

	{algNum, denY, {p, r}, y -> radical, nonAlgPart}
]


(* ::Input:: *)
(*normalise[1/(x^3 C[0]+C[1])^(1/3),{x,y}]*)


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
	"TableSize" -> "Small"};

polynomialsUndetermined[x_Symbol, opts:OptionsPattern[]] := 
	polynomialsUndetermined[x, opts] = 
	Switch[OptionValue["TableSize"],
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
(*solveAlgebraicIntegral*)


$Testing = False;
$timeConstraint = 0.25;


ClearAll[solveAlgebraicIntegral];

Options[solveAlgebraicIntegral] = {
	VerifySolutions -> True, 
	"MaxRationalDegree" -> 8,
	"MaxNumeratorDegree" -> 8,
	"MaxDenominatorDegree" -> 4,
	"TableSize" -> "Small",
	"DegreeBound" -> 8,
	"LinearRational" -> True,
	"Elementary" -> True
}; 

solveAlgebraicIntegral[integrand_, x_, opts : OptionsPattern[]] := Module[
{rationalPart, unintegratedPart, integratedPart, linRat, result},

{rationalPart, unintegratedPart, integratedPart} = {0, integrand, 0};

(* Linear rational substitution. *)

If[TrueQ[OptionValue["LinearRational"]],
	result = linearRationalIntegrate[unintegratedPart, x, opts];
	rationalPart    += result[[1]]; 
	unintegratedPart = result[[2]];
	integratedPart  += result[[3]];
	debugPrint1["Linear rational : ", {rationalPart, unintegratedPart, integratedPart}];
];

(* Rational substitution with undetermined coefficients. *)

result = rationalUndeterminedIntegrate[unintegratedPart, x, opts];
rationalPart    += result[[1]]; 
unintegratedPart = result[[2]];
integratedPart  += result[[3]];
debugPrint1["Rational undetermined : ", {rationalPart, unintegratedPart, integratedPart}];

(* Gunther substitution. *)

result = guntherIntegrate[unintegratedPart, x, opts];
rationalPart    += result[[1]]; 
unintegratedPart = result[[2]];
integratedPart  += result[[3]];
debugPrint1["Gunther : ", {rationalPart, unintegratedPart, integratedPart}];

(* Solving for the logarithmic part with undetermined coefficients. *)

result = logPartIntegrate[unintegratedPart, x, opts];
rationalPart    += result[[1]]; 
unintegratedPart = result[[2]];
integratedPart  += result[[3]];
debugPrint1["logPart : ", {rationalPart, unintegratedPart, integratedPart}];

(* Partial fraction expansion and integrate term-by-term. *)

result = partialFractionIntegrate[unintegratedPart, x, opts];
rationalPart    += result[[1]]; 
unintegratedPart = result[[2]];
integratedPart  += result[[3]];
debugPrint1["partialFractionIntegrate : ", {rationalPart, unintegratedPart, integratedPart}];

(* Expand and integrate term-by-term. *)

result = expandIntegrate[unintegratedPart, x, opts];
rationalPart    += result[[1]]; 
unintegratedPart = result[[2]];
integratedPart  += result[[3]];
debugPrint1["expandIntegrate : ", {rationalPart, unintegratedPart, integratedPart}];


{rationalPart, unintegratedPart, integratedPart}
]


(* ::Subsection::Closed:: *)
(*rationalUndeterminedIntegrate*)


ClearAll[rationalUndeterminedIntegrate];

Options[rationalUndeterminedIntegrate] = Options[solveAlgebraicIntegral]; 

rationalUndeterminedIntegrate[0|0., x_, OptionsPattern[]] := {0, 0, 0}

rationalUndeterminedIntegrate[integrand_, x_, opts : OptionsPattern[]] := Module[
	{start, k = 0, y(* = Symbol["y"]*), u(* = Symbol["u"]*), radicands, pSqCan, solution, rationalPart, 
	integrandNumerator, integrandDenominator, p, r, a, b, y2radical, matched, radicandMatchRules, 
	rationalMatchRules, rationalFormU, integrandU, intU, intX, unintegratedPart, integratedPart,
	cancellingCoefficient, cancellingTerm, uform, radicandNumeratorU, radicandU, integral,
	radicandDenominatorU, usubstitutionParam, radicandNumeratorUParam, radicandDenominatorUParam, 
	exnum, lexnum, integrated},
 
	maxRationalDegree    = OptionValue["MaxRationalDegree"];
	maxNumeratorDegree   = OptionValue["MaxNumeratorDegree"];
	maxDenominatorDegree = OptionValue["MaxDenominatorDegree"];
	degreeBound          = OptionValue["DegreeBound"];

	If[$Testing, start = AbsoluteTime[]];
 
	(* Single radical? *)

	If[! algebraicQ[integrand, x] || ! singleRadicalQ[integrand, x], 
		Return[{0, integrand, 0}]];

	(* Rewrite the integrand in the form r(x)+(q[x]/h[x]) y^r, where 
		y \[Equal] p[x], r(x) is the non-algebraic (rational) part, and 
		q[x], h[x], p[x] are polynomials in x. *)

	{integrandNumerator, integrandDenominator, {p, r}, y2radical, rationalPart} = normalise[integrand, {x, y}];
	debugPrint3["normalise returned ", {rationalPart, integrandNumerator, integrandDenominator, {p, r}, y2radical}];

	(* Defaults. *)
	RationalSubstitution = $Failed;
	solution = False;
	integratedPart = 0;	
	unintegratedPart = integrandNumerator/integrandDenominator /. y2radical;

	(* Check the algebraic part of the integrand is p(x)/q(r)*r(x)^n/m. *)
	integrandNumerator = Cancel[integrandNumerator/y^Exponent[integrandNumerator, y] ];
	If[! FreeQ[integrandNumerator, y], 
		debugPrint3["normalise failed ", {integrandNumerator, integrandDenominator}];
		Return[ {0, integrand, 0} ]
	];

	(* Possible radicands in u. *)
	If[(Numerator[r] === 2 && Exponent[p, x] > 2) || ! OptionValue["Elementary"],
		radicands = {A[1] # + A[0] &, A[2] #^2 + A[0] &, A[2] #^2 + A[1] # + A[0] &},
		radicands = {A[1] # + A[0] &}
	];

	(* Loop over substitution numerators. *)
	Catch @ 
	Do[
		++k;
		(* Radicands of the form uForm, uForm^2 + b, uForm^2 + b uForm + c. *)
		Do[
			(* Loop over radicand denominators 1, x, x^2, ... *)
			Do[
				(* All further candidate substitutions will not match (as the 
					substitution numerators are sorted by degree). *)
				 If[Exponent[usubstitution, x] > Exponent[p, x],
					Throw[{}]];

				(* Check the degrees agree.  *)
				If[Exponent[radicand[usubstitution], x] =!= Exponent[p, x],
					Continue[]];

				(* Form the substitution. *)
				uform = usubstitution/x^denP;
				radicandU = Together @ radicand[uform];
				radicandNumeratorU = Numerator @ radicandU;
				radicandDenominatorU = Denominator @ radicandU;

				(* Check the denominator can be removed from the radical. *)
				If[Mod[Exponent[radicandDenominatorU, x], Numerator[r]] != 0, 
					Continue[]];

				(* Find solution to the numerator of the radical part. *)
				{matched, radicandMatchRules} = solveRadicand[p, radicandNumeratorU, x];

				If[matched,				
					(* Loop over solutions to the radical part. *)
					debugPrint3["radicand of the form ", radicand[u]];
					debugPrint3["u substitution = ", uform];
					debugPrint3["radicand numerator/denominator = ", radicandNumeratorU, ", ", radicandDenominatorU];
			
					Do[
						debugPrint["radicand matches ", Style[radicand[u], Darker @ Green], ", ", radicandNumeratorU, radicandMatchRule];
						
						(* Parameterise. *)
						radicandDenominatorUParam = Expand[ radicandDenominatorU /. radicandMatchRule /. {A[1]|A[2]|B[1]|B[2] -> 1, A[0]|B[0] -> 0} ];
						radicandNumeratorUParam = Expand[ radicandNumeratorU /. radicandMatchRule /. {A[1]|A[2]|B[1]|B[2] -> 1, A[0]|B[0] -> 0} ];

						If[! FreeQ[{radicandDenominatorUParam, radicandDenominatorUParam}, C[_]|A[_]], 
							Continue[]];

						debugPrint3["radicand numerator = ", radicandNumeratorUParam];
						debugPrint3["radicand denominator = ", radicandDenominatorUParam];

						usubstitutionParam = Together[ uform /. radicandMatchRule /. {A[1]|A[2]|B[1]|B[2] -> 1, A[0]|B[0] -> 0} ];

						If[FreeQ[usubstitutionParam, x] || ! FreeQ[usubstitutionParam, B[_]|A[_]] || 
							! FreeQ[radicandNumeratorUParam, B[_]|A[_]], 
								Continue[]];

						debugPrint3["u substitution = ", usubstitution, ", ", Style[usubstitutionParam, Red]];

						(* Solve for the rational part of the integral. *)
						{matched, rationalFormU, rationalMatchRules} = solveRational[
							integrandNumerator, integrandDenominator, p, r, 
							usubstitutionParam, radicandDenominatorUParam, x, u, 
							"MaxRationalDegree" -> maxRationalDegree];

						If[matched, 
							(* We have found a match. *)
							debugPrint3["solution to rational part is ", rationalFormU];
	
							integrandU = rationalFormU (radicand[u]^(1/r) /. radicandMatchRule /. {A[1]|A[2]|B[1]|B[2] -> 1, A[0]|B[0] -> 0});
							debugPrint3["calling Integrate on ", integrandU];
	
							intU = integrate[integrandU, u];
							debugPrint3["Integrate returned ", intU];
	
							If[FreeQ[intU, Integrate(* | RootSum | Root *)] && (! OptionValue["Elementary"] || elementaryQ[intU]),
								intX = intU /. u -> usubstitutionParam;
								debugPrint3["integral is ", intX];
								intX = postProcess[intX, x];
								debugPrint3["post processed integral is ", intX];
								(* Sanity check. *)
								If[TrueQ[! OptionValue[VerifySolutions]] || TrueQ[(
										(* Order tests from fastest to slowest. *)
										(* seriesZeroQ[intX, unintegratedPart, x] ||  *)
										numericZeroQ[D[intX, x] - unintegratedPart] || 
										numericZeroQ[D[intX, x] - unintegratedPart, Precision -> 30] || 
										PossibleZeroQ[Together[D[intX, x] - unintegratedPart]] || 
										PossibleZeroQ[D[Simplify[D[intX, x] - unintegratedPart], x]])],
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
			{denP, 0, maxDenominatorDegree}], 
		{radicand, radicands}],
	{usubstitution, polynomialsUndetermined[x, 
						"MaxNumeratorDegree" -> maxNumeratorDegree, 
						"TableSize" -> OptionValue["TableSize"]]}];

	debugPrint[k];

	If[$Testing,
		testSolveAlgebraicIntegral[integrand // Hash] = {
			integrand, 
			AbsoluteTime[] - start, 
			None, 
			{rationalPart, unintegratedPart, integratedPart}}
	];

	If[integratedPart === 0 && rationalPart === 0,
		{0, integrand, 0},
		{rationalPart, unintegratedPart, integratedPart}
	]
]


(* ::Input:: *)
(*uu=substitutionTable[2, x][[10]]*)
(*uu= uu /. {C[0]->1,C[1]->-1}*)
(*Sqrt[u]/u du /. {u->uu, du->D[uu,x]} // togetherAll // PowerExpand*)
(*solveAlgebraicIntegral[%, x]*)


(* ::Subsection::Closed:: *)
(*solveRational*)


ClearAll[solveRational];

Options[solveRational] = {"MaxRationalDegree" -> 8};

solveRational[num_, den_, p_, r_, usubstitution_, radicandDenominator_, x_, u_, OptionsPattern[]] := Catch @ Module[
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


(* ::Subsection::Closed:: *)
(*solveRadicand*)


solveRadicand[poly_, form_, x_] := Module[{rules},
	(* rules = SolveAlways[poly == form, x]; *)
	
	If[Exponent[poly, x] === Exponent[form, x],
		rules = Quiet[Solve[! Eliminate[!(poly == form), {x}], Union @ Cases[form, (A|B)[_], Infinity]], {Solve::"svars"}];
		{! MatchQ[rules, {} | _SolveAlways], rules},
		{False, {}}	
	]
]


(* ::Input:: *)
(*DeleteCases[CoefficientRules[C[0]+x^3 C[1]+x^6 C[2],x][[All,1]], {0}]*)
(*Most[CoefficientRules[x^6-x^3,x][[All,1]]]*)


(* ::Input:: *)
(*substitutionTable[2, x][[50]]*)
(*solveRadicand[x^6-x^3, Numerator[%], x]*)


(* ::Input:: *)
(*substitutionTable[2, x][[250]]*)
(*solveRadicand[2x^5-x^4, Numerator[%], x]*)


(* ::Input:: *)
(*substitutionTable[2, x][[2]]*)
(*solveRadicand[2x^5-x^4, Numerator[%], x]*)


(* ::Input:: *)
(*substitutionTable[2, x][[2]]*)
(*((C[0]+x^2 C[1])/x^4)^2-V[0]((C[0]+x^2 C[1])/x^4)+V[1]//Expand//Together*)
(*((1-x^2)/x^4)^2-4((1-x^2)/x^4)+9//Expand//Together*)
(*solveRadicand[Numerator[%], Numerator[%%], x]*)
(*%%%/.%[[2]]*)


(* ::Input:: *)
(*((C[0]+x^2 C[1])/x^4)^2-V[0]((C[0]+x^2 C[1])/x^4)+V[1]//Expand//Together*)
(*Coefficient[%//Numerator, %//Denominator]*)


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

continuousQ[e_, x_] := TimeConstrained[
	TrueQ[! Reduce[1/e == 0, x, Reals]], 
	$timeConstraint, 
	True
]


ClearAll[nicerQ];

nicerQ[a_, b_, x_] /; FreeQ[a, x] && ! FreeQ[b, x] := True
nicerQ[a_, b_, x_] /; ! FreeQ[a, x] && FreeQ[b, x] := False
nicerQ[a_, b_, x_] := continuousQ[1/a, x] && ! continuousQ[1/b, x]
nicerQ[a_, b_, x_] := !(! continuousQ[1/a, x] && continuousQ[1/b, x])


(* A simplification based on
	 D[(Log[a[x] + b[x]] - Log[-a[x] + b[x]]) - 2*ArcTanh[a[x]/b[x]], x] \[Equal] 0 *)
ClearAll[log2ArcTanh];

log2ArcTanh = c1_. Log[p_] + c2_. Log[q_] /;
		! PossibleZeroQ[p - q] && 
		PossibleZeroQ[c1 + c2] && 
		PossibleZeroQ[(p + q)/2 + (p - q)/2 - p] && 
		PossibleZeroQ[(p + q)/2 - (p - q)/2 - q] && 
		LeafCount[collectnumden @ Cancel[(p + q)/(q - p)]] < 2 LeafCount[p/q] :> 
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

arcTanDiff = a_. ArcTan[z1_] + b_. ArcTan[z2_] /; PossibleZeroQ[a + b] && 
	LeafCount[collectnumden @ canonic[(z1 - z2)/(1 + z1 z2)]] < LeafCount[z1/z2] :> 
	(a - b)/2 ArcTan[collectnumden @ canonic[(z1 - z2)/(1 + z1 z2)]];

arcTanSum = a_. ArcTan[z1_] + b_. ArcTan[z2_] /; PossibleZeroQ[a - b] && 
	LeafCount[collectnumden @ canonic[(z1 + z2)/(1 - z1 z2)]] < LeafCount[z1/z2] :> 
	a ArcTan[collectnumden @ canonic[(z1 + z2)/(1 - z1 z2)]];


(* ::Text:: *)
(*ArcTanh[x] \[PlusMinus] ArcTanh[y] == ArcTanh[(x \[PlusMinus] y)/(1 \[PlusMinus] x y)]*)


ClearAll[arcTanhDiff, arcTanhSum];

arcTanhDiff = a_. ArcTanh[z1_] + b_. ArcTanh[z2_] /; PossibleZeroQ[a + b]  && 
	LeafCount[collectnumden @ canonic[(z1 - z2)/(1 - z1 z2)]] < LeafCount[z1/z2] :> 
	(a - b)/2 ArcTanh[collectnumden @ canonic[(z1 - z2)/(1 - z1 z2)]];

arcTanhSum = a_. ArcTanh[z1_] + b_. ArcTanh[z2_] /; PossibleZeroQ[a - b]  && 
	LeafCount[collectnumden @ canonic[(z1 + z2)/(1 + z1 z2)]] < LeafCount[z1/z2] :> 
	a ArcTanh[collectnumden @ canonic[(z1 + z2)/(1 + z1 z2)]];


ClearAll[collect];

collect[e_] := Module[{permutations, simp},
	permutations = Table[Select[Tuples[{Power[_, _Rational], _Log, _ArcTan, _ArcTanh, _$rootSum},{k}], Length[Union[#]] == k&], {k, 4}];
	simp = SortBy[Table[Fold[Collect[#1, #2, Together]&, e, permutations[[k]]], {k, Length @ permutations}], LeafCount][[1]];
	simp
]


ClearAll[collectnumden];

collectnumden[e_] := collectnumden[e] = Collect[Numerator[e], Power[_, _Rational]]/Collect[Denominator[e], Power[_, _Rational]]


ClearAll[partialExpand];

partialExpand[e_] /; MatchQ[e, c_?NumericQ p_Plus] := e /. c_?NumericQ (p_Plus) :> Distribute[c p]
partialExpand[e_] := e


(* ::Text:: *)
(*postProcess is not only for asthetic reasons. We also attempt to correct for the substitution taking a branch of the radical. *)
(**)
(*TODO: make logands monic.*)


Clear[postProcess];
postProcess[e_, x_] := Module[{$function, simp, permutations, numerics},

	(* Remove constants. *)
	simp = partialExpand[simp];
	If[Head[simp] === Plus, 
		numerics = Cases[simp, n_ /; FreeQ[n, x], {1}];
		simp -= Total[numerics];
	];

	simp = e /. {RootSum -> $rootSum, Function -> $function};

	simp = simp /. (h:Sin|Cos|Tan|Cot|Sec|Csc)[Pi r_Rational] :> FunctionExpand[h[Pi r]];
	simp = simp /. (h : ArcSinh | ArcCosh | ArcSin | ArcCos)[a_] :> TrigToExp[h[a]];

	(* The order of the next three lines is important, for example 
		int[(x Sqrt[x^4 - x^2])/(-3 + 2 x^2), x]
    we don't want to write Sqrt[x^4 - x^2] as x Sqrt[x^2 - 1.] *)
	simp = simp // togetherAll;
	simp = simp /. Power[p_, r_Rational] /; PolynomialQ[p, x] :> Expand[p]^r;
	simp = simp // powerExpand // togetherAll;

	(* Some examples for the following rule:
		int[((1 + x^6)*Sqrt[-x - x^4 + x^7])/(1 + 2*x^3 - 2*x^9 + x^12), x]
		int[((-x + x^3)^(1/3)*(-2 + x^4))/(x^4*(1 + x^2)), x] *)
	simp = simp /. (p_ x^m_Integer)^n_Rational /; PolynomialQ[p, x] && 
		m < 0 :> Expand[Numerator[p]x^(Ceiling[Abs[m], Denominator[n]] + m)]^n /x^(n (Ceiling[Abs[m], Denominator[n]]));

	(* Collect and partially simplify terms. *)
	simp = collect[simp];	

	(* This can often result in a simplification as denominators of sums of logs often cancel. *)
	simp = simp /. (h:Log|ArcTan|ArcTanh)[arg_] :> h[Cancel @ Together @ arg];
	simp = simp /. c_. Log[ex_] /; Denominator[ex] =!= 1 :> c Log[Numerator[ex]] - c Log[Denominator[ex]];

	simp = partialExpand[simp];

	(* Another simplification to cancel logarithms. *)
	simp = simp /. Log[ex_^n_Integer] :> n Log[ex];
	simp = collect[simp];

	simp = simp /. (h:Log|ArcTan|ArcTanh)[arg_] :> h[collectnumden @ canonic @ arg]; (* Yes, we have to do this twice. *)

	simp = simp /. Log[ex_] :> Log[Collect[ex, Power[_, _Rational]]];

	(* Pick the nicer of ArcTan[a/b] or -ArcTan[b/a] *)
	simp = simp /. ArcTan[a_] /; nicerQ[Numerator[a], Denominator[a], x] :> -ArcTan[collectnumden @ canonic[Denominator[a]/Numerator[a]]];

	(* Remove constant multiples in logands. *)
	simp = simp /. Log[logand_] /; FactorSquareFreeList[logand][[1]] =!= {1,1} :> 
		Log[ Apply[Times, Power @@@ Rest[FactorSquareFreeList[logand]]] ];

	simp = simp /. Log[ex_^n_Integer] :> n Log[ex]; (* Yes, using this one twice as well. *)
	simp = collect[simp];

	simp = simp //. log2ArcTanh;
	simp = simp //. {arcTanDiff, arcTanSum, arcTanhDiff, arcTanhSum};

	simp = simp /. ArcTan[a_] :> ArcTan[collectnumden @ canonic[a]];

	(* Pick the nicer of ArcTan[a/b] or -ArcTan[b/a]. *)
	simp = simp /. ArcTan[a_] /; nicerQ[Numerator[a], Denominator[a], x] :> -ArcTan[collectnumden @ canonic[Denominator[a]/Numerator[a]]];

	(* Pick the nicer of ArcTanh[a/b] or ArcTan[b/a]. *)
	simp = simp /. ArcTanh[a_] /; nicerQ[Numerator[a], Denominator[a], x] :> ArcTanh[collectnumden @ canonic[Denominator[a]/Numerator[a]]];

	(* Remove constants. *)
	simp = partialExpand[simp];
	If[Head[simp] === Plus, 
		numerics = Cases[simp, n_ /; FreeQ[n, x], {1}];
		simp -= Total[numerics];
	];

	simp = collect[simp] /. Power[p_, r_Rational] :> Expand[p]^r;
	simp = simp /. p_ /; PolynomialQ[p,x] :> Collect[p, x];
	simp = simp /. Log[ex_] :> Log[Collect[ex, Power[_, _Rational]]];

	simp = partialExpand[simp];
	simp /. {$rootSum -> RootSum, $function -> Function}
]


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
(*numericZeroQ*)


(* ::Text:: *)
(*TODO: Perhaps this should be clever and look at $Assumptions? Or if assumptions are made then we just (possibly) fail and rely on PossibleZeroQ and Simplify in subsequent (more time intensive) testing. *)


ClearAll[numericZeroQ];

Options[numericZeroQ] = {Precision -> $MachinePrecision, Tolerance -> 1.0*^-6};

numericZeroQ[e_, OptionsPattern[]] := Module[
	{v, ef, lower, upper, step, numericeval},
	v = Union[Flatten[Variables /@ Level[e, {-1}]]];
	ef = Function @@ {v, e};

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
(*Mathematica has trouble with some linear and quadratic radical rationals which have elementary forms. eg. ((1 + x)^(1/3)/x^3).*)


(* ::Input:: *)
(*Integrate[(1+u)^(1/3)/u^3,u]*)


rational2dQ[e_, {x_, y_}] := With[{te = Together[e]},
Denominator[te] =!= 1 && PolynomialQ[Numerator[te],{x,y}] && PolynomialQ[Denominator[te],{x,y}]]


ClearAll[integrate];

integrate[e_, x_] := Integrate[e, x]


integrate[e_, x_] /; ListQ[ linearRadicalToRational[e, x, $u] ] := 
	Module[{integrand, subst, integral},

	{integrand, subst} = linearRadicalToRational[e, x, $u];
	integral = Integrate[integrand, $u] /. subst;
	integral	
]


(* ::Input:: *)
(*integrate[(1+u)^(1/3)/u^3,u]*)


integrate[e_, x_] /; ListQ[ quadraticRadicalToRational[e, x, $u] ] := 
	Module[{integrand, subst, integral, numerics},

	{integrand, subst} = quadraticRadicalToRational[e, x, $u];
	integral = Integrate[integrand, $u] /. subst;
	integral = integral // Apart // Expand;

	(* Remove constants. *)
	If[Head[integral] === Plus, 
		numerics = Cases[integral, n_ /; FreeQ[n, x], {1}];
		integral -= Total[numerics];
	];

	integral
]


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
{y, radicals, a, b, c, exy, dx, U, X, \[Alpha], \[Beta], euler1, euler2, euler3, transformed},
(* Find radicals of the form ((a x + b)/(c x + d))^(n/m) *)
radicals = Cases[e, y:r_^n_/;
PolynomialQ[r,x] && Exponent[r,x] == 2 && Head[n] == Rational :>
	{Expand[r]^Abs[n], Collect[Expand[r],x], Numerator[n // Abs], Denominator[n // Abs]},
{0, Infinity}];

(* Check we have some radicals with a common radicand. *)
If[radicals === {} || Length[Union[ radicals[[All,2]] ]] > 1, 
	Return[ False ]];

(* Convert to R(x,y), where y^n = a x^2 + b x + c. *)
exy = Cancel @ Together[e /. {radicals[[1,1]] -> y, radicals[[1,1]]^-1 -> y^-1}];

(* We should now have a rational function of x and y. *)
If[!(PolynomialQ[exy, {x, y}] || rational2dQ[exy, {x, y}]), 
	Return[ False ]];

a = Coefficient[radicals[[1,2]], x, 2];
b = Coefficient[radicals[[1,2]], x, 1];
c = Coefficient[radicals[[1,2]], x, 0];

(* This is here for compact forms for solveAlgebraicIntegral. *)

If[! (MatchQ[Head[a], Integer|Rational] && 
	  MatchQ[Head[b], Integer|Rational] && 
	  MatchQ[Head[c], Integer|Rational]),
	Return[ False ]];

transformed = {};

If[a > 0,
	(* Euler's first substitution. *)
	X = (u^2 - c)/(b - 2 Sqrt[a] u);
	U = radicals[[1,1]] - Sqrt[a] x;
	dx = 2(-Sqrt[a] u^2 + b u - Sqrt[a]c)/(-2Sqrt[a]u + b)^2;
	euler1 = {Cancel[exy dx /. {y -> Sqrt[a]X + u, x -> X}], u -> U};
	AppendTo[transformed, euler1]
];

If[a < 0 && c > 0,
	(* Euler's second substitution. *)
	X = Cancel @ Together[(2 Sqrt[c] u - b)/(a - u^2)];
	U = (radicals[[1,1]] - Sqrt[c])/x;	
	dx = 2(Sqrt[c] u^2 - b u + a Sqrt[c])/(u^2 - a)^2;
	euler2 = {Cancel[exy dx /. {y -> X u + Sqrt[c], x -> X}], u -> U};
	AppendTo[transformed, euler2]
];

If[b^2 - 4 a c > 0, 
	(* Euler's third substitution. *)
	\[Alpha] = -b/(2 a) - Sqrt[b^2 - 4 a c]/(2 a);
	\[Beta] = -b/(2 a) + Sqrt[b^2 - 4 a c]/(2 a);
	X = Cancel @ Together[(a \[Beta] - \[Alpha] u^2)/(a - u^2)];
	U = radicals[[1,1]]/(x - \[Alpha]);
	dx = 2 u a (\[Beta] - \[Alpha])/(u^2 - a)^2;
	euler3 = {Cancel[exy dx /. {y -> (X - \[Alpha])u, x -> X}], u -> U};
	AppendTo[transformed, euler3]
];

SortBy[transformed, LeafCount] // First
]


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
(*Linear rational substitutions*)


(* ::Text:: *)
(*This routine tries a substitution of the form u == (a x + b)/(x + d) to transform an integral of the form *)
(**)
(*Integrate[R(x, p(x)^(n/m)), x]*)
(**)
(*to *)
(**)
(*Integrate[R((d u - b)/(a - u), (p u^D + q)^(n/m)/(s u + r)^D)]*)


ClearAll[linearRationalIntegrate];

Options[linearRationalIntegrate] = Options[solveAlgebraicIntegral];

linearRationalIntegrate[0|0., _, OptionsPattern[]] := {0, 0, 0}

linearRationalIntegrate[e_, x_, opts : OptionsPattern[]] := Module[{t, linRat, options, result},

	t = Unique[Symbol["u"]];

	linRat = TimeConstrained[
		linearRationalSubstitution[e, x, t], 
		$timeConstraint, 
		False];

	If[!ListQ[linRat], 
		Return[ {0, e, 0} ]
	];

	debugPrint2["Linear rational substitution is ", linRat];

	options = Append[DeleteCases[{opts}, HoldPattern["LinearRational" -> True]], "LinearRational" -> False];
	result = solveAlgebraicIntegral[linRat // First, t, Sequence @@ options];
	
	debugPrint2["Integral of ", linRat // First, " is ", result];

	postProcess[result /. Last[linRat], x]
]


(* ::Input:: *)
(*$verboseLevel=2;*)
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
(*semiRationalise[(-2 2^(3/4) u^2)/((1+u) (1+u^2) (1+u^4)^(3/4)),u]*)


ClearAll[linearRationalSubstitution];

linearRationalSubstitution[e_, x_, u_] := linearRationalSubstitution[e, x, u] = Module[
{radicals, radicand, deg, a, b, d, p, q, r, s, radU,
 eqn, soln, solns, subX, subU, intU, dx, subs, goodsubs},

radicals = Cases[e, Power[p_ /; PolynomialQ[p,x], r_Rational] :> {p, r}, {0,Infinity}];

If[radicals === {} || Length[Union[radicals[[All,1]]]] > 1,
	Return[False]
];

radicand = radicals[[1,1]];
deg = Exponent[radicand, x];

radU = Collect[radicand /. {x -> (d u - b)/(a - u)} // Together // Cancel, u];
eqn = Numerator[radU](r + s u)^deg == Denominator[radU](p u^deg + q);
solns = Solve[!Eliminate[!eqn, {u}] && a!=0 && b!=0 && p!=0 && q!=0, {a,b,d,p,q}];
solns = DeleteCases[solns, s_ /; !FreeQ[s, Power[_Integer, _Rational]]];

If[MatchQ[solns, {}|{{}} | _Solve | {{(_ -> _?PossibleZeroQ) ..}..}], 
	Return[ False ]];

subs = Table[
	subX = (d u - b)/(a - u) /. soln // Cancel;
	subX = subX /. a|b|d|p|q|r|s -> 1 // Cancel;
	intU = e dx /. {x -> subX, dx -> D[subX, u]} // Together // Cancel // PowerExpand // Cancel;
	subU = (a x + b)/(x + d) /. soln // Cancel;
	subU = subU /. a|b|d|p|q|r|s -> 1 // Cancel;

	{semiRationalise[intU, u], u -> subU},
{soln, solns}];

goodsubs = Cases[subs, 
	{integrand_, u -> usub_} /; 
	PossibleZeroQ[e - PowerExpand @ Cancel @ Together[integrand D[usub, x] /. u -> usub]]];

If[goodsubs === {}, 
	Return[ False ]
];

SortBy[goodsubs, LeafCount] // First
]


(* ::Input:: *)
(*linearRationalSubstitution[(x-1)/((1+x) Sqrt[x+x^2+x^3]),x,u]*)


(* ::Input:: *)
(*linearRationalSubstitution[(3 x^2+1)/Sqrt[x^3+x-1],x,u]*)


(* ::Input:: *)
(*linearRationalSubstitution[(1-x^2)^2/((x^2+1) (x^4+6 x^2+1)^(3/4)),x,u]*)


(* ::Input:: *)
(*linearRationalSubstitution[1/((x+1) (x^4+6 x^2+1)^(1/4)),x,u]*)
(*postProcess[int[%//First,u] /. Last[%], x]*)
(*D[% // Last, x] - 1/((x+1) (x^4+6 x^2+1)^(1/4)) // Together // Cancel*)


(* ::Input:: *)
(*linearRationalSubstitution[(1+15 x^2+15 x^4+x^6)^(1/6)/((-1+x) (1+x)^2),x,u]*)
(*postProcess[int[%//First,u] /. Last[%], x]*)
(*D[% // Last, x]-(1+15 x^2+15 x^4+x^6)^(1/6)/((-1+x) (1+x)^2)//Together//Cancel*)


(* ::Subsection::Closed:: *)
(*Generalised Gunther Substitutions*)


(* ::Text:: *)
(*Ref: S. Gunther, "Sur l'\[EAcute]valuation de certaines int\[EAcute]grales pseudo-elliptiques", Bulletin de la S. M. F., tome 10 (1882), p. 88-97*)


Clear[guntherIntegrate];

Options[guntherIntegrate] = Options[solveAlgebraicIntegral];

guntherIntegrate[0|0., OptionsPattern[]] := {0, 0, 0}

guntherIntegrate[e_, x_, OptionsPattern[]] := Module[
{t, p, q, r, n, result, ratIntegral},

t = Unique[Symbol["u"]];

If[Not[algebraicQ[e, x] && singleEllipticRadicalQ[e, x]], 
	Return[ {0, e, 0} ]
];

{{r,n}} = Cases[e, Power[p_, n_Rational] /; (! FreeQ[p, x] && PolynomialQ[p, x]) :> {p,Abs[n]}, {0, Infinity}];

Which[
PolynomialQ[Numerator[e], x] && PolynomialQ[Denominator[e]/r^n, x],
	(* Radical is in the denominator. *)
	p = Numerator[e];
	q = Denominator[e]/r^FractionalPart[n];
	r = r^Max[1,IntegerPart[n]];
	result = guntherSolve1[p, q, r, 1/FractionalPart[n], x, t];
	If[result === $Failed, 
		{0, e, 0},
		debugPrint2["guntherSolve1 returned ", result];
		ratIntegral = Integrate[result // First, t];
		{0, 0, postProcess[ratIntegral /. t -> Last[result], x]}
	],
PolynomialQ[Denominator[e], x] && PolynomialQ[Numerator[e]/r^n, x],
	(* Radical is in the numerator. *)
	p = Numerator[e]/r^FractionalPart[n];
	q = Denominator[e];
	r = r^Max[1,IntegerPart[n]];
	result = guntherSolve2[p, q, r, 1/FractionalPart[n], x, t];
	If[result === $Failed, 
		{0, e, 0},
		debugPrint2["guntherSolve2 returned ", result];
		ratIntegral = Integrate[result // First, t];
		{0, 0, postProcess[ratIntegral /. t -> Last[result], x]}
	], 
True, 
	{0, e, 0}
]
]


(* ::Input:: *)
(*guntherIntegrate[((-1+x^2)^2 (x+x^3))/(Sqrt[1+x^4] (1-2 x^2+4 x^4-2 x^6+x^8)),x]*)
(*D[%//Last,x]-((-1+x^2)^2 (x+x^3))/(Sqrt[1+x^4] (1-2 x^2+4 x^4-2 x^6+x^8))//Simplify*)


(* ::Input:: *)
(*guntherIntegrate[((x+x^3) Sqrt[1+x^4])/(1-2 x^2+4 x^4-2 x^6+x^8),x]*)
(*D[%//Last,x]-((x+x^3) Sqrt[1+x^4])/(1-2 x^2+4 x^4-2 x^6+x^8)//Simplify*)


(* ::Input:: *)
(*guntherIntegrate[((x+x^3) (1+x^4)^(3/2))/((-1+x^2)^2 (1-2 x^2+4 x^4-2 x^6+x^8)),x]*)
(*D[%//Last,x]-((x+x^3) (1+x^4)^(3/2))/((-1+x^2)^2 (1-2 x^2+4 x^4-2 x^6+x^8))//Simplify*)


(* ::Input:: *)
(*guntherIntegrate[(1-x^4)/(Sqrt[1+x^4] (3-x^2+3 x^4)),x]*)
(*D[%//Last,x]-(1-x^4)/(Sqrt[1+x^4] (3-x^2+3 x^4))//Simplify*)


(* ::Input:: *)
(*guntherIntegrate[Sqrt[1+x^4]/(3x^3+7x^7),x]*)
(*D[%//Last,x]-Sqrt[1+x^4]/(3x^3+7 x^7)//Simplify*)


(* ::Input:: *)
(*guntherIntegrate[x/((3 x-1)Sqrt[x^4-x^3]),x]*)
(*D[%//Last,x]-x/((3 x-1)Sqrt[x^4-x^3])//Simplify*)


(* ::Input:: *)
(*guntherIntegrate[(2 x^2-x)/((2-x+x^2)Sqrt[x^4-x^3]),x]*)
(*D[%//Last,x]-(2 x^2-x)/((2-x+x^2)Sqrt[x^4-x^3])//Simplify*)


(* ::Subsubsection::Closed:: *)
(*guntherSolve1	(u == p[x]/(q[x])^(1/m))*)


(* ::Text:: *)
(*guntherSolve1 uses a substitution of the form u == p[x]/(q[x])^(1/m)*)


ClearAll[guntherSolve1];

guntherSolve1[p_, q_, r_, m_, x_, u_] := Catch @ Module[
{degp, degq, degMax, y, form, yform, unparameterised, eqn, vars, soln, solnre},

debugPrint2["Trying guntherSolve1"];

degp = Exponent[p,x];
degq = Exponent[q,x];

degMax = Max[3, degp - degq + Exponent[r,x] - 1];
debugPrint2["Degree bound = ", degMax];

Do[
	If[ndeg == 0 && ddeg == 0(* || ndeg > ddeg *), 
		Continue[]];

	debugPrint2["numerator/denominator degree = ", ndeg, ", ", ddeg];

	y = Sum[C[k] x^k, {k, 0, deg}]/r^(1/m);
	debugPrint2["Substitution form is ", y];

	form = Sum[V[k] u^(m k), {k, 0, ndeg}]/Sum[V[ndeg + 1 + k] u^(m k), {k, 0, ddeg}];
	form = form /. V[ndeg|ndeg + 1 + ddeg] -> 1;
	yform = form /. u -> y;

	unparameterised = Cancel[r^(1/m) Together[yform D[y,x]]];

	If[degp > Exponent[Numerator @ unparameterised, x] || 
		degq > Exponent[Denominator @ unparameterised, x],
		Continue[]];

	eqn = Numerator[unparameterised] q - Denominator[unparameterised] p == 0;
	vars = Union @ Cases[eqn, (C|V)[_], Infinity];

	soln = TimeConstrained[
		Solve[! Eliminate[! eqn, {x}], vars],
		$timeConstraint, 
		{}
	];

	soln = DeleteCases[soln, {Rule[_,0]..}];
	solnre = Cases[soln, s_ /; FreeQ[N[s], _Complex]];
	If[solnre =!= {}, 
		soln = solnre];

	If[soln =!= {},
		{form, y} = {form, y} /. soln[[1]] /. {V[0] -> 1, V[1] -> 0, C[_] -> 1};
		RationalSubstitution = y;
		If[!MatchQ[form, Indeterminate|0],
			Throw @ Cancel[ {form, y} ]
		]
	],
{ndeg, 0, 2}, {ddeg, 1, 2}, {deg, 1, degMax}];

$Failed
]


(* ::Input:: *)
(*(u^2 du)/(u^4+1)/.{u -> (1-x^2)/Sqrt[1+x^4], du->D[(1-x^2)/Sqrt[1+x^4],x]}//Simplify*)
(*guntherSolve1[x (-1+x^2)^2 (1+x^2),1-2 x^2+4 x^4-2 x^6+x^8,1+x^4,2,x,u]//Timing*)
(*Integrate[%//Last//First,u] /. u-> Last[Last[%]]*)
(*postProcess[%, x]*)
(*D[%,x] - ((-1+x^2)^2 (x+x^3))/(Sqrt[1+x^4] (1-2 x^2+4 x^4-2 x^6+x^8))//Simplify*)


(* ::Subsubsection::Closed:: *)
(*guntherSolve2	(u == (q[x])^(1/m)/p[x])*)


(* ::Text:: *)
(*guntherSolve2 uses a substitution of the form u == (q[x])^(1/m)/p[x]*)


ClearAll[guntherSolve2];

guntherSolve2[p_, q_, r_, m_, x_, u_] := Catch @ Module[
{degp, degq, degMax, y, form, yform, unparameterised, eqn, vars, soln, solnre},

debugPrint2["Trying guntherSolve2"];

degp = Exponent[p,x];
degq = Exponent[q,x];

degMax = Max[3, Exponent[p,x] - Exponent[q,x] + Exponent[r,x] - 1];
debugPrint2["Degree bound = ", degMax];

Do[
	If[ndeg == 0 && ddeg == 0 || ndeg > ddeg, 
		Continue[]];

	debugPrint2["numerator/denominator degree = ", ndeg, ", ", ddeg];

	y = r^(1/m)/Sum[C[k] x^k, {k, 0, deg}];
	debugPrint2["Substitution form is ", y];

	form = Sum[V[k] u^(m k), {k, 0, ndeg}]/Sum[V[ndeg + 1 + k] u^(m k), {k, 0, ddeg}];
	form = form /. V[ndeg|ndeg + 1 + ddeg] -> 1;
	yform = form /. u -> y;

	unparameterised = Cancel[r^(-1/m) Together[yform D[y,x]]];

	If[degp > Exponent[Numerator @ unparameterised, x] || 
		degq > Exponent[Denominator @ unparameterised, x],
		Continue[]];

	eqn = Numerator[unparameterised] q - Denominator[unparameterised] p == 0;
	vars = Union @ Cases[eqn, (C|V)[_], Infinity];

	soln = TimeConstrained[
		Solve[!Eliminate[!eqn, {x}], vars],
		$timeConstraint, 
		{}
	];

	soln = DeleteCases[soln, {Rule[_,0]..}];
	solnre = Cases[soln, s_ /; FreeQ[N[s], _Complex]];
	If[solnre =!= {}, 
		soln = solnre];

	If[soln =!= {},
		{form, y} = {form, y} /. soln[[1]] /. {V[0] -> 1, V[1] -> 0, C[_] -> 1};
		RationalSubstitution = y;
		If[!MatchQ[form, Indeterminate|0],
			Throw @ Cancel[ {form, y} ]
		]
	],
{ndeg, 0, 2}, {ddeg, 1, 2}, {deg, 1, degMax}];

$Failed
]


(* ::Input:: *)
(*(u^2 du)/(u^4+1)/.{u -> Sqrt[1+x^4]/(1-x^2), du->D[Sqrt[1+x^4]/(1-x^2),x]}//Simplify*)
(*guntherSolve2[x+x^3,1-2 x^2+4 x^4-2 x^6+x^8,1+x^4,2,x,u]*)
(*Integrate[%//First,u] /. u-> Last[%]*)
(*postProcess[%, x]*)
(*D[%,x] - ((x+x^3) Sqrt[1+x^4])/(1-2 x^2+4 x^4-2 x^6+x^8)//Simplify*)


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
{degreeBound, a, b, r, n, sol},

degreeBound = OptionValue["DegreeBound"];

a = Numerator[e];
b = Denominator[e];

If[Not[algebraicQ[e, x] && singleEllipticRadicalQ[e, x] && PolynomialQ[a, x]], 
	Return[ {0, e, 0} ]
];

{{r,n}} = Cases[e, Power[p_, n_Rational] /; (! FreeQ[p, x] && PolynomialQ[p, x]) :> {p, Abs[n]}, {0, Infinity}];

If[! PolynomialQ[b/r^n, x] || TrueQ[n != 2],
	Return[ {0, e, 0} ]
];

sol = logPartSolve1[a, b/r^n, r, x, degreeBound];
If[sol =!= False, 
	Return[ {0, 0, sol} ]
];

(* Many more to come... *)



{0, e, 0}]


(* ::Input:: *)
(*logPartIntegrate[x/Sqrt[1+4 x+3 x^2-2 x^3+x^4],x]*)


(* ::Subsubsection::Closed:: *)
(*logPartSolve1	\[LongDash]	Integrate[a[x]/(b[x] Sqrt[r[x]])] == c Log[p[x] + q[x] Sqrt[r[x]]]*)


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
	sol = Solve[! Eliminate[!eqns, {x}], vars];

	sol = sol /. ConditionalExpression[e_,___] :> e /. C[1] -> 1;
	sol = DeleteCases[sol, {(_ -> _?PossibleZeroQ) ..}];

	If[! MatchQ[sol, {} | {{}} | _Solve | False], 
		Break[]
	],
	{bound, degreeBound}];

	If[MatchQ[sol, {} | {{}} | _Solve | False], 
		Return[ False ]
	];

	logs = Table[
		c Log[p[x] + q[x]Sqrt[r[x]]] /. sol[[k]] /. V[_] -> Apply[LCM, Denominator /@ Most[sol[[k]][[All,-1]]]],
		{k, Length[sol]}];

	logs = logs /. LCM -> Times; (* Incase LCM doesn't simplify. eg. LCM[1, Sqrt[2]] *)

	Select[logs, Cancel[Together[D[#, x] - a[x]/(b[x] Sqrt[r[x]])]] === 0&, 1] /. {{e_} :> e, {} -> False}
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


(* ::Subsection::Closed:: *)
(*Expand Integrate*)


rationalQ[e_, x_] := With[
    {te = Together[e]}, 
    Denominator[te] =!=1 && PolynomialQ[Numerator[te], x] && PolynomialQ[Denominator[te], x]
]


ClearAll[expandIntegrate];

Options[expandIntegrate] = Options[solveAlgebraicIntegral];

expandIntegrate[0|0., x_, opts:OptionsPattern[]] := {0, 0, 0}


expandIntegrate[e_, x_, opts:OptionsPattern[]] := Module[
{exnum, lexnum, unintegratedPart, integratedPart, term, integrated, rationalPart},

exnum = Expand @ Numerator[e];

If[Head[exnum] === Plus, 
	debugPrint2["Expanding integrand and integrating term-by-term."];
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
(*$verboseLevel=3;*)
(*expandIntegrate[-((2 2^(3/4) (-1+u) u^2)/((-1+u^4) (1+u^4)^(3/4))),u]*)


(* ::Subsection::Closed:: *)
(*Partial Fraction Integrate*)


ClearAll[apartList];

apartList[e_, x_] := Module[{pf},

pf = List @@ Apart[e, x];

pf = GatherBy[pf, Union[Cases[#, Power[p_, n_Rational] /; (! FreeQ[p, x] && PolynomialQ[p, x]) :> p^Abs[n],{0,Infinity}]]&];

Simplify[ Cancel[Together[Total[#]]]& /@ pf ]
]


ClearAll[partialFractionIntegrate];

Options[partialFractionIntegrate] = Options[solveAlgebraicIntegral];

partialFractionIntegrate[0|0., x_, opts:OptionsPattern[]] := {0, 0, 0}


partialFractionIntegrate[e_, x_, opts:OptionsPattern[]] := Module[
{exy, pf, unintegratedPart, integratedPart, integrated, rationalPart},

If[! algebraicQ[e, x], 
	Return[ {0, e, 0} ]
];

pf = apartList[e, x];

If[Length[pf] > 1, 
	debugPrint2["Expanding integrand and integrating term-by-term."];
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
	{rationalPart, unintegratedPart, integratedPart}, 
	{0, e, 0}
]
]


(* ::Subsection::Closed:: *)
(*End Package*)


End[];
EndPackage[];


(* ::Section:: *)
(*Examples, wish list, bugs/deficiencies, regression tests, ...*)


(* ::Input:: *)
(*$verboseLevel=3;*)
(*int=solveAlgebraicIntegral;*)


(* ::Text:: *)
(*Example from paper: *)


(* ::Input:: *)
(*int[((-2+x^3) Sqrt[1-x^2+x^3])/(1+x^3)^2,x]*)


(* ::Text:: *)
(*My favourite examples thus far:*)


(* ::Input:: *)
(*int[((x^2-1) Sqrt[x^4+x^2+1])/((x^2+1) (x^4+x^3+x^2+x+1)),x]*)


(* ::Input:: *)
(*int[((x^4-1) Sqrt[1+x^2+x^4])/((1+x^4) (1-x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(-2+3 x^5)/((1+x^5) Sqrt[1+x^2+x^5]),x]*)


(* ::Input:: *)
(*int[(1+x^8)/((1-x^8) Sqrt[1-x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(x^6-x^4)^(1/6),x]*)


(* ::Text:: *)
(*We now allow symbolic summation over the roots of a polynomial in the results.*)


(* ::Input:: *)
(*int[((-1+x^8) (-1+x^4)^(1/4))/(x^6 (1+x^8)),x]*)


(* ::Text:: *)
(*A difficult integral which was solved by Euler in 1777.*)


(* ::Input:: *)
(*int[(1-x^2)^2/((x^2+1) (x^4+6 x^2+1)^(3/4)),x]*)


(* ::Subsection::Closed:: *)
(*current bugs and deficiencies*)


(* ::Text:: *)
(*This solution to this integral is terrible. *)


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
(*int[x/((4-x^3) Sqrt[1-x^3]),x]*)


(* ::Input:: *)
(*Kauers[x/((4-x^3) Sqrt[1-x^3]),x]*)


(* ::Input:: *)
(*int[1/((x+1) (x^3+2)^(1/3)),x]*)


(* ::Text:: *)
(*This isn't specifically a bug, but the resulting form is unfortunate.*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+2 x^2+x^4])/((1+x^2) (1+x^4)),x]*)


(* ::Text:: *)
(*Investigate if these are bugs:*)


(* ::Input:: *)
(*int[((2 x^2+3) Sqrt[2 x^3+x])/(2 x^2+1)^2,x]*)


(* ::Input:: *)
(*int[((2 x^5+3) Sqrt[-x^6-2 x^4+x])/(x^5-1)^2,x]*)


(* ::Input:: *)
(*int[((x^4+3) Sqrt[x-x^5])/(x^8-x^6-2 x^4+1),x]*)


(* ::Subsection::Closed:: *)
(*wish list*)


(* ::Input:: *)
(*AlgebraicIntegrateHeuristic`Private`$algebraicIntegrateDebug=.;*)


(* ::Text:: *)
(*Surely this one should be computable? *)


(* ::Input:: *)
(*int[(1+x^2)/((1-x^2) Sqrt[1-x^4-x^8]),x]*)


(* ::Input:: *)
(*int[1/((-2+x) (-x^2+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-1+x) (x+x^3)^(1/4)),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->8,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[(-2+x)/((-1+x^2) (x+x^3)^(1/4)),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->8,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[(-5+x^4)/((x+x^3)^(1/4) (-1+x^4)),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->8,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[((1+2 x+2 x^2) (x^2+x^4)^(1/4))/(1+2 x^2),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->8,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[((x+2 x^2) (x^2+x^4)^(1/4))/(1+2 x^2),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->8,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[((-1+2 x) (-x^2+x^4)^(1/4))/((-1+x) x),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->8,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[((-1+2 x+2 x^2) (-x^2+x^4)^(1/4))/(-1+2 x^2),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->8,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[((-2+x) (-x^2+x^4)^(1/4))/(-1+2 x^2),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->8,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) (x^2+x^6)^(1/4)),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->24,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[(-1+x^4)/((1+x^2+x^4) (x^2+x^6)^(1/4)),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->24,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[(-1+x^8)/((x^2+x^6)^(1/4) (1+x^8)),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->8,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[((-1+x^2) (x^2+x^6)^(1/4))/(x^2 (1+x^2)),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->24,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[((1+x^2) (-x^3+x^4)^(1/4))/(-1+x+2 x^2),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->24,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*int[((1+x) (x^3+x^5)^(1/4))/(x (-1+x^3)),x,"MaxRationalDegree"->8,"MaxNumeratorDegree"->8,"MaxDenominatorDegree"->24,"TableSize"->"Medium"]*)


(* ::Subsection::Closed:: *)
(*previously bugs or edge cases*)


(* ::Input:: *)
(*AlgebraicIntegrateHeuristic`Private`$algebraicIntegrateDebug=.;*)


(* ::Input:: *)
(*int[(x-1)/(x (1+x^4)^(1/4)),x]*)


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
(*regression testing*)


(* ::Input:: *)
(*ClearAll[AlgebraicIntegrateHeuristic`Private`testSolveAlgebraicIntegral];*)
(*AlgebraicIntegrateHeuristic`Private`$Testing=True;*)


(* ::Text:: *)
(*The missing integrals here are computed with the option: "TableSize" -> "Medium":*)


(* ::Input:: *)
(*int[((3+x) Sqrt[-2 x-x^2+x^4])/(2+x+x^3)^2,x,"TableSize"->"Medium"]*)
(*int[((3+x) Sqrt[-2 x-x^2+x^4])/(2+x+x^3)^2,x,"TableSize"->"Medium"]*)
(*int[((-3+2 x+2 x^5) Sqrt[x-x^2+x^6])/(1-2 x+x^2-x^3+x^4+2 x^5-3 x^6-x^8+x^10),x,"TableSize"->"Medium"]*)
(*int[((-3+2 x+2 x^5) Sqrt[x-x^2+x^6])/(1-2 x+x^2-x^3+x^4+2 x^5-3 x^6-x^8+x^10),x,"TableSize"->"Medium"]*)
(*int[((-3+x^4+3 x^6) Sqrt[-x-x^5-x^7])/((1+x^4+x^6) (1-x^3+x^4+x^6)),x,"TableSize"->"Medium"]*)
(*int[((-3+x^4+3 x^6) Sqrt[-x-x^5-x^7])/((1+x^4+x^6) (1-x^3+x^4+x^6)),x,"TableSize"->"Medium"]*)


(* ::Input:: *)
(*Cases[DownValues[AlgebraicIntegrateHeuristic`Private`testSolveAlgebraicIntegral][[All,-1]],{_,_,_,Except[{0,0,_}]}]*)
(*Median[DownValues[AlgebraicIntegrateHeuristic`Private`testSolveAlgebraicIntegral][[All,-1,2]]]*)


(* ::Text:: *)
(*15-Apr-2020 0.409 Seconds*)
(*16-Apr-2020 0.372 Seconds*)


(* ::Input:: *)
(*$verboseLevel = 0;*)


(* ::InheritFromParent:: *)
(*int[(1+x^3-(1+x^4)^(1/4)+x^3 (1+x^4)^(1/4))/((-1+x^3) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[(1-x)/Sqrt[3+2 x-5 x^2-4 x^3+x^4+2 x^5+x^6],x]*)


(* ::Input:: *)
(*int[1/((x+1) (x^4+6 x^2+1)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x (x^6+1)^(1/3)),x]*)


(* ::Input:: *)
(*int[x/(1+x^6)^(1/3),x,"MaxDenominatorDegree"->6]*)


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
(*int[((2+x^4) (-2-2 x^2+x^4) Sqrt[-2-x^2+x^4])/((-2+x^4) (-2+x^2+x^4)^2),x]*)


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
(*int[((-3+x^4+3 x^6) Sqrt[-x-x^5-x^7])/((1+x^4+x^6) (1-x^3+x^4+x^6)),x]*)


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
(*Assuming[x>0,int[(x Sqrt[-x^2+x^4])/(-3+2 x^2),x]]*)


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
(*int[((-1+2 x^4) Sqrt[1-3 x^2+2 x^4])/((1-2 x^2+2 x^4) (1-x^2+2 x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^4])/(1+x^4+x^8),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[-1-x^4])/((1+x^2) (1+x^4)),x]*)


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
(*int[(Sqrt[-1-x^2-x^5] (-2+3 x^5))/(1+x^5)^2,x]*)


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
(*int[(Sqrt[1-x^2-x^3+x^6] (-2-x^3+4 x^6) (1+x^2-2 x^3-x^4-x^5+3 x^6+x^8-2 x^9+x^12))/((1-x^3+x^6)^2 (1-2 x^3-x^4+3 x^6-2 x^9+x^12)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2-x^3-x^6] (2+x^3+4 x^6))/(1-2 x^3-x^4-x^6+2 x^9+x^12),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^2+x^5] (-2+3 x^5))/(1+x^4+2 x^5+x^10),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2+x^4+x^5] (2+2 x^4+3 x^5))/(-1+x^4+x^5)^2,x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1-x^2+x^4])/((1+x^4) (1+x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1-x^2+x^5] (2+3 x^5))/(1-x^4-2 x^5+x^10),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^5] (-2+3 x^5))/((1+x^5) (1-x^2+x^5)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2-x^5] (-2+3 x^5))/((1+x^5) (1+x^2+x^5)),x]*)


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
(*int[(Sqrt[-1-x^6] (-1+2 x^6))/((1+x^6) (1+x^2+x^6)),x]*)


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
(*int[((3 x^7+8) (x^14-x^11-x^8-4 x^7+2 x^4+4)^(3/2))/x^17,x]*)


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
(*int[((-1+x^7) (1+6 x^7))/((-1+x+x^7)^2 Sqrt[1+x^2-2 x^7+x^14]),x]*)


(* ::Input:: *)
(*int[((1-x^2+x^6) (-1+2 x^6))/(Sqrt[1+x^6] (1+x^6)^2),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (1-x+x^2-x^3+x^4))/((1-x+x^2)^2 (1+x+x^2) Sqrt[1+3 x^2+x^4]),x]*)


(* ::Input:: *)
(*int[((1+2 x^6) (1+x^2-x^4-2 x^6-x^8+x^12))/((-1-x^2+x^6)^(5/2) (-1+x^2+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (1+x^4))/((-1+3 x^4-x^8)^(1/4) (1-x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)^(3/4)/(1+x^4),x]*)


(* ::Input:: *)
(*int[(-1+x^4)^(3/4)/(1+x^4+x^8),x]*)


(* ::Input:: *)
(*int[((-1+x^6) (1+x^6))/((x-x^4+x^7)^(1/4) (1+3 x^6+x^12)),x]*)


(* ::Input:: *)
(*int[((-2+x^2) (-1+x^2))/((-1+x^2-x^4)^(1/4) (1-2 x^2+x^4+x^8)),x]*)


(* ::Input:: *)
(*int[((-4+x^3) (-1+x^3)^(3/4))/(1-2 x^3+x^6+x^8),x]*)


(* ::Input:: *)
(*int[((-1+x^6) (1+x^6))/((-x-x^4+x^7)^(1/4) (1-x^6+x^12)),x]*)


(* ::Input:: *)
(*int[((1-x^3+x^4+x^6)^(3/4) (-4+x^3+2 x^6))/(1-x^3+x^6)^2,x]*)


(* ::Input:: *)
(*int[((-1+x^8) (1+x^4+x^8))/(1+x^8)^(9/4),x,"MaxRationalDegree"->24,"MaxNumeratorDegree"->24,"MaxDenominatorDegree"->8]*)


(* ::Input:: *)
(*int[((-1+x^8) (1+x^8))/((-1-x^4+x^8)^(1/4) (1-3 x^8+x^16)),x]*)


(* ::Input:: *)
(*int[x/(1+x^8)^(5/4),x,"MaxDenominatorDegree"->8]*)


(* ::Input:: *)
(*int[((2+x^6) (-1-x^4+x^6))/((1-x^4-x^6)^(1/4) (-1+x^6)^2),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (1+x^2+x^4))/(Sqrt[1+x^4] (1+3 x^4+x^8)),x]*)


(* ::Input:: *)
(*int[((-1-x^8)^(3/4) (-1+x^8))/(1+x^8+x^16),x]*)


(* ::Input:: *)
(*int[((-1+x^8) (1+2 x^4+x^8)^(3/4))/(1+x^4+x^8)^2,x]*)


(* ::Input:: *)
(*int[((1+x^8) (1-x+x^8) (-1+7 x^8))/((1+x+x^8) (1+x^2+2 x^8+x^16)^(3/2)),x]*)


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
(*int[(-1+x^2)/((2-x+2 x^2) Sqrt[x+x^3]),x]*)
