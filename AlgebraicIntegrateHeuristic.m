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

Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*debugPrint*)


$algebraicIntegrateDebug = False;


debugPrint[e__] /; TrueQ[$algebraicIntegrateDebug] := Print @ Style[Row @ {e} /. 
	{A[l_] :> Symbol["A"][l], V[l_] :> Symbol["V"][l], X[l_] :> Symbol["X"][l]}, Brown]


(* ::Subsection::Closed:: *)
(*singleEllipticRadical*)


singleEllipticRadical[e_, x_] := 
	Length[Union[Cases[e, Power[p_, r_Rational] /; (! FreeQ[p, x] && PolynomialQ[p, x]), {0, Infinity}]]] === 1 && 
	Length[Union[
		Cases[e, Power[p_, r_Rational] /; PolynomialQ[p, x] && Exponent[p, x] > 2 :> {p, Abs[r]}, {0, Infinity}], 
		SameTest -> (#1[[2]] == #2[[2]] && PossibleZeroQ[#1[[1]] - #2[[1]]]&)]] === 1


(* ::Input:: *)
(*singleEllipticRadical[Sqrt[-1+x^3],x]*)


(* ::Input:: *)
(*singleEllipticRadical[Sqrt[x]/Sqrt[-1+x^3],x]*)


(* ::Input:: *)
(*singleEllipticRadical[((2+x^3)+Sqrt[(-1+x) (1+x+x^2)])/(x^2 (-2-4 x^2+2 x^3)Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*singleEllipticRadical[x/Sqrt[-(-11+8 Sqrt[3]+2 Sqrt[3] x-x^2) (11+8 Sqrt[3]+2 Sqrt[3] x+x^2)],x]*)


(* ::Subsection::Closed:: *)
(*algebraicQ*)


algebraicQ[e_, x_Symbol] := Complement[
Cases[e, s_Symbol /; (Context[s] === "System`" && !NumericQ[s]), {-1}, Heads -> True],
{Plus, Times, Power, x}] === {} && 
Cases[e, Power[p_, q_] /; (! FreeQ[p,x] && ! MatchQ[Head[q], Integer|Rational]), {0, Infinity}] === {}


(* ::Input:: *)
(*algebraicQ[x/Sqrt[-(-11+8 Sqrt[3]+2 Sqrt[3] x-x^2) (11+8 Sqrt[3]+2 Sqrt[3] x+x^2)],x]*)


(* ::Input:: *)
(*algebraicQ[((2+x^3)+x^2+Sqrt[(-1+x) (1+x+x^2)])/(x^2 (-2-4 x^2+2 x^3)Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*algebraicQ[((2+x^3)+x^Sqrt[2]+Sqrt[(-1+x) (1+x+x^2)])/(x^2 (-2-4 x^2+2 x^3)Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*algebraicQ[((2+x^3)+x^Pi+Sqrt[(-1+x) (1+x+x^2)])/(x^2 (-2-4 x^2+2 x^3)Sqrt[-1+x^3]),x]*)


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

Clear[polynomialsUndetermined];

polynomialsUndetermined[x_Symbol] := polynomialsUndetermined[x] = 
	Join[polynomialsUndetermined0[x], polynomialsUndetermined1[x], 
	polynomialsUndetermined2[x], polynomialsUndetermined3[x]]

polynomialsUndetermined0[x_Symbol] := 
	Flatten @ Table[C[0] + C[1] x^n, {n, tableSize}];

polynomialsUndetermined1[x_Symbol] := 
	Flatten @ Table[
		If[n > m, 
			C[0] + C[1] x^n + C[2] x^m, 
			Sequence @@ {}
		], 
	{n, 0, tableSize}, {m, tableSize}];

polynomialsUndetermined2[x_Symbol] := 
	Flatten @ Table[
		If[n > m > k, 
			C[0] + C[1] x^n + C[2] x^m + C[3] x^k, 
			Sequence @@ {}
		], 
	{n, 0, tableSize}, {m, tableSize}, {k, tableSize}];

polynomialsUndetermined3[x_Symbol] := 
	Flatten @ Table[
		If[n > m > k > l, 
			C[0] + C[1] x^n + C[2] x^m + C[3] x^k + C[3] x^l, 
			Sequence @@ {}
		], 
	{n, 0, tableSize}, {m, tableSize}, {k, tableSize}, {l, tableSize}];


(* ::Subsection::Closed:: *)
(*solveAlgebraicIntegral*)


maxDenominatorPower = 4;


Clear[solveAlgebraicIntegral];

Options[solveAlgebraicIntegral] = {"Verify" -> True};

solveAlgebraicIntegral[integrand_, x_, OptionsPattern[]] := Catch @ Module[
	{k = 0, y = Symbol["y"], u = Symbol["u"], radicands, pSqCan, solution, rationalPart, 
	integrandNumerator, integrandDenominator, p, r, y2radical, matched, radicandMatchRules, 
	rationalMatchRules, rationalFormU, integrandU, intU, intX, unintegratedPart, integratedPart,
	cancellingCoefficient, cancellingTerm, uform, radicandNumeratorU, radicandU,
	radicandDenominatorU, usubstitutionParam, radicandNumeratorUParam, radicandDenominatorUParam},
  
	(* Single radical? *)

	If[! algebraicQ[integrand, x] || ! singleEllipticRadical[integrand, x], 
		Return[{integrand, 0, 0}]];
  
	(* Rewrite the integrand in the form r(x)+(q[x]/h[x]) y^r, where 
		y \[Equal] p[x], r(x) is the non-algebraic (rational) part, and 
		q[x], h[x], p[x] are polynomials in x. *)

	{integrandNumerator, integrandDenominator, {p, r}, y2radical, rationalPart} = normalise[integrand, {x, y}];
	debugPrint["normalise returned ", {rationalPart, integrandNumerator, integrandDenominator, {p, r}, y2radical}];

	(* Defaults. *)
	RationalSubstitution = $Failed;
	solution = False;
	integratedPart = 0;	
	unintegratedPart = integrandNumerator/integrandDenominator /. y2radical;

	(* Check the algebraic part of the integrand is p(x)/q(r)*r(x)^n/m. *)
	integrandNumerator = Cancel[integrandNumerator/y^Exponent[integrandNumerator, y] ];
	If[! FreeQ[integrandNumerator, y], 
		debugPrint["normalise failed ", {integrandNumerator, integrandDenominator}];
		Return[ {0, integrand, 0} ]
	];

	(* Possible radicands in u. *)
	If[Numerator[r] === 2,
		radicands = {A[1] # + A[0] &, A[2] #^2 + A[0] &, A[2] #^2 + A[1] # + A[0] &},
		radicands = {A[1] # + A[0] &}
	];

	(* Loop over substitution numerators. *)
	Do[
		++k;
		(* Radicands of the form uForm, uForm^2 + b, uForm^2 + b uForm + c. *)
		Do[
			(* Loop over radicand denominators 1, x, x^2, ... *)
			Do[

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
					debugPrint["radicand of the form ", radicand[u]];
					debugPrint["u substitution = ", uform];
					debugPrint["radicand numerator/denominator = ", radicandNumeratorU, ", ", radicandDenominatorU];
			
					Do[
						debugPrint["radicand matches ", Style[radicand[u], Darker @ Green], ", ", radicandNumeratorU, radicandMatchRule];
						
						(* Parameterise. *)
						radicandDenominatorUParam = Expand[ radicandDenominatorU /. radicandMatchRule /. {A[1]|A[2]|C[1]|C[2] -> 1, A[0]|C[0] -> 0} ];
						radicandNumeratorUParam = Expand[ radicandNumeratorU /. radicandMatchRule /. {A[1]|A[2]|C[1]|C[2] -> 1, A[0]|C[0] -> 0} ];

						If[! FreeQ[{radicandDenominatorUParam, radicandDenominatorUParam}, C[_]|A[_]], 
							Continue[]];

						debugPrint["radicand numerator = ", radicandNumeratorUParam];
						debugPrint["radicand denominator = ", radicandDenominatorUParam];

						usubstitutionParam = Together[ uform /. radicandMatchRule /. {A[1]|A[2]|C[1]|C[2] -> 1, A[0]|C[0] -> 0} ];

						If[FreeQ[usubstitutionParam, x] || ! FreeQ[usubstitutionParam, C[_]|A[_]] || 
							! FreeQ[radicandNumeratorUParam, C[_]|A[_]], 
								Continue[]];

						debugPrint["u substitution = ", usubstitution, ", ", Style[usubstitutionParam, Red]];

						(* Solve for the rational part of the integral. *)
						{matched, rationalFormU, rationalMatchRules} = solveRational[
							integrandNumerator, integrandDenominator, p, r, 
							usubstitutionParam, radicandDenominatorUParam, x, u];

						If[matched, 
							(* We have found a match. *)
							debugPrint["solution to rational part is ", rationalFormU];
	
							integrandU = rationalFormU (radicand[u]^(1/r) /. radicandMatchRule /. {A[1]|A[2]|C[1]|C[2] -> 1, A[0]|C[0] -> 0});
							debugPrint["calling Integrate on ", integrandU];
	
							intU = Integrate[integrandU, u];
							debugPrint["Integrate returned ", intU];
	
							If[FreeQ[intU, Integrate(* | RootSum | Root *)] && elementaryQ[intU],
								intX = intU /. u -> usubstitutionParam;
								debugPrint["integral is ", intX];
								intX = postProcess[intX];
								debugPrint["post processed integral is ", intX];
								(* Sanity check. *)
								If[TrueQ[! OptionValue["Verify"]] || TrueQ[(
										(* Order tests from fastest to slowest. *)
										numericZeroQ[D[intX, x] - unintegratedPart] || 
										numericZeroQ[D[intX, x] - unintegratedPart, Precision -> 30] || 
										PossibleZeroQ[Together[D[intX, x] - unintegratedPart]] || 
										PossibleZeroQ[D[Simplify[D[intX, x] - unintegratedPart], x]])],
									integratedPart = intX;
									unintegratedPart = 0;
									uform = RationalSubstitution = usubstitutionParam; (* Keep the u-substitution. *)
									(* Throw[{rationalPart, unintegratedPart, integratedPart, {k, u == uform, integrandU}}]; *)
									Throw[{rationalPart, unintegratedPart, integratedPart}],
									(* else *)
									debugPrint["we got something wrong!"];
								],
								(* else *)
								debugPrint["could not integrate wrt u"];
							]
						],
					{radicandMatchRule, radicandMatchRules}]
				],
			{denP, 0, maxDenominatorPower}], 
		{radicand, radicands}],
	{usubstitution, polynomialsUndetermined[x]}];

	debugPrint[k];

	{rationalPart, unintegratedPart, integratedPart}
]


(* ::Input:: *)
(*uu=substitutionTable[2, x][[10]]*)
(*uu= uu /. {C[0]->1,C[1]->-1}*)
(*Sqrt[u]/u du /. {u->uu, du->D[uu,x]} // togetherAll // PowerExpand*)
(*solveAlgebraicIntegral[%, x]*)


(* ::Subsection::Closed:: *)
(*solveRational*)


maxIntegrandDegree = 8;


Clear[solveRational];

solveRational[num_, den_, p_, r_, usubstitution_, radicandDenominator_, x_, u_] := Catch @ Module[
	{degreeBound, du, ratU, ratX, radrat, ratSolution, parameterisedFormU, uMaxDegX},

	uMaxDegX = Max[Exponent[Numerator[usubstitution], x], Exponent[Denominator[usubstitution], x]];
	degreeBound = Max[Exponent[num, x], Exponent[den, x]]/uMaxDegX;
	degreeBound = 1 + Floor[degreeBound];
	debugPrint["degree bound on the rational part is ", degreeBound];
	degreeBound = Min[degreeBound, maxIntegrandDegree];

	Do[
		ratU = rationalUndetermined[u, maxDegree];
		radrat = PowerExpand[radicandDenominator^(-1/r)]; (* Radical contribution to the rational part of the integrand. *)
		debugPrint[{ratU, Together[ratU /. u -> usubstitution], radrat, usubstitution, Together[D[usubstitution, x]]}];
		ratX = Cancel @ Together[ ratU du radrat /. {u -> usubstitution, du -> Together[D[usubstitution, x]]} ];

		(* Print[ {ratU, du, radrat, {u -> usubstitution, du -> Together[D[usubstitution, x]]}} ]; *)

		debugPrint[num/den == ratX];
		debugPrint[Collect[Expand[num Denominator[ratX] - Numerator[ratX] den], x]];	

		ratSolution = SolveAlways[Collect[Expand[num Denominator[ratX] - Numerator[ratX] den], x] == 0, x];

		If[TrueQ[ratSolution =!= {} && ! MatchQ[ratSolution, _SolveAlways] && ! MatchQ[ratSolution, {{(_ -> 0) ..}..}]],
			ratSolution = ratSolution[[1]];
			debugPrint["solution to the rational part is ", ratSolution];
			parameterisedFormU = Cancel[ratU /. ratSolution];
			debugPrint["parameterised forms are ", parameterisedFormU];
			Throw[ {True, parameterisedFormU, ratSolution} ],
			debugPrint["no solution to the rational part"];
		],
	{maxDegree, degreeBound}];

	{False, 0, 0}
]


(* ::Subsection::Closed:: *)
(*solveRadicand*)


solveRadicand[poly_, form_, x_] := Module[{rules},
	(* Exponent[poly, x] === Exponent[form, x] *)
	(* DeleteCases[CoefficientRules[poly, x]\[LeftDoubleBracket]All,1\[RightDoubleBracket], {0}] === DeleteCases[CoefficientRules[form, x]\[LeftDoubleBracket]All,1\[RightDoubleBracket], {0}] *)
	If[Exponent[poly, x] === Exponent[form, x],
		rules = SolveAlways[poly == form, x];
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


polyQ[e_] := With[{v = Union[Flatten[Variables /@ Level[e, {-1}]]]}, 
	PolynomialQ[e, v]]


nicerQ[a_, b_] /; NumberQ[a] && ! NumberQ[b] := True
nicerQ[a_, b_] /; polyQ[a] && polyQ[b] := LeafCount[a] < LeafCount[b]
nicerQ[a_, b_] /; polyQ[a] && ! polyQ[b] := True
nicerQ[_, _] := False


(* ::Text:: *)
(*postProcess is not only for asthetic reasons. We also attempt to correct for the substitution taking a branch of the radical. *)


Clear[postProcess];
postProcess[e_] := Module[{simp, permutations, numerics},

	simp = e /. (h : ArcSinh | ArcCosh)[a_] :> TrigToExp[h[a]];
	simp = simp // togetherAll // powerExpand // togetherAll;
	simp = simp /. Power[p_?polyQ, r_Rational] :> Expand[p]^r;

	(* Some examples for the following rule:
		int[((1 + x^6)*Sqrt[-x - x^4 + x^7])/(1 + 2*x^3 - 2*x^9 + x^12), x]
		int[((-x + x^3)^(1/3)*(-2 + x^4))/(x^4*(1 + x^2)), x] *)
	simp = simp /. (p_?polyQ x_Symbol^m_Integer)^n_Rational /; 
		m < 0 :> Expand[Numerator[p]x^(Ceiling[Abs[m], Denominator[n]] + m)]^n /x^(n (Ceiling[Abs[m], Denominator[n]]));

	(* Collect and partially simplify terms. *)
	permutations = Table[Select[Tuples[{Power[_,_Rational],_Log,_ArcTan},{k}], Length[Union[#]] == k&], {k, 3}];
	simp = SortBy[Table[Fold[Collect[#1, #2, Together]&, simp, permutations[[k]]], {k, Length @ permutations}], LeafCount][[1]];
	
	(* Pick the nicer of ArcTan[a/b] or -ArcTan[b/a] *)
	simp = simp /. ArcTan[a_] /; nicerQ[Numerator[a], Denominator[a]] :> -ArcTan[Denominator[a]/Numerator[a]];

	(* This can often result in a simplification as denominators of sums of logs often cancel. *)
	simp = simp /. c_. Log[ex_] /; Denominator[ex] =!= 1 :> c Log[Numerator[ex]] - c Log[Denominator[ex]];

	(* Just for display purposes, as we often require multiple lines to display result. *)
	simp = simp /. c_?NumericQ (p_Plus) :> Distribute[c p];

	(* Remove constants. *)
	If[Head[simp] === Plus, 
		numerics = Cases[simp, n_ /; NumericQ[n], {1}];
		simp -= Total[numerics];
	];

	simp
]


(* ::Input:: *)
(*postProcess[Sqrt[(1+x^2)/x]/(1+(1+x^2)/x)+ArcTan[Sqrt[(1+x^2)/x]]]*)


(* ::Input:: *)
(*postProcess[(Sqrt[2] Sqrt[1-x^6])/(5 x^5)-(Sqrt[2] Sqrt[1-x^6])/(3 x^3)-2/5 Sqrt[2] x Sqrt[1-x^6]+1/3 Sqrt[2] x^3 Sqrt[1-x^6]+1/5 Sqrt[2] x^7 Sqrt[1-x^6]]*)


(* ::Input:: *)
(*postProcess[-((4 Sqrt[-2+2 x^5-x^7+x^8])/(3 x^6))+(6 Sqrt[-2+2 x^5-x^7+x^8])/x^2+(4 Sqrt[-2+2 x^5-x^7+x^8])/(3 x)-2/3 x Sqrt[-2+2 x^5-x^7+x^8]+2/3 x^2 Sqrt[-2+2 x^5-x^7+x^8]+3 Log[1-Sqrt[-2+2 x^5-x^7+x^8]/x^2]-3 Log[1+Sqrt[-2+2 x^5-x^7+x^8]/x^2]]*)


(* ::Subsection::Closed:: *)
(*numericZeroQ*)


(* ::Text:: *)
(*TODO: Perhaps this should be clever and look at $Assumptions? Or if assumptions are made then we just (possibly) fail and rely on PossibleZeroQ and Simplify in subsequent (more time intensive) testing. *)


Clear[numericZeroQ];

Options[numericZeroQ] = {Precision -> $MachinePrecision, Tolerance -> 1.0*^-6};

numericZeroQ[e_, OptionsPattern[]] := Module[{v, ef, lower, upper, step},
	v = Union[Flatten[Variables /@ Level[e, {-1}]]];
	ef = Function @@ {v, e};

	lower = -10.0 - (119. E)/(121. Pi); (* Some crazy numbers to hopefully not hit a pole. *)
	upper = 10.0 + (119. E)/(121. Pi);
	step  = (1999. Sqrt[2])/(4003.);

	(* TODO: test using Compile? *)
	Norm[
		Mean[
			Select[
				Table[
					Quiet[
						ef @@ SetPrecision[Table[r Random[] Exp[2.0 Pi I k/12], {Length @ v}], OptionValue[Precision]]
						],
					{k, 0, 11},
					{r, lower, upper, step}
				] // Flatten, 
			NumericQ]
		]
	] < SetPrecision[OptionValue[Tolerance], OptionValue[Precision]] // TrueQ
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
(*elementaryQ*)


elementaryQ[expr_] := Complement[
	Cases[Level[expr // TrigToExp, {-1}, Heads -> True], s_Symbol /; Context[s] === "System`"] // Union, 
	{Log, Exp, Plus, Times, Power, RootSum, Root, List, Function, Slot}
] === {}


(* ::Subsection::Closed:: *)
(*integrating linear and quadratic radicals *)


(* ::Text:: *)
(*Mathematica has trouble with some linear and quadratic radical rationals which have elementary forms. eg. ((1+x)^(2/3) (1-4 x+2 x^2))/(3 (-1+x)^2). We have not stitched these into solveAlgebraicIntegral[] yet. *)


rational2dQ[e_, {x_, y_}] := With[{te = Together[e]},
Denominator[te] =!= 1 && PolynomialQ[Numerator[te],{x,y}] && PolynomialQ[Denominator[te],{x,y}]]


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


linearRadicalToRational[e_,x_, u_] := Module[{y,radicals,a, b, n,reps,exy},

(* Find radicals of the form (a x + b)^(n/m). *)
radicals = Cases[e, y:(a_. x + b_.)^n_ /; 
	FreeQ[{a,b},x] && Head[n] == Rational :> {y,a x + b, Numerator[n], Denominator[n], a, b}, {0, Infinity}];

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
(*linearRadicalToRational[((1+u)^(2/3) (1-4 u+2 u^2))/(3 (-1+u)^2),u,t]*)
(*Integrate[First[%],t] /. Last[%]*)
(*Integrate[((1+u)^(2/3) (1-4 u+2 u^2))/(3 (-1+u)^2),u]*)


(* ::Subsubsection::Closed:: *)
(*quadraticRadicalToRational*)


quadraticRadicalToRational[e_, x_, u_] := Module[
{y, radicals, a, b, c, exy, dx, U, X, \[Alpha], \[Beta]},
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

a = Coefficient[radicals[[1,2]], x,2];
b = Coefficient[radicals[[1,2]], x,1];
c = Coefficient[radicals[[1,2]], x,0];

Which[
a > 0,
	(* Euler's first substitution. *)
	X = (u^2 - c)/(b - 2 Sqrt[a] u);
	U = radicals[[1,1]] - Sqrt[a] x;
	dx = 2(-Sqrt[a] u^2 + b u - Sqrt[a]c)/(-2Sqrt[a]u + b)^2;
	exy = exy dx /. {y -> Sqrt[a]X + u, x -> X},
c > 0,
	(* Euler's second substitution. *)
	X = Cancel @ Together[(2Sqrt[c]u - b)/(a - u^2)];
	U = (radicals[[1,1]] - Sqrt[c])/x;	
	dx = 2(Sqrt[c] u^2 - b u + a Sqrt[c])/(u^2 - a)^2;
	exy = exy dx /. {y -> X u + Sqrt[c], x -> X},
True, 
	(* Euler's third substitution. *)
	\[Alpha] = -b/(2 a) - Sqrt[b^2 - 4 a c]/(2 a);
	\[Beta] = -b/(2 a) + Sqrt[b^2 - 4 a c]/(2 a);
	X = Cancel @ Together[(a \[Beta] - \[Alpha] u^2)/(a - u^2)];
	U = radicals[[1,1]]/(x - \[Alpha]);
	dx = 2 u a (\[Beta] - \[Alpha])/(u^2 - a)^2;
	exy = exy dx /. {y -> (X - \[Alpha])u, x -> X}
];

{exy // Cancel, u -> U}
]


(* ::Subsection::Closed:: *)
(*End Package*)


End[];
EndPackage[];


(* ::Section:: *)
(*Examples, wish list, regression tests, ...*)


(* ::Input:: *)
(*AlgebraicIntegrateHeuristic`Private`$algebraicIntegrateDebug=.;*)
(*int = solveAlgebraicIntegral;*)


(* ::Text:: *)
(*Example from paper:*)


(* ::Input:: *)
(*int[((-2+x^3) Sqrt[1+x^2+x^3])/(1+x^3)^2,x]*)


(* ::Text:: *)
(*My favourite 4 examples thus far:*)


(* ::Input:: *)
(*int[((x^2-1) Sqrt[x^4+x^2+1])/((x^2+1) (x^4+x^3+x^2+x+1)),x]*)


(* ::Input:: *)
(*int[((x^4-1) Sqrt[1+x^2+x^4])/((1+x^4) (1-x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(-2+3 x^5)/((1+x^5) Sqrt[1+x^2+x^5]),x]*)


(* ::Input:: *)
(*int[(1+x^8)/((1-x^8) Sqrt[1-x^2+x^4]),x]*)


(* ::Text:: *)
(*An example where we need the recursive call to do better at returning an elementary solution.*)


(* ::Input:: *)
(*int[((-1-x^4)^(1/4) (-1+x^4))/(x^6 (1+2 x^4)),x]*)


(* ::Text:: *)
(*We now allow symbolic summation over the roots of a polynomial in the results.*)


(* ::Input:: *)
(*int[((-1+x^8) (-1+x^4)^(1/4))/(x^6 (1+x^8)),x]*)


(* ::Subsection::Closed:: *)
(*current bugs*)


(* ::Text:: *)
(*This isn't specifically a bug, but the result form is unfortunate.*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+2 x^2+x^4])/((1+x^2) (1+x^4)),x]*)


(* ::Text:: *)
(*Can we get a solution for all x?*)


(* ::Input:: *)
(*AlgebraicIntegrateHeuristic`Private`$algebraicIntegrateDebug=.;*)
(*Assuming[x>0,int[(x-1)/((1+x) Sqrt[x+x^2+x^3]),x]]*)


(* ::Text:: *)
(*Investigate if these are bugs:*)


(* ::Input:: *)
(*int[((3+2 x^2) Sqrt[x+2 x^3])/(1+2 x^2)^2,x]*)


(* ::Input:: *)
(*int[((3+2 x^5) Sqrt[x-2 x^4-x^6])/(-1+x^5)^2,x]*)


(* ::Input:: *)
(*int[((3+x^4) Sqrt[x-x^5])/(1-2 x^4-x^6+x^8),x]*)


(* ::Subsection:: *)
(*wish list*)


(* ::Text:: *)
(*Work out why we can do one but not the other in these two:*)


(* ::Input:: *)
(*int[1/(x (x^6+1)^(1/3)),x]*)
(*int[x/(x^6+1)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x^3+x^2)^(1/3),x]*)
(*int[(x^3+x^2)^(1/3),x]*)


(* ::Input:: *)
(*AlgebraicIntegrateHeuristic`Private`$algebraicIntegrateDebug=True;*)


(* ::Text:: *)
(*Top of the wish list. From sci.math.symbolic in the early 90's:*)


(* ::Input:: *)
(*int[x/Sqrt[-71-96 x+10 x^2+x^4],x]*)


(* ::Text:: *)
(*Surely this one should be computable? *)


(* ::Input:: *)
(*int[(1+x^2)/((1-x^2) Sqrt[1-x^4-x^8]),x]*)


(* ::Subsection::Closed:: *)
(*assumptions*)


(* ::Text:: *)
(*We now have a solution for all x. (used to require Assuming[x > 0, int[(1+x^6)/((1-x^6) Sqrt[x+x^2+x^3]),x]])*)


(* ::Input:: *)
(*int[(1+x^6)/((1-x^6) Sqrt[x+x^2+x^3]),x]*)


(* ::Subsection::Closed:: *)
(*previously bugs or edge cases*)


(* ::Input:: *)
(*AlgebraicIntegrateHeuristic`Private`$algebraicIntegrateDebug=True;*)


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


(* ::Subsection:: *)
(*regression testing*)


(* ::Text:: *)
(*All tests pass. *)


(* ::Input:: *)
(*AlgebraicIntegrateHeuristic`Private`$algebraicIntegrateDebug=False;*)


(* ::Input:: *)
(*int[1/(x^4-x^2)^(1/4),x]*)


(* ::Input:: *)
(*int[1/(x (x^3+1)^(1/3)),x]*)


(* ::Input:: *)
(*int[((x^4-1) (x^4+1)^(1/4))/x,x]*)


(* ::Input:: *)
(*int[((x^4-1) Power[x^4+1, (4)^-1])/x^2,x]*)


(* ::Input:: *)
(*int[1/((x^4-1)^(1/4) (3 x^4+1)),x]*)


(* ::Input:: *)
(*int[1/(x Power[x^4-1, (4)^-1]),x]*)


(* ::Input:: *)
(*int[(2 x^8-2 x^4+1)/((x^4-1)^(1/4) (2 x^8-x^4-2)),x]*)


(* ::Input:: *)
(*int[((x-1) (x^4+x^3)^(1/4))/(x (x+1)),x]*)


(* ::Input:: *)
(*int[((x^2+1) (x^4+x^3)^(1/4))/(x^2 (x^2-1)),x]*)


(* ::Input:: *)
(*int[(x^2+1)/((x^2-1) (x^5-x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x^8 Power[x^4+x^3, (4)^-1]),x]*)


(* ::Input:: *)
(*int[1/(x^4 Power[x^4+x^3, (4)^-1]),x]*)


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
(*int[(x^2+1)/((x^2+x-1) Power[x^4-x^2, (3)^-1]),x]*)


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
(*int[(Sqrt[-1-x^3] (-2+x^3))/((1+x^3) (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^3] (2+x^3))/((-1+x^3) (-1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((2+x^3) Sqrt[-1-x^2+x^3])/((-1+x^3) (-1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1-x^2-x^3] (-2+x^3))/((1+x^3) (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^4] (1+x^4))/(1-x^4+x^8),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^2+x^4] (1+x^2+3 x^4+x^6+x^8))/((1+x^4)^3 (1-x^2+x^4)),x]*)


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
(*int[(Sqrt[-1-x^2-x^4] (-1+x^4) (1+x^2+3 x^4+x^6+x^8))/((1+x^2+x^4)^2 (1+3 x^2+5 x^4+3 x^6+x^8)),x]*)


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
(*int[(Sqrt[-1-x^2-x^4] (-1+x^4))/(1+x^4+x^8),x]*)


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
