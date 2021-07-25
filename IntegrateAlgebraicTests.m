(* ::Package:: *)

(* ::Title:: *)
(*IntegrateAlgebraic - examples, wish list, bugs, deficiencies, previously bugs, edge cases & regression tests*)


(* ::Subtitle:: *)
(*Sam Blake, 2020*)


(* ::Text:: *)
(*samuel.thomas.blake@gmail.com*)


(* ::Text:: *)
(*Started on 16 March 2020.*)


(* ::Input:: *)
(*$verboseLevel=0;*)
(*int=IntegrateAlgebraic;*)


(* ::Subsection::Closed:: *)
(*current bugs and deficiencies*)


(* ::Text:: *)
(*For this integral, we have issues when repairing branch cuts:*)


(* ::Input:: *)
(*IntegrateAlgebraic[*)
(*  ((1 + 4*x + a*x + 6*x^2 + 4*a*x^2 + 4*x^3 + *)
(*     6*a*x^3 + x^4 + 4*a*x^4 + a*x^5)/*)
(*    ((-c)*x^4 + b*x^5))^(1/4), x]*)


(* ::Text:: *)
(*This should be fixed in post processing of linearRationalIntegrate:*)


(* ::Input:: *)
(*int[x/(x^3+x^2-x-1)^(1/3),x]*)


(* ::Input:: *)
(*int[((-7+x) (1+x-5 x^2+3 x^3)^(1/3))/((-5+x) (-1+x)^3),x]*)


(* ::Text:: *)
(*This one should pass verifySolution faster:*)


(* ::Input:: *)
(*int[(x^3 (x^3 C[3]-2 C[4]))/((x^2+2 x^3 C[3]+2 C[4]) (-x^4+x^6 C[3]^2+2 x^3 C[3] C[4]+C[4]^2) ((x^2 C[0]+x^3 C[3]+C[4])/(x^2 C[1]+x^3 C[3]+C[4]))^(1/3)),x,"SingleStepTimeConstraint"->5]*)


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
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)-(1/Sqrt[1-Sqrt[x]]-Sqrt[1-Sqrt[x]-x])//Simplify*)


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


(* ::Subsubsection::Closed:: *)
(*rationalUndeterminedIntegrate*)


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
(*int[(x (x^2 C[3]-C[4]))/((x+3 x^2 C[3]+3 C[4]) (-x^2+x^4 C[3]^2+2 x^2 C[3] C[4]+C[4]^2) Sqrt[(x C[0]+x^2 C[3]+C[4])/(x C[1]+x^2 C[3]+C[4])]),x]*)


(* ::Input:: *)
(*int[((x^2 C[3]-C[4]) (x+3 x^2 C[3]+3 C[4]))/((-x^3+x^6 C[3]^3+3 x^4 C[3]^2 C[4]+3 x^2 C[3] C[4]^2+C[4]^3) Sqrt[(x C[0]+x^2 C[3]+C[4])/(x C[1]+x^2 C[3]+C[4])]),x,"SingleStepTimeConstraint"->2]*)


(* ::Input:: *)
(*int[(x^5 (x^2 C[3]+3 C[4]))/((x^3+3 x^2 C[3]+3 C[4]) (x^6-x^4 C[3]^2-2 x^2 C[3] C[4]-C[4]^2) Sqrt[(x^3 C[0]+x^2 C[3]+C[4])/(x^3 C[1]+x^2 C[3]+C[4])]),x,"SingleStepTimeConstraint"->10]*)


(* ::Input:: *)
(*int[(x^3 (x^3 C[3]-2 C[4]) Sqrt[(x^2 C[0]+x^3 C[3]+C[4])/(x^2 C[1]+x^3 C[3]+C[4])])/((x^2+2 x^3 C[3]+2 C[4]) (-x^4+x^6 C[3]^2+2 x^3 C[3] C[4]+C[4]^2)),x]*)


(* ::Input:: *)
(*int[(x^5 (2 x^5 C[3]-3 C[4]) Sqrt[(x^3 C[0]+x^5 C[3]+C[4])/(x^3 C[1]+x^5 C[3]+C[4])])/((x^3+2 x^5 C[3]+2 C[4]) (-x^6+x^10 C[3]^2+2 x^5 C[3] C[4]+C[4]^2)),x,"SingleStepTimeConstraint"->2.5]*)


(* ::Input:: *)
(*int[(x (2 x^3 C[3]-C[4]))/((-x+x^3 C[3]+C[4]) (x^2+x^4 C[3]+x^6 C[3]^2+x C[4]+2 x^3 C[3] C[4]+C[4]^2) ((x C[0]+x^3 C[3]+C[4])/(x C[1]+x^3 C[3]+C[4]))^(1/3)),x,"SingleStepTimeConstraint"->2.5]*)


(* ::Input:: *)
(*int[(x^2 (x^2 C[3]-C[4]) ((x C[0]+x^2 C[3]+C[4])/(x C[1]+x^2 C[3]+C[4]))^(1/4))/((-x+x^2 C[3]+C[4]) (x+x^2 C[3]+C[4]) (x^2+x^4 C[3]^2+2 x^2 C[3] C[4]+C[4]^2)),x,"SingleStepTimeConstraint"->2.5]*)


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
(*int[((2+x) (-1-x+x^2) ((1+x-2 x^2)/(1+x+4 x^2))^(1/4))/(2 x (1+2 x+x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-4-3 x+2 x^2) (1+x-x^2+x^4) ((1+x-x^2+2 x^4)/(1+x-x^2+3 x^4))^(1/3))/(x^5 (-1-x+x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-2+x) (1-x+x^2))/(x^3 (-1+x+x^2) ((1-x+2 x^2)/(1-x+3 x^2))^(1/3)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (1-3 x^2+x^4))/(x^2 (1-x-3 x^2+x^3+x^4) Sqrt[(-2+x+2 x^2)/(-1+x+x^2)]),x]*)


(* ::Input:: *)
(*int[(x^2-x+1)/((x^2-1) Sqrt[x^4-x^3+x^2-x+1]),x]*)


(* ::Input:: *)
(*int[(x^2-1)/((x^2+1) Sqrt[x^4-x^3-x^2-x+1]),x]*)


(* ::Input:: *)
(*int[(2+x)/((x-1) Sqrt[x^3+3 x-1]),x]*)


(* ::Input:: *)
(*int[((d+1) x-a)/(((e-1) x^2+2 a x-a^2) (x^2 (x-a))^(1/3)),x]*)


(* ::Input:: *)
(*int[(x-a)/(((d-1) x^2-2 d a x+d a^2) (x^2 (x-a))^(1/3)),x]*)


(* ::Input:: *)
(*int[((d+1) x^2-2 d a x+d a^2)/((a-x)^2 ((1-e) x+a e) (x^2 (x-a))^(1/3)),x]*)


(* ::Input:: *)
(*int[(-Sqrt[a b]+x)/(Sqrt[x (a+x) (b+x)] (Sqrt[a b]+x)),x]*)


(* ::Input:: *)
(*int[(a b x-x^3)/(Sqrt[x (-a+x) (-b+x)] (a^2 b^2 d-2 a b (a+b) d x+(-1+a^2 d+4 a b d+b^2 d) x^2-2 (a+b) d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x) (-a b+x^2))/(Sqrt[x (-a+x) (-b+x)] (a^2 b^2 d-2 a b (a+b) d x+(-1+a^2 d+4 a b d+b^2 d) x^2-2 (a+b) d x^3+d x^4)),x]*)


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
(*int[1/((-1+x) (2-2 x+x^2)^(1/3)),x]*)


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
(*int[(k^3 x^3-1)/((k^3 x^3+1) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(k^4 x^4-1)/((k^4 x^4+1) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(k x^2-1)/((a k x+b) (b x+a) Sqrt[x (1-x) (1-k x)]),x]*)


(* ::Input:: *)
(*int[(2+x-x^3)/(Sqrt[1+x+x^3] (1+x-x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(1-2 x)/Sqrt[5+5 x-4 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(1+2 x)/Sqrt[-4-3 x-2 x^2+2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+2 x)/Sqrt[-4-4 x+5 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[1/(x (x^6+1)^(1/3)),x]*)


(* ::Input:: *)
(*int[x/(1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[((1+x+x^2) (2 x+x^2) Sqrt[1+2 x+x^2-x^4])/(1+x)^4,x]*)


(* ::Input:: *)
(*int[(1+x^6)/((1-x^6) Sqrt[x+x^2+x^3]),x]*)


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
(*int[x/(x^3+x^2)^(1/3),x]*)


(* ::Input:: *)
(*int[(x^3+x^2)^(1/3)/x,x]*)


(* ::Input:: *)
(*int[(x^2+2)/((x^2+1) (x^3-x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^3 (x^3+1) (x^3-x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^3 (c x^3+d) (a x^3-b x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[((x^3+1) (x^3-x^2)^(1/3))/(x^6+1),x]*)


(* ::Input:: *)
(*int[(x^3 (x^3-x^2)^(1/3))/(x^6+1),x]*)


(* ::Input:: *)
(*int[x^3/((x^3-x^2)^(1/3) (x^6-1)),x]*)


(* ::Input:: *)
(*int[x^3/((a x^3-b x^2)^(1/3) (a^2 x^6-b^2)),x]*)


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
(*int[((1+x^4) (-1+x^2+x^4)^(3/2))/((-1+x^4) (1+x^2-x^4-x^6+x^8)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+2 x^2-x^4] (-1+x^4) (1+x^4))/((-1-x^2+x^4) (1+3 x^2-x^4-3 x^6+x^8)),x]*)


(* ::Input:: *)
(*int[((2+x^3) Sqrt[-1+x^2+x^3])/(-1+x^3)^2,x]*)


(* ::Input:: *)
(*int[((3+x^4) Sqrt[x+x^4-x^5])/((-1+x^4) (-1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((1+2 x^3)^(4/3) (1+3 x^3))/(x^8 (1+4 x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^6) (-1+2 x^6) (-1+x^4+2 x^6)^(5/4))/(x^10 (-1-x^4+2 x^6)),x]*)


(* ::Input:: *)
(*int[((2+3 x^4)^(1/4) (4+6 x^4+x^8))/(x^6 (1+x^4) (1+2 x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(1/4) (-1+x^8))/(x^6 (1+x^8)),x]*)


(* ::Input:: *)
(*int[((1+2 x^4) Sqrt[2-x^2-4 x^4])/((-1+2 x^4) (-1-x^2+2 x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^4] (1+x^4))/(4-7 x^4+4 x^8),x]*)


(* ::Input:: *)
(*int[((-2+x^4) Sqrt[2+x^4])/(4+3 x^4+x^8),x]*)


(* ::Input:: *)
(*int[(1+x^6)/((1-x^6) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+x^4])/(1+x^2)^3,x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+x^4])/((1-x+x^2) (1+x+x^2)^2),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (1+x+x^2) Sqrt[1+3 x^2+x^4])/(1+x+x^2+x^3+x^4)^2,x]*)


(* ::Input:: *)
(*int[((2+x^3) Sqrt[-1-x^2+x^3])/(1-x^2-2 x^3-x^4+x^5+x^6),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^4] (1+x^4))/(1-x^4+x^8),x]*)


(* ::Input:: *)
(*int[((x^4-1) (1+x^2+3 x^4+x^6+x^8) Sqrt[1+x^2+x^4])/((1+x^4)^3 (1-x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[-1+2 x^2+x^4])/((-1+x^4) (-1+x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^4])/(1+x^2+3 x^4+x^6+x^8),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^4] (-1+x^4) (1+x^2+3 x^4+x^6+x^8))/((1+x^2+x^4)^2 (1+3 x^2+5 x^4+3 x^6+x^8)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^4] (-1+x^4))/(1+x^8),x]*)


(* ::Input:: *)
(*int[((-2+x^4) Sqrt[2+x^4])/((2+x^2+x^4) (2+2 x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-3 x^2-2 x^4] (1+2 x^4))/((-1+x^2+2 x^4) (-1+2 x^2+2 x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^5] (-2+3 x^5))/(1+x^4+2 x^5+x^10),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^6] (1+2 x^6))/(1+x^4-2 x^6+x^12),x]*)


(* ::Input:: *)
(*int[(Sqrt[-2+x^2-2 x^3+2 x^4] (2-x^3+2 x^4))/((-1-x^3+x^4) (-2-x^2-2 x^3+2 x^4)),x]*)


(* ::Input:: *)
(*int[((-4+x^3) Sqrt[2-x^2+x^3])/((2+x^3) (2+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-2+x^3) Sqrt[2+x^2+2 x^3])/((1+x^3) (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2-x^3+x^5] (2-x^3+3 x^5))/(1+2 x^3+x^4-2 x^5+x^6-2 x^8+x^10),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2+x^5] (2+3 x^5))/(1+x^4-2 x^5+x^10),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^6] (1+2 x^6) (1+x^2-x^4-2 x^6-x^8+x^12))/((-1+x^6) (-1+2 x^6-3 x^12+x^18)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^5] (-2+3 x^5))/((1+x^5) (1-x^2+x^5)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^4])/(1+3 x^4+x^8),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2+x^4+x^6] (1+x^4+2 x^6))/(1-x^4-2 x^6+x^8+2 x^10+x^12),x]*)


(* ::Input:: *)
(*int[((1-x^2+2 x^4) Sqrt[1-x^2-x^4-x^6])/((-1+x^2) (1+x^2) (-1+x^4+x^6)),x]*)


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
(*int[(Sqrt[3 x^4-2] (3 x^4-1))/(x^3 (3 x^8-3 x^4+1)),x]*)


(* ::Input:: *)
(*int[(Sqrt[2 x^4-1] (2 x^8-1))/(x^7 (x^8+2 x^4-1)),x]*)


(* ::Input:: *)
(*int[((x^2+1) (x^8+1) Sqrt[x^8+x^6+x^4+x^2+1])/(x^7 (x^2-1)),x]*)


(* ::Input:: *)
(*int[((x^6+1) (x^6+x^3-1) Sqrt[x^12+1])/(x^7 (x^6-x^3-1)),x]*)


(* ::Input:: *)
(*int[((x^3-1)^3 (x^3+1) Sqrt[2 x^12+3 x^6+2])/(x^7 (x^6+1)),x]*)


(* ::Input:: *)
(*int[((3 x^4-1) Sqrt[x^8+x^5+2 x^4+x+1])/(x^2 (4 x^4+x+4)),x]*)


(* ::Input:: *)
(*int[((x^6-4) (x^6-x^4+2)^(5/2))/(x^7 (x^6+2)^2),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1-2 x^2-2 x^3-x^8] (-1+x^3+3 x^8))/((1+2 x^3+x^8) (1+x^2+2 x^3+x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (1-x+x^2-x^3+x^4))/((1-x+x^2)^2 (1+x+x^2) Sqrt[1+3 x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^4)^(3/4)/(1+x^4),x]*)


(* ::Input:: *)
(*int[((-1+x^6) (1+x^6))/((x-x^4+x^7)^(1/4) (1+3 x^6+x^12)),x]*)


(* ::Input:: *)
(*int[((1-x^3+x^4+x^6)^(3/4) (-4+x^3+2 x^6))/(1-x^3+x^6)^2,x]*)


(* ::Input:: *)
(*int[((-1+x^8) (1+x^8))/((-1-x^4+x^8)^(1/4) (1-3 x^8+x^16)),x]*)


(* ::Input:: *)
(*int[((2+x^6) (-1-x^4+x^6))/((1-x^4-x^6)^(1/4) (-1+x^6)^2),x]*)


(* ::Input:: *)
(*int[((1+a x^8)^(3/4) (-1+a x^8))/(1+x^8+a^2 x^16),x]*)


(* ::Input:: *)
(*int[((1+x^6)^2 (-1+2 x^6))/((1-x^2+x^6)^(3/2) (1-x^2-x^4+2 x^6-x^8+x^12)),x]*)


(* ::Input:: *)
(*int[(1+3 x^4)/(Sqrt[1+3 x^4] (-1+3 x^4)),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[x+x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(-3+x^4)/((1+x^4) (-3 x+4 x^4-3 x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^8)/((-1+x^8) (1+x^8)^(1/4)),x]*)


(* ::Subsubsection::Closed:: *)
(*directRationalise*)


(* ::Input:: *)
(*int[1/(x^3+x^2-x-1)^(1/3),x]*)


(* ::Input:: *)
(*int[1/((-b+a x) (b^2 x+a^2 x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[((x^2 C[3]-C[4]) (x+3 x^2 C[3]+3 C[4]))/(x (-x^2+x^4 C[3]^2+2 x^2 C[3] C[4]+C[4]^2) Sqrt[(x C[0]+x^2 C[3]+C[4])/(x C[1]+x^2 C[3]+C[4])]),x]*)


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
(*int[x/Sqrt[x^4+2 x^3-3 x^2+3 x-3],x]*)


(* ::Input:: *)
(*int[(x+2)/((x-3) (x^2+1) (1-x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((a x^2-2 b) (a x^2-b)^(1/4)),x]*)


(* ::Input:: *)
(*int[(k^2 x^3+(k^2-2) x)/((x^4+(d k^2-2) x^2-d+1) ((1-x^2) (1-k^2 x^2))^(1/3)),x]*)


(* ::Input:: *)
(*int[((x+a-2 b) (x-b))/((x^4-4 a x^3+(6 a^2-d) x^2-2 (2 a^3-b d) x+a^4-b^2 d) ((x-a) (x-b))^(1/3)),x]*)


(* ::Input:: *)
(*int[(a-2 b+x)/(((-a+x) (-b+x))^(1/3) (b+a^2 d+(-1-2 a d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(x (-b+x) (a b-2 a x+x^2))/((-a+x) Sqrt[x (-a+x) (-b+x)] (a d+(-b-d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(a^2 b-a (2 a+b) x+3 a x^2-x^3)/(x (-b+x) Sqrt[x (-a+x) (-b+x)] (a+(-1-b d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(-a^2 b+a (2 a+b) x-3 a x^2+x^3)/(Sqrt[x (-a+x) (-b+x)] (-a^2+2 a x+(-1+b^2 d) x^2-2 b d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(-a^2 b+a (2 a+b) x-3 a x^2+x^3)/(Sqrt[x (-a+x) (-b+x)] (-a^2-a (-2+b e) x+(-1+b^2 d+a e+b e) x^2+(-2 b d-e) x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[1/(x (2-3 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-5+x)/((-2-x+x^2)^(1/3) (-3+4 x+x^2)),x]*)


(* ::Input:: *)
(*int[(6+2 x+x^2)/((2+x) (2+x^2) (2+x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(6+2 x+x^2)/((1+x) (2+x+x^2)^(1/3) (2-x+2 x^2)),x]*)


(* ::Input:: *)
(*int[(k^2 x^2-2 k^2 x+1)/(((a k^2+b k^2) x^2-b x-a) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(x^2-x)/((k^2 x^2-2 x+1) Sqrt[x (1-x) (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[1/(x (3 x^2-6 x+4)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1-2 x+k^2 x^2)/((-1+2 x-2 x^2+k^2 x^2) Sqrt[x-x^2-k^2 x^2+k^2 x^3]),x]*)


(* ::Input:: *)
(*int[1/(x ((x-1) (x^2-2 q x+q))^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x ((x+1) (x^2+2 q x+q))^(1/3)),x]*)


(* ::Input:: *)
(*int[(x+3)/((x-1)^2 (x^2-1)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x+1)/((x+3) (2 x+1) (x^2+1)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x+1)/((x-1) (2 x+1) (3 x^2-1)^(1/3)),x]*)


(* ::Input:: *)
(*int[(3 x+2)/((9 x^2+52 x-12) (3 x^2+4)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((x+1) (x^4+6 x^2+1)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-3+2 x+2 x^5) Sqrt[x-x^2+x^6])/(1-2 x+x^2-x^3+x^4+2 x^5-3 x^6-x^8+x^10),x]*)


(* ::Subsubsection::Closed:: *)
(*logPartIntegrate*)


(* ::Input:: *)
(*int[x/Sqrt[-71-96 x+10 x^2+x^4],x]*)


(* ::Input:: *)
(*int[(1+4 x)/Sqrt[1-2 x+3 x^2+2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+x)/Sqrt[-2-x+6 x^2-4 x^3+x^4],x]*)


(* ::Input:: *)
(*int[x/Sqrt[-17+18 x-11 x^2+6 x^3+x^4],x]*)


(* ::Input:: *)
(*int[x/Sqrt[1+4 x+3 x^2-2 x^3+x^4],x]*)


(* ::Subsubsection::Closed:: *)
(*decreasePolynomialRadicandDegreeIntegrate*)


(* ::Input:: *)
(*$verboseLevel=2;*)


(* ::Input:: *)
(*int[Sqrt[-1-11 x-36 x^2-27 x^3+16 x^4+9 x^5+x^6],x]*)


(* ::Input:: *)
(*int[(x^4+b)^2/(a b^4+4 a b^3 x^4+b^4 x^4+6 a b^2 x^8+4 b^3 x^8+4 a b x^12+6 b^2 x^12+a x^16+4 b x^16+x^20)^(1/4),x]*)


(* ::Input:: *)
(*int[(1-(1-3 x+3 x^3-9 x^4+3 x^6-9 x^7+x^9-3 x^10)^(1/3))/(x^2+(1-3 x+3 x^3-9 x^4+3 x^6-9 x^7+x^9-3 x^10)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x+Sqrt[1+4 x+7 x^2+8 x^3+5 x^4+2 x^5])/(1-Sqrt[1+4 x+7 x^2+8 x^3+5 x^4+2 x^5]),x]*)


(* ::Input:: *)
(*int[1/(1-3 x+3 x^3-9 x^4+3 x^6-9 x^7+x^9-3 x^10)^(1/3),x]*)


(* ::Input:: *)
(*int[(1-3 x+3 x^3-9 x^4+3 x^6-9 x^7+x^9-3 x^10)^(1/3),x]*)


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
(*int[Sqrt[1-12 u^4+16 u^6]/((-2+u^2) (1+4 u^2)),u]*)


(* ::Input:: *)
(*int[1/Sqrt[3-5 x+x^2+x^3],x]*)


(* ::Input:: *)
(*int[1/Sqrt[3+4 x+x^4],x]*)


(* ::Input:: *)
(*int[(1-x)/Sqrt[3+2 x-5 x^2-4 x^3+x^4+2 x^5+x^6],x]*)


(* ::Input:: *)
(*int[(x Sqrt[-x^2+x^4])/(-3+2 x^2),x]*)


(* ::Input:: *)
(*int[(x+1)/Sqrt[-7+4 x+14 x^2-12 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-x+x^2)/Sqrt[-2 x+4 x^2-2 x^3+x^4-2 x^5+x^6],x]*)


(* ::Subsubsection::Closed:: *)
(*decreaseRationalRadicandDegreeIntegrate*)


(* ::InheritFromParent:: *)
(*IntegrateAlgebraic[Sqrt[(1 + a*x^2 + 4*b*x^2 + 4*a*b*x^4 + 6*b^2*x^4 + 6*a*b^2*x^6 + 4*b^3*x^6 + *)
(*      4*a*b^3*x^8 + b^4*x^8 + a*b^4*x^10)/(1 + c*x^2 + 4*d*x^2 + 4*c*d*x^4 + 6*d^2*x^4 + *)
(*      6*c*d^2*x^6 + 4*d^3*x^6 + 4*c*d^3*x^8 + d^4*x^8 + c*d^4*x^10)]/x, x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[Sqrt[(-1 + a*x - 2*x^2 + 2*a*x^3 - x^4 + a*x^5)/*)
(*    (1 + a*x - 2*x^2 - 2*a*x^3 + x^4 + a*x^5)], x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x - a)/((-1 - 3*x + a*x - 3*x^2 + 3*a*x^2 - x^3 + 3*a*x^3 + a*x^4)/*)
(*     (-1 + 3*x - a*x - 3*x^2 + 3*a*x^2 + x^3 - 3*a*x^3 + a*x^4))^(1/3), x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x/(-1 - a*x + 3*x^2 + 3*a*x^3 - 3*x^4 - 3*a*x^5 + x^6 + a*x^7))^(1/3), x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x/(-1 - a*x + 3*x^2 + 3*a*x^3 - 3*x^4 - 3*a*x^5 + x^6 + a*x^7))^(1/3)/x^3, x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[((1 + a*x - 4*x^2 - 4*a*x^3 + 6*x^4 + 6*a*x^5 - 4*x^6 - 4*a*x^7 + x^8 + a*x^9)/*)
(*    (-c + b*x))^(1/4), x]*)


(* ::Input:: *)
(*IntegrateAlgebraic[((1 + 4*x^2 + a*x^2 + 6*x^4 + 4*a*x^4 + 4*x^6 + 6*a*x^6 + x^8 + 4*a*x^8 + *)
(*      a*x^10)/x^2)^(1/4)/x, x]*)


(* ::Subsubsection::Closed:: *)
(*goursatIntegrate*)


(* ::Input:: *)
(*int[(-7+x)/((-11+5 x) Sqrt[-60+83 x-21 x^2-3 x^3+x^4]),x]*)


(* ::Input:: *)
(*int[(77-46 x+5 x^2)/((-23+82 x-23 x^2) Sqrt[-60+83 x-21 x^2-3 x^3+x^4]),x]*)


(* ::Subsubsection::Closed:: *)
(*linearRationalIntegrate*)


(* ::Input:: *)
(*int[(-1+x)/(x^3-x^2-x+1)^(1/3),x]*)


(* ::Input:: *)
(*int[(3-x^2)/((1-x^2) (1-6 x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^2)^2/((1-x^2) (1-6 x^2+x^4)^(3/4)),x]*)


(* ::Subsubsection::Closed:: *)
(*integrateLinearRatioRadical*)


(* ::Input:: *)
(*int[x^2/Sqrt[(a x+b)/(c x+d)],x]*)


(* ::Input:: *)
(*int[(x^2-c x^2 ((a x+b)/(c x+d))^(3/2))/(a-b Sqrt[(a x+b)/(c x+d)]),x]*)


(* ::Input:: *)
(*int[(x^2-1)/((a x+b)/(c x+d))^(1/4),x]*)


(* ::Input:: *)
(*int[(d x+b)/(x^4 ((a x+b)/(c x+d))^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x)/((1-a x) ((1-b x)/(c+x))^(1/4)),x]*)


(* ::Input:: *)
(*int[((1+d x^2) ((1-b x)/(c+x))^(1/6))/((1+c x) (1+b x)),x]*)


(* ::Subsubsection::Closed:: *)
(*integrateMultipleRadicals*)


(* ::Input:: *)
(*int[1/(Sqrt[x-1] (Sqrt[x-1]+2 Sqrt[x])^2),x]*)


(* ::Input:: *)
(*int[1/(Sqrt[x^2-1] (Sqrt[x^2-1]+Sqrt[x])^2),x]*)


(* ::Input:: *)
(*int[((x-1)^(3/2)+(x+1)^(3/2))/((x+1)^(3/2) (x-1)^(3/2)),x]*)


(* ::Input:: *)
(*int[(1+x^3-(1+x^4)^(1/4)+x^3 (1+x^4)^(1/4))/((-1+x^3) Sqrt[1+x^4]),x]*)


(* ::Subsubsection::Closed:: *)
(*integrateLinearRadical*)


(* ::Input:: *)
(*int[Sqrt[1-x]/(8 (1+x)^(7/2)),x]*)


(* ::Input:: *)
(*int[(1-x^5)/((1+x^5)Sqrt[a+b x]),x]*)


(* ::Input:: *)
(*int[(b-a x^5)/((a b+x^5)Sqrt[a+b x]),x]*)


(* ::Input:: *)
(*int[(b Sqrt[a+b x]-a x+1)/((a b Sqrt[a+b x]+x) Sqrt[a+b x]),x]*)


(* ::Subsubsection::Closed:: *)
(*integrateQuadraticRadical*)


(* ::Input:: *)
(*int[((-3+x)^6 (-1-x+x^2)^(3/2))/(-1+x),x]*)


(* ::Input:: *)
(*int[(Sqrt[2] u Sqrt[-1+u^2] (-1-2 u+u^2))/((-1+u) (1+u) (-1+2 u+u^2)^2),u]*)


(* ::Input:: *)
(*int[1/(Sqrt[x^2+1]+2 x)^2,x]*)


(* ::Input:: *)
(*int[1/(Sqrt[x^2-1] (3 x^2-4)^2),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2-1]/(x-I)^2,x]*)


(* ::Input:: *)
(*int[(x^2+1)/((x^2-1) (2 x^2+1)^(3/2)),x]*)


(* ::Input:: *)
(*int[((2 x^2+1)^(1/2)-x^2+(2 x^2+1)^(5/2))/(x^2-x (2 x^2+1)^(3/2)),x]*)


(* ::Subsubsection::Closed:: *)
(*expandIntegrate*)


(* ::Input:: *)
(*int[(a+b x)/((2-x^2) (x^2-1)^(1/4)),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[(2 E^-x^2)/(Sqrt[\[Pi]] Erf[x])-(Erf[Sqrt[3] Tan[x]]+2 E^(-3 Tan[x]^2) Sqrt[3/\[Pi]] x Sec[x]^2)/(1+x Erf[Sqrt[3] Tan[x]]),x,"Expansion"->True]*)


(* ::Subsubsection::Closed:: *)
(*integrateNestedRadicals*)


(* ::Input:: *)
(*int[1/((1+Sqrt[x]) Sqrt[x-Sqrt[x]]),x]*)


(* ::Input:: *)
(*int[1/((-1+x) Sqrt[x-Sqrt[x]]),x]*)


(* ::Input:: *)
(*int[Sqrt[x]/((-1+x) Sqrt[x-Sqrt[x]]),x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^4]/((1+x)^3 Sqrt[x^2+Sqrt[1+x^4]]),x,"SingleStepTimeConstraint"->0.5]*)


(* ::Input:: *)
(*int[Sqrt[1+x^4]/((1+x)^4 Sqrt[x^2+Sqrt[1+x^4]]),x,"SingleStepTimeConstraint"->2.]*)


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
(*int[(1-x+x^3)^2/((1+x+x^3)^2 Sqrt[x^2+Sqrt[1+x^4]]),x,"SingleStepTimeConstraint"->5]*)


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


(* ::Subsubsection::Closed:: *)
(*derivdivides*)


(* ::Input:: *)
(*int[Sqrt[u]/Sqrt[-1-u+u^2+u^3],u]*)


(* ::Input:: *)
(*int[x^2/Sqrt[1-Sqrt[1-Sqrt[1-1/x]]],x]*)


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
(*int[(1/(2 Sqrt[-1+x])-((4 x^3)/Sqrt[-1+x]-x^4/(2 (-1+x)^(3/2)))/(2^(3/4) Sqrt[x^4/Sqrt[-1+x]])) (Sqrt[-1+x]-2^(1/4) Sqrt[x^4/Sqrt[-1+x]]-1/((Sqrt[-1+x]-2^(1/4) Sqrt[x^4/Sqrt[-1+x]]) Sqrt[1+(Sqrt[-1+x]-2^(1/4) Sqrt[x^4/Sqrt[-1+x]])^2])),x]*)


(* ::Input:: *)
(*int[Sec[x Log[(Sqrt[2] x^4)/Sqrt[-1+x]]]^2 ((Sqrt[-1+x] ((4 Sqrt[2] x^3)/Sqrt[-1+x]-x^4/(Sqrt[2] (-1+x)^(3/2))))/(Sqrt[2] x^3)+Log[(Sqrt[2] x^4)/Sqrt[-1+x]]) (Tan[x Log[(Sqrt[2] x^4)/Sqrt[-1+x]]]-Cot[x Log[(Sqrt[2] x^4)/Sqrt[-1+x]]]/Sqrt[1+Tan[x Log[(Sqrt[2] x^4)/Sqrt[-1+x]]]^2]),x]*)


(* ::Input:: *)
(*int[(E^((Sqrt[2] x^4)/Sqrt[-1+x]) (Sqrt[3]-E^((4 Sqrt[2] x^4)/Sqrt[-1+x])) ((4 Sqrt[2] x^3)/Sqrt[-1+x]-x^4/(Sqrt[2] (-1+x)^(3/2))))/Sqrt[1+E^((2 Sqrt[2] x^4)/Sqrt[-1+x])],x]*)


(* ::Input:: *)
(*int[(E^(x^4/Sqrt[-1+x]) (1-E^((2 x^4)/Sqrt[-1+x])) ((4 x^3)/Sqrt[-1+x]-x^4/(2 (-1+x)^(3/2))))/Sqrt[1+E^((2 x^4)/Sqrt[-1+x])],x]*)


(* ::Input:: *)
(*int[E^(x^4/(-1+x)) (1-E^((2 x^4)/(-1+x))/Sqrt[1+E^((2 x^4)/(-1+x))]) ((4 x^3)/(-1+x)-x^4/(-1+x)^2),x]*)


(* ::Input:: *)
(*int[((-(E^(-(x^4/(-1+x)))/(2 Sqrt[1-x]))+E^(-(x^4/(-1+x))) Sqrt[1-x] (-((4 x^3)/(-1+x))+x^4/(-1+x)^2)) Sqrt[1-E^(-(x^4/(-1+x))) Sqrt[1-x]])/Sqrt[1+E^(-(x^4/(-1+x))) Sqrt[1-x]],x]*)


(* ::Input:: *)
(*int[(Sqrt[1+E^(1-x^4)-Sqrt[1-x]] (-(1/(2 Sqrt[1-x]))+4 E^(1-x^4) x^3))/Sqrt[1-E^(1-x^4)+Sqrt[1-x]],x]*)


(* ::Input:: *)
(*int[(x (b+a x^2)^(1/4) Sqrt[1+b+a x^2])/((1-b-a x^2) (b+a x^2)),x]*)


(* ::Input:: *)
(*int[((-1+8 Sqrt[1-x] x^3+x^4) Sqrt[1-Sqrt[1-x]+Log[1-x^4]])/(2 Sqrt[1-x] (-1+x^4) Sqrt[1+Sqrt[1-x]-Log[1-x^4]]),x]*)


(* ::Input:: *)
(*int[((8 x^4+(-1+x^4) Log[1-x^4]) (Sqrt[1-Sqrt[x] Log[1-x^4]]+Sqrt[1+Sqrt[x] Log[1-x^4]]))/(2 Sqrt[x] (-1+x^4)),x]*)


(* ::Input:: *)
(*int[((-1-8 x^(7/2)+x^4) Sqrt[1-Sqrt[x]+Log[1-x^4]])/(2 Sqrt[x] (-1+x^4) Sqrt[1+Sqrt[x]-Log[1-x^4]]),x]*)


(* ::Input:: *)
(*int[(Sqrt[x-Sqrt[Log[x]]]-Sqrt[1-x-Sqrt[x-Sqrt[Log[x]]]+Sqrt[Log[x]]]) (1-1/(2 x Sqrt[Log[x]])),x]*)


(* ::Input:: *)
(*int[((Sqrt[-1+x+Sqrt[x-Log[x]]]+Sqrt[1+x+Sqrt[x-Log[x]]]) (-1+x+2 x Sqrt[x-Log[x]]))/(x Sqrt[-1+x+Sqrt[x-Log[x]]] Sqrt[1+x+Sqrt[x-Log[x]]] Sqrt[x-Log[x]]),x]*)


(* ::Input:: *)
(*int[(x (-1+x^2))/(Sqrt[1+4/3 (x/(x^2+1))^2+Sqrt[1+4/3 (x/(x^2+1))^2]] (1+x^2)^3),x]*)
