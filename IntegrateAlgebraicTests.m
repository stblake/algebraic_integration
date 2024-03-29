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


(* ::Input:: *)
(*IntegrateAlgebraic[Sqrt[(-1+x^2)/(1+x^2)+1]/ Sqrt[(-1+x^2)/(1+x^2)+Sqrt[(-1+x^2)/(1+x^2)+1]],x](* FIX ME! *)*)


(* ::Input:: *)
(*IntegrateAlgebraic[((x+2 x^2) (x^2+x^4)^(1/4))/(1+2 x^2),x,"Expansion"->True]*)


(* ::Input:: *)
(*IntegrateAlgebraic[((1+2 x+2 x^2) (x^2+x^4)^(1/4))/(1+2 x^2),x,"Expansion"->True]*)


(* ::Input:: *)
(*IntegrateAlgebraic[(x-1)/(x (1+x^4)^(1/4)),x,"Expansion"->True]*)


(* ::Text:: *)
(*For this integral, we have issues when repairing branch cuts:*)


(* ::Input:: *)
(*IntegrateAlgebraic[((1+4 x+a x+6 x^2+4 a x^2+4 x^3+6 a x^3+x^4+4 a x^4+a x^5)/(-c x^4+b x^5))^(1/4),x]*)


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
(*This example takes far too long in verifying the solution: *)


(* ::Input:: *)
(*Timing[int[((-(E^(-1+x^4)/(2 Sqrt[1-x]))+4 E^(-1+x^4) Sqrt[1-x] x^3) Sqrt[1-E^(-1+x^4) Sqrt[1-x]])/Sqrt[1+E^(-1+x^4) Sqrt[1-x]],x]]*)


(* ::Text:: *)
(*Why does simplify hang here? This should be fixed!*)


(* ::Input:: *)
(*Timing[simplify[1/3 ArcTan[(1-x^2)/((1+x+x^2) (1-x^4)^(1/3))]+1/6 (-1+Sqrt[3]) ArcTan[Sqrt[3]-(2 (1-x^2))/((1+x+x^2) (1-x^4)^(1/3))]+1/6 (1+Sqrt[3]) ArcTan[Sqrt[3]+(2 (1-x^2))/((1+x+x^2) (1-x^4)^(1/3))]+1/6 Log[1+(1-x^2)^2/((1+x+x^2)^2 (1-x^4)^(2/3))]-1/12 (1+Sqrt[3]) Log[1+(1-x^2)^2/((1+x+x^2)^2 (1-x^4)^(2/3))-(Sqrt[3] (1-x^2))/((1+x+x^2) (1-x^4)^(1/3))]+1/12 (-1+Sqrt[3]) Log[1+(1-x^2)^2/((1+x+x^2)^2 (1-x^4)^(2/3))+(Sqrt[3] (1-x^2))/((1+x+x^2) (1-x^4)^(1/3))],x]]*)


(* ::Text:: *)
(*This should return the partial answer it found (if not the complete integral):*)


(* ::Input:: *)
(*int[(-1+x^3)^(1/3)/(1+x),x,"Expansion"->True]*)


(* ::Subsection::Closed:: *)
(*wish list*)


(* ::Text:: *)
(*The following integral should be an arctan with the substitution t == (u*Sqrt[3 + 10*u^4 + 3*u^8])/(1 + 3*u^4)*)


(* ::Input:: *)
(*IntegrateAlgebraic[(-1+u^2)^2/((1+u^2) Sqrt[3+10 u^4+3 u^8]),u,"SingleStepTimeConstraint"->2]*)


(* ::Input:: *)
(*Integrate`IntegrateAlgebraicDump`subst[(-1+u^2)^2/((1+u^2) Sqrt[3+10 u^4+3 u^8]),t->(u Sqrt[3+10 u^4+3 u^8])/(1+3u^4),u]*)


(* ::Text:: *)
(*Can we generate a real-valued solution to this integral? *)


(* ::Input:: *)
(*int[1/((1+u^2) Sqrt[3+3 u^2+u^4]),u]*)


(* ::Input:: *)
(*int[1/((x-1/2 (1-Sqrt[5]))^2 (x^2-x-1)^2//Expand)^(1/3),x]*)


(* ::Input:: *)
(*int[Sqrt[Sqrt[2]+Sqrt[x]+Sqrt[2+Sqrt[8] Sqrt[x]+2 x]],x]*)


(* ::Input:: *)
(*int[Sqrt[x^3+x^2 Sqrt[-1+x^2]],x]*)


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
(*int[r Sqrt[2+1/(r^2-1)],r]*)


(* ::Input:: *)
(*int[-(1/Sqrt[-a+x])+Sqrt[a-x]/Sqrt[-a+x],x]*)


(* ::Input:: *)
(*int[Sqrt[1-x^2-y^4],y]*)


(* ::Input:: *)
(*int[x Sqrt[1+Sqrt[2]+Sqrt[2] x+x^2],x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x) Sqrt[1-x^2+x^4]),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[(1+x)/((-1+x) Sqrt[1-x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(2 x-1)/((x+1) Sqrt[(x+1)^6-a^2 x^2]),x]*)


(* ::Input:: *)
(*int[Sqrt[x^4-1]/(x^4+1),x]*)


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
(*int[(b^2+a x)/((-b^2+a x) Sqrt[b+Sqrt[b^2+a x^2]]),x]*)
(*Simplify[\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(x\)]%\)-(b^2+a x)/((-b^2+a x) Sqrt[b+Sqrt[b^2+a x^2]])]*)


(* ::Input:: *)
(*int[-((Sqrt[-1+u^2] (1+u^2)^2 (-3-2 u+3 u^2))/(Sqrt[2] (-1+u) u (1+u) (-1+2 u+u^2)^3)),u]*)


(* ::Input:: *)
(*int[Sqrt[x]/(-2+x^2)^(3/4),x]*)


(* ::Input:: *)
(*int[u^(1/3)/(-1+5 u^2-4 u^4),u]*)


(* ::Input:: *)
(*int[1/Sqrt[1-Sqrt[x]]-Sqrt[1-Sqrt[x]-x],x]*)
(*Simplify[\!\( *)
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
(*Integrals from my table*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[x+Sqrt[1+x^2]] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^4),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2] (1+x^4) Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^4),x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/((1-x^4) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(Sqrt[1+x^2] (1-x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^4),x]*)


(* ::Input:: *)
(*int[((1+x^2)^2 Sqrt[x+Sqrt[1+x^2]] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^2)^2,x]*)


(* ::Input:: *)
(*int[((1+x^2)^(5/2) Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^2)^2,x]*)


(* ::Input:: *)
(*int[((1+x^2)^2 Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/((1-x^2)^2 Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[((1+x^2)^(3/2) Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^2)^2,x]*)


(* ::Input:: *)
(*int[((1+x^2) Sqrt[x+Sqrt[1+x^2]] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^2)^2,x]*)


(* ::Input:: *)
(*int[((1+x^2)^(3/2) Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^2)^2,x]*)


(* ::Input:: *)
(*int[((1+x^2) Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/((1-x^2)^2 Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^2)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[x+Sqrt[1+x^2]] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^2)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^2)^2,x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/((1-x^2)^2 Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/((1-x^2)^2 Sqrt[1+x^2]),x]*)


(* ::Input:: *)
(*int[(Sqrt[x+Sqrt[1+x^2]] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^2),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1-x^2),x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/((1-x^2) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/((1-x^2) Sqrt[1+x^2]),x]*)


(* ::Input:: *)
(*int[(Sqrt[x+Sqrt[1+x^2]] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1+x^2)^2,x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/(1+x^2)^(3/2),x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/((1+x^2)^2 Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/(1+x^2)^(5/2),x]*)


(* ::Input:: *)
(*int[(Sqrt[x+Sqrt[1+x^2]] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/(1+x^2),x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/Sqrt[1+x^2],x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/((1+x^2) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/(1+x^2)^(3/2),x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/(Sqrt[1+x^2] Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]])/Sqrt[x+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x^2]] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^2] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/Sqrt[x+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[c+Sqrt[a x+Sqrt[b+a^2 x^2]]]/Sqrt[b+a^2 x^2],x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/Sqrt[1+x^2],x]*)


(* ::Input:: *)
(*int[Sqrt[c+Sqrt[a x+Sqrt[b+a^2 x^2]]]/(b+a^2 x^2),x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]]/(1+x^2),x]*)


(* ::Input:: *)
(*int[Sqrt[c+Sqrt[a x+Sqrt[b+a^2 x^2]]]/((b+a^2 x^2)^(3/2) Sqrt[a x+Sqrt[b+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[(Sqrt[a x+Sqrt[b+a^2 x^2]] Sqrt[c+Sqrt[a x+Sqrt[b+a^2 x^2]]])/(b+a^2 x^2)^(3/2),x]*)


(* ::Input:: *)
(*int[(b+a^2 x^2)^(3/2) Sqrt[a x+Sqrt[b+a^2 x^2]] Sqrt[c+Sqrt[a x+Sqrt[b+a^2 x^2]]],x]*)


(* ::Input:: *)
(*int[(1+x^2)^(3/2) Sqrt[x+Sqrt[1+x^2]] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[b+a^2 x^2] Sqrt[a x+Sqrt[b+a^2 x^2]] Sqrt[c+Sqrt[a x+Sqrt[b+a^2 x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^2] Sqrt[x+Sqrt[1+x^2]] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[a x+Sqrt[b+a^2 x^2]] Sqrt[c+Sqrt[a x+Sqrt[b+a^2 x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x^2]] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[b+a^2 x^2] Sqrt[c+Sqrt[a x+Sqrt[b+a^2 x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^2] Sqrt[1+Sqrt[x+Sqrt[1+x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[c+Sqrt[a x+Sqrt[b+a^2 x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[x+Sqrt[1+x^2]]],x]*)


(* ::Input:: *)
(*int[(Sqrt[b+a x] Sqrt[1+Sqrt[b+a x]])/(x^2 Sqrt[1+Sqrt[1+Sqrt[b+a x]]]),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x] Sqrt[1+Sqrt[1+x]])/(x^2 Sqrt[1+Sqrt[1+Sqrt[1+x]]]),x]*)


(* ::Input:: *)
(*int[(Sqrt[b+a x] Sqrt[c+Sqrt[b+a x]])/(x Sqrt[d+Sqrt[c+Sqrt[b+a x]]]),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x] Sqrt[1+Sqrt[1+x]])/(x Sqrt[1+Sqrt[1+Sqrt[1+x]]]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/(Sqrt[1+x] (1+x^2) Sqrt[1+Sqrt[1+x]] Sqrt[1+Sqrt[1+Sqrt[1+x]]]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[1+Sqrt[1+x]] Sqrt[1+Sqrt[1+Sqrt[1+x]]]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/(Sqrt[1+x] (1+x^2) Sqrt[1+Sqrt[1+Sqrt[1+x]]]),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) Sqrt[1+Sqrt[1+Sqrt[1+x]]]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[1+Sqrt[1+Sqrt[1+x]]]),x]*)


(* ::Input:: *)
(*int[1/((f+e x) Sqrt[d+Sqrt[c+Sqrt[b+a x]]]),x]*)


(* ::Input:: *)
(*int[(f+e x)/((h+g x) Sqrt[d+Sqrt[c+Sqrt[b+a x]]]),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+x) Sqrt[1+Sqrt[1+Sqrt[1+x]]]),x]*)


(* ::Input:: *)
(*int[(-1+x)/((1+x) Sqrt[1+Sqrt[1+Sqrt[1+x]]]),x]*)


(* ::Input:: *)
(*int[(f+e x)/Sqrt[d+Sqrt[c+Sqrt[b+a x]]],x]*)


(* ::Input:: *)
(*int[1/Sqrt[d+Sqrt[c+Sqrt[b+a x]]],x]*)


(* ::Input:: *)
(*int[1/Sqrt[1+Sqrt[1+Sqrt[1+x]]],x]*)


(* ::Input:: *)
(*int[Sqrt[d+Sqrt[c+Sqrt[b+a x]]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[1+Sqrt[1+x]]],x]*)


(* ::Input:: *)
(*int[(1+x^4)^2/((-1+x^4)^2 Sqrt[x^2+Sqrt[1+x^4]]),x]*)


(* ::Input:: *)
(*int[((1+x^2+x^4)^2 Sqrt[x^2+Sqrt[1+x^4]])/(Sqrt[1+x^4] (-1+x^2+x^4)^2),x]*)


(* ::Input:: *)
(*int[((1+x^2)^2 Sqrt[x^2+Sqrt[1+x^4]])/(Sqrt[1+x^4] (-1+x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^2+x^4) Sqrt[x^2+Sqrt[1+x^4]])/(Sqrt[1+x^4] (-1+x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^2) Sqrt[x^2+Sqrt[1+x^4]])/(Sqrt[1+x^4] (-1+x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)^2/((1+x^4)^2 Sqrt[x^2+Sqrt[1+x^4]]),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^2 Sqrt[x^2+Sqrt[1+x^4]])/(1+x^4)^2,x]*)


(* ::Input:: *)
(*int[(-1+x^2)^2/((1+x^2)^2 Sqrt[x^2+Sqrt[1+x^4]]),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/((1+x^2)^2 Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[((-1+x^2)^2 Sqrt[x^2+Sqrt[1+x^4]])/((1+x^2)^2 Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[(1+x^2)^2/((-1+x^2)^2 Sqrt[x^2+Sqrt[1+x^4]]),x]*)


(* ::Input:: *)
(*int[(d+c x^2)^2/((-d+c x^2)^2 Sqrt[b+a^2 x^4] Sqrt[a x^2+Sqrt[b+a^2 x^4]]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)^2/((1+x^2)^2 Sqrt[1+x^4] Sqrt[x^2+Sqrt[1+x^4]]),x]*)


(* ::Input:: *)
(*int[(1+x^2)^2/((-1+x^2)^2 Sqrt[1+x^4] Sqrt[x^2+Sqrt[1+x^4]]),x]*)


(* ::Input:: *)
(*int[Sqrt[a x^2+Sqrt[b+a^2 x^4]]/((d+c x)^2 Sqrt[b+a^2 x^4]),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/((1+x)^2 Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[Sqrt[a x^2+Sqrt[b+a^2 x^4]]/((d+c x) Sqrt[b+a^2 x^4]),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/((1+x) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[((1+x^2) Sqrt[x^2+Sqrt[1+x^4]])/((-1+x^2) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[((b+a^2 x^2) Sqrt[a x^2+Sqrt[b+a^2 x^4]])/((-b+a^2 x^2) Sqrt[b+a^2 x^4]),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[x^2+Sqrt[1+x^4]])/((1+x^2) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[((-b+a^2 x^4) Sqrt[a x^2+Sqrt[b+a^2 x^4]])/Sqrt[b+a^2 x^4],x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[x^2+Sqrt[1+x^4]])/Sqrt[1+x^4],x]*)


(* ::Input:: *)
(*int[((-b+a^2 x^2) Sqrt[a x^2+Sqrt[b+a^2 x^4]])/Sqrt[b+a^2 x^4],x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[x^2+Sqrt[1+x^4]])/Sqrt[1+x^4],x]*)


(* ::Input:: *)
(*int[(Sqrt[b+a^2 x^4] Sqrt[a x^2+Sqrt[b+a^2 x^4]])/(-b+a^2 x^4),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^4] Sqrt[x^2+Sqrt[1+x^4]])/(-1+x^4),x]*)


(* ::Input:: *)
(*int[Sqrt[a x^2+Sqrt[b+a^2 x^4]]/(Sqrt[b+a^2 x^4] (d+c x^4)),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/((-1+x^4) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[Sqrt[a x^2+Sqrt[b+a^2 x^4]]/((d+c x^2) Sqrt[b+a^2 x^4]),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/((-1+x^2) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[Sqrt[b+a^2 x^4]/Sqrt[a x^2+Sqrt[b+a^2 x^4]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^4]/Sqrt[x^2+Sqrt[1+x^4]],x]*)


(* ::Input:: *)
(*int[x^4 Sqrt[b+a^2 x^4] Sqrt[a x^2+Sqrt[b+a^2 x^4]],x]*)


(* ::Input:: *)
(*int[x^4 Sqrt[1+x^4] Sqrt[x^2+Sqrt[1+x^4]],x]*)


(* ::Input:: *)
(*int[x^2 Sqrt[b+a^2 x^4] Sqrt[a x^2+Sqrt[b+a^2 x^4]],x]*)


(* ::Input:: *)
(*int[x^2 Sqrt[1+x^4] Sqrt[x^2+Sqrt[1+x^4]],x]*)


(* ::Input:: *)
(*int[Sqrt[b+a^2 x^4] Sqrt[a x^2+Sqrt[b+a^2 x^4]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^4] Sqrt[x^2+Sqrt[1+x^4]],x]*)


(* ::Input:: *)
(*int[Sqrt[a x^2+Sqrt[b+a^2 x^4]]/Sqrt[b+a^2 x^4],x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/Sqrt[1+x^4],x]*)


(* ::Input:: *)
(*int[(Sqrt[b+a^2 x^4] Sqrt[a x^2+Sqrt[b+a^2 x^4]])/(d+c x^2),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^4] Sqrt[x^2+Sqrt[1+x^4]])/(1+x^2),x]*)


(* ::Input:: *)
(*int[(-d+c x^2)/((d+c x^2) Sqrt[a x^2+Sqrt[b+a^2 x^4]]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[x^2+Sqrt[1+x^4]]),x]*)


(* ::Input:: *)
(*int[((-d+c x^2) Sqrt[a x^2+Sqrt[b+a^2 x^4]])/(d+c x^2),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[x^2+Sqrt[1+x^4]])/(1+x^2),x]*)


(* ::Input:: *)
(*int[Sqrt[a x^2+Sqrt[b+a^2 x^4]]/(d+c x^2),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/(1+x^2),x]*)


(* ::Input:: *)
(*int[Sqrt[a x^2+Sqrt[b+a^2 x^4]]/x^2,x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]]/x^2,x]*)


(* ::Input:: *)
(*int[x^2/Sqrt[a x^2+Sqrt[b+a^2 x^4]],x]*)


(* ::Input:: *)
(*int[x^2/Sqrt[x^2+Sqrt[1+x^4]],x]*)


(* ::Input:: *)
(*int[1/Sqrt[a x^2+Sqrt[b+a^2 x^4]],x]*)


(* ::Input:: *)
(*int[1/Sqrt[x^2+Sqrt[1+x^4]],x]*)


(* ::Input:: *)
(*int[x^2 Sqrt[a x^2+Sqrt[b+a^2 x^4]],x]*)


(* ::Input:: *)
(*int[x^2 Sqrt[x^2+Sqrt[1+x^4]],x]*)


(* ::Input:: *)
(*int[Sqrt[a x^2+Sqrt[b+a^2 x^4]],x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+Sqrt[1+x^4]],x]*)


(* ::Input:: *)
(*int[(f+e x^2+Sqrt[b^2+a^2 x^2])/(d+c x^2+Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[(x^2-Sqrt[1+x^2])/(x^2+Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[Sqrt[b^2+a^2 x^2]/(d+c x^2+Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^2]/(x^2+Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(f+e x)/(d+c x+Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[x/(x+Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[1/(d+c x+Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[1/(x+Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(d+c x^4)/((f+e x^4) Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[x+Sqrt[1+x^2]])/(1+x^4),x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[x+Sqrt[1+x^2]])/(-1+x^4),x]*)


(* ::Input:: *)
(*int[(1+x^4)/((-1+x^4) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/((1+x^4) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(d+c x)/((f+e x) Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[(d+c x^4)/(x Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[(1+x^4)/(x^4 Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[((d+c x^2) Sqrt[a x+Sqrt[b^2+a^2 x^2]])/(f+e x^2),x]*)


(* ::Input:: *)
(*int[((d+c x^2) Sqrt[a x+Sqrt[b^2+a^2 x^2]])/(-d+c x^2),x]*)


(* ::Input:: *)
(*int[((-d+c x^2) Sqrt[a x+Sqrt[b^2+a^2 x^2]])/(d+c x^2),x]*)


(* ::Input:: *)
(*int[(d+c x^2)/((-d+c x^2) Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[(-d+c x^2)/((d+c x^2) Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[1/((d+c x^2) Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[1/((1+x^2)^2 Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[1/((1+x^2) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[1/((-1+x^2)^2 Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[1/((-1+x^2) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(d+c x^2)/Sqrt[a x+Sqrt[b^2+a^2 x^2]],x]*)


(* ::Input:: *)
(*int[(d+c x)^2/Sqrt[a x+Sqrt[b^2+a^2 x^2]],x]*)


(* ::Input:: *)
(*int[(d+c x)/Sqrt[a x+Sqrt[b^2+a^2 x^2]],x]*)


(* ::Input:: *)
(*int[1/((d+c x)^2 Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[1/((d+c x) Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[(d+c x)/((-d+c x) Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[(-1+x)/((1+x) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+x) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[((-d+c x) Sqrt[a x+Sqrt[b^2+a^2 x^2]])/(d+c x),x]*)


(* ::Input:: *)
(*int[((1+x) Sqrt[x+Sqrt[1+x^2]])/(-1+x),x]*)


(* ::Input:: *)
(*int[((-1+x) Sqrt[x+Sqrt[1+x^2]])/(1+x),x]*)


(* ::Input:: *)
(*int[1/((1+x) Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[x/(1+Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[1/(c+Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[1/(1+Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(1+Sqrt[1+x^2])/(1+Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(a x+Sqrt[b^2+a^2 x^2])/(b+Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[(x+Sqrt[1+x^2])/(1+Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[1/(x^2 Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[1/(x^2 Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[a x+Sqrt[b^2+a^2 x^2]]),x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(1+Sqrt[1+x^2])/Sqrt[x+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[b^2+a^2 x^2]/Sqrt[a x+Sqrt[b^2+a^2 x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^2]/Sqrt[x+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[(-1+x^2)/(Sqrt[1+x^2] Sqrt[x+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[(b^2+a^2 x^2)/Sqrt[a x+Sqrt[b^2+a^2 x^2]],x]*)


(* ::Input:: *)
(*int[(1+x^2)/Sqrt[x+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[1/Sqrt[a x+Sqrt[b^2+a^2 x^2]],x]*)


(* ::Input:: *)
(*int[1/Sqrt[x+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[a x+Sqrt[b^2+a^2 x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[(b^2+a x^4)^2/((-b^2+a x^4)^2 Sqrt[b+Sqrt[b^2+a x^2]]),x]*)


(* ::Input:: *)
(*int[(-b^2+a x^4)^2/((b^2+a x^4)^2 Sqrt[b+Sqrt[b^2+a x^2]]),x]*)


(* ::Input:: *)
(*int[(-b^2+a x^4)/((b^2+a x^4) Sqrt[b+Sqrt[b^2+a x^2]]),x]*)


(* ::Input:: *)
(*int[(b^2+a x^4)/((-b^2+a x^4) Sqrt[b+Sqrt[b^2+a x^2]]),x]*)


(* ::Input:: *)
(*int[(1+x^4)/((-1+x^4) Sqrt[1+Sqrt[1+x^2]]),x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[1+Sqrt[1+x^2]])/(-1+x^4),x]*)


(* ::Input:: *)
(*int[(-b^2+a x^2)^2/((b^2+a x^2)^2 Sqrt[b+Sqrt[b^2+a x^2]]),x]*)


(* ::Input:: *)
(*int[(b^2+a x^2)^2/((-b^2+a x^2)^2 Sqrt[b+Sqrt[b^2+a x^2]]),x]*)


(* ::Input:: *)
(*int[((b^2+a x^2)^2 Sqrt[b+Sqrt[b^2+a x^2]])/(-b^2+a x^2)^2,x]*)


(* ::Input:: *)
(*int[(-b^2+a x^2)/((b^2+a x^2) Sqrt[b+Sqrt[b^2+a x^2]]),x]*)


(* ::Input:: *)
(*int[(b^2+a x^2)/((-b^2+a x^2) Sqrt[b+Sqrt[b^2+a x^2]]),x]*)


(* ::Input:: *)
(*int[((b^2+a x^2) Sqrt[b+Sqrt[b^2+a x^2]])/(-b^2+a x^2),x]*)


(* ::Input:: *)
(*int[((1+x^2) Sqrt[1+Sqrt[1+x^2]])/(-1+x^2),x]*)


(* ::Input:: *)
(*int[((-b^2+a x^2)^2 Sqrt[b+Sqrt[b^2+a x^2]])/(b^2+a x^2)^2,x]*)


(* ::Input:: *)
(*int[((-b^2+a x^4) Sqrt[b+Sqrt[b^2+a x^2]])/(b^2+a x^4),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+Sqrt[1+x^2]])/(1+x^4),x]*)


(* ::Input:: *)
(*int[((-b^2+a x^2) Sqrt[b+Sqrt[b^2+a x^2]])/(b^2+a x^2),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+Sqrt[1+x^2]])/(1+x^2),x]*)


(* ::Input:: *)
(*int[(x^2-Sqrt[1+x^2])/Sqrt[1+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[(x-Sqrt[1+x^2])/Sqrt[1+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[(b^2+a x^2)^(5/2)/Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*int[(b^2+a x^2)^(3/2)/Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[b^2+a x^2]/Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^2]/Sqrt[1+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/(b^2+a x^2)^4,x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/(b^2+a x^2)^3,x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/(b^2+a x^2)^2,x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/(b^2+a x^2),x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[1+x^2]]/(1+x^2),x]*)


(* ::Input:: *)
(*int[1/(x^4 Sqrt[b+Sqrt[b^2+a x^2]]),x]*)


(* ::Input:: *)
(*int[1/(x^2 Sqrt[b+Sqrt[b^2+a x^2]]),x]*)


(* ::Input:: *)
(*int[1/Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*int[1/Sqrt[1+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/(b^2+a x^2)^(5/2),x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/(b^2+a x^2)^(3/2),x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/(x^6 Sqrt[b^2+a x^2]),x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/(x^4 Sqrt[b^2+a x^2]),x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/(x^2 Sqrt[b^2+a x^2]),x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[1+x^2]]/(x^2 Sqrt[1+x^2]),x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/x^6,x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/x^4,x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/x^2,x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[1+x^2]]/x^2,x]*)


(* ::Input:: *)
(*int[Sqrt[c+d Sqrt[b+a x^2]]/x,x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[1+x^2]]/x,x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[1+x^2]]/(x+Sqrt[1+x^2]),x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]]/Sqrt[b^2+a x^2],x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[1+x^2]]/Sqrt[1+x^2],x]*)


(* ::Input:: *)
(*int[(b^2+a x^2)^2 Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*int[(b^2+a x^2) Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[b+Sqrt[b^2+a x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[1+x^2]],x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[x+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/(Sqrt[1+x] (1+x^2) Sqrt[x+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x] (-1+x^2))/((1+x^2) Sqrt[x+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x] (1+x^2))/((-1+x^2) Sqrt[x+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x]]/(1+x^2),x]*)


(* ::Input:: *)
(*int[Sqrt[1+x]/(x+Sqrt[x+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x]]/(x^2-Sqrt[1+x]),x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x]]/(x-Sqrt[1+x]),x]*)


(* ::Input:: *)
(*int[(x Sqrt[1+x])/(x+Sqrt[x+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[x/(x+Sqrt[x+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[Sqrt[a x+Sqrt[-b+a x]]/(Sqrt[-b+a x] (1+a^2 x^2)),x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x]]/(Sqrt[1+x] (1+x^2)),x]*)


(* ::Input:: *)
(*int[Sqrt[a x+Sqrt[-b+a x]]/x^2,x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x]]/x^2,x]*)


(* ::Input:: *)
(*int[Sqrt[-b+a x]/(1+Sqrt[a x+Sqrt[-b+a x]]),x]*)


(* ::Input:: *)
(*int[Sqrt[1+x]/(1+Sqrt[x+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[Sqrt[a x+Sqrt[-b+a x]]/(x^2 Sqrt[-b+a x]),x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x]]/(x^2 Sqrt[1+x]),x]*)


(* ::Input:: *)
(*int[Sqrt[a x+Sqrt[-b+a x]]/(x Sqrt[-b+a x]),x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x]]/(x Sqrt[1+x]),x]*)


(* ::Input:: *)
(*int[(x^2 Sqrt[a x+Sqrt[-b+a x]])/Sqrt[-b+a x],x]*)


(* ::Input:: *)
(*int[(x^2 Sqrt[x+Sqrt[1+x]])/Sqrt[1+x],x]*)


(* ::Input:: *)
(*int[(x Sqrt[a x+Sqrt[-b+a x]])/Sqrt[-b+a x],x]*)


(* ::Input:: *)
(*int[(x Sqrt[x+Sqrt[1+x]])/Sqrt[1+x],x]*)


(* ::Input:: *)
(*int[((-1+a x) Sqrt[a x+Sqrt[-b+a x]])/Sqrt[-b+a x],x]*)


(* ::Input:: *)
(*int[((-1+x) Sqrt[x+Sqrt[1+x]])/Sqrt[1+x],x]*)


(* ::Input:: *)
(*int[Sqrt[a x+Sqrt[-b+a x]]/(1+Sqrt[-b+a x]),x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x]]/(1+Sqrt[1+x]),x]*)


(* ::Input:: *)
(*int[Sqrt[a x+Sqrt[-b+a x]]/(1-Sqrt[-b+a x]),x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x]]/(1-Sqrt[1+x]),x]*)


(* ::Input:: *)
(*int[Sqrt[a x+Sqrt[-b+a x]]/Sqrt[-b+a x],x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x]]/Sqrt[1+x],x]*)


(* ::Input:: *)
(*int[Sqrt[1+x] Sqrt[x+Sqrt[1+x]],x]*)


(* ::Input:: *)
(*int[Sqrt[x+Sqrt[1+x]],x]*)


(* ::Input:: *)
(*int[Sqrt[b+a x]/Sqrt[a b x+Sqrt[b+a x]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+x]/Sqrt[x+Sqrt[1+x]],x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+Sqrt[1+x]])/(Sqrt[1+x] (1+x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x] (-1+x^4) Sqrt[1+Sqrt[1+x]])/(1+x^4),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+Sqrt[1+x]])/(1+x^4),x]*)


(* ::Input:: *)
(*int[(Sqrt[b+a x] (-g+f x^2))/((e+d x^2) Sqrt[c+Sqrt[b+a x]]),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x] (-1+x^2))/((1+x^2) Sqrt[1+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[((-g+f x^2) Sqrt[c+Sqrt[b+a x]])/(e+d x^2),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1+Sqrt[1+x]])/(1+x^2),x]*)


(* ::Input:: *)
(*int[(x^2 Sqrt[b+a x])/(x^2-Sqrt[b+a x] Sqrt[c+Sqrt[b+a x]]),x]*)


(* ::Input:: *)
(*int[(x^2 Sqrt[1+x])/(x^2-Sqrt[1+x] Sqrt[1+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[x^2/(x^2-Sqrt[b+a x] Sqrt[c+Sqrt[b+a x]]),x]*)


(* ::Input:: *)
(*int[x^2/(x^2-Sqrt[1+x] Sqrt[1+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[x/(x^2-Sqrt[b+a x] Sqrt[c+Sqrt[b+a x]]),x]*)


(* ::Input:: *)
(*int[x/(x^2-Sqrt[1+x] Sqrt[1+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[1/(x^2-Sqrt[b+a x] Sqrt[c+Sqrt[b+a x]]),x]*)


(* ::Input:: *)
(*int[1/(x^2-Sqrt[1+x] Sqrt[1+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[x/(x-Sqrt[b+a x] Sqrt[c+Sqrt[b+a x]]),x]*)


(* ::Input:: *)
(*int[1/(x-Sqrt[b+a x] Sqrt[c+Sqrt[b+a x]]),x]*)


(* ::Input:: *)
(*int[(1+x^2)/(Sqrt[b+a x] Sqrt[c+Sqrt[b+a x]]),x]*)


(* ::Input:: *)
(*int[(x Sqrt[b+a x])/(x+Sqrt[c+Sqrt[b+a x]]),x]*)


(* ::Input:: *)
(*int[(x Sqrt[1+x])/(x+Sqrt[1+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[(Sqrt[b+a x] Sqrt[c+Sqrt[b+a x]])/(x-Sqrt[b+a x]),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x] Sqrt[1+Sqrt[1+x]])/(x-Sqrt[1+x]),x]*)


(* ::Input:: *)
(*int[(Sqrt[b+a x] Sqrt[c+Sqrt[b+a x]])/(1-Sqrt[b+a x]),x]*)


(* ::Input:: *)
(*int[Sqrt[c+Sqrt[b+a x]]/(x-Sqrt[b+a x]),x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[1+x]]/(x-Sqrt[1+x]),x]*)


(* ::Input:: *)
(*int[x/(x+Sqrt[c+Sqrt[b+a x]]),x]*)


(* ::Input:: *)
(*int[x/(x+Sqrt[1+Sqrt[1+x]]),x]*)


(* ::Input:: *)
(*int[Sqrt[c+Sqrt[b+a x]]/(d-Sqrt[b+a x]),x]*)


(* ::Input:: *)
(*int[(e+d x^2)/Sqrt[c+Sqrt[b+a x]],x]*)


(* ::Input:: *)
(*int[(1+x^2)/Sqrt[1+Sqrt[1+x]],x]*)


(* ::Input:: *)
(*int[Sqrt[d+c Sqrt[b+a x]],x]*)


(* ::Input:: *)
(*int[Sqrt[1+Sqrt[1+x]],x]*)


(* ::Input:: *)
(*int[(Sqrt[(x^2 (C[0]+C[1])+C[1] C[4]+x C[0] C[5])/(x^2 (C[2]+C[3])+C[3] C[4]+x C[2] C[5])] (-2 x C[4]+x^2 C[5]-C[4] C[5]))/(x^2+C[4])^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[(x^2 (C[0]+C[1])+C[1] C[4]+x C[0] C[5]+C[0] C[6])/(x^2 (C[2]+C[3])+C[3] C[4]+x C[2] C[5]+C[2] C[6])] (x^2 C[5]-C[4] C[5]+x (-2 C[4]+2 C[6])))/(x^2+C[4])^2,x]*)


(* ::Input:: *)
(*int[((-4 b+a x^4) (-b+a x^4)^(1/4))/(x^6 (-8 b+a x^8)),x]*)


(* ::Input:: *)
(*int[((b+a x^4)^(1/4) (2 b+3 a x^4))/(x^6 (b+a x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(1/4) (1+x^4+x^8))/(x^6 (-1+2 x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(1/4) (2+x^4))/(x^2 (2+2 x^4+x^8)),x]*)


(* ::Input:: *)
(*int[((1+x^4)^(1/4) (2+x^4))/(x^2 (-1+2 x^4+x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(1/4) (-1+x^4+2 x^8))/(x^6 (1-x^4+x^8)),x]*)


(* ::Input:: *)
(*int[((-b+a x^4)^(1/4) (b+c x^4+a x^8))/(x^6 (b+2 a x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(1/4) (1-x^4+x^8))/(x^6 (1+2 x^8)),x]*)


(* ::Input:: *)
(*int[((2 b+a x^4)^(1/4) (-4 b+a x^8))/(x^6 (-4 b+c x^4+a x^8)),x]*)


(* ::Input:: *)
(*int[((2+x^4)^(1/4) (-4+x^8))/(x^6 (-4-2 x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(x^2 (-2+x^8) (2-2 x^4+x^8)^(1/4))/((2+x^8) (4-x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[((-1+2 x^4)^(1/4) (-1+x^4+x^8))/(x^6 (-1+x^4)),x]*)


(* ::Input:: *)
(*int[((1+2 x^8) (-1-2 x^4+2 x^8)^(1/4) (1-3 x^8+4 x^16))/(x^10 (-1+2 x^8)),x]*)


(* ::Input:: *)
(*int[((1+2 x^4)^(1/4) (-1-x^4+2 x^8))/(x^6 (2+x^4)),x]*)


(* ::Input:: *)
(*int[((-8+x^5) (2+x^5) (2-3 x^4+x^5)^(1/4))/(x^6 (4-3 x^4+2 x^5)),x]*)


(* ::Input:: *)
(*int[((4+x^2+x^5) (-2-x^2-2 x^4+2 x^5)^(1/4))/(x^2 (-2-x^2+2 x^5)),x]*)


(* ::Input:: *)
(*int[((4+x^5) (-2+x^4+2 x^5)^(1/4) (2-4 x^5+x^8+2 x^10))/(x^10 (-1+x^5)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) (4+x^6) (-2+2 x^4+x^6)^(1/4))/(x^6 (-4-x^4+2 x^6)),x]*)


(* ::Input:: *)
(*int[((4+3 x) (-1-x+x^4) (-1-x+2 x^4)^(1/4))/(x^6 (1+x+x^4)),x]*)


(* ::Input:: *)
(*int[((-2+x^2) (-1+x^2) (-1+x^2+x^4)^(1/4))/(x^6 (-1+x^2+2 x^4)),x]*)


(* ::Input:: *)
(*int[((-4+x^5) (1-2 x^4+x^5)^(1/4))/(x^2 (1+x^5)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(1/4) (2-x^4+2 x^8))/(x^10 (-1+2 x^4)),x]*)


(* ::Input:: *)
(*int[(-4 b+a x^3)/((-b+a x^3) (b-a x^3+c x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^4)/((b+a x^4) (b^2+c x^4+a^2 x^8)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^8)/((b+a x^8) (b-c x^4+a x^8)^(1/4)),x]*)


(* ::Input:: *)
(*int[(4 b+a x^5)/((-b+a x^5) (-b+c x^4+a x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (-1+2 x^2+2 x^4)^(1/4))/(x^2 (-1+2 x^2)),x]*)


(* ::Input:: *)
(*int[((2+x^2) (-1-x^2+x^4)^(1/4) (1+x^2+x^4))/(x^6 (1+x^2)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) (1+x^6) (1-x^4+x^6)^(1/4))/(x^6 (1-2 x^4+x^6)),x]*)


(* ::Input:: *)
(*int[((4+x^6) (-2-x^4+x^6)^(1/4))/(x^2 (-2+x^6)),x]*)


(* ::Input:: *)
(*int[((-4+x^2) (2-x^2-2 x^4)^(1/4))/(x^2 (-2+x^2)),x]*)


(* ::Input:: *)
(*int[((8+3 x) (-2-x+2 x^4)^(1/4))/(x^2 (2+x+x^4)),x]*)


(* ::Input:: *)
(*int[((-1+2 x^4)^(1/4) (-2+x^8))/(x^6 (-1+x^4)^2),x]*)


(* ::Input:: *)
(*int[(x^4 (4 b+a x^5))/((-b+a x^5)^2 (-b+c x^4+a x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-b+a x^4)^(1/4) (-8 b+a x^8))/(x^10 (b+a x^4)),x]*)


(* ::Input:: *)
(*int[(x^4 (-2 b+a x^2))/((-b+a x^2)^2 (-b+a x^2+c x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b x^7+a x^8)^(1/4),x]*)


(* ::Input:: *)
(*int[(b x^5+a x^8)^(1/4),x]*)


(* ::Input:: *)
(*int[(-b+a x^4)/(x^4 (-b+2 a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^8)/((b+a x^8)^(1/4) (b-c x^4+a x^8)),x]*)


(* ::Input:: *)
(*int[(x^2 (b+3 a x^8))/((b+a x^8)^(1/4) (-1+b x^4+a x^12)),x]*)


(* ::Input:: *)
(*int[(b+a x^4)/((-b+a x^4) (b^2+c x^4+a^2 x^8)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^8)/((-b+a x^8) (-b+c x^4+a x^8)^(1/4)),x]*)


(* ::Input:: *)
(*int[(4 b+a x^3)/((b+a x^3) (-b-a x^3+c x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(4 b+x^3)/((b+x^3) (-b-x^3+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2 b+c x^2)/((-b+c x^2) (-b+c x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^8)/((b-a x^8)^(1/4) (-b+c x^4+a x^8)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/(x^8 (-1+2 x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2+x^6)/(x^4 (1+x^4+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^8)/(x^4 (1+x^4+x^8)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^8)/(x^4 (-1+x^4+x^8)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^8)/((1-x^8)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[(x^5 (-7 b+10 a x^3))/((-b x^3+a x^6)^(1/4) (-1-b x^7+a x^10)),x]*)


(* ::Input:: *)
(*int[(b+2 a x^3)/((-b+x+a x^3) (-b x^3+a x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^3)/((b-x+a x^3) (b x^3+a x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^5 (7 b+10 a x^3))/((b x^3+a x^6)^(1/4) (1+b x^7+a x^10)),x]*)


(* ::Input:: *)
(*int[((x^2+x^6)^(1/4) (1+x^8))/(x^4 (-1+x^4)),x]*)


(* ::Input:: *)
(*int[(1-x^4+x^8)/((x^2+x^6)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[x^4/((x^2+x^6)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[(1+x^8)/((x^2+x^6)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[(1+x^2+x^4)/((-1+x^4) (x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1-x^2+x^4)/((-1+x^4) (x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^4)/((-1+x^4) (x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^2/((-1+x^4) (x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[x/((-1+x^4) (x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (x^2+x^6)^(1/4))/(1+x^4+x^8),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (x^2+x^6)^(1/4))/(x^2 (1+x^2)),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+x) (x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (x^2+x^6)^(1/4))/(1-x^4+x^8),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (x^2+x^6)^(1/4))/(1+x^8),x]*)


(* ::Input:: *)
(*int[(-1+x^8)/((x^2+x^6)^(1/4) (1+x^8)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/((1+x^2+x^4) (x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) (x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (x^2+x^6)^(1/4))/(x^2 (-1+x^2)),x]*)


(* ::Input:: *)
(*int[(-b+a x^4)/((b-2 x^2+a x^4) (b x^2+a x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (x^2+x^6)^(1/4))/x^4,x]*)


(* ::Input:: *)
(*int[(-1+x^4)/((1+x^4) (x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/(x^2 (x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-x^2+x^6)^(1/4) (1-x^4+x^8))/(x^4 (1+x^4)),x]*)


(* ::Input:: *)
(*int[((-x^2+x^6)^(1/4) (1+x^4+x^8))/(x^4 (1+x^4)),x]*)


(* ::Input:: *)
(*int[x^2/((1+x^4) (-x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1-x^4+x^8)/((-x^2+x^6)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[(1+x^4+x^8)/((-x^2+x^6)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[(1+x^8)/((-x^2+x^6)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/((1+x^4) (-x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^8)/((-x^2+x^6)^(1/4) (1+x^8)),x]*)


(* ::Input:: *)
(*int[(b+a x^4)/((-b+x^2+a x^4) (-b x^2+a x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^8)/(x^4 (-x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[((1+x^4) (-x^2+x^6)^(1/4))/x^4,x]*)


(* ::Input:: *)
(*int[(1+x^4)/((-1+x^4) (-x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^4)/(x^2 (-x^2+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[((1-x^3+x^5) (-3+2 x^5))/(x^3 (1+x^3+x^5) (x+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-3+2 x^5) (1+2 x^5+x^6+x^10))/(x^6 (1-x^3+x^5) (x+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-3 b+2 a x^5)/((2 b+x^3+2 a x^5) (b x+a x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^3 (b+2 a x^5))/((b x+a x^6)^(1/4) (-1+b x^5+a x^10)),x]*)


(* ::Input:: *)
(*int[((1-x^3+x^5) (-3+2 x^5))/(x^6 (x+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-3+2 x^5)/(x^3 (x+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^5) (-1+x^3+x^5) (3+2 x^5))/(x^6 (-1-x^3+x^5) (-x+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(3 b+2 a x^5)/((-b+x^3+a x^5) (-b x+a x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b x^3+2 a x^8)/((-b x+a x^6)^(1/4) (-1-b x^5+a x^10)),x]*)


(* ::Input:: *)
(*int[((-1+x^5) (3+2 x^5))/(x^6 (-x+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) (1-x^4+x^6))/((1+x^6)^(1/4) (1+2 x^6+x^8+x^12)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) (1-x^4+x^6))/(x^4 (1+x^6)^(1/4) (1+x^4+x^6)),x]*)


(* ::Input:: *)
(*int[(2+x^3+x^6)/(x (1+x^6)^(1/4) (-4+5 x^3-4 x^6+x^9)),x]*)


(* ::Input:: *)
(*int[(-2 b c+a c x^6)/((b+a x^6)^(1/4) (b-c^4 x^4+a x^6)),x]*)


(* ::Input:: *)
(*int[(1+x^6)^(1/4)/x^7,x]*)


(* ::Input:: *)
(*int[(1+x^6)^(1/4)/x,x]*)


(* ::Input:: *)
(*int[((-2+x^6) (1-x^4+x^6))/(x^8 (1+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2+x^6)/(x^4 (1+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^5 (1+x^6)^(1/4),x]*)


(* ::Input:: *)
(*int[((2+x^6) (-1+x^4+x^6))/((-1+x^6)^(1/4) (1-2 x^6+x^8+x^12)),x]*)


(* ::Input:: *)
(*int[((2+x^6) (1-2 x^6+x^8+x^12))/(x^8 (-1+x^6)^(1/4) (-1+x^4+x^6)),x]*)


(* ::Input:: *)
(*int[((2 b+a x^6) (-b-x^4+a x^6))/(x^4 (-b+a x^6)^(1/4) (-b-2 x^4+a x^6)),x]*)


(* ::Input:: *)
(*int[(x^4 (2 b+a x^6))/((-b+a x^6)^(1/4) (-b-x^4+a x^6)^2),x]*)


(* ::Input:: *)
(*int[(2 b+a x^6)/((-b+a x^6)^(1/4) (-b-2 x^4+a x^6)),x]*)


(* ::Input:: *)
(*int[1/(x (-1+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^6)^(1/4)/x^7,x]*)


(* ::Input:: *)
(*int[(-1+x^6)^(1/4)/x,x]*)


(* ::Input:: *)
(*int[(2+x^6)/(x^4 (-1+x^6)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^5 (-1+x^6)^(1/4),x]*)


(* ::Input:: *)
(*int[((x^3+x^5)^(1/4) (1+x^4+x^8))/(x^4 (-1+x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (x^3+x^5)^(1/4))/(x^2 (-1+x^2)),x]*)


(* ::Input:: *)
(*int[((1+x) (x^3+x^5)^(1/4))/(x (-1+x^3)),x]*)


(* ::Input:: *)
(*int[(1+x^3+x^6)/((x^3+x^5)^(1/4) (1-x^6)),x]*)


(* ::Input:: *)
(*int[(1+x^6)/((x^3+x^5)^(1/4) (1-x^6)),x]*)


(* ::Input:: *)
(*int[(1+x^2+x^4)/((1-x^4) (x^3+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1-x^4)/((1+x^4) (x^3+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^4)/((1-x^4) (x^3+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((b-a x^2) (b x^3+a x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/((b+x+a x^2) (b x^3+a x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^5 (7 b+9 a x^2))/((b x^3+a x^5)^(1/4) (-2+b x^7+a x^9)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (x^3+x^5)^(1/4))/x^4,x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) (x^3+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (x^3+x^5)^(1/4))/(x^2 (1+x^2)),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/(x (x^3+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^6)/((-x^3+x^5)^(1/4) (1+x^3-x^6)),x]*)


(* ::Input:: *)
(*int[(1+x^6)/((-x^3+x^5)^(1/4) (1-x^6)),x]*)


(* ::Input:: *)
(*int[(1-x^4)/((1+x^2+x^4) (-x^3+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1-x^4)/((1+x^4) (-x^3+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b-a x^2)/((b+a x^2) (-b x^3+a x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((-b+x+a x^2) (-b x^3+a x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^5 (-7 b+9 a x^2))/((-b x^3+a x^5)^(1/4) (1-b x^7+a x^9)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (-x^3+x^5)^(1/4))/x^4,x]*)


(* ::Input:: *)
(*int[(-1+x^4)/(x^2 (-x^3+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) (-x^3+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^2 (10 b+7 a x^3))/((b x^2+a x^5)^(1/4) (b+a x^3+x^10)),x]*)


(* ::Input:: *)
(*int[(-2 b+a x^3)/((b+x^2+a x^3) (b x^2+a x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-10 b x^2+7 a x^5)/((-b x^2+a x^5)^(1/4) (-b+a x^3+x^10)),x]*)


(* ::Input:: *)
(*int[(2 b+a x^3)/((-b+x^2+a x^3) (-b x^2+a x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4) (1+x^3+x^4))/(x^6 (1-x^3+x^4) (x+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+2 x^4+x^6+x^8))/(x^6 (1-x^3+x^4) (x+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1-x^3+2 x^4-x^6-x^7+x^8))/(x^6 (1-x^3+x^4) (x+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-3 b+a x^4)/((b-x^3+a x^4) (b x+a x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^3 (5 b+9 a x^4))/((b x+a x^5)^(1/4) (1+b x^5+a x^9)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1-2 x^3+x^4) (1-x^3+x^4))/(x^6 (1+x^4) (x+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1-x^3+x^4))/(x^6 (x+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4))/(x^6 (x+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-3+x^4)/((1+x^4) (x+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (3+x^4) (-1-x^3+x^4))/(x^6 (-1-2 x^3+x^4) (-x+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(3 b+a x^4)/((-b+x^3+a x^4) (-b x+a x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^3 (-5 b+9 a x^4))/((-b x+a x^5)^(1/4) (-2-b x^5+a x^9)),x]*)


(* ::Input:: *)
(*int[(3 b+a x^4)/((-b-x^3+a x^4) (-b x+a x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[((3+x^4) (-1-x^3+x^4))/(x^6 (-x+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[((3+x^4) (-1+x^3+x^4))/(x^6 (-x+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (3+x^4))/(x^6 (-x+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(3+x^4)/(x^3 (-x+x^5)^(1/4)),x]*)


(* ::Input:: *)
(*int[(3-x^2)/((1-x^2) (1-6 x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-b+a x^2) (b x^3+a x^4)^(1/4))/(b+a x^2+x^4),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (x^3+x^4)^(1/4))/(1+x^2+x^4),x]*)


(* ::Input:: *)
(*int[(x^2 (b x^3+a x^4)^(1/4))/(-b+a x^4),x]*)


(* ::Input:: *)
(*int[(x^2 (x^3+x^4)^(1/4))/(-1+x^4),x]*)


(* ::Input:: *)
(*int[((b+a x) (b x^3+a x^4)^(1/4))/(x (-b+a x^3)),x]*)


(* ::Input:: *)
(*int[((1+x) (x^3+x^4)^(1/4))/(x (-1+x^3)),x]*)


(* ::Input:: *)
(*int[((-b+a x^3) (b x^3+a x^4)^(1/4))/(b+a x^3),x]*)


(* ::Input:: *)
(*int[((-1+x^3) (x^3+x^4)^(1/4))/(1+x^3),x]*)


(* ::Input:: *)
(*int[(b x^3+a x^4)^(1/4)/(-2 b+a x+2 x^2),x]*)


(* ::Input:: *)
(*int[(x^3+x^4)^(1/4)/(-2+x+2 x^2),x]*)


(* ::Input:: *)
(*int[(b x^3+a x^4)^(1/4)/(x (b+a x+x^2)),x]*)


(* ::Input:: *)
(*int[((-b-a x+x^4) (b x^3+a x^4)^(1/4))/(-b+a x),x]*)


(* ::Input:: *)
(*int[((b x+a x^2) (b x^3+a x^4)^(1/4))/(-b+a x+x^2),x]*)


(* ::Input:: *)
(*int[((x+x^2) (x^3+x^4)^(1/4))/(-1+x+x^2),x]*)


(* ::Input:: *)
(*int[((b+2 a x) (b x^3+a x^4)^(1/4))/(-b+a x+x^2),x]*)


(* ::Input:: *)
(*int[((1+2 x) (x^3+x^4)^(1/4))/(-1+x+x^2),x]*)


(* ::Input:: *)
(*int[(x^2 (b x^3+a x^4)^(1/4))/(-b+a x),x]*)


(* ::Input:: *)
(*int[((b x^3+a x^4)^(1/4) (-c+d x^8))/x^4,x]*)


(* ::Input:: *)
(*int[((b x^3+a x^4)^(1/4) (-d+c x^8))/x^8,x]*)


(* ::Input:: *)
(*int[((d+c x^2) (b x^3+a x^4)^(1/4))/(x^2 (-d+c x^2)),x]*)


(* ::Input:: *)
(*int[((b+a x^2) (b x^3+a x^4)^(1/4))/(x^2 (-b+a x^2)),x]*)


(* ::Input:: *)
(*int[(x^4 (b x^3+a x^4)^(1/4))/(b+a x),x]*)


(* ::Input:: *)
(*int[x^2 (b x^3+a x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x (b x^3+a x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[(b x^3+a x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[((-d+2 c x) (b x^3+a x^4)^(1/4))/x,x]*)


(* ::Input:: *)
(*int[((-1+2 x) (x^3+x^4)^(1/4))/x,x]*)


(* ::Input:: *)
(*int[((-f+e x) (b x^3+a x^4)^(1/4))/(x (d+c x)),x]*)


(* ::Input:: *)
(*int[((-b+a x) (b x^3+a x^4)^(1/4))/(x (b+a x)),x]*)


(* ::Input:: *)
(*int[((-1+x) (x^3+x^4)^(1/4))/(x (1+x)),x]*)


(* ::Input:: *)
(*int[(b x^3+a x^4)^(1/4)/(x^2 (-d+c x^2)),x]*)


(* ::Input:: *)
(*int[(x^3+x^4)^(1/4)/(x^2 (-1+x^2)),x]*)


(* ::Input:: *)
(*int[(b x^3+a x^4)^(1/4)/x^2,x]*)


(* ::Input:: *)
(*int[(b x^3+a x^4)^(1/4)/x,x]*)


(* ::Input:: *)
(*int[1/(x^8 (x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (x^3+x^4)^(1/4))/x^8,x]*)


(* ::Input:: *)
(*int[1/(x^4 (x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((1+x) (x^3+x^4)^(1/4))/x^4,x]*)


(* ::Input:: *)
(*int[1/(x^2 (x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x (1+x) (x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((1+x) (x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x (x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((b+a x^4) (-b x^3+a x^4)^(1/4))/(x^4 (-b+a x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^4) (-x^3+x^4)^(1/4))/(x^4 (-1+x^4)),x]*)


(* ::Input:: *)
(*int[(-b x^3+a x^4)^(1/4)/(x (b+a x^3)),x]*)


(* ::Input:: *)
(*int[(-x^3+x^4)^(1/4)/(x (1+x^3)),x]*)


(* ::Input:: *)
(*int[((-b+a x) (-b x^3+a x^4)^(1/4))/(x (b+a x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x) (-x^3+x^4)^(1/4))/(x (1+x^3)),x]*)


(* ::Input:: *)
(*int[((2-x+2 x^2) (-x^3+x^4)^(1/4))/(-2-2 x+x^2),x]*)


(* ::Input:: *)
(*int[(-b x^3+a x^4)^(1/4)/(-d-2 c x+x^2),x]*)


(* ::Input:: *)
(*int[(-x^3+x^4)^(1/4)/(-1-2 x+x^2),x]*)


(* ::Input:: *)
(*int[((1+x) (-x^3+x^4)^(1/4))/(1-x+x^2),x]*)


(* ::Input:: *)
(*int[(-b x^3+a x^4)^(1/4)/(x (d+c x+x^2)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (-x^3+x^4)^(1/4))/(-1+x+2 x^2),x]*)


(* ::Input:: *)
(*int[((-1+x+2 x^2) (-x^3+x^4)^(1/4))/(-1-x+x^2),x]*)


(* ::Input:: *)
(*int[((-x+x^2) (-x^3+x^4)^(1/4))/(-1-x+x^2),x]*)


(* ::Input:: *)
(*int[((-1+x+x^4) (-x^3+x^4)^(1/4))/(1+x),x]*)


(* ::Input:: *)
(*int[(x^2 (-x^3+x^4)^(1/4))/(2+x),x]*)


(* ::Input:: *)
(*int[((-b x^3+a x^4)^(1/4) (-d+c x^8))/x^4,x]*)


(* ::Input:: *)
(*int[((-x^3+x^4)^(1/4) (-1+x^8))/x^4,x]*)


(* ::Input:: *)
(*int[(-b x^3+a x^4)^(1/4)/(x (-d+c x)),x]*)


(* ::Input:: *)
(*int[(-x^3+x^4)^(1/4)/(x (1+x)),x]*)


(* ::Input:: *)
(*int[((-b x^3+a x^4)^(1/4) (-d+c x^4))/x^4,x]*)


(* ::Input:: *)
(*int[((-b x^3+a x^4)^(1/4) (-d+c x^4))/x^2,x]*)


(* ::Input:: *)
(*int[((d+c x) (-b x^3+a x^4)^(1/4))/x^2,x]*)


(* ::Input:: *)
(*int[(-b x^3+a x^4)^(1/4)/(x^2 (-d+c x^2)),x]*)


(* ::Input:: *)
(*int[(-x^3+x^4)^(1/4)/(x^2 (-1+x^2)),x]*)


(* ::Input:: *)
(*int[(-b x^3+a x^4)^(1/4)/(d+c x^2),x]*)


(* ::Input:: *)
(*int[(-x^3+x^4)^(1/4)/x^2,x]*)


(* ::Input:: *)
(*int[(-x^3+x^4)^(1/4)/x,x]*)


(* ::Input:: *)
(*int[(-x^3+x^4)^(1/4)/(x (-b+a x)),x]*)


(* ::Input:: *)
(*int[(-x^3+x^4)^(1/4)/((-1+x) x),x]*)


(* ::Input:: *)
(*int[x^2 (-x^3+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[(-x^3+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (-x^3+x^4)^(1/4))/x^8,x]*)


(* ::Input:: *)
(*int[1/(x^4 (-x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+x) (-x^3+x^4)^(1/4))/x^4,x]*)


(* ::Input:: *)
(*int[1/(x^2 (-x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(a+b x)/(x (-d+c x) (-x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+x) x (-x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-b+a x) (-x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-1+x) (-x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-x^3+x^4)^(1/4)/x^3,x]*)


(* ::Input:: *)
(*int[1/(x (-x^3+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^4+2 x^8)/((b x^2+a x^4)^(1/4) (b+a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(2 b-a x^4+2 x^8)/((b x^2+a x^4)^(1/4) (b+a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(b+2 a x^8)/((b x^2+a x^4)^(1/4) (b+a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^4+2 x^8)/((b x^2+a x^4)^(1/4) (-2 b-a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(b+a x^4)/((b x^2+a x^4)^(1/4) (b+a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[x^4/((b x^2+a x^4)^(1/4) (b+a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[x^4/((x^2+x^4)^(1/4) (1+x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(2 b+a x^4)/((b x^2+a x^4)^(1/4) (-b-a x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[(2+x^4)/((x^2+x^4)^(1/4) (-1-x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[(x^4 (b x^2+a x^4)^(1/4))/(-b+a x^8),x]*)


(* ::Input:: *)
(*int[(x^4 (x^2+x^4)^(1/4))/(-1+x^8),x]*)


(* ::Input:: *)
(*int[1/((b x^2+a x^4)^(1/4) (-2 b-a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[((-b-a x^2+x^4) (b x^2+a x^4)^(1/4))/(b+a x^4),x]*)


(* ::Input:: *)
(*int[((b x^2+a x^4)^(1/4) (-b+2 a x^4))/(-2 b+a x^4),x]*)


(* ::Input:: *)
(*int[(-b x^4+2 a x^8)/((b x^2+a x^4)^(1/4) (b-2 a x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[(x^4 (b x^2+a x^4)^(1/4))/(b+a x^2+x^4),x]*)


(* ::Input:: *)
(*int[x^4/((b+a x^4)^2 (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^4/((-b+a x^4)^2 (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^4/((1+x^4)^2 (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^4/((-1+x^4)^2 (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b x^2+a x^4)^(1/4)/(x^4 (b+a x^4)),x]*)


(* ::Input:: *)
(*int[(b x^2+a x^4)^(1/4)/(-b+2 a x^8),x]*)


(* ::Input:: *)
(*int[(b x^2+a x^4)^(1/4)/(b+2 a x^8),x]*)


(* ::Input:: *)
(*int[(b+a x^4)/(x^4 (2 b+a x^4) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^4)/(x^4 (-2 b+a x^4) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((b-a x^2+x^4) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/((b-a x^2+x^4) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((1-x^2+x^4) (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((b+a x^4) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/((b+a x^4) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((b x^2+a x^4)^(1/4) (-2 b+a x^8)),x]*)


(* ::Input:: *)
(*int[1/((-2 b+a x^4) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-2+x^4) (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/((b+a x^2+x^4) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((b+a x^2+x^4) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((1+2 x+2 x^2) (x^2+x^4)^(1/4))/(1+2 x^2),x]*)


(* ::Input:: *)
(*int[((b x^2+a x^4)^(1/4) (b+a x^4+x^8))/(b+a x^4),x]*)


(* ::Input:: *)
(*int[((b x^2+a x^4)^(1/4) (-b-a x^4+x^8))/(-b+a x^4),x]*)


(* ::Input:: *)
(*int[((x^2+x^4)^(1/4) (-1-x^4+x^8))/(-1+x^4),x]*)


(* ::Input:: *)
(*int[(b+2 a x^4)/((-b+a x^4) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^4)/((b+a x^4) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+2 x^4)/((-1+x^4) (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^2)/((b+a x^2) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^2)/((-b+a x^2) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^4/((b+a x^4)^2 (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^4/((1+x^4)^2 (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^4/((-b+a x^4)^2 (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^4/((-1+x^4)^2 (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((b+a x^4) (b x^2+a x^4)^(1/4))/x^2,x]*)


(* ::Input:: *)
(*int[((-b+a x^4) (b x^2+a x^4)^(1/4))/x^2,x]*)


(* ::Input:: *)
(*int[(b+a x^2)/(x^2 (-b+a x^2) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^2/((-b+a x^4) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^2/((-1+x^4) (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((b+2 a x^2) (b x^2+a x^4)^(1/4))/(-b+a x^2),x]*)


(* ::Input:: *)
(*int[((-b+2 a x^2) (b x^2+a x^4)^(1/4))/(b+a x^2),x]*)


(* ::Input:: *)
(*int[(b x^2+a x^4)^(1/4)/(x^4 (b+a x^4)),x]*)


(* ::Input:: *)
(*int[(b x^2+a x^4)^(1/4)/(x^4 (-b+a x^4)),x]*)


(* ::Input:: *)
(*int[(x^2+x^4)^(1/4)/(x^4 (-1+x^4)),x]*)


(* ::Input:: *)
(*int[((2 b+a x^2) (b x^2+a x^4)^(1/4))/x^2,x]*)


(* ::Input:: *)
(*int[((-2 b+a x^2) (b x^2+a x^4)^(1/4))/x^2,x]*)


(* ::Input:: *)
(*int[(2 b+a x^4)/(x^4 (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2 b+a x^4)/(x^4 (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^2)/((b+a x^2) (b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b x^2+a x^4)^(1/4)/x^2,x]*)


(* ::Input:: *)
(*int[x^2 (x^2+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[(x^2+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[1/(x^8 (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/(x^2 (1+x^2) (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x^4 (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^2+x^4)^(1/4)/x^4,x]*)


(* ::Input:: *)
(*int[1/((1+x^2) (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x^2 (x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^2+x^4)^(1/4)/(x^2 (1+x^2)),x]*)


(* ::Input:: *)
(*int[((-1-x+x^2) (-x^2+x^4)^(1/4))/(-1+2 x^2),x]*)


(* ::Input:: *)
(*int[((-2+x) (-x^2+x^4)^(1/4))/(-1+2 x^2),x]*)


(* ::Input:: *)
(*int[((-b+a x^4) (-b x^2+a x^4)^(1/4))/(-b-a x^2+x^4),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (-x^2+x^4)^(1/4))/(-1-x^2+x^4),x]*)


(* ::Input:: *)
(*int[((-b x^2+a x^4)^(1/4) (-b x^4+a x^8))/(b+a x^4+x^8),x]*)


(* ::Input:: *)
(*int[(b+a x^8)/((-b x^2+a x^4)^(1/4) (-b+a x^8)),x]*)


(* ::Input:: *)
(*int[(-b+a x^8)/((-b x^2+a x^4)^(1/4) (b+a x^8)),x]*)


(* ::Input:: *)
(*int[(1+x^8)/((-x^2+x^4)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[(-1+x^8)/((-x^2+x^4)^(1/4) (1+x^8)),x]*)


(* ::Input:: *)
(*int[(b+a x^4)/((-b-a x^2+x^4) (-b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^4)/((-1-x^2+x^4) (-x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^4 (-b x^2+a x^4)^(1/4))/(b+a x^4+x^8),x]*)


(* ::Input:: *)
(*int[(x^4 (-x^2+x^4)^(1/4))/(1+x^4+x^8),x]*)


(* ::Input:: *)
(*int[((b+a x^4) (-b x^2+a x^4)^(1/4))/(b+a x^4+x^8),x]*)


(* ::Input:: *)
(*int[((1+x^4) (-x^2+x^4)^(1/4))/(1+x^4+x^8),x]*)


(* ::Input:: *)
(*int[1/((-b x^2+a x^4)^(1/4) (b+a x^8)),x]*)


(* ::Input:: *)
(*int[1/((-b x^2+a x^4)^(1/4) (-b+a x^8)),x]*)


(* ::Input:: *)
(*int[1/((-x^2+x^4)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[1/((-b x^2+a x^4)^(1/4) (2 b+a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[1/((-b x^2+a x^4)^(1/4) (-2 b-a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[((b+a x^2+x^4) (-b x^2+a x^4)^(1/4))/(-b+a x^4),x]*)


(* ::Input:: *)
(*int[((-b-a x^2+x^4) (-b x^2+a x^4)^(1/4))/(b+a x^4),x]*)


(* ::Input:: *)
(*int[((-b x^2+a x^4)^(1/4) (-b+2 a x^4))/(b+a x^4),x]*)


(* ::Input:: *)
(*int[(b+2 a x^4)/((-b+a x^4) (-b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+2 x^4)/((-1+x^4) (-x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((b+a x^2) (-b x^2+a x^4)^(1/4))/(-b+a x^2),x]*)


(* ::Input:: *)
(*int[((-b+a x^2) (-b x^2+a x^4)^(1/4))/(b+a x^2),x]*)


(* ::Input:: *)
(*int[(a x^4+x^8)/((-b x^2+a x^4)^(1/4) (-b+2 a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[((-b+a x^2) (-b x^2+a x^4)^(1/4))/(b-a x^2+x^4),x]*)


(* ::Input:: *)
(*int[(x^4 (-b x^2+a x^4)^(1/4))/(b+a x^8),x]*)


(* ::Input:: *)
(*int[((-b+a x^4) (-b x^2+a x^4)^(1/4))/(x^4 (b+a x^4)),x]*)


(* ::Input:: *)
(*int[1/((-b+a x^4)^2 (-b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-1+x^4)^2 (-x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b x^2+a x^4)^(1/4)/(-b+2 a x^8),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/((-b+a x^2+x^4) (-b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2 b+a x^4)/(x^4 (b+a x^4) (-b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b x^2+a x^4)^(1/4)/(x^4 (-b+a x^4)),x]*)


(* ::Input:: *)
(*int[(-x^2+x^4)^(1/4)/(x^4 (-1+x^4)),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((b+a x^2+x^4) (-b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-b+a x^4) (-b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-1+x^4) (-x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((b-a x^2+x^4) (-b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/(x^2 (b+a x^2) (-b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-b x^2+a x^4)^(1/4) (a+b x^8)),x]*)


(* ::Input:: *)
(*int[(-b+a x^4)/((-b x^2+a x^4)^(1/4) (-b+2 a x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[1/((-2 b+a x^4) (-b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((b-a x^2+x^4) (-b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-1+2 x+2 x^2) (-x^2+x^4)^(1/4))/(-1+2 x^2),x]*)


(* ::Input:: *)
(*int[((-1+2 x) (-x^2+x^4)^(1/4))/((-1+x) x),x]*)


(* ::Input:: *)
(*int[((-b+a x^2) (-b x^2+a x^4)^(1/4))/x^2,x]*)


(* ::Input:: *)
(*int[x^2/((-b+a x^2) (-b x^2+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b x^2+a x^4)^(1/4)/x^2,x]*)


(* ::Input:: *)
(*int[x^4 (-x^2+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x^2 (-x^2+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[(-x^2+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[1/(x^8 (-x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x^4 (-x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-1+x^2) (-x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x^2 (-x^2+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-x^2+x^4)^(1/4)/x^6,x]*)


(* ::Input:: *)
(*int[(-x^2+x^4)^(1/4)/x^4,x]*)


(* ::Input:: *)
(*int[(b+a x^3)/(x^6 (-b+a x^3) (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^6)/(x^6 (-b+a x^3) (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^6)/(x^3 (b+a x^3) (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^6)/(x^3 (-b+a x^3) (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^3)/(x^3 (-b+a x^3) (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^6)/(x^6 (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^6)/(x^6 (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((b+a x^3) (b+2 a x^3))/(x^6 (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b-a x^3+x^6)/(x^6 (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b-a x^3+x^6)/(x^6 (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^3)/(x^3 (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^3)/(x^3 (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^3 (5 b+8 a x^3))/((b x+a x^4)^(1/4) (-2+b x^5+a x^8)),x]*)


(* ::Input:: *)
(*int[1/((b+2 a x^3) (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-b+a x^3) (b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b x+a x^4)^(1/4)/x^2,x]*)


(* ::Input:: *)
(*int[1/(b x+a x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x^7 (-x+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x^4 (-x+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x (-x+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[(-1+x^3)/(x^6 (1+x^3) (x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^6)/(x^6 (1+x^3) (x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^3)/(x^3 (1+x^3) (x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^3)/(x^6 (x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+2 x^3)/(x^6 (x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x+x^4)^(1/4)/x^8,x]*)


(* ::Input:: *)
(*int[(1+x^3)/(x^6 (x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-x+x^4)^(1/4)/x^5,x]*)


(* ::Input:: *)
(*int[(b-3 a x^3+3 x^6)/(x^6 (-b+2 a x^3) (-b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1-3 x^3+3 x^6)/(x^6 (-1+2 x^3) (-x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^6)/(x^6 (b+a x^3) (-b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^3)/(x^6 (b+a x^3) (-b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^3)/(x^3 (b+a x^3) (-b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-b+a x^3) (-b+2 a x^3))/(x^6 (-b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b-a x^3+x^6)/(x^6 (-b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^6)/(x^6 (-b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^6)/(x^6 (-b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^3)/(x^3 (-b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^3 (5 b+8 a x^3))/((b x+a x^4)^(1/4) (-2+b x^5+a x^8)),x]*)


(* ::Input:: *)
(*int[(b+a x^3)/(x^3 (-b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((b+a x^3) (-b x+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^7 (-x+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x^4 (-x+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x (-x+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[(-x+x^4)^(1/4)/x^2,x]*)


(* ::Input:: *)
(*int[(1+x^6)/(x^6 (-1+x^3) (-x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^3)/(x^6 (-1+x^3) (-x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^3)/(x^3 (-1+x^3) (-x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-x+x^4)^(1/4)/x^8,x]*)


(* ::Input:: *)
(*int[1/((-1+x^3) (-x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^3)/(x^6 (-x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^3)/(x^6 (-x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-x+x^4)^(1/4)/x^5,x]*)


(* ::Input:: *)
(*int[1/(x^3 (-x+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2 x^2+x^4)/((1+x^4)^(1/4) (1+2 x^4)),x]*)


(* ::Input:: *)
(*int[(-2+x^2)/((1+x^4)^(1/4) (1+2 x^4)),x]*)


(* ::Input:: *)
(*int[(-b-a x^4+2 x^8)/((b+a x^4)^(1/4) (-b-a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[1/((b+a x^4)^(1/4) (-2 b-2 a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-1+2 x^2)/((1+x^4)^(1/4) (1-x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((b+a x^4)^(1/4) (b x^4+2 a x^8))/(2+b x^4+a x^8),x]*)


(* ::Input:: *)
(*int[(-b+a x^4)/((b+a x^4)^(1/4) (b-a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(2 b+a x^4)/((b+a x^4)^(1/4) (-2 b-2 a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-1+x)/((1+x+x^2) (1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^4/((b+a x^4)^(1/4) (b+2 a x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[x^4/((1+x^4)^(1/4) (1+2 x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[(b+a x^4)^(3/4)/(b+2 a x^4+2 x^8),x]*)


(* ::Input:: *)
(*int[(1+x^4)^(3/4)/(1+2 x^4+2 x^8),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^4+2 x^8)/((b+a x^4)^(1/4) (-b+a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-1+2 x^4+2 x^8)/((1+x^4)^(1/4) (-1+x^4+x^8)),x]*)


(* ::Input:: *)
(*int[1/((b+a x^4)^(1/4) (-b+a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[1/((1+x^4)^(1/4) (-1+x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^4)/((b+a x^4)^(1/4) (-b+a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-1+2 x^4)/((1+x^4)^(1/4) (-1+x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(b-a x^4+2 x^8)/((b+a x^4)^(1/4) (-b-2 a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(1-x^4+2 x^8)/((1+x^4)^(1/4) (-1-2 x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-2 b+a x^4)/((b+a x^4)^(1/4) (-b+a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-2+x^4)/((1+x^4)^(1/4) (-1+x^4+x^8)),x]*)


(* ::Input:: *)
(*int[((1+x^4)^(1/4) (x^2+2 x^4))/(1+x^2+x^4),x]*)


(* ::Input:: *)
(*int[(2 b-a x^4+2 x^8)/((b+a x^4)^(1/4) (-2 b+a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(2-x^4+2 x^8)/((1+x^4)^(1/4) (-2+x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(1+2 x^2+2 x^4)/((1+x^4)^(1/4) (1+2 x^4)),x]*)


(* ::Input:: *)
(*int[(b-2 a x^4+2 x^8)/((b+a x^4)^(1/4) (-2 b-a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(1-2 x^4+2 x^8)/((1+x^4)^(1/4) (-2-x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-2 b+a x^4)/((b+a x^4)^(1/4) (-b-a x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[(-2+x^4)/((1+x^4)^(1/4) (-1-x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[(-2 b+a x^4)/((b+a x^4)^(1/4) (-2 b+a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-2+x^4)/((1+x^4)^(1/4) (-2+x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-2 x+x^2)/((1-x+x^2) (1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^8)/((b+a x^4)^(1/4) (-b+a x^8)),x]*)


(* ::Input:: *)
(*int[(-1+2 x^8)/((1+x^4)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[(1+3 x+3 x^4)/(x (1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-3 b+2 a x^4)/((-2 b+a x^4) (b+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^4 (b+a x^4)^(1/4))/(b+2 a x^4),x]*)


(* ::Input:: *)
(*int[(-b+a x^4)/((b+a x^4)^(1/4) (-b+3 a x^4)),x]*)


(* ::Input:: *)
(*int[(2+x)/(x (1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+2 a x^4)/((b+a x^4)^(1/4) (-2 b-a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(1+2 x^4)/((1+x^4)^(1/4) (-2-x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-b+a x^4+x^8)/(x^8 (-b+a x^4) (b+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^8)/((b+a x^4)^(1/4) (b+a x^8)),x]*)


(* ::Input:: *)
(*int[(-1+x^8)/((1+x^4)^(1/4) (1+x^8)),x]*)


(* ::Input:: *)
(*int[x^2/((b+a x^4)^(1/4) (b+2 a x^4)),x]*)


(* ::Input:: *)
(*int[(b+a x^4)^(3/4)/(x^4 (2 b+a x^4)),x]*)


(* ::Input:: *)
(*int[((b+a x^4)^(1/4) (b x^4+2 a x^8))/(-1+b x^4+a x^8),x]*)


(* ::Input:: *)
(*int[(-2+a x^4)/((b+a x^4)^(1/4) (-b+a x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[(-2+x^4)/((1+x^4)^(1/4) (-1+x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[(-1-x+x^4)/(x (1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((b+a x^4)^(1/4) (-b+a x^8)),x]*)


(* ::Input:: *)
(*int[1/((1+x^4)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[1/((b+a x^4)^(1/4) (2 b+a x^4)),x]*)


(* ::Input:: *)
(*int[((-b+a x^4) (b+a x^4)^(1/4))/x^2,x]*)


(* ::Input:: *)
(*int[(-2 b-2 a x^4+x^8)/(x^4 (b+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x)/(x (1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x (b+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^4)^(1/4)/x,x]*)


(* ::Input:: *)
(*int[x^10 (1+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x^6 (1+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x^2 (1+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x^3 (1+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[1/(x^4 (1+x^4)^(5/4)),x]*)


(* ::Input:: *)
(*int[(1+2 x^4)/(x^4 (1+x^4)^(5/4)),x]*)


(* ::Input:: *)
(*int[1/(x^4 (1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[((1+x^4)^(1/4) (-3+2 x^4))/x^4,x]*)


(* ::Input:: *)
(*int[(-x^2+x^4)/((-1+x^4)^(1/4) (-1+2 x^4)),x]*)


(* ::Input:: *)
(*int[(b-2 c x^4+2 a x^8)/((-b+a x^4)^(1/4) (-2 b-c x^4+2 a x^8)),x]*)


(* ::Input:: *)
(*int[(1-2 x^4+2 x^8)/((-1+x^4)^(1/4) (-2-x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[(1-2 x+2 x^4)/(x (-1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-3+2 x)/(x (-1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+c x^4+2 a x^8)/((-b+a x^4)^(1/4) (b-c x^4+a x^8)),x]*)


(* ::Input:: *)
(*int[(-1+x^4+2 x^8)/((-1+x^4)^(1/4) (1-x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^4)/((-b+a x^4)^(1/4) (-2 b-c x^4+2 a x^8)),x]*)


(* ::Input:: *)
(*int[(-1+2 x^4)/((-1+x^4)^(1/4) (-2-x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[(2 d+c x^4)/((-b+a x^4)^(1/4) (-2 f+e x^8)),x]*)


(* ::Input:: *)
(*int[(2+x^4)/((-1+x^4)^(1/4) (-2+x^8)),x]*)


(* ::Input:: *)
(*int[((-b+a x^4)^(1/4) (-b x^4+2 a x^8))/(-2-b x^4+a x^8),x]*)


(* ::Input:: *)
(*int[(b+a x^4)/((-b+a x^4)^(1/4) (b+c x^4+a x^8)),x]*)


(* ::Input:: *)
(*int[(1+x^4)/((-1+x^4)^(1/4) (1+x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(b-2 x^4+a x^8)/((-b+a x^4)^(1/4) (b-2 x^4+2 a x^8)),x]*)


(* ::Input:: *)
(*int[(1-2 x^4+x^8)/((-1+x^4)^(1/4) (1-2 x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[1/((-b+a x^4)^(1/4) (-b-a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[1/((-b+a x^4)^(1/4) (-b-x^4+a x^8)),x]*)


(* ::Input:: *)
(*int[1/((-b^4+a^4 x^4)^(1/4) (-b^8-c x^4+a^8 x^8)),x]*)


(* ::Input:: *)
(*int[1/((-1+x^4)^(1/4) (-1-x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-b-2 a x^4+2 x^8)/((-b+a x^4)^(1/4) (-b-a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-1-2 x^4+2 x^8)/((-1+x^4)^(1/4) (-1-x^4+x^8)),x]*)


(* ::Input:: *)
(*int[x^4/((-b+a x^4)^(1/4) (b-2 a x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[x^4/((-1+x^4)^(1/4) (1-2 x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[(-b+a x^4)^(3/4)/(b-2 a x^4+2 x^8),x]*)


(* ::Input:: *)
(*int[(-1+x^4)^(3/4)/(1-2 x^4+2 x^8),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^4)/((-b+a x^4)^(1/4) (-b-a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-b^8+2 a^4 x^4)/((-b^8+a^4 x^4)^(1/4) (-b^8-c x^4+a^8 x^8)),x]*)


(* ::Input:: *)
(*int[(-1+2 x^4)/((-1+x^4)^(1/4) (-1-x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-2 b-a x^4+2 x^8)/((-b+a x^4)^(1/4) (-2 b-a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-2-x^4+2 x^8)/((-1+x^4)^(1/4) (-2-x^4+x^8)),x]*)


(* ::Input:: *)
(*int[x^4/((-b+a x^4)^(1/4) (-b+2 a x^4+x^8)),x]*)


(* ::Input:: *)
(*int[x^4/((-1+x^4)^(1/4) (-1+2 x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(-1-2 x^2+2 x^4)/((-1+x^4)^(1/4) (-1+2 x^4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^4)^(3/4)/(-b+2 a x^4+x^8),x]*)


(* ::Input:: *)
(*int[(-1+x^4)^(3/4)/(-1+2 x^4+x^8),x]*)


(* ::Input:: *)
(*int[(-2 b+a x^4)/((-b+a x^4)^(1/4) (-b+a x^4+2 x^8)),x]*)


(* ::Input:: *)
(*int[(-2 b-a x^4+2 x^8)/(x^4 (-b+a x^4)^(1/4) (-b+2 a x^4)),x]*)


(* ::Input:: *)
(*int[(b-a x^4+2 x^8)/((-b+a x^4)^(1/4) (b+3 a x^4)),x]*)


(* ::Input:: *)
(*int[(b+a x^4)/((-b+a x^4)^(1/4) (b+3 a x^4)),x]*)


(* ::Input:: *)
(*int[(x^4 (-b+a x^4)^(1/4))/(-b+2 a x^4),x]*)


(* ::Input:: *)
(*int[(-b-2 a x^4+2 x^8)/(-b+a x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x^2/((-b+a x^4)^(1/4) (-b+2 a x^4)),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^4)/((-b+a x^4)^(1/4) (-b+a x^8)),x]*)


(* ::Input:: *)
(*int[(-1+2 x^4)/((-1+x^4)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[x^4/((-b+a x^4)^(1/4) (-b+a x^8)),x]*)


(* ::Input:: *)
(*int[x^4/((-1+x^4)^(1/4) (-1+x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(1/4) (-1+2 x^4))/(x^2 (-1+x^2)),x]*)


(* ::Input:: *)
(*int[(-3 b+2 a x^8)/(x^8 (-b+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2-x^2+2 x^4)/((-1+x^2) (-1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x (-b+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2 b+a x^4)/(x^4 (-b+a x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-b+a x^4)^(1/4) (b+a x^4)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)^(1/4)/x^5,x]*)


(* ::Input:: *)
(*int[(-1+x^4)^(1/4)/x^2,x]*)


(* ::Input:: *)
(*int[(-1+x^4)^(1/4)/x,x]*)


(* ::Input:: *)
(*int[x^10 (-1+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x^6 (-1+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[x^2 (-1+x^4)^(1/4),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(1/4) (2+x^4+2 x^8))/x^4,x]*)


(* ::Input:: *)
(*int[(-1+2 x^4)/(x^8 (-1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1-2 x^2+2 x^4)/((-1+x^2) (-1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x^4 (-1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+2 x^4)/(x^2 (-1+x^4)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^2 (10 b+9 a x))/((b x^2+a x^3)^(1/4) (-b-a x+x^10)),x]*)


(* ::Input:: *)
(*int[(x (6 b+5 a x))/((b x^2+a x^3)^(1/4) (-b-a x+x^6)),x]*)


(* ::Input:: *)
(*int[(2 b+a x)/((b+a x+x^2) (b x^2+a x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((2 b+a x) (b x^2+a x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(2+x)/(x^2 (x^2+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(2+x)/((1+x) (x^2+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2+x)/(x (x^2+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x (-6 b+5 a x))/((-b x^2+a x^3)^(1/4) (b-a x+x^6)),x]*)


(* ::Input:: *)
(*int[(-2 b+a x)/((-b+a x+x^2) (-b x^2+a x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-2 b+a x) (-b x^2+a x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2-x+2 x^2)/((-1+x) x (-x^2+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2+x)/((-1+x) (-x^2+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(2+x)/(x (-x^2+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-5+x^4)/((x+x^3)^(1/4) (-1+x^4)),x]*)


(* ::Input:: *)
(*int[1/((-1+x^2) (x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+2 x)/((-1+x^2) (x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2+x)/((-1+x^2) (x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((b+a x) (b^2 x+a^2 x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-b+a x) (b^2 x+a^2 x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (3+x^2) (-1-x^2+x^3))/(x^6 (x+x^3)^(1/4) (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((3+x^2) (1+2 x^2+x^4+x^6))/(x^6 (x+x^3)^(1/4) (-1-x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(x^2 (11 b+9 a x^2))/((b x+a x^3)^(1/4) (b+a x^2+x^11)),x]*)


(* ::Input:: *)
(*int[(3 b+a x^2)/((b+a x^2+x^3) (b x+a x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[((3+x^2) (-1-x^2+2 x^3))/(x^6 (x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[((3+x^2) (1+x^2+x^3))/(x^6 (x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (3+x^2))/(x^6 (x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(3+x^2)/((1+x^2) (x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-5+x^2)/(x^2 (x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[x/((1+x^2) (-x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((1+x^2) (-x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x)/((1+x^2) (-x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x)/((1+x^2) (-x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2+x)/((1+x^2) (-x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(3+x)/((1+x^2) (-x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-3 b+a x^2) (b-a x^2+x^3))/(x^3 (-b+a x^2+x^3) (-b x+a x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-7 b x+5 a x^3)/((-b x+a x^3)^(1/4) (b-a x^2+x^7)),x]*)


(* ::Input:: *)
(*int[(x^2 (-11 b+9 a x^2))/((-b x+a x^3)^(1/4) (-b+a x^2+x^11)),x]*)


(* ::Input:: *)
(*int[(-3 b+a x^2)/((-b+a x^2+x^3) (-b x+a x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-3+x^2) (1-x^2+x^3))/(x^6 (-x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-3+x^2)/((-1+x^2) (-x+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[((4+x^3) (1+x^3+x^4))/((1+x^3)^(1/4) (1+2 x^3+x^6+x^8)),x]*)


(* ::Input:: *)
(*int[((4+x^3) (1+2 x^3+x^6+x^8))/(x^4 (1+x^3)^(1/4) (-1-2 x^3-x^6+x^8)),x]*)


(* ::Input:: *)
(*int[(x (8 b+5 a x^3))/((b+a x^3)^(1/4) (-b-a x^3+x^8)),x]*)


(* ::Input:: *)
(*int[(4 b+a x^3)/((b+a x^3)^(1/4) (b+a x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((4 b+a x^3) (-b-a x^3+x^4))/(x^4 (b+a x^3)^(1/4) (-b-a x^3+2 x^4)),x]*)


(* ::Input:: *)
(*int[(4 b+a x^3)/((b+a x^3)^(1/4) (-b-a x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(1+x^3)^(1/4)/x^7,x]*)


(* ::Input:: *)
(*int[(1+x^3)^(1/4)/x^4,x]*)


(* ::Input:: *)
(*int[(1+x^3)^(1/4)/x,x]*)


(* ::Input:: *)
(*int[1/(x^7 (1+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x^4 (1+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x (1+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[((4+x^3) (-1-x^3+x^4))/(x^8 (1+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^8 (1+x^3)^(1/4),x]*)


(* ::Input:: *)
(*int[x^8/(1+x^3)^(1/4),x]*)


(* ::Input:: *)
(*int[x^2 (1+x^3)^(1/4),x]*)


(* ::Input:: *)
(*int[x^2/(1+x^3)^(1/4),x]*)


(* ::Input:: *)
(*int[(4+x^3)/(x^4 (1+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^4 (-4 b+a x^3))/((-b+a x^3)^(1/4) (-b^2+2 a b x^3-a^2 x^6+x^8)),x]*)


(* ::Input:: *)
(*int[(x^4 (-4+x^3))/((-1+x^3)^(1/4) (-1+2 x^3-x^6+x^8)),x]*)


(* ::Input:: *)
(*int[((-4 b+a x^3) (b-a x^3+x^4))/(x^4 (-b+a x^3)^(1/4) (-b+a x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(x (-8 b+5 a x^3))/((-b+a x^3)^(1/4) (b-a x^3+x^8)),x]*)


(* ::Input:: *)
(*int[(-4 b+a x^3)/((-b+a x^3)^(1/4) (-b+a x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(-4 b+a x^3)/((-b+a x^3)^(1/4) (b-a x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^3)^(1/4)/x^7,x]*)


(* ::Input:: *)
(*int[(-b+a x^3)^(1/4)/x^4,x]*)


(* ::Input:: *)
(*int[(-b+a x^3)^(1/4)/x,x]*)


(* ::Input:: *)
(*int[1/(x^7 (-b+a x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x^4 (-b+a x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x (-b+a x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[((-4+x^3) (1-x^3+x^4))/(x^8 (-1+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-4+x^3)/(x^4 (-1+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[x^2 (-1+x^3)^(1/4),x]*)


(* ::Input:: *)
(*int[x^2/(-1+x^3)^(1/4),x]*)


(* ::Input:: *)
(*int[(x^5 (-4 b+5 a x^2))/((-b+a x^2)^(1/4) (b-b x^8+a x^10)),x]*)


(* ::Input:: *)
(*int[(-3 b+a x^2)/((3 b-2 a x^2)^(1/4) (3 b-2 a x^2+3 x^4)),x]*)


(* ::Input:: *)
(*int[(4 c+3 b x+2 a x^2)/((c+b x+a x^2)^(1/4) (-c-b x-a x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(b+2 a x)/((-b+a x) (2 b+a x) (-1+b x+a x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(x^3 (-5 b+6 a x))/((-b x+a x^2)^(1/4) (c-b x^5+a x^6)),x]*)


(* ::Input:: *)
(*int[(b+2 a x)/((c+b x+a x^2)^(1/4) (5 c+4 b x+4 a x^2)),x]*)


(* ::Input:: *)
(*int[(x^2 (2 b+3 a x^2))/((b+a x^2)^(1/4) (b+b x^4+a x^6)),x]*)


(* ::Input:: *)
(*int[(-2 b+a x^2)/((-b+a x^2)^(1/4) (-b+a x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(-3 b+2 a x)/((-b x+a x^2)^(1/4) (b-a x+x^3)),x]*)


(* ::Input:: *)
(*int[(-3+2 x)/((-x+x^2)^(1/4) (1-x+x^3)),x]*)


(* ::Input:: *)
(*int[(2+x)/((-3+x) (1-x^2)^(1/4) (1+x^2)),x]*)


(* ::Input:: *)
(*int[(-a+2 x)/((-1+b-a x+x^2) (b-a x+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1+x)/((-3+x) (1+x) (-2-2 x+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(a+2 x)/((b+a x+x^2)^(1/4) (-1+2 b+2 a x+2 x^2)),x]*)


(* ::Input:: *)
(*int[1/((1+2 x) (1+2 x+2 x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-1+x) (2-2 x+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(a+x)/((-1+2 b+2 a x+x^2) (2 b+2 a x+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((1+x) (2+2 x+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-5+4 x^2)/((1+x^2)^(1/4) (-4+3 x^4)),x]*)


(* ::Input:: *)
(*int[(-2+x)/((1+x^2)^(1/4) (1-x+x^2)),x]*)


(* ::Input:: *)
(*int[(2 b+a x^2)/((b+a x^2)^(1/4) (b n+a n x^2+2 x^4)),x]*)


(* ::Input:: *)
(*int[(-2+2 x+x^2)/(x^2 (1+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^2+2 x^4)/((1+x^2)^(1/4) (2+3 x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(2 b+a x^2)/((b+a x^2)^(1/4) (-2 b-2 a x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(2 b+a x^2)/((b+a x^2)^(1/4) (-b-a x^2+x^4)),x]*)


(* ::Input:: *)
(*int[1/(x (1+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(1+x^2)^(1/4)/x,x]*)


(* ::Input:: *)
(*int[(2-x^2+x^4)/(x^4 (1+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[x (1+x^2)^(1/4),x]*)


(* ::Input:: *)
(*int[x/(1+x^2)^(1/4),x]*)


(* ::Input:: *)
(*int[(x^4 (-1+x^2)^(1/4))/(-4+3 x^4),x]*)


(* ::Input:: *)
(*int[1/((-1+x^2)^(1/4) (-4+3 x^4)),x]*)


(* ::Input:: *)
(*int[(-1-x+x^2)/((-1+x) x (-1+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(2-x+x^2)/(x^2 (-1+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2 b+a x^2)/((-b+a x^2)^(1/4) (-b+a x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-1+a x^2)^(1/4) (-4+3 a x^2))/(-2+a x^2),x]*)


(* ::Input:: *)
(*int[(3-3 x^2+2 x^4)/((-1+x^2)^(1/4) (2-3 x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(a+b x)/((2-x^2) (-1+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(x (-b+a x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)^(1/4)/x,x]*)


(* ::Input:: *)
(*int[1/((-2 b+a x^2) (-b+a x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(4+x^4)/(x^4 (-1+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-2+x)/((-1+x) (-1+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(2+x)/((1+x) (-1+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[x (-1+x^2)^(1/4),x]*)


(* ::Input:: *)
(*int[x/(-1+x^2)^(1/4),x]*)


(* ::Input:: *)
(*int[(a^3 b-a^2 (4 a+b) x+a (10 a-b) x^2+(-8 a+b) x^3+2 x^4)/(x (-b+x) (x (-a+x)^2 (-b+x)^3)^(1/4) (-a^2+(2 a-b^3 d) x+(-1+3 b^2 d) x^2-3 b d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(a b^3-(6 a-b) b^2 x+9 a b x^2-(4 a+3 b) x^3+2 x^4)/((x (-a+x)^2 (-b+x)^3)^(1/4) (-a^2+(2 a-b^3 d) x+(-1+3 b^2 d) x^2-3 b d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[((-b+x) (a b-3 b x+2 x^2))/(x (-a+x) (x (-a+x)^2 (-b+x)^3)^(1/4) (b+(-1+a^2 d) x-2 a d x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(-a^2 b+4 a b x-(2 a+3 b) x^2+2 x^3)/((x (-a+x)^2 (-b+x)^3)^(1/4) (b+(-1+a^2 d) x-2 a d x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[((a b-2 (2 a-b) x+x^2) (-a^3+3 a^2 x-3 a x^2+x^3))/(x (-b+x) (x (-a+x) (-b+x)^3)^(1/4) (a^3-(3 a^2+b^3 d) x+3 (a+b^2 d) x^2-(1+3 b d) x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(a b^3-2 (3 a-b) b^2 x+3 (3 a-b) b x^2-4 a x^3+x^4)/((x (-a+x) (-b+x)^3)^(1/4) (a^3-(3 a^2+b^3 d) x+3 (a+b^2 d) x^2-(1+3 b d) x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(-3 a b^3+2 b^2 (3 a+b) x-3 b (a+b) x^2+x^4)/((x (-a+x) (-b+x)^3)^(1/4) (a b^3 d-b^2 (3 a+b) d x+3 b (a+b) d x^2-(1+a d+3 b d) x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(x^3 (-3 a b+2 b x+x^2))/((-a+x) (-b+x) (x (-a+x) (-b+x)^3)^(1/4) (a b^3 d-b^2 (3 a+b) d x+3 b (a+b) d x^2-(1+a d+3 b d) x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[((-b+x) (a b-2 b x+x^2))/(x (-a+x) (x (-a+x) (-b+x)^3)^(1/4) (b-(1+a d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(a b-2 b x+x^2)/((x (-a+x) (-b+x)^3)^(1/4) (b-(1+a d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[((-2 k+(-1+k) (1+k) x+2 k x^2) (1+3 k x+3 k^2 x^2+k^3 x^3))/((-1+k x) (-1+x^2) ((1-x^2) (1-k^2 x^2))^(1/4) (1-d+(3+d) k x+(d+3 k^2) x^2+k (-d+k^2) x^3)),x]*)


(* ::Input:: *)
(*int[((-2 k-(-1+k) (1+k) x+2 k x^2) (-1+3 k x-3 k^2 x^2+k^3 x^3))/((1+k x) (-1+x^2) ((1-x^2) (1-k^2 x^2))^(1/4) (1-d-(3+d) k x+(d+3 k^2) x^2+k (d-k^2) x^3)),x]*)


(* ::Input:: *)
(*int[(((1-3 k^2) x+2 k^2 x^3) (-1+3 k^2 x^2-3 k^4 x^4+k^6 x^6))/((-1+x^2) ((1-x^2) (1-k^2 x^2))^(1/4) (-1+d+(-d+3 k^2) x^2-3 k^4 x^4+k^6 x^6)),x]*)


(* ::Input:: *)
(*int[((-2+(-1+k) (1+k) x+2 k^2 x^2) (-1+3 x-3 x^2+x^3))/((1+x) ((1-x^2) (1-k^2 x^2))^(1/4) (-1+k^2 x^2) (1-d-(3+d) x+(3+d k^2) x^2+(-1+d k^2) x^3)),x]*)


(* ::Input:: *)
(*int[((-2-(-1+k) (1+k) x+2 k^2 x^2) (1+3 x+3 x^2+x^3))/((-1+x) ((1-x^2) (1-k^2 x^2))^(1/4) (-1+k^2 x^2) (-1+d-(3+d) x-(3+d k^2) x^2+(-1+d k^2) x^3)),x]*)


(* ::Input:: *)
(*int[(((-3+k^2) x+2 k^2 x^3) (-1+3 x^2-3 x^4+x^6))/((1-k^2 x^2) ((1-x^2) (1-k^2 x^2))^(1/4) (-1+d+(3-d k^2) x^2-3 x^4+x^6)),x]*)


(* ::Input:: *)
(*int[(-2 k+(-1+k) (1+k) x+2 k x^2)/(((1-x^2) (1-k^2 x^2))^(1/4) (1-d+(3+d) k x+(d+3 k^2) x^2+k (-d+k^2) x^3)),x]*)


(* ::Input:: *)
(*int[(-2 k-(-1+k) (1+k) x+2 k x^2)/(((1-x^2) (1-k^2 x^2))^(1/4) (-1+d+(3+d) k x-(d+3 k^2) x^2+k (-d+k^2) x^3)),x]*)


(* ::Input:: *)
(*int[(-2+(-1+k) (1+k) x+2 k^2 x^2)/(((1-x^2) (1-k^2 x^2))^(1/4) (1-d-(3+d) x+(3+d k^2) x^2+(-1+d k^2) x^3)),x]*)


(* ::Input:: *)
(*int[(-2-(-1+k) (1+k) x+2 k^2 x^2)/(((1-x^2) (1-k^2 x^2))^(1/4) (-1+d-(3+d) x-(3+d k^2) x^2+(-1+d k^2) x^3)),x]*)


(* ::Input:: *)
(*int[((1-3 k^2) x+2 k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/4) (-1+d+(-d+3 k^2) x^2-3 k^4 x^4+k^6 x^6)),x]*)


(* ::Input:: *)
(*int[((-3+k^2) x+2 k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/4) (-1+d+(3-d k^2) x^2-3 x^4+x^6)),x]*)


(* ::Input:: *)
(*int[((-a b-a c+3 b c+2 (a-b-c) x+x^2) (-a^3+3 a^2 x-3 a x^2+x^3))/((-b+x) (-c+x) ((-a+x) (-b+x) (-c+x))^(1/4) (-a^3-b c d+(3 a^2+b d+c d) x-(3 a+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-a b-a c+3 b c+2 (a-b-c) x+x^2)/(((-a+x) (-b+x) (-c+x))^(1/4) (-a^3-b c d+(3 a^2+b d+c d) x-(3 a+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-a b+(3 a-2 b) x) (-a^3+3 a^2 x-3 a x^2+x^3))/(x (-b+x) (x (-a+x) (-b+x)^2)^(1/4) (a^3+(-3 a^2+b^2 d) x+(3 a-2 b d) x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[(a b^3-b^2 (a+2 b) x-(a-4 b) b x^2+(a-2 b) x^3)/(x (-a+x) (x (-a+x) (-b+x)^2)^(1/4) (-b^2+(2 b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(a b^2-2 (2 a-b) b x+(3 a-2 b) x^2)/((x (-a+x) (-b+x)^2)^(1/4) (a^3+(-3 a^2+b^2 d) x+(3 a-2 b d) x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[(x^3 (-3 a b+(a+2 b) x))/((-a+x) (-b+x) (x (-a+x) (-b+x)^2)^(1/4) (-a b^2 d+b (2 a+b) d x-(a+2 b) d x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[(3 a b^2-2 b (2 a+b) x+(a+2 b) x^2)/((x (-a+x) (-b+x)^2)^(1/4) (-a b^2 d+b (2 a+b) d x-(a+2 b) d x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[(-a b+(-a+2 b) x)/((x (-a+x) (-b+x)^2)^(1/4) (-b^2+(2 b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((2 a-3 b+x) (-a^3+3 a^2 x-3 a x^2+x^3))/((-b+x) ((-a+x) (-b+x)^2)^(1/4) (-a^3-b^2 d+(3 a^2+2 b d) x-(3 a+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-6 a+b+5 x))/(((-a+x) (-b+x)^2)^(1/4) (b^6+a d-(6 b^5+d) x+15 b^4 x^2-20 b^3 x^3+15 b^2 x^4-6 b x^5+x^6)),x]*)


(* ::Input:: *)
(*int[(-((2 a-b) b^2)+(4 a-b) b x-(2 a+b) x^2+x^3)/((-a+x) ((-a+x) (-b+x)^2)^(1/4) (b^2+a d-(2 b+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(-((2 a-3 b) b)+2 (a-2 b) x+x^2)/(((-a+x) (-b+x)^2)^(1/4) (-a^3-b^2 d+(3 a^2+2 b d) x-(3 a+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-2 a+b+x)/(((-a+x) (-b+x)^2)^(1/4) (b^2+a d-(2 b+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[((-2 a b+(3 a-b) x) (-a^3+3 a^2 x-3 a x^2+x^3))/(x (-b+x) (x^2 (-a+x) (-b+x))^(1/4) (a^3-3 a^2 x+(3 a-b d) x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[(-2 a b x^2+(a+b) x^3)/((-a+x) (-b+x) (x^2 (-a+x) (-b+x))^(1/4) (-a b d+(a+b) d x+(1-d) x^2)),x]*)


(* ::Input:: *)
(*int[(2 a b x+(-3 a+b) x^2)/((x^2 (-a+x) (-b+x))^(1/4) (a^3-3 a^2 x+(3 a-b d) x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[(-2 a b+(a+b) x)/((x^2 (-a+x) (-b+x))^(1/4) (a b d-(a+b) d x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((-a b+2 (a-b) x+x^2) (-a^3+3 a^2 x-3 a x^2+x^3))/(x (-b+x) (x (-a+x) (-b+x))^(1/4) (-a^3+(3 a^2+b d) x-(3 a+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(x^3 (3 a b-2 (a+b) x+x^2))/((-a+x) (-b+x) (x (-a+x) (-b+x))^(1/4) (-a b d+(a+b) d x-d x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-a b+2 (a-b) x+x^2)/((x (-a+x) (-b+x))^(1/4) (-a^3+(3 a^2+b d) x-(3 a+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(3 a b-2 (a+b) x+x^2)/((x (-a+x) (-b+x))^(1/4) (-a b d+(a+b) d x-d x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-1-2 (-1+k) x+k x^2) (-1+3 k x-3 k^2 x^2+k^3 x^3))/((-1+x) x ((1-x) x (1-k x))^(1/4) (-1+(d+3 k) x-(d+3 k^2) x^2+k^3 x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x)^3 (-1+2 (-1+k) x+k x^2))/(x ((1-x) x (1-k x))^(1/4) (-1+k x) (-1+(3+d) x-(3+d k) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(x^3 (3-2 (1+k) x+k x^2))/((-1+x) ((1-x) x (1-k x))^(1/4) (-1+k x) (-d+d (1+k) x-d k x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-1-2 (-1+k) x+k x^2)/(((1-x) x (1-k x))^(1/4) (-1+(d+3 k) x-(d+3 k^2) x^2+k^3 x^3)),x]*)


(* ::Input:: *)
(*int[(-1+2 (-1+k) x+k x^2)/(((1-x) x (1-k x))^(1/4) (-1+(3+d) x-(3+d k) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(3-2 (1+k) x+k x^2)/(((1-x) x (1-k x))^(1/4) (-d+d (1+k) x-d k x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((a-3 b+2 x) (-a^3+3 a^2 x-3 a x^2+x^3))/((-b+x) ((-a+x) (-b+x))^(1/4) (-a^3+b d-(-3 a^2+d) x-3 a x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((a-3 b+2 x) (a^2-2 a x+x^2))/(((-a+x) (-b+x))^(3/4) (-b+a^3 d+(1-3 a^2 d) x+3 a d x^2-d x^3)),x]*)


(* ::Input:: *)
(*int[((-b+x) (a-3 b+2 x))/((-a+x) ((-a+x) (-b+x))^(3/4) (b-a^3 d-(1-3 a^2 d) x-3 a d x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(a-3 b+2 x)/(((-a+x) (-b+x))^(1/4) (-a^3+b d-(-3 a^2+d) x-3 a x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-2 b+a x^6) (b-c x^4+a x^6))/(x^2 (b+a x^6)^(3/4) (b+c x^4+a x^6)),x]*)


(* ::Input:: *)
(*int[(x^2 (-2 b+a x^6))/((b+a x^6)^(3/4) (b+c x^4+a x^6)),x]*)


(* ::Input:: *)
(*int[((-2 b+a x^6) (b+a x^6)^(3/4))/(x^4 (b-c x^4+a x^6)),x]*)


(* ::Input:: *)
(*int[(b+a x^6)^(3/4)/x^7,x]*)


(* ::Input:: *)
(*int[(b+a x^6)^(3/4)/x,x]*)


(* ::Input:: *)
(*int[1/(x^7 (b+a x^6)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x (b+a x^6)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) (1+x^6)^(3/4) (1-x^4+x^6))/x^12,x]*)


(* ::Input:: *)
(*int[((-2+x^6) (1-x^4+x^6))/(x^6 (1+x^6)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) (1+x^6)^(3/4))/x^8,x]*)


(* ::Input:: *)
(*int[(-2+x^6)/(x^2 (1+x^6)^(3/4)),x]*)


(* ::Input:: *)
(*int[((2 b+a x^6) (-b-c x^4+a x^6))/(x^2 (-b+a x^6)^(3/4) (-b+c x^4+a x^6)),x]*)


(* ::Input:: *)
(*int[(x^2 (2 b+a x^6))/((-b+a x^6)^(3/4) (-b+c x^4+a x^6)),x]*)


(* ::Input:: *)
(*int[(x^2 (2 b+a x^6))/((-b+a x^6)^(3/4) (-b-2 c x^4+a x^6)),x]*)


(* ::Input:: *)
(*int[(-b+a x^6)^(3/4)/x^7,x]*)


(* ::Input:: *)
(*int[(-b+a x^6)^(3/4)/x,x]*)


(* ::Input:: *)
(*int[1/(x^7 (-b+a x^6)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x (-b+a x^6)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(3/4) (2+x^6) (-1-x^4+x^6))/x^12,x]*)


(* ::Input:: *)
(*int[((2+x^6) (1-2 x^6+x^8+x^12))/(x^10 (-1+x^6)^(3/4)),x]*)


(* ::Input:: *)
(*int[((2+x^6) (-1-x^4+x^6))/(x^6 (-1+x^6)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(3/4) (2+x^6))/x^8,x]*)


(* ::Input:: *)
(*int[(2+x^6)/(x^2 (-1+x^6)^(3/4)),x]*)


(* ::Input:: *)
(*int[x^5 (-1+x^6)^(3/4),x]*)


(* ::Input:: *)
(*int[(c x^6 (-4 b+a x^5))/((b+a x^5)^(3/4) (b^2+2 a b x^5-c^2 x^8+a^2 x^10)),x]*)


(* ::Input:: *)
(*int[((-4 b+a x^5) (b-c x^4+a x^5))/(x^2 (b+a x^5)^(3/4) (b+c x^4+a x^5)),x]*)


(* ::Input:: *)
(*int[((-4 b+a x^5) (b+a x^5)^(3/4))/(x^4 (2 b+c x^4+2 a x^5)),x]*)


(* ::Input:: *)
(*int[(x^2 (-4 b+a x^5))/((b+a x^5)^(3/4) (b+c x^4+a x^5)),x]*)


(* ::Input:: *)
(*int[(b+a x^5)^(3/4)/x^6,x]*)


(* ::Input:: *)
(*int[(b+a x^5)^(3/4)/x,x]*)


(* ::Input:: *)
(*int[1/(x^11 (b+a x^5)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x^6 (b+a x^5)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x (b+a x^5)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-4+x^5) (1+x^5)^(3/4) (2-x^4+2 x^5))/x^12,x]*)


(* ::Input:: *)
(*int[((-4+x^5) (1+x^4+x^5))/(x^6 (1+x^5)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-4+x^5) (1+x^5)^(3/4))/x^8,x]*)


(* ::Input:: *)
(*int[(-4+x^5)/(x^2 (1+x^5)^(3/4)),x]*)


(* ::Input:: *)
(*int[((4+x^5) (1-x^4-2 x^5+x^8+x^9+x^10))/(x^2 (-1+x^5)^(3/4) (1+x^4-2 x^5-x^8-x^9+x^10)),x]*)


(* ::Input:: *)
(*int[(x^6 (4+x^5))/((-1+x^5)^(3/4) (1-2 x^5+x^8+x^10)),x]*)


(* ::Input:: *)
(*int[(x^2 (4 b+a x^5))/((-b+a x^5)^(3/4) (-b+c x^4+a x^5)),x]*)


(* ::Input:: *)
(*int[((-b+a x^5)^(3/4) (4 b+a x^5))/(x^4 (-b+c x^4+a x^5)),x]*)


(* ::Input:: *)
(*int[((4 b+a x^5) (-b+c x^4+a x^5))/(x^2 (-b+a x^5)^(3/4) (-b-c x^4+a x^5)),x]*)


(* ::Input:: *)
(*int[(-b+a x^5)^(3/4)/x^6,x]*)


(* ::Input:: *)
(*int[(-b+a x^5)^(3/4)/x,x]*)


(* ::Input:: *)
(*int[1/(x^11 (-b+a x^5)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x^6 (-b+a x^5)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x (-b+a x^5)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^5)^(3/4) (4+x^5) (-1-x^4+x^5))/x^12,x]*)


(* ::Input:: *)
(*int[((4+x^5) (-1+x^4+x^5))/(x^6 (-1+x^5)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-1+x^5)^(3/4) (4+x^5))/x^8,x]*)


(* ::Input:: *)
(*int[(4+x^5)/(x^2 (-1+x^5)^(3/4)),x]*)


(* ::Input:: *)
(*int[(1+x^2)^2/((1-x^2) (1-6 x^2+x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[(1-x^2)^2/((1+x^2) (1+6 x^2+x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[(-b^2+a^2 x^8)/(x^2 (b+a x^4)^(3/4) (b^2+a^2 x^8)),x]*)


(* ::Input:: *)
(*int[x^6/((b+a x^4)^(3/4) (b^2+a^2 x^8)),x]*)


(* ::Input:: *)
(*int[x^2/((b+a x^4)^(3/4) (b^2+a^2 x^8)),x]*)


(* ::Input:: *)
(*int[(1+3 x^4+x^8)/(x^2 (1+x^4)^(3/4) (1+3 x^4+3 x^8)),x]*)


(* ::Input:: *)
(*int[x^6/((-b+a x^4) (b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-b+a x^4) (b+a x^4)^(3/4))/(x^8 (b+2 a x^4)),x]*)


(* ::Input:: *)
(*int[((b+a x^4)^(3/4) (2 b+a x^4))/(x^8 (4 b+a x^4)),x]*)


(* ::Input:: *)
(*int[((-4 b+a x^4) (b+a x^4)^(3/4))/(x^8 (4 b+a x^4)),x]*)


(* ::Input:: *)
(*int[x^2/((b+a x^4)^(3/4) (-b^2+a^2 x^8)),x]*)


(* ::Input:: *)
(*int[x^6/((b+a x^4)^(3/4) (-b^2+a^2 x^8)),x]*)


(* ::Input:: *)
(*int[((-2 b+a x^4) (b+a x^4)^(3/4))/x^8,x]*)


(* ::Input:: *)
(*int[(b+a x^8)/(x^6 (-b+a x^4) (b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[(x^2 (-b+a x^4))/(b+a x^4)^(3/4),x]*)


(* ::Input:: *)
(*int[(-b+a x^8)/(x^6 (b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^8)/(x^2 (b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[x^6/(b+a x^4)^(3/4),x]*)


(* ::Input:: *)
(*int[(b+2 a x^4)/(x^2 (b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[x^2/(b+a x^4)^(3/4),x]*)


(* ::Input:: *)
(*int[(b+a x^4)^(3/4)/x^4,x]*)


(* ::Input:: *)
(*int[(b+a x^4)^(3/4)/x,x]*)


(* ::Input:: *)
(*int[1/(x^9 (b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x^5 (b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x (b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[x^8 (b+a x^4)^(3/4),x]*)


(* ::Input:: *)
(*int[x^4 (b+a x^4)^(3/4),x]*)


(* ::Input:: *)
(*int[(b+a x^4)^(3/4),x]*)


(* ::Input:: *)
(*int[((-4+x^4) (1+x^4)^(3/4))/x^12,x]*)


(* ::Input:: *)
(*int[(-1+x^4)/(x^6 (1+x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x^6 (1+x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x^2 (1+x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[(1+x^4)^(3/4)/x^8,x]*)


(* ::Input:: *)
(*int[(1-x^4+x^8)/(x^2 (-1+x^4)^(3/4) (-1-x^4+x^8)),x]*)


(* ::Input:: *)
(*int[x^6/((-b+a x^4)^(3/4) (b+a x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(3/4) (4+x^4))/(x^8 (-4+x^4)),x]*)


(* ::Input:: *)
(*int[((-b+a x^4)^(3/4) (-b+2 a x^4))/x^8,x]*)


(* ::Input:: *)
(*int[(-b+c x^4+a x^8)/(x^2 (-b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^8)/(x^2 (-b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[x^6/(-b+a x^4)^(3/4),x]*)


(* ::Input:: *)
(*int[x^2/((-b+a x^4)^(3/4) (b+a x^4)),x]*)


(* ::Input:: *)
(*int[(-b+2 a x^4)/(x^2 (-b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[x^2/(-b+a x^4)^(3/4),x]*)


(* ::Input:: *)
(*int[(-b+a x^4)^(3/4)/x^4,x]*)


(* ::Input:: *)
(*int[(-b+a x^4)^(3/4)/x,x]*)


(* ::Input:: *)
(*int[1/(x^9 (-b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x^5 (-b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x (-b+a x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[x^8 (-b+a x^4)^(3/4),x]*)


(* ::Input:: *)
(*int[x^4 (-b+a x^4)^(3/4),x]*)


(* ::Input:: *)
(*int[(-b+a x^4)^(3/4),x]*)


(* ::Input:: *)
(*int[((-4+x^4) (-1+x^4)^(3/4))/x^12,x]*)


(* ::Input:: *)
(*int[1/(x^6 (-1+x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x^2 (-1+x^4)^(3/4)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)^(3/4)/x^8,x]*)


(* ::Input:: *)
(*int[x^3 (-1+x^4)^(3/4),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(3/4) (4+x^3) (-1-x^3+x^4))/(x^8 (-1-x^3+2 x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(3/4) (4+x^3))/(x^4 (1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(x^6 (4+x^3))/((1+x^3)^(3/4) (-1-2 x^3-x^6+x^8)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(3/4) (4+x^3) (2+2 x^3+x^4))/(x^8 (4+4 x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((4+x^3) (-1-x^3+x^4))/(x^2 (1+x^3)^(3/4) (1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(3/4) (4+x^3) (-2-2 x^3+x^4))/(x^8 (-1-x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(x^2 (4+x^3))/((1+x^3)^(3/4) (1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(3/4) (4+x^3))/(x^4 (-1-x^3+x^4)),x]*)


(* ::Input:: *)
(*int[1/(x^7 (b+a x^3)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x^4 (b+a x^3)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x (b+a x^3)^(3/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^3)^(3/4)/x^4,x]*)


(* ::Input:: *)
(*int[(b+a x^3)^(3/4)/x,x]*)


(* ::Input:: *)
(*int[((1+x^3)^(3/4) (4+x^3) (1+x^3+x^4))/x^12,x]*)


(* ::Input:: *)
(*int[((4+x^3) (-1-x^3+2 x^4))/(x^6 (1+x^3)^(3/4)),x]*)


(* ::Input:: *)
(*int[((4+x^3) (-1-x^3+x^4))/(x^6 (1+x^3)^(3/4)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(3/4) (4+x^3))/x^8,x]*)


(* ::Input:: *)
(*int[(4+x^3)/(x^2 (1+x^3)^(3/4)),x]*)


(* ::Input:: *)
(*int[x^2 (1+x^3)^(3/4),x]*)


(* ::Input:: *)
(*int[((-4+x^3) (-1+x^3)^(3/4) (2-2 x^3+x^4))/(x^8 (-1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-4+x^3) (-1+x^3)^(3/4))/(x^4 (-1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(x^6 (-4+x^3))/((-1+x^3)^(3/4) (1-2 x^3+x^6+x^8)),x]*)


(* ::Input:: *)
(*int[((-4+x^3) (1-x^3+x^4))/(x^2 (-1+x^3)^(3/4) (-1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-4+x^3) (-1+x^3)^(3/4) (-2+2 x^3+x^4))/(x^8 (1-x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(x^2 (-4+x^3))/((-1+x^3)^(3/4) (-1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(x^2 (-4+x^3))/((-1+x^3)^(3/4) (1-x^3+x^4)),x]*)


(* ::Input:: *)
(*int[1/(x^7 (-b+a x^3)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x^4 (-b+a x^3)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x (-b+a x^3)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-4+x^3) (-1+x^3)^(3/4))/(x^4 (1-x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^3)^(3/4)/x^4,x]*)


(* ::Input:: *)
(*int[(-b+a x^3)^(3/4)/x,x]*)


(* ::Input:: *)
(*int[((-4+x^3) (-1+x^3)^(3/4) (1-x^3+x^4))/x^12,x]*)


(* ::Input:: *)
(*int[((-4+x^3) (-1+x^3+x^4)^2)/(x^10 (-1+x^3)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-4+x^3) (-1+x^3+x^4))/(x^6 (-1+x^3)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-4+x^3) (-1+x^3)^(3/4))/x^8,x]*)


(* ::Input:: *)
(*int[(-4+x^3)/(x^2 (-1+x^3)^(3/4)),x]*)


(* ::Input:: *)
(*int[x^5 (-1+x^3)^(3/4),x]*)


(* ::Input:: *)
(*int[x^2 (-1+x^3)^(3/4),x]*)


(* ::Input:: *)
(*int[x^2/((b+a x^2)^(3/4) (2 b+a x^2)),x]*)


(* ::Input:: *)
(*int[(-x+x^2)/((1+x^2)^(3/4) (1+x+x^2)),x]*)


(* ::Input:: *)
(*int[((1+x^2)^(3/4) (7+5 x^2))/(2+x^2),x]*)


(* ::Input:: *)
(*int[((1+x^2)^(3/4) (-2-3 x+5 x^2))/x^2,x]*)


(* ::Input:: *)
(*int[x^2/((b+a x^2)^(3/4) (2 b+a x^2)),x]*)


(* ::Input:: *)
(*int[(2+x+x^2)/(x^2 (1+x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-3 b+2 a x^2) (b^2+a^2 x^2)^(3/4))/x,x]*)


(* ::Input:: *)
(*int[(2 b+a x^2)/(x (b^2+a^2 x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[(b+a x^2)^(3/4)/x^3,x]*)


(* ::Input:: *)
(*int[(b+a x^2)^(3/4)/x,x]*)


(* ::Input:: *)
(*int[1/(x^3 (b+a x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x^3 (1+x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x (b+a x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x (1+x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[x^3 (1+x^2)^(3/4),x]*)


(* ::Input:: *)
(*int[x (1+x^2)^(3/4),x]*)


(* ::Input:: *)
(*int[((-1+x^2)^(3/4) (-8+6 x+5 x^2))/((-1+x) x),x]*)


(* ::Input:: *)
(*int[(1-x+x^2)/(x (1+x) (-1+x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[(-2+x+x^2)/(x^2 (-1+x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[((-b+a x^2)^(3/4) (3 b+2 a x^2))/x,x]*)


(* ::Input:: *)
(*int[(x^2 (-2 b+a x^2))/((-b+a x^2)^(3/4) (4 b-4 a x^2+x^4)),x]*)


(* ::Input:: *)
(*int[x^2/((-2 b+a x^2) (-b+a x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[x^2/((-2+x^2) (-1+x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)^(3/4)/x^3,x]*)


(* ::Input:: *)
(*int[(-b+a x^2)^(3/4)/x,x]*)


(* ::Input:: *)
(*int[1/(x^3 (-b+a x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x^3 (-1+x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[1/(x (-b+a x^2)^(3/4)),x]*)


(* ::Input:: *)
(*int[x^3/(-1+x^2)^(3/4),x]*)


(* ::Input:: *)
(*int[x/(-1+x^2)^(3/4),x]*)


(* ::Input:: *)
(*int[x^3 (-1+x^2)^(3/4),x]*)


(* ::Input:: *)
(*int[x (-1+x^2)^(3/4),x]*)


(* ::Input:: *)
(*int[(x (a b+(-4 a+b) x+2 x^2) (-b^5+5 b^4 x-10 b^3 x^2+10 b^2 x^3-5 b x^4+x^5))/((x (-a+x)^2 (-b+x)^3)^(3/4) (-a^2 d+(-b^3+2 a d) x+(3 b^2-d) x^2-3 b x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(x (-b+x) (a b-3 b x+2 x^2) (-a^3+3 a^2 x-3 a x^2+x^3))/((x (-a+x)^2 (-b+x)^3)^(3/4) (b d+(a^2-d) x-2 a x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((b^2-2 b x+x^2) (-a^2 b+4 a b x-(2 a+3 b) x^2+2 x^3))/((x (-a+x)^2 (-b+x)^3)^(3/4) (b d+(a^2-d) x-2 a x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-3 a b+2 b x+x^2) (-b^5+5 b^4 x-10 b^3 x^2+10 b^2 x^3-5 b x^4+x^5))/(x (x (-a+x) (-b+x)^3)^(3/4) (a b^3-b^2 (3 a+b) x+3 b (a+b) x^2-(a+3 b+d) x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(x^2 (-3 a b^3+2 b^2 (3 a+b) x-3 b (a+b) x^2+x^4))/((x (-a+x) (-b+x)^3)^(3/4) (a b^3-b^2 (3 a+b) x+3 b (a+b) x^2-(a+3 b+d) x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((a b-2 b x+x^2) (b^2-2 b x+x^2))/((x (-a+x) (-b+x)^3)^(3/4) (b d-(a+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(x (-a+x) (-b+x) (a b-2 b x+x^2))/((x (-a+x) (-b+x)^3)^(3/4) (b d-(a+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(x (-a b+(3 a-2 b) x) (-b^3+3 b^2 x-3 b x^2+x^3))/((-a+x) (x (-a+x) (-b+x)^2)^(3/4) (-a^3 d+(-b^2+3 a^2 d) x+(2 b-3 a d) x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[((a^2-2 a x+x^2) (a b^2-2 (2 a-b) b x+(3 a-2 b) x^2))/((x (-a+x) (-b+x)^2)^(3/4) (a^3 d+(b^2-3 a^2 d) x+(-2 b+3 a d) x^2+(1-d) x^3)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-3 a b+(a+2 b) x) (-b^3+3 b^2 x-3 b x^2+x^3))/(x (x (-a+x) (-b+x)^2)^(3/4) (a b^2-b (2 a+b) x+(a+2 b) x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[(x^2 (3 a b^2-2 b (2 a+b) x+(a+2 b) x^2))/((x (-a+x) (-b+x)^2)^(3/4) (a b^2-b (2 a+b) x+(a+2 b) x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[(a b^3-b^2 (a+2 b) x-(a-4 b) b x^2+(a-2 b) x^3)/((x (-a+x) (-b+x)^2)^(3/4) (b^2 d+(a-2 b d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(x (-a+x) (a b+(a-2 b) x))/((x (-a+x) (-b+x)^2)^(3/4) (b^2 d+(a-2 b d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((-1+x) (1-k^2 x^2) (-2-(-1+k) (1+k) x+2 k^2 x^2))/((1+x) ((1-x^2) (1-k^2 x^2))^(3/4) (1-d-(1+3 d) x-(3 d+k^2) x^2+(-d+k^2) x^3)),x]*)


(* ::Input:: *)
(*int[((1+k x) (-1+x^2) (2 k+(-1+k) (1+k) x-2 k x^2))/((-1+k x) ((1-x^2) (1-k^2 x^2))^(3/4) (1-d+(1+3 d) k x-(1+3 d k^2) x^2+k (-1+d k^2) x^3)),x]*)


(* ::Input:: *)
(*int[((-1+k x) (-1+x^2) (-2 k+(-1+k) (1+k) x+2 k x^2))/((1+k x) ((1-x^2) (1-k^2 x^2))^(3/4) (-1+d+(1+3 d) k x+(1+3 d k^2) x^2+k (-1+d k^2) x^3)),x]*)


(* ::Input:: *)
(*int[(((1-3 k^2) x+2 k^2 x^3) (1-2 k^2 x^2+k^4 x^4))/(((1-x^2) (1-k^2 x^2))^(3/4) (1-d+(-1+3 d k^2) x^2-3 d k^4 x^4+d k^6 x^6)),x]*)


(* ::Input:: *)
(*int[((1-x^2) ((1-3 k^2) x+2 k^2 x^3))/((1-k^2 x^2) ((1-x^2) (1-k^2 x^2))^(3/4) (1-d+(-1+3 d k^2) x^2-3 d k^4 x^4+d k^6 x^6)),x]*)


(* ::Input:: *)
(*int[((-2 k-(-1+k) (1+k) x+2 k x^2) (1-2 k x+k^2 x^2))/(((1-x^2) (1-k^2 x^2))^(3/4) (1-d+(1+3 d) k x-(1+3 d k^2) x^2+k (-1+d k^2) x^3)),x]*)


(* ::Input:: *)
(*int[((1+x) (1-k^2 x^2) (-2+(-1+k) (1+k) x+2 k^2 x^2))/((-1+x) ((1-x^2) (1-k^2 x^2))^(3/4) (-1+d-(1+3 d) x+(3 d+k^2) x^2+(-d+k^2) x^3)),x]*)


(* ::Input:: *)
(*int[((-2 k+(-1+k) (1+k) x+2 k x^2) (1+2 k x+k^2 x^2))/(((1-x^2) (1-k^2 x^2))^(3/4) (-1+d+(1+3 d) k x+(1+3 d k^2) x^2+k (-1+d k^2) x^3)),x]*)


(* ::Input:: *)
(*int[((1+2 x+x^2) (-2-(-1+k) (1+k) x+2 k^2 x^2))/(((1-x^2) (1-k^2 x^2))^(3/4) (1-d-(1+3 d) x-(3 d+k^2) x^2+(-d+k^2) x^3)),x]*)


(* ::Input:: *)
(*int[((-1+k^2 x^2) ((-3+k^2) x+2 k^2 x^3))/((-1+x^2) ((1-x^2) (1-k^2 x^2))^(3/4) (1-d+(3 d-k^2) x^2-3 d x^4+d x^6)),x]*)


(* ::Input:: *)
(*int[((1-2 x+x^2) (-2+(-1+k) (1+k) x+2 k^2 x^2))/(((1-x^2) (1-k^2 x^2))^(3/4) (-1+d-(1+3 d) x+(3 d+k^2) x^2+(-d+k^2) x^3)),x]*)


(* ::Input:: *)
(*int[(((-3+k^2) x+2 k^2 x^3) (1-2 x^2+x^4))/(((1-x^2) (1-k^2 x^2))^(3/4) (1-d+(3 d-k^2) x^2-3 d x^4+d x^6)),x]*)


(* ::Input:: *)
(*int[((a^2-2 a x+x^2) (-2 a b x+(3 a-b) x^2))/((x^2 (-a+x) (-b+x))^(3/4) (a^3 d-3 a^2 d x+(-b+3 a d) x^2+(1-d) x^3)),x]*)


(* ::Input:: *)
(*int[(x^3 (-b+x) (-2 a b+(3 a-b) x))/((-a+x) (x^2 (-a+x) (-b+x))^(3/4) (-a^3 d+3 a^2 d x+(b-3 a d) x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x) (-2 a b+(a+b) x))/((x^2 (-a+x) (-b+x))^(3/4) (-a b+(a+b) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(-2 a b x^2+(a+b) x^3)/((x^2 (-a+x) (-b+x))^(3/4) (-a b+(a+b) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-c+x) (-a b-a c+3 b c+2 (a-b-c) x+x^2))/((-a+x) ((-a+x) (-b+x) (-c+x))^(3/4) (-b c-a^3 d+(b+c+3 a^2 d) x-(1+3 a d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[((a^2-2 a x+x^2) (-a b-a c+3 b c+2 (a-b-c) x+x^2))/(((-a+x) (-b+x) (-c+x))^(3/4) (-b c-a^3 d+(b+c+3 a^2 d) x-(1+3 a d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(x (-b+x) (-a b+2 (a-b) x+x^2))/((-a+x) (x (-a+x) (-b+x))^(3/4) (-a^3 d+(b+3 a^2 d) x-(1+3 a d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[((a^2-2 a x+x^2) (-a b+2 (a-b) x+x^2))/((x (-a+x) (-b+x))^(3/4) (-a^3 d+(b+3 a^2 d) x-(1+3 a d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x) (3 a b-2 (a+b) x+x^2))/(x (x (-a+x) (-b+x))^(3/4) (-a b+(a+b) x-x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(x^2 (3 a b-2 (a+b) x+x^2))/((x (-a+x) (-b+x))^(3/4) (-a b+(a+b) x-x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[((-6 a+b+5 x) (-b^5+5 b^4 x-10 b^3 x^2+10 b^2 x^3-5 b x^4+x^5))/(((-a+x) (-b+x)^2)^(3/4) (a+b^6 d-(1+6 b^5 d) x+15 b^4 d x^2-20 b^3 d x^3+15 b^2 d x^4-6 b d x^5+d x^6)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-6 a+b+5 x))/((-b+x) ((-a+x) (-b+x)^2)^(3/4) (a+b^6 d-(1+6 b^5 d) x+15 b^4 d x^2-20 b^3 d x^3+15 b^2 d x^4-6 b d x^5+d x^6)),x]*)


(* ::Input:: *)
(*int[((2 a-3 b+x) (-b^3+3 b^2 x-3 b x^2+x^3))/((-a+x) ((-a+x) (-b+x)^2)^(3/4) (-b^2-a^3 d+(2 b+3 a^2 d) x-(1+3 a d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[((a^2-2 a x+x^2) (-((2 a-3 b) b)+2 (a-2 b) x+x^2))/(((-a+x) (-b+x)^2)^(3/4) (-b^2-a^3 d+(2 b+3 a^2 d) x-(1+3 a d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(-((2 a-b) b^2)+(4 a-b) b x-(2 a+b) x^2+x^3)/(((-a+x) (-b+x)^2)^(3/4) (a+b^2 d-(1+2 b d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-2 a+b+x))/(((-a+x) (-b+x)^2)^(3/4) (a+b^2 d-(1+2 b d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[((-1-2 (-1+k) x+k x^2) (1-2 k x+k^2 x^2))/(((1-x) x (1-k x))^(3/4) (-d+(1+3 d k) x-(1+3 d k^2) x^2+d k^3 x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x) x (-1-2 (-1+k) x+k x^2))/(((1-x) x (1-k x))^(3/4) (-1+k x) (-d+(1+3 d k) x-(1+3 d k^2) x^2+d k^3 x^3)),x]*)


(* ::Input:: *)
(*int[(x (-1+k x) (-1+2 (-1+k) x+k x^2))/((-1+x) ((1-x) x (1-k x))^(3/4) (-d+(1+3 d) x-(3 d+k) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[((1-2 x+x^2) (-1+2 (-1+k) x+k x^2))/(((1-x) x (1-k x))^(3/4) (-d+(1+3 d) x-(3 d+k) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x) (-1+k x) (3-2 (1+k) x+k x^2))/(x ((1-x) x (1-k x))^(3/4) (-1+(1+k) x-k x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(x^2 (3-2 (1+k) x+k x^2))/(((1-x) x (1-k x))^(3/4) (-1+(1+k) x-k x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^6) (-1+x^3+x^6)^(2/3))/(1-x^6+x^12),x]*)


(* ::Input:: *)
(*int[((-3+x^2) (1-x^2+x^3)^(2/3))/(1-2 x^2-x^3+x^4+x^5+x^6),x]*)


(* ::Input:: *)
(*int[((3+x^2) (1+x^2+x^3)^(2/3))/(-1-2 x^2+x^3-x^4+x^5+x^6),x]*)


(* ::Input:: *)
(*int[((3+2 x) (1+x+x^3)^(2/3))/(1+2 x+x^2+x^3+x^4+x^6),x]*)


(* ::Input:: *)
(*int[((-1+x+x^3+x^6)^(2/3) (3-2 x+3 x^6))/((-1+x+x^6) (-1+x-x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-3+4 x) (-1+2 x+x^3)^(2/3))/(x^3 (2-4 x+x^3)),x]*)


(* ::Input:: *)
(*int[((3+2 x^2) (1+2 x^2+2 x^3)^(2/3))/(x^3 (-1-2 x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-3+2 x) (1-x+x^3)^(2/3))/(x^3 (-2+2 x+x^3)),x]*)


(* ::Input:: *)
(*int[((3+2 x) (1+x+3 x^3)^(2/3))/(x^3 (1+x+x^3)),x]*)


(* ::Input:: *)
(*int[((-6+x^2) (2-x^2+x^3)^(2/3))/(x^3 (-2+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x^3) (1+3 x^3)^(2/3))/(x^6 (1+x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^3) (-1+2 x^3)^(2/3))/(x^6 (1+2 x^3)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1-x^3+x^4)^(2/3))/(x^3 (1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-3+x) (-2+x) (2-x+2 x^3)^(2/3))/(x^6 (-2+x+2 x^3)),x]*)


(* ::Input:: *)
(*int[((-3+2 x) (1-x+x^3)^(2/3))/(1-2 x+x^2+2 x^3-2 x^4+2 x^6),x]*)


(* ::Input:: *)
(*int[((2+x^3) (1+2 x^3)^(2/3))/(x^6 (-1+x^3)),x]*)


(* ::Input:: *)
(*int[(-3-x^4+3 x^6)/((1-x^4+x^6) (1-x^3-x^4+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1-x^3+x^4) (1+x^3+x^4)^(2/3))/(x^6 (1+x^4)),x]*)


(* ::Input:: *)
(*int[((-3+2 x^4) (1+2 x^4)^(2/3))/(x^3 (2-x^3+4 x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^6) (1+x^3+x^6)^(2/3))/(1+x^6+x^12),x]*)


(* ::Input:: *)
(*int[((-3+x^5) (2+x^3+x^5)^(2/3))/(x^3 (2+x^5)),x]*)


(* ::Input:: *)
(*int[((1+x^6) (-1-x^3+x^6)^(2/3))/(x^3 (-1+x^6)),x]*)


(* ::Input:: *)
(*int[((3+x^4) (-1-x^3+x^4)^(2/3))/(x^3 (-1+x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^6) (1+2 x^3+x^6)^(2/3))/(1+x^3+x^6)^2,x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(2/3) (1+x^6) (-1-x^3+x^6))/(x^6 (-1+x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^3+x^8)^(2/3) (-3+5 x^8))/(x^3 (1+x^8)),x]*)


(* ::Input:: *)
(*int[((2+x^3) (1+2 x^3)^(2/3))/(x^6 (1+x^3)),x]*)


(* ::Input:: *)
(*int[((-2+x^3)^(2/3) (4+x^3))/(x^6 (-1+x^3)),x]*)


(* ::Input:: *)
(*int[((1-x^3)^(2/3) (-1+x^3))/(x^6 (-1+2 x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x^7)^(2/3) (3+4 x^7))/(x^3 (-1+x^3+x^7)),x]*)


(* ::Input:: *)
(*int[x/((-1+x^3) (-1+2 x^3)^(2/3)),x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(2/3) (1+x^6) (-2+x^3+2 x^6))/(x^6 (-1-x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(2/3) (1+x^6))/(x^3 (-1-x^3+x^6)),x]*)


(* ::Input:: *)
(*int[(-1+x^6)^(2/3)/x^7,x]*)


(* ::Input:: *)
(*int[(-1+x^6)^(2/3)/x,x]*)


(* ::Input:: *)
(*int[x^7 (-1+x^6)^(2/3),x]*)


(* ::Input:: *)
(*int[x (-1+x^6)^(2/3),x]*)


(* ::Input:: *)
(*int[((-1+x^3) (1+x^6)^(2/3) (1-x^3+x^6))/(x^6 (1+x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x^6) (1+x^6)^(2/3))/(x^3 (2-x^3+2 x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3) (1+x^3)^3 (1+x^6)^(2/3))/(x^6 (1-x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^6) (1+x^6)^(2/3))/(x^3 (1-x^3+x^6)),x]*)


(* ::Input:: *)
(*int[(1+x^6)^(2/3)/x^7,x]*)


(* ::Input:: *)
(*int[(1+x^6)^(2/3)/x,x]*)


(* ::Input:: *)
(*int[x^7 (1+x^6)^(2/3),x]*)


(* ::Input:: *)
(*int[x (1+x^6)^(2/3),x]*)


(* ::Input:: *)
(*int[((1+x^5)^(2/3) (-3+2 x^5) (2+x^3+2 x^5))/(x^6 (2-x^3+2 x^5)),x]*)


(* ::Input:: *)
(*int[((1+x^5)^(2/3) (-3+2 x^5))/(x^3 (1-x^3+x^5)),x]*)


(* ::Input:: *)
(*int[(1+x^5)^(2/3)/x^6,x]*)


(* ::Input:: *)
(*int[(1+x^5)^(2/3)/x,x]*)


(* ::Input:: *)
(*int[((1+x^5)^(2/3) (-3+2 x^5) (4+3 x^3+4 x^5))/x^9,x]*)


(* ::Input:: *)
(*int[((1+x^5)^(2/3) (1+x^3+x^5) (-3+2 x^5))/x^9,x]*)


(* ::Input:: *)
(*int[((1+x^5)^(2/3) (6+x^5))/x^11,x]*)


(* ::Input:: *)
(*int[x^4 (1+x^5)^(2/3),x]*)


(* ::Input:: *)
(*int[((-1+x^5)^(2/3) (3+2 x^5) (-2+x^3+2 x^5))/(x^6 (-1+x^3+x^5)),x]*)


(* ::Input:: *)
(*int[((-1+x^5)^(2/3) (3+2 x^5))/(x^3 (-1-x^3+x^5)),x]*)


(* ::Input:: *)
(*int[(-1+x^5)^(2/3)/x^6,x]*)


(* ::Input:: *)
(*int[(-1+x^5)^(2/3)/x,x]*)


(* ::Input:: *)
(*int[((-1+x^5)^(2/3) (-1+x^3+x^5) (3+2 x^5))/x^9,x]*)


(* ::Input:: *)
(*int[((-6+x^5) (-1+x^5)^(2/3))/x^11,x]*)


(* ::Input:: *)
(*int[x^4 (-1+x^5)^(2/3),x]*)


(* ::Input:: *)
(*int[((b+a x^3) (9 a q x^2-4 b p x^3+5 a p x^6))/((q+p x^4)^(2/3) (b^3 c+d q+3 a b^2 c x^3+d p x^4+3 a^2 b c x^6+a^3 c x^9)),x]*)


(* ::Input:: *)
(*int[(2 (b+a x^2) (3 a q x-2 b p x^3+a p x^5))/(3 (q+p x^4)^(2/3) (b^3 c+d q+3 a b^2 c x^2+(3 a^2 b c+d p) x^4+a^3 c x^6)),x]*)


(* ::Input:: *)
(*int[((b+a x) (-3 a q+4 b p x^3+a p x^4))/((q+p x^4)^(2/3) (b^3 c+d q+3 a b^2 c x+3 a^2 b c x^2+a^3 c x^3+d p x^4)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4)^(2/3) (2+x^3+2 x^4))/(x^6 (4-x^3+4 x^4)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4)^(2/3))/(x^3 (2+x^3+2 x^4)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4)^(2/3) (1+2 x^3+x^4))/(x^6 (1-x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4)^(2/3) (1+x^3+x^4))/(x^6 (1-x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4)^(2/3))/(x^3 (1-x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(1+x^4)^(2/3)/x^5,x]*)


(* ::Input:: *)
(*int[(1+x^4)^(2/3)/x,x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4)^(2/3) (1+x^3+x^4))/x^9,x]*)


(* ::Input:: *)
(*int[x^7 (1+x^4)^(2/3),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4)^(2/3))/x^6,x]*)


(* ::Input:: *)
(*int[x^3 (1+x^4)^(2/3),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(2/3) (3+x^4) (-2-x^3+2 x^4))/(x^6 (-2+3 x^3+2 x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(2/3) (3+x^4) (-1-x^3+x^4))/(x^6 (-2-x^3+2 x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(2/3) (3+x^4) (-1+x^3+x^4))/(x^6 (-1-x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(2/3) (3+x^4))/(x^3 (-1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)^(2/3)/x,x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(2/3) (3+x^4) (-2-x^3+2 x^4))/x^9,x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(2/3) (3+x^4))/x^6,x]*)


(* ::Input:: *)
(*int[x^3 (-1+x^4)^(2/3),x]*)


(* ::Input:: *)
(*int[((c+b x+a x^3) (b q-c p x^2+3 a q x^2+2 a p x^5))/((q+p x^3)^(2/3) (d (c+b x+a x^3)^3+e (q+p x^3))),x]*)


(* ::Input:: *)
(*int[(x^2 (b+a x^3) (-b p+3 a q+2 a p x^3))/((q+p x^3)^(2/3) (b^3 c+d q+(3 a b^2 c+d p) x^3+3 a^2 b c x^6+a^3 c x^9)),x]*)


(* ::Input:: *)
(*int[((c+b x+a x^2) (b q+2 a q x-c p x^2+a p x^4))/((q+p x^3)^(2/3) (d (c+b x+a x^2)^3+e (q+p x^3))),x]*)


(* ::Input:: *)
(*int[((b+a x^2) (2 a q x-b p x^2+a p x^4))/((q+p x^3)^(2/3) (b^3 c+d q+3 a b^2 c x^2+d p x^3+3 a^2 b c x^4+a^3 c x^6)),x]*)


(* ::Input:: *)
(*int[((b+a x) (b c q-a d q+b d p x^2+2 b c p x^3+a c p x^4))/((q+p x^3)^(2/3) (f (b+a x)^3+e (d+c x)^3 (q+p x^3))),x]*)


(* ::Input:: *)
(*int[((b+a x) (-a q+b p x^2))/((q+p x^3)^(2/3) (b^3 c+d q+3 a b^2 c x+3 a^2 b c x^2+(a^3 c+d p) x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (4+6 x^3+3 x^6))/(x^6 (8+6 x^3+3 x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (2+x^3) (4+3 x^3))/(x^6 (4+2 x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (4+x^3))/(x^6 (8-4 x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (1-2 x^3+x^6))/(x^6 (-2+x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (1+x^3+2 x^6))/(x^6 (-1+2 x^6)),x]*)


(* ::Input:: *)
(*int[((-2+x^3) (1+x^3)^(2/3))/(x^3 (-2+x^3+2 x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3) (1+x^3)^(2/3))/(x^3 (-1-x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (2+x^3+2 x^6))/(x^6 (1+x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-2+x^3) (1+x^3)^(2/3))/(x^3 (2+x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (-1+x^6))/(x^6 (-1-2 x^3+2 x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (1-2 x^3+2 x^6))/(x^6 (-1-x^3+2 x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (8-4 x^3+x^6))/(x^6 (2+x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (-1+2 x^6))/(x^6 (-1+2 x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (2+x^3+x^6))/(x^6 (-2+x^3)^2),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (2+x^3))/(x^6 (-2-x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (2+x^3))/(x^6 (4+x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (2+x^6))/(x^6 (-1+x^3)^2),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (-1-2 x^3+2 x^6))/(x^9 (-1+x^3)),x]*)


(* ::Input:: *)
(*int[((-2+x^3) (1+x^3)^(2/3))/(x^6 (-1+2 x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (-1-2 x^3+2 x^6))/(x^6 (-1+x^3+2 x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (1+2 x^6))/x^6,x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (-2+x^3+x^6))/x^9,x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (-1+3 x^6))/(x^9 (1+2 x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (2+x^3))/(x^6 (1+2 x^3)),x]*)


(* ::Input:: *)
(*int[x^6 (1+x^3)^(2/3),x]*)


(* ::Input:: *)
(*int[x^3 (1+x^3)^(2/3),x]*)


(* ::Input:: *)
(*int[(1+x^3)^(2/3),x]*)


(* ::Input:: *)
(*int[(1+x^3)^(2/3)/x^4,x]*)


(* ::Input:: *)
(*int[(1+x^3)^(2/3)/x^3,x]*)


(* ::Input:: *)
(*int[(1+x^3)^(2/3)/x,x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (1-x^3+2 x^6))/x^12,x]*)


(* ::Input:: *)
(*int[((1+x^3)^(2/3) (2+x^3))/x^9,x]*)


(* ::Input:: *)
(*int[x^5 (1+x^3)^(2/3),x]*)


(* ::Input:: *)
(*int[x^2 (1+x^3)^(2/3),x]*)


(* ::Input:: *)
(*int[(1+x^3)^(2/3)/x^6,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (1-x^3+x^6))/(x^6 (-2-x^3+2 x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (4+x^6))/(x^6 (4+2 x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (2+x^3))/(x^6 (4+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (-1+x^6))/(x^6 (-2+x^3+2 x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (2+x^3))/(x^3 (-4+x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (1-2 x^3+x^6))/(x^6 (1-x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (2+x^3))/(x^6 (2+x^3+2 x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (2-2 x^3+x^6))/(x^6 (-4+4 x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (4-2 x^3+x^6))/(x^6 (-8+4 x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (2+x^3))/(x^6 (-4-2 x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (4+x^6))/(x^6 (-4+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (8-8 x^3+x^6))/(x^6 (-4+x^3) (-2+x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (4+x^3))/(x^6 (-2-x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (8+2 x^3+x^6))/(x^6 (-2+x^3)),x]*)


(* ::Input:: *)
(*int[((-2+x^3) (-1+x^3)^(2/3))/(x^3 (-1+2 x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (1+x^3))/(x^6 (2+x^3)),x]*)


(* ::Input:: *)
(*int[((-4+x^3) (-2+x^3) (-1+x^3)^(2/3))/(x^6 (-2+x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (-2+2 x^3+x^6))/(x^6 (1+x^3)^2),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (4+x^3+x^6))/(x^9 (-2+x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (4+4 x^3+x^6))/(x^9 (1+x^3)),x]*)


(* ::Input:: *)
(*int[((-4+x^3) (-2+x^3) (-1+x^3)^(2/3))/(x^9 (-2+3 x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (1+x^3))/(x^6 (-2+x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (1+x^3+x^6))/(x^6 (-1+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (1-5 x^3+4 x^6))/(x^6 (-1+2 x^3)^2),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (-2+2 x^3+x^6))/x^9,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (-2+x^3+x^6))/x^6,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (2+x^6))/x^6,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (1+x^3))/x^3,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (-1+3 x^3))/(x^6 (-1+2 x^3)),x]*)


(* ::Input:: *)
(*int[x^6 (-1+x^3)^(2/3),x]*)


(* ::Input:: *)
(*int[x^3 (-1+x^3)^(2/3),x]*)


(* ::Input:: *)
(*int[(-1+x^3)^(2/3),x]*)


(* ::Input:: *)
(*int[(-1+x^3)^(2/3)/x^4,x]*)


(* ::Input:: *)
(*int[(-1+x^3)^(2/3)/x^3,x]*)


(* ::Input:: *)
(*int[(-1+x^3)^(2/3)/x,x]*)


(* ::Input:: *)
(*int[x/(-b+a x^3)^(2/3),x]*)


(* ::Input:: *)
(*int[((-4+x^3) (-1+x^3)^(2/3))/x^12,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(2/3) (2+x^3))/x^9,x]*)


(* ::Input:: *)
(*int[(-1+x^3)^(2/3)/x^6,x]*)


(* ::Input:: *)
(*int[(-3+x^2)/((1+x^2)^(2/3) (9+x^2)),x]*)


(* ::Input:: *)
(*int[(3+x^2)/((-3+x^2) (1+x^2)^(2/3)),x]*)


(* ::Input:: *)
(*int[((1+x^2)^(2/3) (-3+4 x+7 x^2))/x^2,x]*)


(* ::Input:: *)
(*int[(1+2 x^2)/(x (1+x^2)^(2/3)),x]*)


(* ::Input:: *)
(*int[(1+x^2)^(2/3)/x^3,x]*)


(* ::Input:: *)
(*int[(1+x^2)^(2/3)/x,x]*)


(* ::Input:: *)
(*int[1/(x (1+x^2)^(2/3)),x]*)


(* ::Input:: *)
(*int[1/(x^3 (1+x^2)^(2/3)),x]*)


(* ::Input:: *)
(*int[((1+x^2)^(2/3) (-3+7 x^2))/x^2,x]*)


(* ::Input:: *)
(*int[x^3/(1+x^2)^(2/3),x]*)


(* ::Input:: *)
(*int[((-1+x) (3+x))/((-1+x^2)^(2/3) (2-x+x^2)),x]*)


(* ::Input:: *)
(*int[(x (-3+x^2))/((-1+x^2)^(2/3) (1-x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-1+x^2)^(2/3)/x,x]*)


(* ::Input:: *)
(*int[(-1+x^2)^(2/3)/x^3,x]*)


(* ::Input:: *)
(*int[x^3 (-1+x^2)^(2/3),x]*)


(* ::Input:: *)
(*int[x (-1+x^2)^(2/3),x]*)


(* ::Input:: *)
(*int[(3 k+2 (1+k^2) x-k (1+k^2) x^2-4 k^2 x^3-k^3 x^4)/(((1-x^2) (1-k^2 x^2))^(2/3) (1-d-(1+2 d) k x-(1+d k^2) x^2+k x^3)),x]*)


(* ::Input:: *)
(*int[((1-k^2 x^2) ((2-k^2) x-2 x^3+k^2 x^5))/(((1-x^2) (1-k^2 x^2))^(2/3) (1-d+2 (-2+d k^2) x^2+(6-d k^4) x^4-4 x^6+x^8)),x]*)


(* ::Input:: *)
(*int[(-3 k-2 (1+k^2) x+k (1+k^2) x^2+4 k^2 x^3+k^3 x^4)/(((1-x^2) (1-k^2 x^2))^(2/3) (-1+d-(2+d) k x-(d+k^2) x^2+d k x^3)),x]*)


(* ::Input:: *)
(*int[(-3 k+2 (1+k^2) x+k (1+k^2) x^2-4 k^2 x^3+k^3 x^4)/(((1-x^2) (1-k^2 x^2))^(2/3) (-1+d-(1+2 d) k x+(1+d k^2) x^2+k x^3)),x]*)


(* ::Input:: *)
(*int[(-3+2 (1+k^2) x+(1+k^2) x^2-4 k^2 x^3+k^2 x^4)/(((1-x^2) (1-k^2 x^2))^(2/3) (1-d-(2+d) x+(1+d k^2) x^2+d k^2 x^3)),x]*)


(* ::Input:: *)
(*int[(-3 k+2 (1+k^2) x+k (1+k^2) x^2-4 k^2 x^3+k^3 x^4)/(((1-x^2) (1-k^2 x^2))^(2/3) (1-d-(2+d) k x+(d+k^2) x^2+d k x^3)),x]*)


(* ::Input:: *)
(*int[(-3-2 (1+k^2) x+(1+k^2) x^2+4 k^2 x^3+k^2 x^4)/(((1-x^2) (1-k^2 x^2))^(2/3) (-1+d-(2+d) x-(1+d k^2) x^2+d k^2 x^3)),x]*)


(* ::Input:: *)
(*int[(((-2+k^2) x+k^2 x^3) (1+d-(2 d+k^2) x^2+d x^4))/((-1+x^2) ((1-x^2) (1-k^2 x^2))^(2/3) (-1+e+(-2 e+k^2) x^2+e x^4)),x]*)


(* ::Input:: *)
(*int[(-3+2 (1+k^2) x+(1+k^2) x^2-4 k^2 x^3+k^2 x^4)/(((1-x^2) (1-k^2 x^2))^(2/3) (-1+d+(-1-2 d) x+(d+k^2) x^2+k^2 x^3)),x]*)


(* ::Input:: *)
(*int[((-1+2 k^2) x-2 k^4 x^3+k^4 x^5)/(((1-x^2) (1-k^2 x^2))^(2/3) (-1+d+(1-2 d k^2) x^2+d k^4 x^4)),x]*)


(* ::Input:: *)
(*int[((-1+2 k^2) x-2 k^4 x^3+k^4 x^5)/(((1-x^2) (1-k^2 x^2))^(2/3) (1-d+(d-2 k^2) x^2+k^4 x^4)),x]*)


(* ::Input:: *)
(*int[((2-k^2) x-2 x^3+k^2 x^5)/(((1-x^2) (1-k^2 x^2))^(2/3) (-1+d+(-2 d+k^2) x^2+d x^4)),x]*)


(* ::Input:: *)
(*int[((2-k^2) x-2 x^3+k^2 x^5)/(((1-x^2) (1-k^2 x^2))^(2/3) (1-d+(-2+d k^2) x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-a b-a c+2 b c+(2 a-b-c) x) (b c+a^2 d-(b+c+2 a d) x+(1+d) x^2))/((a-x) ((-a+x) (-b+x) (-c+x))^(2/3) (-b c+a^2 e+(b+c-2 a e) x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[(a (a b+a c-2 b c)-2 (a^2-b c) x+(2 a-b-c) x^2)/(((-a+x) (-b+x) (-c+x))^(2/3) (a^2-b c d+(-2 a+b d+c d) x+(1-d) x^2)),x]*)


(* ::Input:: *)
(*int[(-a (a b+a c-2 b c)+2 (a^2-b c) x+(-2 a+b+c) x^2)/(((-a+x) (-b+x) (-c+x))^(2/3) (-b c+a^2 d+(b+c-2 a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((-b+x) (a b-2 b x+x^2) (-b-(-1+a d) x+d x^2))/(x (-a+x) (x (-a+x) (-b+x)^2)^(2/3) (b-(1+a e) x+e x^2)),x]*)


(* ::Input:: *)
(*int[(x (-a+x) (-b+x) (a b-2 b x+x^2))/((x (-a+x) (-b+x)^2)^(2/3) (-b^2+2 b x-(1-a^2 d) x^2-2 a d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(2 a b^2 x-b (2 a+b) x^2+x^4)/((x (-a+x) (-b+x)^2)^(2/3) (-a b^2+b (2 a+b) x-(a+2 b+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((a b-2 b x+x^2) (-b d+(-a+d) x+x^2))/((x (-a+x) (-b+x)^2)^(2/3) (b e-(a+e) x+x^2)),x]*)


(* ::Input:: *)
(*int[((-b+x) (a b-2 b x+x^2))/((x (-a+x) (-b+x)^2)^(2/3) (b-(1+a d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[((-b+x) (a b-2 b x+x^2))/((x (-a+x) (-b+x)^2)^(2/3) (b d-(a+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[((2 a b x-3 a x^2+x^3) (a^2 d-2 a d x+(-b+d) x^2+x^3))/((-a+x) (x^2 (-a+x) (-b+x))^(2/3) (-a^2 e+2 a e x-(b+e) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(x (-a b+x^2) (a b d-(-1+a d+b d) x+d x^2))/((-a+x) (-b+x) (x^2 (-a+x) (-b+x))^(2/3) (a b e-(1+a e+b e) x+e x^2)),x]*)


(* ::Input:: *)
(*int[((4 a b-3 (a+b) x+2 x^2) (a b-(a+b) x+x^2+d x^4))/(x (x^2 (-a+x) (-b+x))^(2/3) (-a b+(a+b) x-x^2+e x^4)),x]*)


(* ::Input:: *)
(*int[(-a b x^2+x^4)/((x^2 (-a+x) (-b+x))^(2/3) (a^2 b^2-2 a b (a+b) x+(a^2+4 a b+b^2-d) x^2-2 (a+b) x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(-2 a^2 b x+a (3 a+2 b) x^2-4 a x^3+x^4)/((x^2 (-a+x) (-b+x))^(2/3) (-a^2 d+2 a d x-(b+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-2 a^2 b x+a (3 a+2 b) x^2-4 a x^3+x^4)/((x^2 (-a+x) (-b+x))^(2/3) (-a^2+2 a x-(1+b d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[((-a b+x^2) (a b+(-a-b+d) x+x^2))/((x^2 (-a+x) (-b+x))^(2/3) (a b-(a+b+e) x+x^2)),x]*)


(* ::Input:: *)
(*int[(x^3 (4 a b-3 (a+b) x+2 x^2))/((x^2 (-a+x) (-b+x))^(2/3) (-a b d+(a+b) d x-d x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(x^3 (4 a b-3 (a+b) x+2 x^2))/((x^2 (-a+x) (-b+x))^(2/3) (-a b+(a+b) x-x^2+d x^4)),x]*)


(* ::Input:: *)
(*int[(x (-a b+x^2))/((x^2 (-a+x) (-b+x))^(2/3) (a b d-(1+a d+b d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(x (-a b+x^2))/((x^2 (-a+x) (-b+x))^(2/3) (a b-(a+b+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[((-2 a b+(a+b) x) (a^2 b^2-2 a b (a+b) x+(a^2+4 a b+b^2) x^2-2 (a+b) x^3+(1+d) x^4))/(x^3 (x (-a+x) (-b+x))^(2/3) (-a b+(a+b) x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[((-a^2 b+2 a^2 x+(-2 a+b) x^2) (a^2-(2 a+b d) x+(1+d) x^2))/(x (-b+x) (x (-a+x) (-b+x))^(2/3) (-a^2+(2 a-b e) x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[(x (-b+x) (-a^2 b+2 a^2 x+(-2 a+b) x^2))/((x (-a+x) (-b+x))^(2/3) (-a^4+4 a^3 x+(-6 a^2+b^2 d) x^2+2 (2 a-b d) x^3+(-1+d) x^4)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x) (-2 a b x+(a+b) x^2))/((x (-a+x) (-b+x))^(2/3) (a^2 b^2 d-2 a b (a+b) d x+(a^2+4 a b+b^2) d x^2-2 (a+b) d x^3+(-1+d) x^4)),x]*)


(* ::Input:: *)
(*int[((-2 a b x+(a+b) x^2) (a b d-(a+b) d x+(1+d) x^2))/((-a+x) (-b+x) (x (-a+x) (-b+x))^(2/3) (-a b e+(a+b) e x+(1-e) x^2)),x]*)


(* ::Input:: *)
(*int[(x^3 (-2 a b+(a+b) x))/((x (-a+x) (-b+x))^(2/3) (-a^2 b^2+2 a b (a+b) x-(a^2+4 a b+b^2) x^2+2 (a+b) x^3+(-1+d) x^4)),x]*)


(* ::Input:: *)
(*int[((-a b+(2 a-b) x) (a^2 d-(b+2 a d) x+(1+d) x^2))/((-a+x) (x (-a+x) (-b+x))^(2/3) (a^2 e+(b-2 a e) x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[((-2 a b+(a+b) x) (a b-(a+b) x+(1+d) x^2))/(x (x (-a+x) (-b+x))^(2/3) (-a b+(a+b) x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[(a^2 b-2 a^2 x+(2 a-b) x^2)/((x (-a+x) (-b+x))^(2/3) (a^2+(-2 a+b d) x+(1-d) x^2)),x]*)


(* ::Input:: *)
(*int[(a^2 b-2 a^2 x+(2 a-b) x^2)/((x (-a+x) (-b+x))^(2/3) (a^2 d+(b-2 a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(-2 a b x+(a+b) x^2)/((x (-a+x) (-b+x))^(2/3) (a b d-(a+b) d x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(-2 a b x+(a+b) x^2)/((x (-a+x) (-b+x))^(2/3) (-a b+(a+b) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((1+(a-2 k) x+(-a+k^2) x^2) (-1+2 x+(-2 k+k^2) x^2))/(((1-x) x (1-k x))^(2/3) (1-4 k x+(-b+6 k^2) x^2+2 (b-2 k^3) x^3+(-b+k^4) x^4)),x]*)


(* ::Input:: *)
(*int[((1+(a-2 k) x+(-a+k^2) x^2) (-1+2 x+(-2 k+k^2) x^2))/((1-x) x ((1-x) x (1-k x))^(2/3) (1-(b+2 k) x+(b+k^2) x^2)),x]*)


(* ::Input:: *)
(*int[-(((-1+x) x (-1+2 x+(-2 k+k^2) x^2))/(((1-x) x (1-k x))^(2/3) (1-4 k x+(-b+6 k^2) x^2+(2 b-4 k^3) x^3+(-b+k^4) x^4))),x]*)


(* ::Input:: *)
(*int[(-1+2 x+(-2 k+k^2) x^2)/(((1-x) x (1-k x))^(2/3) (1-(b+2 k) x+(b+k^2) x^2)),x]*)


(* ::Input:: *)
(*int[(x (-1+k x) (1-2 k x+(-1+2 k) x^2))/(((1-x) x (1-k x))^(2/3) (-1+(4-c) x+(-6+b+2 c+c k) x^2+(4-c-2 b k-2 c k) x^3+(-1+c k+b k^2) x^4)),x]*)


(* ::Input:: *)
(*int[((1-2 k x+(-1+2 k) x^2) (-1+(2-a) x+(-1+a k) x^2))/(((1-x) x (1-k x))^(2/3) (-1+4 x+(-6+b) x^2+(4-2 b k) x^3+(-1+b k^2) x^4)),x]*)


(* ::Input:: *)
(*int[((1-2 k x+(-1+2 k) x^2) (-1+(2-a) x+(-1+a k) x^2))/(x ((1-x) x (1-k x))^(2/3) (-1+k x) (1-(2+b) x+(1+b k) x^2)),x]*)


(* ::Input:: *)
(*int[(x (-1+k x) (1-2 k x+(-1+2 k) x^2))/(((1-x) x (1-k x))^(2/3) (-1+4 x+(-6+b) x^2+(4-2 b k) x^3+(-1+b k^2) x^4)),x]*)


(* ::Input:: *)
(*int[(-1+2 k x+(1-2 k) x^2)/(((1-x) x (1-k x))^(2/3) (1-(2+b) x+(1+b k) x^2)),x]*)


(* ::Input:: *)
(*int[(x^5 (8-7 (1+k) x+6 k x^2))/(((1-x) x (1-k x))^(2/3) (-b+b (1+k) x-b k x^2+x^8)),x]*)


(* ::Input:: *)
(*int[(x^3 (5-4 (1+k) x+3 k x^2))/(((1-x) x (1-k x))^(2/3) (-b+b (1+k) x-b k x^2+x^5)),x]*)


(* ::Input:: *)
(*int[((-2 x+(1+k) x^2) (a-a (1+k) x+(1+a k) x^2))/((-1+x) ((1-x) x (1-k x))^(2/3) (-1+k x) (b-b (1+k) x+(-1+b k) x^2)),x]*)


(* ::Input:: *)
(*int[((-1+x) (-1+k x) (-2 x+(1+k) x^2))/(((1-x) x (1-k x))^(2/3) (b-2 b (1+k) x+(b+4 b k+b k^2) x^2-2 b k (1+k) x^3+(-1+b k^2) x^4)),x]*)


(* ::Input:: *)
(*int[(-2 x+(1+k) x^2)/(((1-x) x (1-k x))^(2/3) (b-b (1+k) x+(-1+b k) x^2)),x]*)


(* ::Input:: *)
(*int[((1+(-2+k) x) (a-4 a k x+(1+6 a k^2) x^2-2 (1+2 a k^3) x^3+(1+a k^4) x^4))/(((1-x) x (1-k x))^(2/3) (-1+k x)^3 (b-(1+2 b k) x+(1+b k^2) x^2)),x]*)


(* ::Input:: *)
(*int[((-1+2 x+(-2 k+k^2) x^2) (a+(1-2 a k) x+(-1+a k^2) x^2))/(((1-x) x (1-k x))^(2/3) (b-4 b k x+(-1+6 b k^2) x^2+(2-4 b k^3) x^3+(-1+b k^4) x^4)),x]*)


(* ::Input:: *)
(*int[((1+(-2+k) x) (a+(1-2 a k) x+(-1+a k^2) x^2))/(((1-x) x (1-k x))^(2/3) (-1+k x) (b-(1+2 b k) x+(1+b k^2) x^2)),x]*)


(* ::Input:: *)
(*int[((1+(-2+k) x) (-1+3 k x-3 k^2 x^2+k^3 x^3))/(((1-x) x (1-k x))^(2/3) (b-4 b k x+(-1+6 b k^2) x^2+(2-4 b k^3) x^3+(-1+b k^4) x^4)),x]*)


(* ::Input:: *)
(*int[(-1+2 x+(-2 k+k^2) x^2)/(((1-x) x (1-k x))^(2/3) (b-(1+2 b k) x+(1+b k^2) x^2)),x]*)


(* ::Input:: *)
(*int[((-1+2 (-2+k) x+3 k x^2) (-1+3 x-3 x^2+x^3))/(((1-x) x (1-k x))^(2/3) (-b+(1+5 b) x-(10 b+k) x^2+10 b x^3-5 b x^4+b x^5)),x]*)


(* ::Input:: *)
(*int[((-1+(-1+2 k) x) (a+(1-2 a) x+(a-k) x^2))/((-1+x) ((1-x) x (1-k x))^(2/3) (b-(1+2 b) x+(b+k) x^2)),x]*)


(* ::Input:: *)
(*int[(-1+2 k x+(1-2 k) x^2)/(((1-x) x (1-k x))^(2/3) (b-(1+2 b) x+(b+k) x^2)),x]*)


(* ::Input:: *)
(*int[((8-7 (1+k) x+6 k x^2) (1-(1+k) x+k x^2+a x^8))/(x^3 ((1-x) x (1-k x))^(2/3) (-1+(1+k) x-k x^2+b x^8)),x]*)


(* ::Input:: *)
(*int[(x^5 (8-7 (1+k) x+6 k x^2))/(((1-x) x (1-k x))^(2/3) (-1+(1+k) x-k x^2+b x^8)),x]*)


(* ::Input:: *)
(*int[((5-4 (1+k) x+3 k x^2) (1-2 (1+k) x+(1+4 k+k^2) x^2-2 k (1+k) x^3+k^2 x^4+a x^10))/(x^7 ((1-x) x (1-k x))^(2/3) (-1+(1+k) x-k x^2+b x^5)),x]*)


(* ::Input:: *)
(*int[(x^3 (5-4 (1+k) x+3 k x^2) (1-(1+k) x+k x^2+a x^5))/(((1-x) x (1-k x))^(2/3) (-1+(2+2 k) x-(1+4 k+k^2) x^2+2 k (1+k) x^3-k^2 x^4+b x^10)),x]*)


(* ::Input:: *)
(*int[((5-4 (1+k) x+3 k x^2) (1-(1+k) x+k x^2+a x^5))/(x^2 ((1-x) x (1-k x))^(2/3) (-1+(1+k) x-k x^2+b x^5)),x]*)


(* ::Input:: *)
(*int[(x^8 (5-4 (1+k) x+3 k x^2))/(((1-x) x (1-k x))^(2/3) (-1+(2+2 k) x-(1+4 k+k^2) x^2+(2 k+2 k^2) x^3-k^2 x^4+b x^10)),x]*)


(* ::Input:: *)
(*int[(x^3 (5-4 (1+k) x+3 k x^2))/(((1-x) x (1-k x))^(2/3) (-1+(1+k) x-k x^2+b x^5)),x]*)


(* ::Input:: *)
(*int[((-2+(1+k) x) (1-2 (1+k) x+(1+4 k+k^2) x^2-2 (k+k^2) x^3+(a+k^2) x^4))/(x^3 ((1-x) x (1-k x))^(2/3) (1-(1+k) x+(-b+k) x^2)),x]*)


(* ::Input:: *)
(*int[((-2 x+(1+k) x^2) (1-(1+k) x+(a+k) x^2))/(((1-x) x (1-k x))^(2/3) (1-2 (1+k) x+(1+4 k+k^2) x^2-2 (k+k^2) x^3+(-b+k^2) x^4)),x]*)


(* ::Input:: *)
(*int[((-2+(1+k) x) (1-(1+k) x+(a+k) x^2))/(x ((1-x) x (1-k x))^(2/3) (1-(1+k) x+(-b+k) x^2)),x]*)


(* ::Input:: *)
(*int[(x^3 (-2+(1+k) x))/(((1-x) x (1-k x))^(2/3) (1-(2+2 k) x+(1+4 k+k^2) x^2-(2 k+2 k^2) x^3+(-b+k^2) x^4)),x]*)


(* ::Input:: *)
(*int[(-2 x+(1+k) x^2)/(((1-x) x (1-k x))^(2/3) (1-(1+k) x+(-b+k) x^2)),x]*)


(* ::Input:: *)
(*int[(1-k x)/((1+(-2+k) x) ((1-x) x (1-k x))^(2/3)),x]*)


(* ::Input:: *)
(*int[((-b+x) (b^4+a^4 c-4 (b^3+a^3 c) x+6 (b^2+a^2 c) x^2-4 (b+a c) x^3+(1+c) x^4))/((a-x)^3 ((-a+x) (-b+x)^2)^(2/3) (-b^2+a^2 d+2 (b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-a^3+3 a^2 x-3 a x^2+x^3))/(((-a+x) (-b+x)^2)^(2/3) (-b^4+a^4 d+4 (b^3-a^3 d) x-6 (b^2-a^2 d) x^2+4 (b-a d) x^3+(-1+d) x^4)),x]*)


(* ::Input:: *)
(*int[((a-x) (-b^3+3 b^2 x-3 b x^2+x^3))/(((-a+x) (-b+x)^2)^(2/3) (a^4-b^4 d-4 (a^3-b^3 d) x+6 (a^2-b^2 d) x^2-4 (a-b d) x^3+(1-d) x^4)),x]*)


(* ::Input:: *)
(*int[((-b+x) (b^2+a^2 c-2 (b+a c) x+(1+c) x^2))/((a-x) ((-a+x) (-b+x)^2)^(2/3) (-b^2+a^2 d+2 (b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((a-x) (a^2+b^2 c-2 (a+b c) x+(1+c) x^2))/((-b+x) ((-a+x) (-b+x)^2)^(2/3) (a^2-b^2 d-2 (a-b d) x+(1-d) x^2)),x]*)


(* ::Input:: *)
(*int[(a b-(a+b) x+x^2)/(((-a+x) (-b+x)^2)^(2/3) (a^2-b^2 d-2 (a-b d) x+(1-d) x^2)),x]*)


(* ::Input:: *)
(*int[(a b-(a+b) x+x^2)/(((-a+x) (-b+x)^2)^(2/3) (-b^2+a^2 d+2 (b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((-4 a+b+3 x) (-b^3+3 b^2 x-3 b x^2+x^3))/(((-a+x) (-b+x)^2)^(2/3) (a+b^4 d-(1+4 b^3 d) x+6 b^2 d x^2-4 b d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[((-4 a+b+3 x) (-b^3+3 b^2 x-3 b x^2+x^3))/(((-a+x) (-b+x)^2)^(2/3) (b^4+a d-(4 b^3+d) x+6 b^2 x^2-4 b x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-b+x) (b^2+a^2 c-2 (b+a c) x+(1+c) x^2))/((-a+x) ((-a+x) (-b+x)^2)^(2/3) (-b^2+a^2 d+2 (b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(a^2+b^2 c-2 (a+b c) x+(1+c) x^2)/(((-a+x) (-b+x)^2)^(2/3) (-a^2+b^2 d+2 (a-b d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((-b+x) (b^2+a^2 c-2 (b+a c) x+(1+c) x^2))/((a-x)^2 ((-a+x) (-b+x)^2)^(2/3) (b-a d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-a-b c+(1+c) x))/(((-a+x) (-b+x)^2)^(2/3) (-a^2+b^2 d+2 (a-b d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-b-a c+(1+c) x))/(((-a+x) (-b+x)^2)^(2/3) (-b^2+a^2 d+2 (b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(a^2+b^2 c-2 (a+b c) x+(1+c) x^2)/((-b+x) ((-a+x) (-b+x)^2)^(2/3) (a-b d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[(-b+x)^2/(((-a+x) (-b+x)^2)^(2/3) (-a^2+b^2 d+2 (a-b d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x))/(((-a+x) (-b+x)^2)^(2/3) (-b^2+a^2 d+2 (b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-b-a c+(1+c) x))/((-a+x) ((-a+x) (-b+x)^2)^(2/3) (b-a d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[(-a-b c+(1+c) x)/(((-a+x) (-b+x)^2)^(2/3) (a-b d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[(-b+x)/(((-a+x) (-b+x)^2)^(2/3) (a-b d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[(-b+x)/(((-a+x) (-b+x)^2)^(2/3) (b-a d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[(x^3 (-a+x) (-4 a+3 x))/((x^2 (-a+x))^(2/3) (-a^2 d+2 a d x-d x^2+x^8)),x]*)


(* ::Input:: *)
(*int[(x (a^2 d-2 a d x+(1+d) x^2))/((-a+x)^2 (x^2 (-a+x))^(2/3) (-a e+(-1+e) x)),x]*)


(* ::Input:: *)
(*int[(a^2-2 a x+(1+d) x^2)/((x^2 (-a+x))^(2/3) (-a^2+2 a x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[(x^7 (-4 a+3 x))/((x^2 (-a+x))^(2/3) (-a^2+2 a x-x^2+d x^8)),x]*)


(* ::Input:: *)
(*int[((-4 a+3 x) (-a+x+d x^4))/(x (x^2 (-a+x))^(2/3) (a-x+e x^4)),x]*)


(* ::Input:: *)
(*int[(x (-a d+(1+d) x))/((x^2 (-a+x))^(2/3) (a^2 e-2 a e x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[(x (-a d+(1+d) x))/((-a+x) (x^2 (-a+x))^(2/3) (-a e+(-1+e) x)),x]*)


(* ::Input:: *)
(*int[(x (-a+(1+d) x))/((x^2 (-a+x))^(2/3) (-a^2+2 a x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[(-a x+x^2)/((x^2 (-a+x))^(2/3) (a^2 d-2 a d x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(-a x+x^2)/((x^2 (-a+x))^(2/3) (a^2-2 a x+(1-d) x^2)),x]*)


(* ::Input:: *)
(*int[(a^2-2 a x+(1+d) x^2)/(x (x^2 (-a+x))^(2/3) (a+(-1+e) x)),x]*)


(* ::Input:: *)
(*int[(x (-a+x))/((x^2 (-a+x))^(2/3) (a^2 d-2 a d x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(x^3 (-4 a+3 x))/((x^2 (-a+x))^(2/3) (a d-d x+x^4)),x]*)


(* ::Input:: *)
(*int[x^2/((x^2 (-a+x))^(2/3) (-a^2+2 a x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(x^3 (-4 a+3 x))/((x^2 (-a+x))^(2/3) (a-x+d x^4)),x]*)


(* ::Input:: *)
(*int[(-a+(1+d) x)/((x^2 (-a+x))^(2/3) (a+(-1+e) x)),x]*)


(* ::Input:: *)
(*int[x/((x^2 (-a+x))^(2/3) (-a d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[x/((x^2 (-a+x))^(2/3) (a+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[((-a (a-2 b)-2 b x+x^2) (a^2-b c+(-2 a+c) x+x^2))/(((-a+x) (-b+x))^(2/3) (a^4-b^2 d-2 (2 a^3-b d) x+(6 a^2-d) x^2-4 a x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((a-5 b+4 x) (-a^3+3 a^2 x-3 a x^2+x^3))/(((-a+x) (-b+x))^(2/3) (b-a^5 d-(1-5 a^4 d) x-10 a^3 d x^2+10 a^2 d x^3-5 a d x^4+d x^5)),x]*)


(* ::Input:: *)
(*int[((a-2 b+x) (b^2+a^4 c-2 (b+2 a^3 c) x+(1+6 a^2 c) x^2-4 a c x^3+c x^4))/((a-x)^3 ((-a+x) (-b+x))^(2/3) (b+a^2 d-(1+2 a d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[((a-2 b+x) (-a^3+3 a^2 x-3 a x^2+x^3))/(((-a+x) (-b+x))^(2/3) (-b^2+a^4 d+2 (b-2 a^3 d) x+(-1+6 a^2 d) x^2-4 a d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[((a-5 b+4 x) (-a^3+3 a^2 x-3 a x^2+x^3))/(((-a+x) (-b+x))^(2/3) (-a^5+b d-(-5 a^4+d) x-10 a^3 x^2+10 a^2 x^3-5 a x^4+x^5)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-a (a-2 b)-2 b x+x^2))/(((-a+x) (-b+x))^(2/3) (a^4-b^2 d-2 (2 a^3-b d) x+(6 a^2-d) x^2-4 a x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((a-2 b+x) (-b+a^2 c+(1-2 a c) x+c x^2))/((a-x) ((-a+x) (-b+x))^(2/3) (b+a^2 d-(1+2 a d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(-a (a-2 b)-2 b x+x^2)/(((-a+x) (-b+x))^(2/3) (b+a^2 d-(1+2 a d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(-a (a-2 b)-2 b x+x^2)/(((-a+x) (-b+x))^(2/3) (a^2+b d-(2 a+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[((-a (a-2 b)-2 b x+x^2) (a^2-b c+(-2 a+c) x+x^2))/((-b+x) ((-a+x) (-b+x))^(2/3) (a^2+b d-(2 a+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[1/(-1-3 x-3 x^2+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[((1+x^5) (1+x^3+x^5)^(1/3) (-3+2 x^5))/(x^2 (2-2 x^3+4 x^5-x^6-2 x^8+2 x^10)),x]*)


(* ::Input:: *)
(*int[((4+x^2) (-2 x+x^3)^(1/3))/(x^4 (-4-4 x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-x+x^3)^(1/3) (8-10 x^2+x^4))/(x^4 (4-2 x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-2+x^2) (x+x^3)^(1/3))/(x^2 (4-2 x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-2+5 x^7) (2 x+x^3+2 x^8)^(1/3))/(4+x^4+8 x^7+4 x^14),x]*)


(* ::Input:: *)
(*int[(x (3+4 x) (-1-2 x+x^3)^(1/3))/(-2-8 x-8 x^2+x^6),x]*)


(* ::Input:: *)
(*int[((2+x^3) (x+x^3-x^4)^(1/3))/(1+x^2-2 x^3+x^4-x^5+x^6),x]*)


(* ::Input:: *)
(*int[((-2+x^3) (x+x^3+x^4)^(1/3))/(1+x^2+2 x^3+x^4+x^5+x^6),x]*)


(* ::Input:: *)
(*int[((2+x^3) (x-x^4)^(1/3))/(1+x^2-2 x^3-x^4-x^5+x^6),x]*)


(* ::Input:: *)
(*int[(x^3 (3+x^2))/((1+x^2) (1+x^2-x^3)^(1/3) (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((2-2 x+2 x^2-3 x^3+3 x^4) (-x-x^3-x^4+x^6)^(1/3))/((1+x) (-1+2 x-2 x^2+x^3) (-1-x^3+x^5)),x]*)


(* ::Input:: *)
(*int[((-2+x^3) (x+2 x^3+x^4)^(1/3))/((1+x^3) (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((2+5 x^7) (-x-x^3+x^8)^(1/3))/((-1+x^7) (-1+x^2+x^7)),x]*)


(* ::Input:: *)
(*int[((-2+x^3) (x+x^3+x^4)^(1/3))/((1+x^3) (1-x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-2+x) (x-x^2+x^3)^(1/3))/((-1+x) (-1+x+x^2)),x]*)


(* ::Input:: *)
(*int[(-3-4 x+3 x^6)/((1+2 x+x^6) (1+2 x+2 x^3+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[((-3+2 x) (-1+x+x^3)^(1/3))/(x^2 (2-2 x+x^3)),x]*)


(* ::Input:: *)
(*int[((-b+a^3 x^3) (b+a^3 x^3)^(1/3))/x^5,x]*)


(* ::Input:: *)
(*int[((-6+x^2) (-2+x^2) (2-x^2+x^3) (-2+x^2+2 x^3)^(1/3))/(x^5 (-2+x^2+x^3)^2),x]*)


(* ::Input:: *)
(*int[((-1+2 x^6) (x+x^7)^(1/3))/((1-2 x^2+x^6) (1-x^2+x^6)),x]*)


(* ::Input:: *)
(*int[((-4 b+a x^3) (b+a x^3)^(1/3))/(x^5 (-2 b+a x^3)),x]*)


(* ::Input:: *)
(*int[((2-2 x^4+3 x^5+4 x^6) (-x+2 x^3-x^5+x^6+x^7)^(1/3))/(-1+x^2-x^4+x^5+x^6)^2,x]*)


(* ::Input:: *)
(*int[((-1+2 x^3+x^8)^(1/3) (3+5 x^8))/(x^2 (-1+x^8)),x]*)


(* ::Input:: *)
(*int[((-2+2 x^4+5 x^7) (x-x^3+x^5+x^8)^(1/3))/(2+x^2+2 x^4+2 x^7)^2,x]*)


(* ::Input:: *)
(*int[(b+a x^6)/((-b+a x^6) (-b+a^3 x^3+a x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-b+a x^6)/((b+a x^6) (-b+a^3 x^3+a x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x^6)/((1+x^6) (1+a^3 x^3+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[((1-x^7)^(1/3) (-2+x^3+2 x^7) (3+4 x^7))/(x^2 (-1+x^7) (-4+x^3+4 x^7)),x]*)


(* ::Input:: *)
(*int[((-4+5 x^7) (-2 x+2 x^3-x^8)^(1/3))/((2+x^7) (2-2 x^2+x^7)),x]*)


(* ::Input:: *)
(*int[((-1+2 x^3+x^8)^(1/3) (3+5 x^8))/(x^2 (-1+x^3+x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^3-x^4)^(1/3) (-3+x^4))/(x^2 (1+x^4)),x]*)


(* ::Input:: *)
(*int[((3+x^5) (-2+x^3+x^5)^(1/3))/(x^2 (-2+x^5)),x]*)


(* ::Input:: *)
(*int[((2+x^3+4 x^6) (x+2 x^3-x^4-x^7)^(1/3))/((-1-2 x^2+x^3+x^6) (-1-x^2+x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3) (1+x^6)^(1/3))/(x^2 (1+x^3)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) (2+x^6)^(1/3))/(x^2 (2+2 x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^5)^(1/3) (-3+2 x^5))/(x^2 (2-x^3+2 x^5)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (x+2 x^3)^(1/3))/(x^4 (-1+x^2)),x]*)


(* ::Input:: *)
(*int[((1+2 x^6) (x+x^3-x^7)^(1/3))/(-1+x^6)^2,x]*)


(* ::Input:: *)
(*int[((-1-x^4+2 x^6) (x-x^5+x^7)^(1/3))/(1+x^2-x^4+x^6)^2,x]*)


(* ::Input:: *)
(*int[((-4+x^2) (x+x^3)^(1/3))/(x^4 (2+x^2)),x]*)


(* ::Input:: *)
(*int[((1+2 x^7)^(1/3) (-3+8 x^7))/(x^2 (1+x^3+2 x^7)),x]*)


(* ::Input:: *)
(*int[(-3+5 x^8)/((1+x^8) (1-x^3+x^8)^(1/3)),x]*)


(* ::Input:: *)
(*int[((-1+x^8)^(1/3) (3+5 x^8))/(x^2 (-1-x^3+x^8)),x]*)


(* ::Input:: *)
(*int[((b x+a x^3)^(1/3) (b+a x^4))/x^4,x]*)


(* ::Input:: *)
(*int[((b-a x^6)^(1/3) (b+a x^6))/(x^2 (-b+c x^3+a x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^4+2 x^6) (x+x^5+x^7)^(1/3))/((1+x^4+x^6) (1-x^2+x^4+x^6)),x]*)


(* ::Input:: *)
(*int[(-1+x)/((1+x) (2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+3 x^4)/((1-x+x^4) (x^2+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[x/(x^2+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[(-1+3 x^4)/((1+x^4) (x^2+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^7 (x^2+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^3 (x^2+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[x/(-x^2+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[(1+3 x^4)/((-1+x+x^4) (-x^2+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1+3 x^4)/((-1+x^4) (-x^2+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^7 (-x^2+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^3 (-x^2+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[(2-3 x^5)/((1-x^2+x^5) (x+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-4 x^2+9 x^7)/((x+x^6)^(1/3) (-8+x^4+x^9)),x]*)


(* ::Input:: *)
(*int[(-4 x^2+9 x^7)/((-x+x^6)^(1/3) (1-x^4+x^9)),x]*)


(* ::Input:: *)
(*int[(2+3 x^5)/((-1+x^2+x^5) (-x+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[x/(1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x (1+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[x^3/(1+x^6)^(2/3),x]*)


(* ::Input:: *)
(*int[x^19/(1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[x^13/(1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[x^7/(1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x^13 (1+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^7 (1+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[x^15 (1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[x^9 (1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[x^3 (1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[x^5 (1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x^5 (1+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[x^5/(1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[(x (-1+x^6)^(1/3))/(1+x^2),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1-x+x^2) (-1+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x+x^2) (-1+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x+x^7)/((-1+x^6)^(2/3) (-1+x^3+x^6)),x]*)


(* ::Input:: *)
(*int[x^3/(-1+x^6)^(2/3),x]*)


(* ::Input:: *)
(*int[x/(-1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x (-1+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(1/3) (1+x^6))/(x^2 (-1+x^3+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(1/3) (-1+2 x^6))/x^9,x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(1/3) (1+x^6))/x^9,x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(1/3) (1+x^6))/x^3,x]*)


(* ::Input:: *)
(*int[x^15 (-1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[x^9 (-1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[x^3 (-1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[x^19/(-1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[x^13/(-1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[x^7/(-1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x^19 (-1+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^13 (-1+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^7 (-1+x^6)^(1/3)),x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(1/3) (1+x^6) (-1+x^3+x^6))/x^8,x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(1/3) (-1+2 x^6))/x^15,x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(1/3) (1+x^6))/x^15,x]*)


(* ::Input:: *)
(*int[x^5 (-1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[x^5/(-1+x^6)^(1/3),x]*)


(* ::Input:: *)
(*int[((-1+x^6)^(1/3) (1+x^6))/x^5,x]*)


(* ::Input:: *)
(*int[(-1+x^6)^(1/3)/x^9,x]*)


(* ::Input:: *)
(*int[(-1+2 x^3)/((1+x+x^3) (x^2+x^5)^(1/3)),x]*)


(* ::Input:: *)
(*int[((-1+2 x^3) (x^2+x^5)^(1/3))/x^3,x]*)


(* ::Input:: *)
(*int[(x^3 (-5+8 x^3))/((-x^2+x^5)^(1/3) (1-x^5+x^8)),x]*)


(* ::Input:: *)
(*int[(1+2 x^3)/((-1+x+x^3) (-x^2+x^5)^(1/3)),x]*)


(* ::Input:: *)
(*int[((1+2 x^3) (-x^2+x^5)^(1/3))/x^3,x]*)


(* ::Input:: *)
(*int[(1+x^6)/((x+x^5)^(1/3) (-1+x^6)),x]*)


(* ::Input:: *)
(*int[(-1+x^6)/((x+x^5)^(1/3) (1+x^6)),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) (x+x^5)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) (x+x^5)^(1/3)),x]*)


(* ::Input:: *)
(*int[((-1-2 x+x^2) (-1+2 x+x^2))/((1-x+2 x^2+x^3+x^4) (-x+x^5)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x^2 (-2+x^4))/((-x+x^5)^(1/3) (-1+x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(1+x^4)/((-1-x^2+x^4) (-x+x^5)^(1/3)),x]*)


(* ::Input:: *)
(*int[((-9+7 x^2) (x^2+x^4)^(1/3))/(-1+x^2),x]*)


(* ::Input:: *)
(*int[(1+x^3+x^6)/((x^2+x^4)^(1/3) (-1+x^6)),x]*)


(* ::Input:: *)
(*int[(1-x^3+x^6)/((x^2+x^4)^(1/3) (-1+x^6)),x]*)


(* ::Input:: *)
(*int[(1+x^6)/((x^2+x^4)^(1/3) (-1+x^6)),x]*)


(* ::Input:: *)
(*int[(1+x+x^2)/((-1+x^2) (x^2+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1-x+x^2)/((-1+x^2) (x^2+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) (x^2+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[x^3/((x^2+x^4)^(1/3) (-1+x^6)),x]*)


(* ::Input:: *)
(*int[(1+x^3)/((-1+x^3) (x^2+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x^6)/((x^2+x^4)^(1/3) (1+x^6)),x]*)


(* ::Input:: *)
(*int[(x (x^2+x^4)^(1/3))/(1+2 x^2),x]*)


(* ::Input:: *)
(*int[(x^2+x^4)^(1/3)/(x (-1+x^2)),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+x) (x^2+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x+x^2) (x^2+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[((-1+x^2) (x^2+x^4)^(1/3))/x^3,x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) (x^2+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/(x (x^2+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[x^3/((-x^2+x^4)^(1/3) (1+x^6)),x]*)


(* ::Input:: *)
(*int[(-1+x^6)/((-x^2+x^4)^(1/3) (1+x^6)),x]*)


(* ::Input:: *)
(*int[(1+x^6)/((-x^2+x^4)^(1/3) (-1+x^6)),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) (-x^2+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-x^2+x^4)^(1/3)/(x (1+x^2)),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x+x^2) (-x^2+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (-x^2+x^4)^(1/3))/x^3,x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) (-x^2+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x^2 (4+7 x^3))/((x+x^4)^(1/3) (-1+x^4+x^7)),x]*)


(* ::Input:: *)
(*int[((x+x^4)^(1/3) (-2-x^3+x^6))/x^6,x]*)


(* ::Input:: *)
(*int[(-2+x^3)/((1+x^3) (x+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[((-2+x^3) (x+x^4)^(1/3))/(1+x^3)^2,x]*)


(* ::Input:: *)
(*int[(x^2 (-4+7 x^3))/((-x+x^4)^(1/3) (-1-x^4+x^7)),x]*)


(* ::Input:: *)
(*int[(2+x^3)/((1+x^3) (-x+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[((2+x^3) (-x+x^4)^(1/3))/(-1+x^3)^2,x]*)


(* ::Input:: *)
(*int[(2 (3 a q x-2 b p x^3+a p x^5))/((q+p x^4)^(1/3) (b^3 d+c q+3 a b^2 d x^2+(3 a^2 b d+c p) x^4+a^3 d x^6)),x]*)


(* ::Input:: *)
(*int[((b+a x) (-3 b c q+3 a d q+4 b d p x^3+(b c p+7 a d p) x^4+4 a c p x^5))/((q+p x^4)^(1/3) (e (d+c x)^3+f (b+a x)^3 (q+p x^4))),x]*)


(* ::Input:: *)
(*int[(-3 a q+4 b p x^3+a p x^4)/((q+p x^4)^(1/3) (b^3 d+c q+3 a b^2 d x+3 a^2 b d x^2+a^3 d x^3+c p x^4)),x]*)


(* ::Input:: *)
(*int[(x (3+7 x^4))/((1+x^4)^(1/3) (-4+x^3+x^7)),x]*)


(* ::Input:: *)
(*int[(x (-3+x^4))/((1+x^4)^(2/3) (1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(-3+x^4)/((1+x^4)^(1/3) (1-x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4)^(1/3))/(x^2 (1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((1+x^4)^(1/3) (3+x^4))/x^17,x]*)


(* ::Input:: *)
(*int[((1+x^4)^(1/3) (3+x^4))/x^13,x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4)^(1/3))/x^13,x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4)^(1/3))/x^9,x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4)^(1/3) (1+x^3+x^4))/x^8,x]*)


(* ::Input:: *)
(*int[((1+x^4)^(1/3) (3+x^4))/x^9,x]*)


(* ::Input:: *)
(*int[((-3+x^4) (1+x^4)^(1/3))/x^5,x]*)


(* ::Input:: *)
(*int[x^3/(1+x^4)^(1/3),x]*)


(* ::Input:: *)
(*int[x^3 (1+x^4)^(1/3),x]*)


(* ::Input:: *)
(*int[(x (-3+7 x^4))/((-1+x^4)^(1/3) (1-x^3+x^7)),x]*)


(* ::Input:: *)
(*int[1/(x (-1+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[(3+x^4)/((-1+x^4)^(1/3) (-1-8 x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(1/3) (3+x^4))/(x^2 (-1-x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-1+x^4)^(1/3) (3+x^4))/(x^2 (-1+x^3+x^4)),x]*)


(* ::Input:: *)
(*int[1/(x^5 (-1+x^4)^(1/3)),x]*)


(* ::Input:: *)
(*int[x^3 (-1+x^4)^(1/3),x]*)


(* ::Input:: *)
(*int[x^3/(-1+x^4)^(1/3),x]*)


(* ::Input:: *)
(*int[(1+x^2+x^3)/((-1+x^2+x^3) (x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1-x^2+x^3)/((-1-x^2+x^3) (x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^6 (b+a x^3) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^6 (-b+a x^3) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^6 (-1+x^3) (x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^6 (1+x^3) (x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^3 (b+a x^3) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^3 (-b+a x^3) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^3 (-1+x^3) (x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((b^2 x^2+a^3 x^3)^(1/3) (b+2 a^6 x^6)),x]*)


(* ::Input:: *)
(*int[1/((b^2 x^2+a^3 x^3)^(1/3) (-b+2 a^6 x^6)),x]*)


(* ::Input:: *)
(*int[1/((b^2 x^2+a^3 x^3)^(1/3) (2 b+a^6 x^6)),x]*)


(* ::Input:: *)
(*int[1/((b^2 x^2+a^3 x^3)^(1/3) (-2 b+a^6 x^6)),x]*)


(* ::Input:: *)
(*int[1/((b^2 x^2+a^3 x^3)^(1/3) (b+a^6 x^6)),x]*)


(* ::Input:: *)
(*int[1/((b^2 x^2+a^3 x^3)^(1/3) (-b+a^6 x^6)),x]*)


(* ::Input:: *)
(*int[(2+x+x^2)/((3+2 x+x^2) (x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(b x+a x^3)/((b+2 a x^3) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-b x+a x^3)/((-b+2 a x^3) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/((-b+2 a x^3) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((b+2 a x^2) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/((-b+2 a x^2) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-b+a x^2) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((b+a x^2) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (x^2+x^3)^(1/3))/(-1+x^2),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/(b^2 x^2+a^3 x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/(b^2 x^2+a^3 x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[(-b+a x)/((b+a x) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(b+a x)/((-b+a x) (b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(b^2 x^2+a^3 x^3)^(1/3)/(b+a x),x]*)


(* ::Input:: *)
(*int[(b^2 x^2+a^3 x^3)^(1/3)/(-b+a x),x]*)


(* ::Input:: *)
(*int[x^2 (x^2+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x (x^2+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[(x^2+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[(x^2+x^3)^(1/3)/x^2,x]*)


(* ::Input:: *)
(*int[(x^2+x^3)^(1/3)/x,x]*)


(* ::Input:: *)
(*int[x^2/(x^2+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x/(x^2+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x^2+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x^2 (x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x (x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1-x+x^3) (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x+x^3)/((1-x+x^3) (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-x^2+x^3)^(1/3)/(1+x+x^2),x]*)


(* ::Input:: *)
(*int[1/((-b x^2+a x^3)^(1/3) (b+a x^4)),x]*)


(* ::Input:: *)
(*int[1/((-b x^2+a x^3)^(1/3) (-b+a x^4)),x]*)


(* ::Input:: *)
(*int[1/((-x^2+x^3)^(1/3) (-1+x^4)),x]*)


(* ::Input:: *)
(*int[1/((-b+a^3 x^3) (-b x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((b+a^3 x^3) (-b x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((1+x^3) (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-1+x^3) (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(b+a^3 x^2)/((-b+a^3 x^2) (-b x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-b+a^3 x^2)/((b+a^3 x^2) (-b x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((b+a^3 x^2) (-b x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-b+a^3 x^2) (-b x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-1+x^2) (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[((1+x^2) (-x^2+x^3)^(1/3))/(-1+x^2),x]*)


(* ::Input:: *)
(*int[(-b^2 x^2+a^3 x^3)^(1/3)/(-b+a x^2),x]*)


(* ::Input:: *)
(*int[(-b^2 x^2+a^3 x^3)^(1/3)/(b+a x^2),x]*)


(* ::Input:: *)
(*int[(-x^2+x^3)^(1/3)/(-1+x^2),x]*)


(* ::Input:: *)
(*int[(b+a x)/((-b+a x) (-b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-b+a x)/((b+a x) (-b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x)/((1+x) (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(f+e x)/((-d+c x) (-b x^2+a x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+x) (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-b+a x) (-b^2 x^2+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[x/((1+x) (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[x/((-1+x) (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1+x)/(x (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x)/(x (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[x/(-x^2+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(-x^2+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[(-x^2+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x^3 (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^2 (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-1+x) (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x (-x^2+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(b+a x^6)/((x+x^3)^(1/3) (d+c x^6)),x]*)


(* ::Input:: *)
(*int[(1+2 x^6)/((x+x^3)^(1/3) (-1+x^6)),x]*)


(* ::Input:: *)
(*int[1/((x+x^3)^(1/3) (b+a x^6)),x]*)


(* ::Input:: *)
(*int[1/((x+x^3)^(1/3) (-1+x^6)),x]*)


(* ::Input:: *)
(*int[((x+x^3)^(1/3) (b+a x^6))/(d+c x^6),x]*)


(* ::Input:: *)
(*int[((b+a x^2) (x+x^3)^(1/3))/(d+c x^2),x]*)


(* ::Input:: *)
(*int[1/((b+a x^2) (x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((d+c x^2) (x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[x^2/((b+a x^2) (x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[((-b+a x^2) (x+x^3)^(1/3))/x^2,x]*)


(* ::Input:: *)
(*int[(b+a x^6)/(x^6 (x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x^6)/(x^6 (x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/(x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[(x+x^3)^(1/3)/x^2,x]*)


(* ::Input:: *)
(*int[x^6 (x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x^4 (x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x^2 (x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[(x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[(1+3 x^2) (x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x^6 (x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[((2+3 x^2) (x+x^3)^(1/3))/(1+x^2),x]*)


(* ::Input:: *)
(*int[(x+x^3)^(1/3)/x^4,x]*)


(* ::Input:: *)
(*int[1/((1+x^2) (x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^2 (x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-b x+a x^3)^(1/3) (d+c x^6)),x]*)


(* ::Input:: *)
(*int[1/((-x+x^3)^(1/3) (1+x^6)),x]*)


(* ::Input:: *)
(*int[((-x+x^3)^(1/3) (-b+a x^6))/(-d+c x^6),x]*)


(* ::Input:: *)
(*int[(-b+a x^6)/((-x+x^3)^(1/3) (-d+c x^6)),x]*)


(* ::Input:: *)
(*int[(-x+x^3)^(1/3)/(b+a x^6),x]*)


(* ::Input:: *)
(*int[((-b+a x^2) (-x+x^3)^(1/3))/(-d+c x^2),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/((-d+c x^2) (-x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[x^2/((-b+a x^2) (-x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[((b+a x^2) (-x+x^3)^(1/3))/x^2,x]*)


(* ::Input:: *)
(*int[1/((-b+a x^2) (-x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(b+a x^6)/(-x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/(x^2 (-x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[((1+3 x) (-x+x^3)^(1/3))/(x (1+x)),x]*)


(* ::Input:: *)
(*int[1/((1+3 x) (-x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((1-3 x) (-x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[x^6 (-x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x^4 (-x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x^2 (-x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[(-x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[(-x+x^3)^(1/3)/x^2,x]*)


(* ::Input:: *)
(*int[(-1+3 x^2) (-x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x^6 (-x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[((1+3 x^2) (-x+x^3)^(1/3))/x^2,x]*)


(* ::Input:: *)
(*int[1/((-1+x^2) (-x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^2 (-x+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-x+x^3)^(1/3)/x^4,x]*)


(* ::Input:: *)
(*int[(-1+3 x^2)/(-x+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[(b^3+a^3 x^3)^(1/3)/(-b+a x),x]*)


(* ::Input:: *)
(*int[x/((-b+a x) (b^3+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-b+a x) (b^3+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-b+a x)/(x (b^3+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-b+a x)/(x (b^3+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x (b+a x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1+x^3)^(1/3)/x^7,x]*)


(* ::Input:: *)
(*int[(1+x^3)^(1/3)/x^4,x]*)


(* ::Input:: *)
(*int[(1+x^3)^(1/3)/x^2,x]*)


(* ::Input:: *)
(*int[(1+x^3)^(1/3)/x,x]*)


(* ::Input:: *)
(*int[(-1+x)/(x^10 (1+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x)/(x^7 (1+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x)/(x^4 (1+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[x^7 (1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x^4 (1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x (1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[((-1+x^3) (1+x^3)^(1/3))/x^5,x]*)


(* ::Input:: *)
(*int[((-1+x^3) (1+x^3)^(1/3))/x^4,x]*)


(* ::Input:: *)
(*int[((-1+x^3) (1+x^3)^(1/3))/x^2,x]*)


(* ::Input:: *)
(*int[((-1+x^3) (1+x^3)^(1/3))/x,x]*)


(* ::Input:: *)
(*int[x^5 (1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x^2 (1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[((-1+x^3) (1+x^3)^(1/3))/x^11,x]*)


(* ::Input:: *)
(*int[((-1+x^3) (1+x^3)^(1/3))/x^8,x]*)


(* ::Input:: *)
(*int[(1+x^3)^(1/3)/x^5,x]*)


(* ::Input:: *)
(*int[(-b^3+a^3 x^3)^(1/3)/(b+a x),x]*)


(* ::Input:: *)
(*int[(-1+x^3)^(1/3)/(1+x),x]*)


(* ::Input:: *)
(*int[(-d+c x)/(x^7 (-b+a x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-d+c x)/(x^4 (-b+a x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-d+c x^7)/(x (-b+a x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-d+c x^4)/(x (-b+a x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-d+c x)/(x (-b+a x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x)/(x (-1+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-b+a x)/((b+a x) (-b^3+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x)/((1+x) (-1+x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[x/((b+a x) (-b^3+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(1+x)/((1+3 x+x^2) (1-x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((b+a x) (-b^3+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-b+a x)/(x (-b^3+a^3 x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(-b+a x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[1/(x (-b+a x^3)^(1/3)),x]*)


(* ::Input:: *)
(*int[x^13 (-1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x^10 (-1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x^7 (-1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x^4 (-1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x (-1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(1/3) (1+x^3))/x^13,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(1/3) (1+x^3))/x^7,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(1/3) (1+x^3))/x^5,x]*)


(* ::Input:: *)
(*int[(-1+x^3)^(1/3)/x^10,x]*)


(* ::Input:: *)
(*int[(-1+x^3)^(1/3)/x^7,x]*)


(* ::Input:: *)
(*int[(-1+x^3)^(1/3)/x^4,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(1/3) (-1+2 x^3))/x^10,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(1/3) (-1+2 x^3))/x^7,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(1/3) (-1+2 x^3))/x^5,x]*)


(* ::Input:: *)
(*int[x^11 (-1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x^8 (-1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[x^5 (-1+x^3)^(1/3),x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(1/3) (-1+2 x^3))/x^11,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(1/3) (-1+2 x^3))/x^8,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(1/3) (1+x^3))/x^11,x]*)


(* ::Input:: *)
(*int[((-1+x^3)^(1/3) (1+x^3))/x^8,x]*)


(* ::Input:: *)
(*int[(-1+x^3)^(1/3)/x^8,x]*)


(* ::Input:: *)
(*int[(-1+x^3)^(1/3)/x^5,x]*)


(* ::Input:: *)
(*int[(-1-2 x+6 x^2)^(1/3)/(-1+6 x),x]*)


(* ::Input:: *)
(*int[(2-8 x+8 x^2)^(1/3)/(3+x),x]*)


(* ::Input:: *)
(*int[(-6-2 x+x^2)^(1/3)/(-1+x),x]*)


(* ::Input:: *)
(*int[(6+2 x+x^2)^(1/3)/(1+x),x]*)


(* ::Input:: *)
(*int[(3 c+2 b x+a x^2)/((c+b x+a x^2)^(1/3) (c+b x+a x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(2+x)^2/(x (4-2 x+x^2) (1+x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(6+2 x+x^2)/((1+x) (2+x+x^2)^(1/3) (2-x+2 x^2)),x]*)


(* ::Input:: *)
(*int[(6+2 x+x^2)/((2+x^2)^(1/3) (1+3 x-2 x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(6+2 x+x^2)/((2+x) (2+x^2) (2+x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(2+x^2)/(x (2-2 x+x^2) (1-x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-5+x)/((-2-x+x^2)^(1/3) (-3+4 x+x^2)),x]*)


(* ::Input:: *)
(*int[(-2+x)/((2+x^2) (-1+x+2 x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1+x)/((-4-2 x+x^2) (-2-2 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((1-3 x^2)^(1/3) (-3+x^2)),x]*)


(* ::Input:: *)
(*int[1/((3+x^2) (1+3 x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-2+x) (1+2 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-2+x) (-4-4 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((1+2 x) (-1+4 x+4 x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((2+x) (1+x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-1+x) (-1-2 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((1+x) (3+2 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-1+x) (-3-2 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((1+x) (-1+2 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((1+x) (2+2 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((-1+x) (2-2 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((1+x) (1-x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x (3+3 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x (2-3 x+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/((1-3 x^2)^(1/3) (-3+x^2)),x]*)


(* ::Input:: *)
(*int[1/((1+x^2)^(1/3) (9+x^2)),x]*)


(* ::Input:: *)
(*int[(1+x)/((-3+x^2) (1+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[(x (3+5 x^2))/((1+x^2)^(1/3) (-1+x^3+x^5)),x]*)


(* ::Input:: *)
(*int[(3+x^2)/((1+x^2)^(1/3) (-1-x^2+x^3)),x]*)


(* ::Input:: *)
(*int[1/(x (1+x^2)^(1/3)),x]*)


(* ::Input:: *)
(*int[x/(1+x^2)^(1/3),x]*)


(* ::Input:: *)
(*int[x (1+x^2)^(1/3),x]*)


(* ::Input:: *)
(*int[(1+2 x)/((-1+x^2)^(1/3) (3+x^2)),x]*)


(* ::Input:: *)
(*int[1/((-1+x^2)^(1/3) (3+x^2)),x]*)


(* ::Input:: *)
(*int[(3+x^2)/((1+x^2)^(1/3) (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-3+x^2)/((-1+x^2)^(1/3) (-1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-3 b+a x)/((b^2-a^2 x^2)^(1/3) (3 b^2+a^2 x^2)),x]*)


(* ::Input:: *)
(*int[(-3+x)/((1-x^2)^(1/3) (3+x^2)),x]*)


(* ::Input:: *)
(*int[(3+x)/((-1+x^2)^(1/3) (5-x+2 x^2)),x]*)


(* ::Input:: *)
(*int[(-3+x)/((-1+x^2)^(1/3) (2+x+x^2)),x]*)


(* ::Input:: *)
(*int[(2-x+x^2)/((-1+x^2)^(1/3) (3+4 x+x^2)),x]*)


(* ::Input:: *)
(*int[x/(-1+x^2)^(1/3),x]*)


(* ::Input:: *)
(*int[x (-1+x^2)^(1/3),x]*)


(* ::Input:: *)
(*int[(((1-2 k^2) x+k^2 x^3) (1+d-(d+2 k^2) x^2+k^4 x^4))/((-1+x^2) ((1-x^2) (1-k^2 x^2))^(1/3) (1-e+(e-2 k^2) x^2+k^4 x^4)),x]*)


(* ::Input:: *)
(*int[(3+2 (1+k^2) x-(1+k^2) x^2-4 k^2 x^3-k^2 x^4)/(((1-x^2) (1-k^2 x^2))^(2/3) (1-d-(1+2 d) x-(d+k^2) x^2+k^2 x^3)),x]*)


(* ::Input:: *)
(*int[((1-x^2) ((1-2 k^2) x+k^2 x^3))/(((1-x^2) (1-k^2 x^2))^(1/3) (1-d+2 (d-2 k^2) x^2+(-d+6 k^4) x^4-4 k^6 x^6+k^8 x^8)),x]*)


(* ::Input:: *)
(*int[(((-2+k^2) x+k^2 x^3) (1-2 x^2+x^4))/(((1-x^2) (1-k^2 x^2))^(1/3) (-1+d-2 (2 d-k^2) x^2+(6 d-k^4) x^4-4 d x^6+d x^8)),x]*)


(* ::Input:: *)
(*int[(((-2+k^2) x+k^2 x^3) (1+d-(2+d k^2) x^2+x^4))/(((1-x^2) (1-k^2 x^2))^(1/3) (-1+k^2 x^2) (1-e+(-2+e k^2) x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((1-k^2 x^2) ((-2+k^2) x+k^2 x^3))/(((1-x^2) (1-k^2 x^2))^(1/3) (1-d+2 (-2+d k^2) x^2+(6-d k^4) x^4-4 x^6+x^8)),x]*)


(* ::Input:: *)
(*int[(((-2+k^2) x+k^2 x^3) (1+d-(2 d+k^2) x^2+d x^4))/((-1+x^2)^2 ((1-x^2) (1-k^2 x^2))^(1/3) (-1+e+(-2 e+k^2) x^2+e x^4)),x]*)


(* ::Input:: *)
(*int[(3+(-1+2 k^2) x-3 k^2 x^2-k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/3) (1-d-(1+2 d) x-(d+k^2) x^2+k^2 x^3)),x]*)


(* ::Input:: *)
(*int[(3 k+(2-k^2) x-3 k x^2-k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/3) (1-d-(1+2 d) k x+(-1-d k^2) x^2+k x^3)),x]*)


(* ::Input:: *)
(*int[(3+(1-2 k^2) x-3 k^2 x^2+k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/3) (1-d-(2+d) x+(1+d k^2) x^2+d k^2 x^3)),x]*)


(* ::Input:: *)
(*int[(-3+(1-2 k^2) x+3 k^2 x^2+k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/3) (-1+d-(2+d) x-(1+d k^2) x^2+d k^2 x^3)),x]*)


(* ::Input:: *)
(*int[(-3 k+(-2+k^2) x+3 k x^2+k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/3) (-1+d-(2+d) k x-(d+k^2) x^2+d k x^3)),x]*)


(* ::Input:: *)
(*int[(3+(1-2 k^2) x-3 k^2 x^2+k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/3) (-1+d-(1+2 d) x+(d+k^2) x^2+k^2 x^3)),x]*)


(* ::Input:: *)
(*int[(3 k+(-2+k^2) x-3 k x^2+k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/3) (-1+d-(1+2 d) k x+(1+d k^2) x^2+k x^3)),x]*)


(* ::Input:: *)
(*int[(3 k+(-2+k^2) x-3 k x^2+k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/3) (1-d-(2+d) k x+(d+k^2) x^2+d k x^3)),x]*)


(* ::Input:: *)
(*int[((1-2 k^2) x+k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/3) (-1+d+(1-2 d k^2) x^2+d k^4 x^4)),x]*)


(* ::Input:: *)
(*int[((1-2 k^2) x+k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/3) (1-d+(d-2 k^2) x^2+k^4 x^4)),x]*)


(* ::Input:: *)
(*int[((-2+k^2) x+k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/3) (-1+d+(-2 d+k^2) x^2+d x^4)),x]*)


(* ::Input:: *)
(*int[((-2+k^2) x+k^2 x^3)/(((1-x^2) (1-k^2 x^2))^(1/3) (1-d+(-2+d k^2) x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(-a b-a c+2 b c+(2 a-b-c) x)/(((-a+x) (-b+x) (-c+x))^(1/3) (-b c+a^2 d+(b+c-2 a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(a b+a c-2 b c+(-2 a+b+c) x)/(((-a+x) (-b+x) (-c+x))^(1/3) (a^2-b c d+(-2 a+b d+c d) x+(1-d) x^2)),x]*)


(* ::Input:: *)
(*int[(-a b^2+(4 a-b) b x-3 a x^2+x^3)/((x (-a+x) (-b+x)^2)^(1/3) (-a^2 d+(b^2+2 a d) x-(2 b+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-a b^2+(4 a-b) b x-3 a x^2+x^3)/((x (-a+x) (-b+x)^2)^(1/3) (-a^2+(2 a+b^2 d) x-(1+2 b d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(2 a b^2-b (2 a+b) x+x^3)/((x (-a+x) (-b+x)^2)^(1/3) (-a b^2 d+b (2 a+b) d x-(1+a d+2 b d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[((a b-2 b x+x^2) (-b+(1-a d) x+d x^2))/(x (-a+x) (x (-a+x) (-b+x)^2)^(1/3) (b-(1+a e) x+e x^2)),x]*)


(* ::Input:: *)
(*int[((a b-2 b x+x^2) (-b d-(a-d) x+x^2))/((-b+x) (x (-a+x) (-b+x)^2)^(1/3) (b e-(a+e) x+x^2)),x]*)


(* ::Input:: *)
(*int[((-b+x) (a b-2 b x+x^2))/((x (-a+x) (-b+x)^2)^(1/3) (-b^2 d+2 b d x-(-a^2+d) x^2-2 a x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(2 a b^2-b (2 a+b) x+x^3)/((x (-a+x) (-b+x)^2)^(1/3) (-a b^2+b (2 a+b) x-(a+2 b+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(x (-a+x) (a b-2 b x+x^2))/((x (-a+x) (-b+x)^2)^(1/3) (-b^2+2 b x+(-1+a^2 d) x^2-2 a d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(a b-2 b x+x^2)/((x (-a+x) (-b+x)^2)^(1/3) (b d-(a+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(a b-2 b x+x^2)/((x (-a+x) (-b+x)^2)^(1/3) (b-(1+a d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[((2 a b x-3 a x^2+x^3) (a^2 d-2 a d x+(-b+d) x^2+x^3))/((a-x)^2 (x^2 (-a+x) (-b+x))^(1/3) (-a^2 e+2 a e x-(b+e) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(x^3 (-b+x) (2 a b-3 a x+x^2))/((x^2 (-a+x) (-b+x))^(1/3) (-a^4+4 a^3 x-6 a^2 x^2+4 a x^3+(-1+b^2 d) x^4-2 b d x^5+d x^6)),x]*)


(* ::Input:: *)
(*int[((2 a b-3 a x+x^2) (a^2-2 a x+(1-b d) x^2+d x^3))/(x (-b+x) (x^2 (-a+x) (-b+x))^(1/3) (-a^2+2 a x-(1+b e) x^2+e x^3)),x]*)


(* ::Input:: *)
(*int[((-a b+x^2) (a b d-(-1+a d+b d) x+d x^2))/((-a+x) (-b+x) (x^2 (-a+x) (-b+x))^(1/3) (a b e-(1+a e+b e) x+e x^2)),x]*)


(* ::Input:: *)
(*int[((4 a b-3 (a+b) x+2 x^2) (a b-(a+b) x+x^2+d x^4))/(x^3 (x^2 (-a+x) (-b+x))^(1/3) (-a b+(a+b) x-x^2+e x^4)),x]*)


(* ::Input:: *)
(*int[(x (-a b+x^2))/((x^2 (-a+x) (-b+x))^(1/3) (a^2 b^2-2 a b (a+b) x+(a^2+4 a b+b^2-d) x^2-2 (a+b) x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-a b+x^2) (a b-(a+b-d) x+x^2))/(x (x^2 (-a+x) (-b+x))^(1/3) (a b-(a+b+e) x+x^2)),x]*)


(* ::Input:: *)
(*int[(2 a b x-3 a x^2+x^3)/((x^2 (-a+x) (-b+x))^(1/3) (-a^2+2 a x-(1+b d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(x (4 a b-3 (a+b) x+2 x^2))/((x^2 (-a+x) (-b+x))^(1/3) (-a b d+(a+b) d x-d x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(x (4 a b-3 (a+b) x+2 x^2))/((x^2 (-a+x) (-b+x))^(1/3) (-a b+(a+b) x-x^2+d x^4)),x]*)


(* ::Input:: *)
(*int[(2 a b x-3 a x^2+x^3)/((x^2 (-a+x) (-b+x))^(1/3) (-a^2 d+2 a d x-(b+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-a b+x^2)/((x^2 (-a+x) (-b+x))^(1/3) (a b d-(1+a d+b d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(a b-x^2)/((x^2 (-a+x) (-b+x))^(1/3) (a b-(a+b+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[((-a b+(2 a-b) x) (a^2-2 a x+x^2))/((x (-a+x) (-b+x))^(1/3) (a^4 d-4 a^3 d x+(-b^2+6 a^2 d) x^2+2 (b-2 a d) x^3+(-1+d) x^4)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x) (-2 a b+(a+b) x))/((x (-a+x) (-b+x))^(1/3) (a^2 b^2 d-2 a b (a+b) d x+(a^2+4 a b+b^2) d x^2-2 (a+b) d x^3+(-1+d) x^4)),x]*)


(* ::Input:: *)
(*int[((-a b+(2 a-b) x) (a^2-(2 a+b d) x+(1+d) x^2))/(x (-b+x) (x (-a+x) (-b+x))^(1/3) (-a^2+(2 a-b e) x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[((-a b+(2 a-b) x) (a^2 d-(b+2 a d) x+(1+d) x^2))/((a-x)^2 (x (-a+x) (-b+x))^(1/3) (a^2 e+(b-2 a e) x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[((-2 a b+(a+b) x) (a b d-(a+b) d x+(1+d) x^2))/((-a+x) (-b+x) (x (-a+x) (-b+x))^(1/3) (-a b e+(a+b) e x+(1-e) x^2)),x]*)


(* ::Input:: *)
(*int[(x (-b+x) (a b+(-2 a+b) x))/((x (-a+x) (-b+x))^(1/3) (-a^4+4 a^3 x+(-6 a^2+b^2 d) x^2+2 (2 a-b d) x^3+(-1+d) x^4)),x]*)


(* ::Input:: *)
(*int[(x^2 (-2 a b+(a+b) x))/((x (-a+x) (-b+x))^(1/3) (-a^2 b^2+2 a b (a+b) x-(a^2+4 a b+b^2) x^2+2 (a+b) x^3+(-1+d) x^4)),x]*)


(* ::Input:: *)
(*int[((-2 a b+(a+b) x) (a b-(a+b) x+(1+d) x^2))/(x^2 (x (-a+x) (-b+x))^(1/3) (-a b+(a+b) x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[(-a b+(2 a-b) x)/((x (-a+x) (-b+x))^(1/3) (-a^2+(2 a-b d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(-2 a b+(a+b) x)/((x (-a+x) (-b+x))^(1/3) (a b d-(a+b) d x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(a b+(-2 a+b) x)/((x (-a+x) (-b+x))^(1/3) (a^2 d+(b-2 a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(-2 a b+(a+b) x)/((x (-a+x) (-b+x))^(1/3) (-a b+(a+b) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((1+(-2+k) x) (-1+(-a+2 k) x+(a-k^2) x^2))/(((1-x) x (1-k x))^(1/3) (1-4 k x+(-b+6 k^2) x^2+(2 b-4 k^3) x^3+(-b+k^4) x^4)),x]*)


(* ::Input:: *)
(*int[((1+(-2+k) x) (-1+(-a+2 k) x+(a-k^2) x^2))/((1-x) x ((1-x) x (1-k x))^(1/3) (1-(b+2 k) x+(b+k^2) x^2)),x]*)


(* ::Input:: *)
(*int[((-1+x) x (1+(-2+k) x))/(((1-x) x (1-k x))^(1/3) (1-4 k x+(-b+6 k^2) x^2+(2 b-4 k^3) x^3+(-b+k^4) x^4)),x]*)


(* ::Input:: *)
(*int[(-1+(2-k) x)/(((1-x) x (1-k x))^(1/3) (1-(b+2 k) x+(b+k^2) x^2)),x]*)


(* ::Input:: *)
(*int[(1-(-3+2 k) x-(4+k) x^2+3 k x^3)/(((1-x) x (1-k x))^(1/3) (-1+(5+b) x-(10+b k) x^2+10 x^3-5 x^4+x^5)),x]*)


(* ::Input:: *)
(*int[(x (-1+k x) (-1+(-1+2 k) x))/(((1-x) x (1-k x))^(1/3) (-1+(4-c) x+(-6+b+2 c+c k) x^2+(4-c-2 b k-2 c k) x^3+(-1+c k+b k^2) x^4)),x]*)


(* ::Input:: *)
(*int[((-1+(-1+2 k) x) (1-4 x+(6+a) x^2-2 (2+a k) x^3+(1+a k^2) x^4))/(x ((1-x) x (1-k x))^(1/3) (-1+k x) (-1+4 x+(-6+b) x^2+(4-2 b k) x^3+(-1+b k^2) x^4)),x]*)


(* ::Input:: *)
(*int[((-1+(-1+2 k) x) (1-4 x+(6+a) x^2-2 (2+a k) x^3+(1+a k^2) x^4))/(x^2 ((1-x) x (1-k x))^(1/3) (-1+k x)^2 (1-(2+b) x+(1+b k) x^2)),x]*)


(* ::Input:: *)
(*int[((-1+(-1+2 k) x) (-1+(2-a) x+(-1+a k) x^2))/(((1-x) x (1-k x))^(1/3) (-1+4 x+(-6+b) x^2+(4-2 b k) x^3+(-1+b k^2) x^4)),x]*)


(* ::Input:: *)
(*int[((-1+(-1+2 k) x) (-1+(2-a) x+(-1+a k) x^2))/(x ((1-x) x (1-k x))^(1/3) (-1+k x) (1-(2+b) x+(1+b k) x^2)),x]*)


(* ::Input:: *)
(*int[(x (-1+k x) (-1+(-1+2 k) x))/(((1-x) x (1-k x))^(1/3) (-1+4 x+(-6+b) x^2+(4-2 b k) x^3+(-1+b k^2) x^4)),x]*)


(* ::Input:: *)
(*int[(-1+(-1+2 k) x)/(((1-x) x (1-k x))^(1/3) (1-(2+b) x+(1+b k) x^2)),x]*)


(* ::Input:: *)
(*int[(x^2 (8-7 (1+k) x+6 k x^2))/(((1-x) x (1-k x))^(1/3) (-b+b (1+k) x-b k x^2+x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x) x (-1+k x) (5-4 (1+k) x+3 k x^2))/(((1-x) x (1-k x))^(1/3) (-b+2 b (1+k) x-b (1+4 k+k^2) x^2+2 b k (1+k) x^3-b k^2 x^4+x^10)),x]*)


(* ::Input:: *)
(*int[(x (5-4 (1+k) x+3 k x^2))/(((1-x) x (1-k x))^(1/3) (-b+(b+b k) x-b k x^2+x^5)),x]*)


(* ::Input:: *)
(*int[((-2+(1+k) x) (a-a (1+k) x+(1+a k) x^2))/((-1+x) ((1-x) x (1-k x))^(1/3) (-1+k x) (b-b (1+k) x+(-1+b k) x^2)),x]*)


(* ::Input:: *)
(*int[((-1+x) (-1+k x) (-2+(1+k) x))/(((1-x) x (1-k x))^(1/3) (b-2 (b+b k) x+(b+4 b k+b k^2) x^2-2 b k (1+k) x^3+(-1+b k^2) x^4)),x]*)


(* ::Input:: *)
(*int[(-2+(1+k) x)/(((1-x) x (1-k x))^(1/3) (b-b (1+k) x+(-1+b k) x^2)),x]*)


(* ::Input:: *)
(*int[(1+(-2+3 k) x-(k+4 k^2) x^2+3 k^2 x^3)/(((1-x) x (1-k x))^(1/3) (-b+(1+5 b k) x-(1+10 b k^2) x^2+10 b k^3 x^3-5 b k^4 x^4+b k^5 x^5)),x]*)


(* ::Input:: *)
(*int[((1+(-2+k) x) (a-4 a k x+(1+6 a k^2) x^2-(2+4 a k^3) x^3+(1+a k^4) x^4))/(((1-x) x (1-k x))^(1/3) (-1+k x)^4 (b-(1+2 b k) x+(1+b k^2) x^2)),x]*)


(* ::Input:: *)
(*int[((1+(-2+k) x) (a+(1-2 a k) x+(-1+a k^2) x^2))/(((1-x) x (1-k x))^(1/3) (b-4 b k x+(-1+6 b k^2) x^2+(2-4 b k^3) x^3+(-1+b k^4) x^4)),x]*)


(* ::Input:: *)
(*int[((1+(-2+k) x) (a+(1-2 a k) x+(-1+a k^2) x^2))/(((1-x) x (1-k x))^(1/3) (-1+k x)^2 (b-(1+2 b k) x+(1+b k^2) x^2)),x]*)


(* ::Input:: *)
(*int[((1+(-2+k) x) (1-2 k x+k^2 x^2))/(((1-x) x (1-k x))^(1/3) (b-4 b k x+(-1+6 b k^2) x^2+(2-4 b k^3) x^3+(-1+b k^4) x^4)),x]*)


(* ::Input:: *)
(*int[(-1+(2-k) x)/(((1-x) x (1-k x))^(1/3) (b-(1+2 b k) x+(1+b k^2) x^2)),x]*)


(* ::Input:: *)
(*int[(1+(3-2 k) x-(4+k) x^2+3 k x^3)/(((1-x) x (1-k x))^(1/3) (-b+(1+5 b) x-(10 b+k) x^2+10 b x^3-5 b x^4+b x^5)),x]*)


(* ::Input:: *)
(*int[((-1+(-1+2 k) x) (-a+(-1+2 a) x+(-a+k) x^2))/(((1-x) x (1-k x))^(1/3) (-b+4 b x+(1-6 b) x^2+(4 b-2 k) x^3+(-b+k^2) x^4)),x]*)


(* ::Input:: *)
(*int[((-1+(-1+2 k) x) (-a+(-1+2 a) x+(-a+k) x^2))/((-1+x)^2 ((1-x) x (1-k x))^(1/3) (b-(1+2 b) x+(b+k) x^2)),x]*)


(* ::Input:: *)
(*int[((-1+(-1+2 k) x) (1-2 x+x^2))/(((1-x) x (1-k x))^(1/3) (-b+4 b x+(1-6 b) x^2+(4 b-2 k) x^3+(-b+k^2) x^4)),x]*)


(* ::Input:: *)
(*int[(-1+(-1+2 k) x)/(((1-x) x (1-k x))^(1/3) (b-(1+2 b) x+(b+k) x^2)),x]*)


(* ::Input:: *)
(*int[((8-7 (1+k) x+6 k x^2) (1-(1+k) x+k x^2+a x^8))/(x^6 ((1-x) x (1-k x))^(1/3) (-1+(1+k) x-k x^2+b x^8)),x]*)


(* ::Input:: *)
(*int[(x^10 (8-7 (1+k) x+6 k x^2))/(((1-x) x (1-k x))^(1/3) (-1+2 (1+k) x-(1+4 k+k^2) x^2+2 k (1+k) x^3-k^2 x^4+b x^16)),x]*)


(* ::Input:: *)
(*int[(x^2 (8-7 (1+k) x+6 k x^2))/(((1-x) x (1-k x))^(1/3) (-1+(1+k) x-k x^2+b x^8)),x]*)


(* ::Input:: *)
(*int[((5-4 (1+k) x+3 k x^2) (1-2 (1+k) x+(1+4 k+k^2) x^2-2 k (1+k) x^3+k^2 x^4+a x^10))/(x^9 ((1-x) x (1-k x))^(1/3) (-1+(1+k) x-k x^2+b x^5)),x]*)


(* ::Input:: *)
(*int[((5 x-4 (1+k) x^2+3 k x^3) (1-(1+k) x+k x^2+a x^5))/(((1-x) x (1-k x))^(1/3) (-1+2 (1+k) x-(1+4 k+k^2) x^2+2 (k+k^2) x^3-k^2 x^4+b x^10)),x]*)


(* ::Input:: *)
(*int[((5-4 (1+k) x+3 k x^2) (1-(1+k) x+k x^2+a x^5))/(x^4 ((1-x) x (1-k x))^(1/3) (-1+(1+k) x-k x^2+b x^5)),x]*)


(* ::Input:: *)
(*int[(x^6 (5-4 (1+k) x+3 k x^2))/(((1-x) x (1-k x))^(1/3) (-1+2 (1+k) x-(1+4 k+k^2) x^2+(2 k+2 k^2) x^3-k^2 x^4+b x^10)),x]*)


(* ::Input:: *)
(*int[(5 x-4 (1+k) x^2+3 k x^3)/(((1-x) x (1-k x))^(1/3) (-1+(1+k) x-k x^2+b x^5)),x]*)


(* ::Input:: *)
(*int[((-2+(1+k) x) (1-(1+k) x+(a+k) x^2))/(((1-x) x (1-k x))^(1/3) (1-2 (1+k) x+(1+c+4 k+k^2) x^2-(c+2 k+c k+2 k^2) x^3+(-b+c k+k^2) x^4)),x]*)


(* ::Input:: *)
(*int[(x^2 (-2+(1+k) x))/(((1-x) x (1-k x))^(1/3) (1-(1+k) x+(1+c+4 k+k^2) x^2-(c+2 k+c k+2 k^2) x^3+(-b+c k+k^2) x^4)),x]*)


(* ::Input:: *)
(*int[((-2+(1+k) x) (1-2 (1+k) x+(1+4 k+k^2) x^2-2 (k+k^2) x^3+(a+k^2) x^4))/(x^4 ((1-x) x (1-k x))^(1/3) (1-(1+k) x+(-b+k) x^2)),x]*)


(* ::Input:: *)
(*int[((-2+(1+k) x) (1-(1+k) x+(a+k) x^2))/(((1-x) x (1-k x))^(1/3) (1-(2+2 k) x+(1+4 k+k^2) x^2-2 (k+k^2) x^3+(-b+k^2) x^4)),x]*)


(* ::Input:: *)
(*int[((-2+(1+k) x) (1-(1+k) x+(a+k) x^2))/(x^2 ((1-x) x (1-k x))^(1/3) (1-(1+k) x+(-b+k) x^2)),x]*)


(* ::Input:: *)
(*int[(x^2 (-2+(1+k) x))/(((1-x) x (1-k x))^(1/3) (1-(2+2 k) x+(1+4 k+k^2) x^2-(2 k+2 k^2) x^3+(-b+k^2) x^4)),x]*)


(* ::Input:: *)
(*int[(-2+(1+k) x)/(((1-x) x (1-k x))^(1/3) (1-(1+k) x+(-b+k) x^2)),x]*)


(* ::Input:: *)
(*int[(2-(1+k) x)/(((1-x) x (1-k x))^(1/3) (1-(1+k) x)),x]*)


(* ::Input:: *)
(*int[((-b+x) (a^2+b^2 c-2 (a+b c) x+(1+c) x^2))/(((-a+x) (-b+x)^2)^(1/3) (a^4-b^4 d-4 (a^3-b^3 d) x+6 (a^2-b^2 d) x^2-4 (a-b d) x^3+(1-d) x^4)),x]*)


(* ::Input:: *)
(*int[((-b+x) (b^2+a^2 c-2 (b+a c) x+(1+c) x^2))/(((-a+x) (-b+x)^2)^(1/3) (-b^4+a^4 d+4 (b^3-a^3 d) x-6 (b^2-a^2 d) x^2+4 (b-a d) x^3+(-1+d) x^4)),x]*)


(* ::Input:: *)
(*int[(a^4+b^4 c-4 (a^3+b^3 c) x+6 (a^2+b^2 c) x^2-4 (a+b c) x^3+(1+c) x^4)/((b-x)^3 ((-a+x) (-b+x)^2)^(1/3) (-a^2+b^2 d+2 (a-b d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(-b^3+3 b^2 x-3 b x^2+x^3)/(((-a+x) (-b+x)^2)^(1/3) (-a^4+b^4 d+4 (a^3-b^3 d) x-6 (a^2-b^2 d) x^2+4 (a-b d) x^3+(-1+d) x^4)),x]*)


(* ::Input:: *)
(*int[((-b+x) (a^2-2 a x+x^2))/(((-a+x) (-b+x)^2)^(1/3) (-b^4+a^4 d+4 (b^3-a^3 d) x-6 (b^2-a^2 d) x^2+4 (b-a d) x^3+(-1+d) x^4)),x]*)


(* ::Input:: *)
(*int[((-b+x) (b^2+a^2 c-2 (b+a c) x+(1+c) x^2))/((a-x)^2 ((-a+x) (-b+x)^2)^(1/3) (-b^2+a^2 d+2 (b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(a^2+b^2 c-2 (a+b c) x+(1+c) x^2)/((b-x) ((-a+x) (-b+x)^2)^(1/3) (-a^2+b^2 d+2 (a-b d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(b-x)/(((-a+x) (-b+x)^2)^(1/3) (a^2-b^2 d-2 (a-b d) x+(1-d) x^2)),x]*)


(* ::Input:: *)
(*int[(-b+x)/(((-a+x) (-b+x)^2)^(1/3) (-b^2+a^2 d+2 (b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(a^2+b^2 c-2 (a+b c) x+(1+c) x^2)/((-b+x) ((-a+x) (-b+x)^2)^(1/3) (-a^2+b^2 d+2 (a-b d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(b^2+a^2 c-2 (b+a c) x+(1+c) x^2)/((-a+x) ((-a+x) (-b+x)^2)^(1/3) (-b^2+a^2 d+2 (b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-4 a+b+3 x))/(((-a+x) (-b+x)^2)^(1/3) (a+b^4 d-(1+4 b^3 d) x+6 b^2 d x^2-4 b d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-4 a+b+3 x))/(((-a+x) (-b+x)^2)^(1/3) (b^4+a d-(4 b^3+d) x+6 b^2 x^2-4 b x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(a^2+b^2 c-2 (a+b c) x+(1+c) x^2)/((-b+x)^2 ((-a+x) (-b+x)^2)^(1/3) (a-b d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[(b^2+a^2 c-2 (b+a c) x+(1+c) x^2)/((a-x)^2 ((-a+x) (-b+x)^2)^(1/3) (-b+a d+(1-d) x)),x]*)


(* ::Input:: *)
(*int[(-a-b c+(1+c) x)/(((-a+x) (-b+x)^2)^(1/3) (-a^2+b^2 d+2 (a-b d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(-b-a c+(1+c) x)/(((-a+x) (-b+x)^2)^(1/3) (-b^2+a^2 d+2 (b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(-b-a c+(1+c) x)/((-a+x) ((-a+x) (-b+x)^2)^(1/3) (b-a d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[(-b+x)/(((-a+x) (-b+x)^2)^(1/3) (-a^2+b^2 d+2 (a-b d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(-a+x)/(((-a+x) (-b+x)^2)^(1/3) (-b^2+a^2 d+2 (b-a d) x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(-a-b c+(1+c) x)/((-b+x) ((-a+x) (-b+x)^2)^(1/3) (a-b d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[1/(((-a+x) (-b+x)^2)^(1/3) (b-a d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[1/(((-a+x) (-b+x)^2)^(1/3) (a-b d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[x^3/((x^2 (-a+x))^(1/3) (-a^4+4 a^3 x-6 a^2 x^2+4 a x^3+(-1+d) x^4)),x]*)


(* ::Input:: *)
(*int[(a^2 d-2 a d x+(1+d) x^2)/((a-x)^2 (x^2 (-a+x))^(1/3) (a e+(1-e) x)),x]*)


(* ::Input:: *)
(*int[(x (-a+x) (-4 a+3 x))/((x^2 (-a+x))^(1/3) (-a^2 d+2 a d x-d x^2+x^8)),x]*)


(* ::Input:: *)
(*int[(a^2-2 a x+(1+d) x^2)/(x (x^2 (-a+x))^(1/3) (-a^2+2 a x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[(a^2-2 a x+(1+d) x^2)/(x (x^2 (-a+x))^(1/3) (-a^2+2 a x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[(x^5 (-4 a+3 x))/((x^2 (-a+x))^(1/3) (-a^2+2 a x-x^2+d x^8)),x]*)


(* ::Input:: *)
(*int[((-4 a+3 x) (-a+x+d x^4))/(x^3 (x^2 (-a+x))^(1/3) (a-x+e x^4)),x]*)


(* ::Input:: *)
(*int[(-a d+(1+d) x)/((x^2 (-a+x))^(1/3) (a^2 e-2 a e x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[(-a d+(1+d) x)/((-a+x) (x^2 (-a+x))^(1/3) (a e+(1-e) x)),x]*)


(* ::Input:: *)
(*int[(-a+(1+d) x)/((x^2 (-a+x))^(1/3) (-a^2+2 a x+(-1+e) x^2)),x]*)


(* ::Input:: *)
(*int[(a^2-2 a x+(1+d) x^2)/(x^2 (x^2 (-a+x))^(1/3) (a+(-1+e) x)),x]*)


(* ::Input:: *)
(*int[(-a+x)/((x^2 (-a+x))^(1/3) (a^2 d-2 a d x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(x (-4 a+3 x))/((x^2 (-a+x))^(1/3) (a d-d x+x^4)),x]*)


(* ::Input:: *)
(*int[x/((x^2 (-a+x))^(1/3) (a^2 d-2 a d x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[x/((x^2 (-a+x))^(1/3) (-a^2+2 a x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[x/((x^2 (-a+x))^(1/3) (-a^2+2 a x+(-1+d) x^2)),x]*)


(* ::Input:: *)
(*int[(-a+(1+d) x)/(x (x^2 (-a+x))^(1/3) (a+(-1+e) x)),x]*)


(* ::Input:: *)
(*int[(x (-4 a+3 x))/((x^2 (-a+x))^(1/3) (a-x+d x^4)),x]*)


(* ::Input:: *)
(*int[1/((x^2 (-a+x))^(1/3) (-a d+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[1/((x^2 (-a+x))^(1/3) (a+(-1+d) x)),x]*)


(* ::Input:: *)
(*int[((a-2 b+x) (b^2+a^4 c-2 (b+2 a^3 c) x+(1+6 a^2 c) x^2-4 a c x^3+c x^4))/((a-x)^4 ((-a+x) (-b+x))^(1/3) (b+a^2 d-(1+2 a d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[((a-2 b+x) (-b+a^2 c+(1-2 a c) x+c x^2))/(((-a+x) (-b+x))^(1/3) (-b^2+a^4 d+2 (b-2 a^3 d) x+(-1+6 a^2 d) x^2-4 a d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[((a-2 b+x) (a^4+b^2 c-2 (2 a^3+b c) x+(6 a^2+c) x^2-4 a x^3+x^4))/((-b+x)^2 ((-a+x) (-b+x))^(1/3) (a^2+b d-(2 a+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(-a (a-5 b)-(3 a+5 b) x+4 x^2)/(((-a+x) (-b+x))^(1/3) (b-a^5 d-(1-5 a^4 d) x-10 a^3 d x^2+10 a^2 d x^3-5 a d x^4+d x^5)),x]*)


(* ::Input:: *)
(*int[((a-2 b+x) (a^2-b c+(-2 a+c) x+x^2))/(((-a+x) (-b+x))^(1/3) (a^4-b^2 d-2 (2 a^3-b d) x+(6 a^2-d) x^2-4 a x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(-a (a-5 b)-(3 a+5 b) x+4 x^2)/(((-a+x) (-b+x))^(1/3) (-a^5+b d-(-5 a^4+d) x-10 a^3 x^2+10 a^2 x^3-5 a x^4+x^5)),x]*)


(* ::Input:: *)
(*int[((a-2 b+x) (a^2-2 a x+x^2))/(((-a+x) (-b+x))^(1/3) (-b^2+a^4 d+2 (b-2 a^3 d) x+(-1+6 a^2 d) x^2-4 a d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[((a-2 b+x) (-b+x))/(((-a+x) (-b+x))^(1/3) (a^4-b^2 d-2 (2 a^3-b d) x+(6 a^2-d) x^2-4 a x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((a-2 b+x) (-b+a^2 c+(1-2 a c) x+c x^2))/((a-x)^2 ((-a+x) (-b+x))^(1/3) (b+a^2 d-(1+2 a d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[((a-2 b+x) (a^2-b c+(-2 a+c) x+x^2))/((-b+x) ((-a+x) (-b+x))^(1/3) (a^2+b d-(2 a+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(a-2 b+x)/(((-a+x) (-b+x))^(1/3) (a^2+b d-(2 a+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(a-2 b+x)/(((-a+x) (-b+x))^(1/3) (b+a^2 d-(1+2 a d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[((-q+2 p x^3) Sqrt[q^2-2 p q x^2+2 p q x^3+p^2 x^6] (b x^6+a (q+p x^3)^6))/x^9,x]*)


(* ::Input:: *)
(*int[((-q+2 p x^3) Sqrt[q^2-2 p q x^2+2 p q x^3+p^2 x^6] (b x^4+a (q+p x^3)^4))/x^7,x]*)


(* ::Input:: *)
(*int[((-q+2 p x^3) Sqrt[q^2-2 p q x^2+2 p q x^3+p^2 x^6] (b x^3+a (q+p x^3)^3))/x^6,x]*)


(* ::Input:: *)
(*int[((-q+2 p x^3) Sqrt[q^2-2 p q x^2+2 p q x^3+p^2 x^6] (b q x+c x^2+b p x^4+a (q+p x^3)^2))/x^5,x]*)


(* ::Input:: *)
(*int[((-q+2 p x^3) Sqrt[q^2-2 p q x^2+2 p q x^3+p^2 x^6] (b x^2+a (q+p x^3)^2))/x^5,x]*)


(* ::Input:: *)
(*int[((-q+2 p x^3) Sqrt[q^2-2 p q x^2+2 p q x^3+p^2 x^6])/(x (b x^2+a (q+p x^3)^2)),x]*)


(* ::Input:: *)
(*int[((-q+2 p x^3) (a q+b x+a p x^3) Sqrt[q^2-2 p q x^2+2 p q x^3+p^2 x^6])/(x^3 (c q+d x+c p x^3)),x]*)


(* ::Input:: *)
(*int[((-q+2 p x^3) Sqrt[q^2-2 p q x^2+2 p q x^3+p^2 x^6])/(x^2 (a q+b x+a p x^3)),x]*)


(* ::Input:: *)
(*int[((-q+2 p x^3) (a q+b x+a p x^3) Sqrt[q^2-2 p q x^2+2 p q x^3+p^2 x^6])/x^4,x]*)


(* ::Input:: *)
(*int[((-2 q+p x^3) Sqrt[q^2+2 p q x^3-2 p q x^4+p^2 x^6] (b x^12+a (q+p x^3)^6))/x^17,x]*)


(* ::Input:: *)
(*int[((-2 q+p x^3) Sqrt[q^2+2 p q x^3-2 p q x^4+p^2 x^6] (b x^8+a (q+p x^3)^4))/x^13,x]*)


(* ::Input:: *)
(*int[((-2 q+p x^3) Sqrt[q^2+2 p q x^3-2 p q x^4+p^2 x^6] (b x^6+a (q+p x^3)^3))/x^11,x]*)


(* ::Input:: *)
(*int[((-2 q+p x^3) Sqrt[q^2+2 p q x^3-2 p q x^4+p^2 x^6] (c x^4+b x^2 (q+p x^3)+a (q+p x^3)^2))/x^9,x]*)


(* ::Input:: *)
(*int[((-2 q+p x^3) Sqrt[q^2+2 p q x^3-2 p q x^4+p^2 x^6] (a q^2+2 a p q x^3+b x^4+a p^2 x^6))/x^9,x]*)


(* ::Input:: *)
(*int[((-2 q+p x^3) Sqrt[q^2+2 p q x^3-2 p q x^4+p^2 x^6])/(x (b x^4+a (q+p x^3)^2)),x]*)


(* ::Input:: *)
(*int[((-2 q+p x^3) (a q+b x^2+a p x^3) Sqrt[q^2+2 p q x^3-2 p q x^4+p^2 x^6])/(x^5 (c q+d x^2+c p x^3)),x]*)


(* ::Input:: *)
(*int[((-2 q+p x^3) Sqrt[q^2+2 p q x^3-2 p q x^4+p^2 x^6])/(x^3 (a q+b x^2+a p x^3)),x]*)


(* ::Input:: *)
(*int[((-2 q+p x^3) (a q+b x^2+a p x^3) Sqrt[q^2+2 p q x^3-2 p q x^4+p^2 x^6])/x^7,x]*)


(* ::Input:: *)
(*int[((1+x^6) Sqrt[-2-x^2+x^6])/(4-3 x^4-4 x^6+x^12),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1-x^2+x^6] (1+2 x^6))/(8-x^4-16 x^6+8 x^12),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+2 x^6] (-1+4 x^6))/(2+x^4+8 x^6+8 x^12),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2-2 x^6] (1+4 x^6))/((-1-4 x^2+2 x^6) (-1-2 x^2+2 x^6)),x]*)


(* ::Input:: *)
(*int[((-3+2 x^5) Sqrt[x+2 x^4+x^6])/((1+x^5) (1+x^3+x^5)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1-x^2+x^6] (1+2 x^6))/((-1+x^6) (-2+x^2+2 x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^2+x^6] (-1+2 x^6))/((1+x^6) (2-x^2+2 x^6)),x]*)


(* ::Input:: *)
(*int[(-2 x+3 x^2)/Sqrt[5-4 x^2+4 x^3+x^4-2 x^5+x^6],x]*)


(* ::Input:: *)
(*int[(4 x+3 x^2)/Sqrt[-5+4 x^2+2 x^3+4 x^4+4 x^5+x^6],x]*)


(* ::Input:: *)
(*int[(-1+2 x^6)/((1+x^6) Sqrt[1-2 x^2+x^6]),x]*)


(* ::Input:: *)
(*int[(1+2 x^6)/((-1+x^6) Sqrt[-1-2 x^2+x^6]),x]*)


(* ::Input:: *)
(*int[(Sqrt[1-x^6] (2+x^6))/(x^3 (-1+x^4+x^6)),x]*)


(* ::Input:: *)
(*int[(-1+2 x^6)/(Sqrt[1+x^6] (1-x^2+x^6)),x]*)


(* ::Input:: *)
(*int[(x+4 x^6)/((-1+x^5) (-a-x+a x^5) Sqrt[-x+x^6]),x]*)


(* ::Input:: *)
(*int[(x+4 x^6)/(Sqrt[-x+x^6] (a-x^2-2 a x^5+a x^10)),x]*)


(* ::Input:: *)
(*int[(x+4 x^6)/(Sqrt[-x+x^6] (1-a x^2-2 x^5+x^10)),x]*)


(* ::Input:: *)
(*int[((-1+x^5) (1+4 x^5))/(x (-1-a x+x^5) Sqrt[-x+x^6]),x]*)


(* ::Input:: *)
(*int[(x^4 (-9+4 x^5))/(Sqrt[-x+x^6] (1-x^5+a x^9)),x]*)


(* ::Input:: *)
(*int[(x^4 (-9+4 x^5))/(Sqrt[-x+x^6] (a-a x^5+x^9)),x]*)


(* ::Input:: *)
(*int[(1+4 x^5)/((-a-x+a x^5) Sqrt[-x+x^6]),x]*)


(* ::Input:: *)
(*int[(1+4 x^5)/((-1-a x+x^5) Sqrt[-x+x^6]),x]*)


(* ::Input:: *)
(*int[x Sqrt[-x+x^6],x]*)


(* ::Input:: *)
(*int[(-x+4 x^6)/(Sqrt[x+x^6] (a-x^2+2 a x^5+a x^10)),x]*)


(* ::Input:: *)
(*int[((1+x^5) (9+4 x^5))/(x^5 Sqrt[x+x^6] (-1-x^5+a x^9)),x]*)


(* ::Input:: *)
(*int[(x^2 (-1+4 x^5))/((1+x^5)^2 (a-x+a x^5) Sqrt[x+x^6]),x]*)


(* ::Input:: *)
(*int[(-x+4 x^6)/((1+x^5) (a-x+a x^5) Sqrt[x+x^6]),x]*)


(* ::Input:: *)
(*int[(x^4 (9+4 x^5))/(Sqrt[x+x^6] (-a-a x^5+x^9)),x]*)


(* ::Input:: *)
(*int[((1+x^5) (-1+4 x^5))/(x (1-a x+x^5) Sqrt[x+x^6]),x]*)


(* ::Input:: *)
(*int[(x-4 x^6)/(Sqrt[x+x^6] (1-a x^2+2 x^5+x^10)),x]*)


(* ::Input:: *)
(*int[(x^4 (9+4 x^5))/(Sqrt[x+x^6] (-1-x^5+a x^9)),x]*)


(* ::Input:: *)
(*int[(-1+4 x^5)/((a-x+a x^5) Sqrt[x+x^6]),x]*)


(* ::Input:: *)
(*int[(-1+4 x^5)/((1-a x+x^5) Sqrt[x+x^6]),x]*)


(* ::Input:: *)
(*int[x Sqrt[x+x^6],x]*)


(* ::Input:: *)
(*int[(x^9 (-2 q+p x^6) Sqrt[q+p x^6])/(b^8 x^16+a^8 (q+p x^6)^4),x]*)


(* ::Input:: *)
(*int[(x (-2 q+p x^6) Sqrt[q+p x^6])/(a q^2+b q x^4+2 a p q x^6+c x^8+b p x^10+a p^2 x^12),x]*)


(* ::Input:: *)
(*int[(x (-2 q+p x^6) Sqrt[q+p x^6])/(b x^8+a (q+p x^6)^2),x]*)


(* ::Input:: *)
(*int[((-2 q+p x^6) Sqrt[q+p x^6])/(x^3 (a q+b x^4+a p x^6)),x]*)


(* ::Input:: *)
(*int[(2 (-2 q+p x^6) Sqrt[q+p x^6] (a q+b x^4+a p x^6))/x^11,x]*)


(* ::Input:: *)
(*int[((1-x^2+x^4) Sqrt[1+x^6])/(-1-2 x^2+2 x^4),x]*)


(* ::Input:: *)
(*int[(x^2+x^4)/((-1-2 x^2+2 x^4) Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[(1+2 x^4)/((-1+2 x^2) Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[(-1-2 x^2+2 x^4)/((2-3 x^2+x^4) Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[(-2-3 x^2+2 x^4)/((-1+2 x^2) Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[(-1-2 x^2+2 x^4)/((1+2 x^4) Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[(-4+13 x^6)/(x Sqrt[-1+x^6] (-1+4 x^6)),x]*)


(* ::Input:: *)
(*int[(-x^2+10 x^8)/(Sqrt[-1+x^6] (-1+4 x^6)),x]*)


(* ::Input:: *)
(*int[(-1+10 x^6)/(x Sqrt[-1+x^6] (-1+4 x^6)),x]*)


(* ::Input:: *)
(*int[(x^2 (-4+x^6))/(Sqrt[-1+x^6] (2+x^6)),x]*)


(* ::Input:: *)
(*int[(-2+x^6)/(x Sqrt[-1+x^6] (2+x^6)),x]*)


(* ::Input:: *)
(*int[(-2+5 x^6)/(x Sqrt[-1+x^6] (2+x^6)),x]*)


(* ::Input:: *)
(*int[(-1+2 x^3)/(x Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[(1+2 x^3)/(x Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[(-2+x^3)/(x Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[x^2/Sqrt[1+x^6],x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^6] (-1+2 x^6)^2)/(x^7 (-1+4 x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^6] (-1+2 x^6)^2)/(x^4 (-1+4 x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^6] (-1+2 x^6)^2)/(x (-1+4 x^6)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) Sqrt[-1+x^6])/(x^7 (2+x^6)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) Sqrt[-1+x^6])/(x^4 (2+x^6)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) Sqrt[-1+x^6])/(x (2+x^6)),x]*)


(* ::Input:: *)
(*int[((1+x^3) Sqrt[-1+x^6])/(x^13 (-1+x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^3) Sqrt[-1+x^6])/(x^7 (-1+x^3)),x]*)


(* ::Input:: *)
(*int[((1+x^3) Sqrt[-1+x^6])/(x (-1+x^3)),x]*)


(* ::Input:: *)
(*int[(1+x^12)/(x^10 Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[(1+x^12)/(x^4 Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[((-1+x^3) Sqrt[-1+x^6])/x^13,x]*)


(* ::Input:: *)
(*int[((-1+x^3) Sqrt[-1+x^6])/x^7,x]*)


(* ::Input:: *)
(*int[((-1+x^3) Sqrt[-1+x^6])/x,x]*)


(* ::Input:: *)
(*int[x^20/Sqrt[1+x^6],x]*)


(* ::Input:: *)
(*int[x^14/Sqrt[1+x^6],x]*)


(* ::Input:: *)
(*int[x^8/Sqrt[1+x^6],x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^6]/x^13,x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^6]/x^7,x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^6]/x^4,x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^6]/x,x]*)


(* ::Input:: *)
(*int[((1+2 x^2+4 x^4) Sqrt[1+x^6])/(x^2 (1+x^2)),x]*)


(* ::Input:: *)
(*int[(-1+x^6)/(x^19 Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[(-1+x^6)/(x^13 Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[(-1+x^6)/(x^7 Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[(-1+x^6)/(x Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[(-1-2 x^2+2 x^4)/((1-x^2+x^4) Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[(-1-2 x^2+2 x^4)/(x^2 (1+x^2) Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[(1+x^12)/(x^16 Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[1/(x^4 Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[(-2+x^6)/(x^3 Sqrt[1+x^6]),x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^6]/x^10,x]*)


(* ::Input:: *)
(*int[((1+x^2+x^4) Sqrt[-1+x^6])/(-1+2 x^2+2 x^4),x]*)


(* ::Input:: *)
(*int[(1+x^2+x^4)/((-1+2 x^2+2 x^4) Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[(-1+2 x^2+2 x^4)/((1+2 x^2) Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[(x^2 (-4+x^6))/(Sqrt[-1+x^6] (2+x^6)),x]*)


(* ::Input:: *)
(*int[(-2+x^6)/(x Sqrt[-1+x^6] (2+x^6)),x]*)


(* ::Input:: *)
(*int[(-2+5 x^6)/(x Sqrt[-1+x^6] (2+x^6)),x]*)


(* ::Input:: *)
(*int[(-1+2 x^3)/(x Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[(1+4 x^3)/(x Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[(1+x^3)/(x Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[((-1+2 x^3) (1+2 x^3))/(Sqrt[-1+x^6] (-1-x^2+x^8)),x]*)


(* ::Input:: *)
(*int[(x (2+x^6))/(Sqrt[-1+x^6] (-1-x^4+x^6)),x]*)


(* ::Input:: *)
(*int[(1+2 x^6)/(Sqrt[-1+x^6] (-1+x^2+x^6)),x]*)


(* ::Input:: *)
(*int[x^2/Sqrt[-1+x^6],x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[((1-2 x^2+4 x^4) Sqrt[-1+x^6])/(x^2 (-1+x^2)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) Sqrt[-1+x^6])/(x^7 (2+x^6)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) Sqrt[-1+x^6])/(x^4 (2+x^6)),x]*)


(* ::Input:: *)
(*int[((-2+x^6) Sqrt[-1+x^6])/(x (2+x^6)),x]*)


(* ::Input:: *)
(*int[((-1+x^3) Sqrt[-1+x^6])/(x^7 (1+x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x^3) Sqrt[-1+x^6])/(x^4 (1+x^3)),x]*)


(* ::Input:: *)
(*int[((-1+x^3) Sqrt[-1+x^6])/(x (1+x^3)),x]*)


(* ::Input:: *)
(*int[((1+2 x^3) Sqrt[-1+x^6])/x^7,x]*)


(* ::Input:: *)
(*int[((1+2 x^3) Sqrt[-1+x^6])/x^4,x]*)


(* ::Input:: *)
(*int[((1+2 x^3) Sqrt[-1+x^6])/x,x]*)


(* ::Input:: *)
(*int[((-1+x^3) Sqrt[-1+x^6])/x^10,x]*)


(* ::Input:: *)
(*int[((-1+x^3) Sqrt[-1+x^6])/x^7,x]*)


(* ::Input:: *)
(*int[((-1+x^3) Sqrt[-1+x^6])/(x^7 (1+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^6] (1+2 x^6))/(x^2 (-1+x^2+x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^6] (2+x^6))/(x^3 (-1-x^4+x^6)),x]*)


(* ::Input:: *)
(*int[Sqrt[-1+x^6]/x^19,x]*)


(* ::Input:: *)
(*int[Sqrt[-1+x^6]/x^13,x]*)


(* ::Input:: *)
(*int[Sqrt[-1+x^6]/x^7,x]*)


(* ::Input:: *)
(*int[Sqrt[-1+x^6]/x^4,x]*)


(* ::Input:: *)
(*int[Sqrt[-1+x^6]/x,x]*)


(* ::Input:: *)
(*int[1/(x^19 Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[(1+x^6)/(x^13 Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[1/(x^13 Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[1/(x^7 Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[x^20/Sqrt[-1+x^6],x]*)


(* ::Input:: *)
(*int[x^14/Sqrt[-1+x^6],x]*)


(* ::Input:: *)
(*int[x^8/Sqrt[-1+x^6],x]*)


(* ::Input:: *)
(*int[x^20 Sqrt[-1+x^6],x]*)


(* ::Input:: *)
(*int[x^14 Sqrt[-1+x^6],x]*)


(* ::Input:: *)
(*int[x^8 Sqrt[-1+x^6],x]*)


(* ::Input:: *)
(*int[x^2 Sqrt[-1+x^6],x]*)


(* ::Input:: *)
(*int[(1+x^12)/(x^16 Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[(1+x^6)/(x^10 Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[1/(x^10 Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[1/(x^4 Sqrt[-1+x^6]),x]*)


(* ::Input:: *)
(*int[Sqrt[-1+x^6]/x^16,x]*)


(* ::Input:: *)
(*int[Sqrt[-1+x^6]/x^10,x]*)


(* ::Input:: *)
(*int[x^5 Sqrt[-1+x^6],x]*)


(* ::Input:: *)
(*int[(x^13 (-9+5 x^4))/(Sqrt[-x+x^5] (-1+2 x^4-x^8+a x^18)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (-9+5 x^4))/(x^5 Sqrt[-x+x^5] (1-x^4+a x^9)),x]*)


(* ::Input:: *)
(*int[(x+3 x^5)/((-1+x^4) (-a-x+a x^4) Sqrt[-x+x^5]),x]*)


(* ::Input:: *)
(*int[(x+3 x^5)/(Sqrt[-x+x^5] (a-x^2-2 a x^4+a x^8)),x]*)


(* ::Input:: *)
(*int[(x+3 x^5)/(Sqrt[-x+x^5] (1-a x^2-2 x^4+x^8)),x]*)


(* ::Input:: *)
(*int[((-1+x^4) (1+3 x^4))/(x (-1-a x+x^4) Sqrt[-x+x^5]),x]*)


(* ::Input:: *)
(*int[(x^4 (-9+5 x^4))/(Sqrt[-x+x^5] (1-x^4+a x^9)),x]*)


(* ::Input:: *)
(*int[(x^4 (-9+5 x^4))/(Sqrt[-x+x^5] (a-a x^4+x^9)),x]*)


(* ::Input:: *)
(*int[(1+3 x^4)/((-a-x+a x^4) Sqrt[-x+x^5]),x]*)


(* ::Input:: *)
(*int[(1+3 x^4)/((-1-a x+x^4) Sqrt[-x+x^5]),x]*)


(* ::Input:: *)
(*int[(-x+3 x^5)/(Sqrt[x+x^5] (a-x^2+2 a x^4+a x^8)),x]*)


(* ::Input:: *)
(*int[(x^2 (-1+3 x^4))/((1+x^4)^2 (a-x+a x^4) Sqrt[x+x^5]),x]*)


(* ::Input:: *)
(*int[(-x+3 x^5)/((1+x^4) (a-x+a x^4) Sqrt[x+x^5]),x]*)


(* ::Input:: *)
(*int[(x^4 (9+5 x^4))/(Sqrt[x+x^5] (-a-a x^4+x^9)),x]*)


(* ::Input:: *)
(*int[((1+x^4) (-1+3 x^4))/(x (1-a x+x^4) Sqrt[x+x^5]),x]*)


(* ::Input:: *)
(*int[(x-3 x^5)/(Sqrt[x+x^5] (1-a x^2+2 x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(x^4 (9+5 x^4))/(Sqrt[x+x^5] (-1-x^4+a x^9)),x]*)


(* ::Input:: *)
(*int[(-1+3 x^4)/((a-x+a x^4) Sqrt[x+x^5]),x]*)


(* ::Input:: *)
(*int[(-1+3 x^4)/((1-a x+x^4) Sqrt[x+x^5]),x]*)


(* ::Input:: *)
(*int[(Sqrt[q+p x^5] (-2 q+3 p x^5))/(c x^4+b x^2 (q+p x^5)+a (q+p x^5)^2),x]*)


(* ::Input:: *)
(*int[(Sqrt[q+p x^5] (-2 q+3 p x^5))/(b x^4+a (q+p x^5)^2),x]*)


(* ::Input:: *)
(*int[(Sqrt[q+p x^5] (-2 q+3 p x^5) (a q+b x^2+a p x^5))/(x^4 (c q+d x^2+c p x^5)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^5] (-2+3 x^5))/(a-x^4+2 a x^5+a x^10),x]*)


(* ::Input:: *)
(*int[(x^4 (3+x^5))/(Sqrt[1+x^5] (-1+a-(1+2 a) x^5+a x^10)),x]*)


(* ::Input:: *)
(*int[(x^2 Sqrt[q+p x^5] (-2 q+3 p x^5))/(b x^6+a (q+p x^5)^3),x]*)


(* ::Input:: *)
(*int[(Sqrt[q+p x^5] (-2 q+3 p x^5))/(x^2 (a q+b x^2+a p x^5)),x]*)


(* ::Input:: *)
(*int[(-2 b+3 a x^5)/(Sqrt[b+a x^5] (b+x^2+a x^5)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^5] (2+x^5))/(x^6 (-1-x^5+a x^10)),x]*)


(* ::Input:: *)
(*int[(x^4 (2+x^5))/(Sqrt[1+x^5] (-1-x^5+a x^10)),x]*)


(* ::Input:: *)
(*int[(-2+3 x^5)/(Sqrt[1+x^5] (a-x^2+a x^5)),x]*)


(* ::Input:: *)
(*int[(2-3 x^5)/(Sqrt[1+x^5] (1-a x^2+x^5)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^5] (2+3 x^5))/(a-x^4-2 a x^5+a x^10),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^5] (2+3 x^5))/(1-a x^4-2 x^5+x^10),x]*)


(* ::Input:: *)
(*int[(2 x^4-x^9)/(Sqrt[-1+x^5] (a-a x^5+x^10)),x]*)


(* ::Input:: *)
(*int[(2+3 x^5)/(Sqrt[-1+x^5] (-a-x^2+a x^5)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^5] (2+3 x^5))/(x^2 (-1-a x^2+x^5)),x]*)


(* ::Input:: *)
(*int[((-2+x^5) Sqrt[-1+x^5])/(x^6 (1-x^5+a x^10)),x]*)


(* ::Input:: *)
(*int[(x^4 (-2+x^5))/(Sqrt[-1+x^5] (1-x^5+a x^10)),x]*)


(* ::Input:: *)
(*int[(2+3 x^5)/(Sqrt[-1+x^5] (-1-a x^2+x^5)),x]*)


(* ::Input:: *)
(*int[(1+3 x)/Sqrt[-1-4 x-5 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+3 x)/Sqrt[-1+4 x-5 x^2+2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[x/Sqrt[1-4 x+3 x^2+2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[x/Sqrt[1+4 x+3 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(1+6 x)/Sqrt[1-2 x-5 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(2+5 x)/Sqrt[-12+20 x+5 x^2+2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+x)/Sqrt[-7+4 x+14 x^2-12 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+x)/Sqrt[10-12 x+7 x^2-6 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+x)/Sqrt[4-16 x+12 x^2-8 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+2 x)/Sqrt[-8+4 x-3 x^2-10 x^3+x^4],x]*)


(* ::Input:: *)
(*int[x/Sqrt[-17+18 x-11 x^2+6 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+x)/Sqrt[-2-x+6 x^2-4 x^3+x^4],x]*)


(* ::Input:: *)
(*int[x/Sqrt[11-11 x-3 x^2+2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(1+x)/Sqrt[2-5 x-3 x^2+2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(1+x)/Sqrt[16+18 x+13 x^2+4 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+2 x)/Sqrt[-8 x+9 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(2+x)/Sqrt[13+16 x^2+8 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+2 x)/Sqrt[13+x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(1+2 x)/Sqrt[3+x^2+2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+2 x)/Sqrt[4+x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[((1+x^4) Sqrt[-1-x^2+x^4])/(1-x^2-3 x^4+x^6+x^8),x]*)


(* ::Input:: *)
(*int[((-1+x^2)^2 (x+x^3))/(Sqrt[1+x^4] (1-2 x^2+4 x^4-2 x^6+x^8)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2-2 x^3+x^4] (1-x^3+x^4))/((-1-2 x^3+x^4) (-2-x^2-4 x^3+2 x^4)),x]*)


(* ::Input:: *)
(*int[((3+4 x) Sqrt[x+2 x^2-2 x^4])/((1+2 x) (1+2 x+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1-x-x^2+x^4] (2+x+2 x^4))/((-1-x+x^4) (-1-x+x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-3+2 x) Sqrt[-2 x+2 x^2+3 x^4])/(-2+2 x+x^3)^2,x]*)


(* ::Input:: *)
(*int[((-1+2 x^4) Sqrt[1+3 x^2+2 x^4])/(1+2 x^2+2 x^4)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x+x^2+x^4] (-2-x+2 x^4))/(1+x+x^4)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x-x^2+x^4] (2-x+2 x^4))/(-1+x+x^4)^2,x]*)


(* ::Input:: *)
(*int[((-1+x) (1+x)^3)/((1+x^2)^2 Sqrt[1+x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(Sqrt[-2+x^4] (2+x^4))/(4-6 x^4+x^8),x]*)


(* ::Input:: *)
(*int[(-3 x+2 x^2)/((-2+2 x+x^3) Sqrt[-2 x+2 x^2+3 x^4]),x]*)


(* ::Input:: *)
(*int[Sqrt[1+6 x^2+x^4]/(x (1+x^2)),x]*)


(* ::Input:: *)
(*int[(-1+2 x)/Sqrt[-2-2 x+3 x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+x)/Sqrt[-1+4 x+2 x^2-4 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(1+2 x)/Sqrt[-4-4 x-3 x^2+2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+2 x^4)/((1+2 x^2+2 x^4) Sqrt[1+3 x^2+2 x^4]),x]*)


(* ::Input:: *)
(*int[(-2-x+2 x^4)/((1+x+x^4) Sqrt[1+x+x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x)/Sqrt[-5+4 x^2-4 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+x)/Sqrt[-5+4 x^2-4 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+2 x)/Sqrt[-3+x^2-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(-1+2 x)/Sqrt[1+x-2 x^3+x^4],x]*)


(* ::Input:: *)
(*int[(1+2 x^4)/((-1+2 x^4) Sqrt[-1-x^2+2 x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[1+x^2+x^4]),x]*)


(* ::Input:: *)
(*int[((-1+x^2) Sqrt[1-x-x^2+x^3+x^4])/(x^2 (1+x^2)),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[1-x-x^2+x^3+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/((1+x^4) Sqrt[1-x-x^2+x^3+x^4]),x]*)


(* ::Input:: *)
(*int[(1+x^4)/((-1+x^4) Sqrt[1-x-x^2+x^3+x^4]),x]*)


(* ::Input:: *)
(*int[(-2-x+2 x^2)/((1+x^2) Sqrt[1+x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) Sqrt[1-x-x^2+x^3+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+2 x^2)/((1+x^2) Sqrt[-1-x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(1+x^4)/((-1+x^4) Sqrt[-1-x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(2+x^2)/((-1+x^2) Sqrt[-1-x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+x) Sqrt[1+x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/((1+x^4) Sqrt[1+x^2+x^4]),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) (2+x^2) Sqrt[-3+x^4]),x]*)


(* ::Input:: *)
(*int[(1-x^4)/(x^2 Sqrt[a+b x+c x^2+b x^3+a x^4]),x]*)


(* ::Input:: *)
(*int[(1-x^2)/(x Sqrt[a+b x+c x^2+b x^3+a x^4]),x]*)


(* ::Input:: *)
(*int[x^3/(Sqrt[a+b x+c x^2+b x^3+a x^4] (1-x^6)),x]*)


(* ::Input:: *)
(*int[x^2/((1-x^4) Sqrt[a+b x+c x^2+b x^3+a x^4]),x]*)


(* ::Input:: *)
(*int[x/((1-x^2) Sqrt[a+b x+c x^2+b x^3+a x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/((1+x^4) Sqrt[a+b x+c x^2+b x^3+a x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^3)/((1+x^3) Sqrt[a+b x+c x^2+b x^3+a x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[a+b x+c x^2+b x^3+a x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x)/((1+x) Sqrt[a+b x+c x^2+b x^3+a x^4]),x]*)


(* ::Input:: *)
(*int[(1+x^4)/((-1+x^4) Sqrt[a+b x+c x^2+b x^3+a x^4]),x]*)


(* ::Input:: *)
(*int[(1+x^3)/((-1+x^3) Sqrt[a+b x+c x^2+b x^3+a x^4]),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) Sqrt[a+b x+c x^2+b x^3+a x^4]),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+x) Sqrt[a+b x+c x^2+b x^3+a x^4]),x]*)


(* ::Input:: *)
(*int[((b+a x^4) Sqrt[-b-c x^2+a x^4])/(-b+a x^4)^2,x]*)


(* ::Input:: *)
(*int[(Sqrt[x+x^4] (b+a x^6))/(-d+c x^6),x]*)


(* ::Input:: *)
(*int[((b+a x^3) Sqrt[x+x^4])/(-d+c x^3),x]*)


(* ::Input:: *)
(*int[(x^6 Sqrt[x+x^4])/(b+a x^6),x]*)


(* ::Input:: *)
(*int[(x^3 Sqrt[x+x^4])/(-b+a x^3),x]*)


(* ::Input:: *)
(*int[(Sqrt[x+x^4] (-b+a x^6))/x^6,x]*)


(* ::Input:: *)
(*int[((-b+a x^3) Sqrt[x+x^4])/x^3,x]*)


(* ::Input:: *)
(*int[(b+a x^3) Sqrt[x+x^4],x]*)


(* ::Input:: *)
(*int[(x+x^2)/((-1-2 x+2 x^2) Sqrt[x+x^4]),x]*)


(* ::Input:: *)
(*int[(1-x+x^2)/((-1-2 x+2 x^2) Sqrt[x+x^4]),x]*)


(* ::Input:: *)
(*int[(2+x+2 x^2)/((-1+2 x) Sqrt[x+x^4]),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+2 x) Sqrt[x+x^4]),x]*)


(* ::Input:: *)
(*int[(-1-2 x+2 x^2)/((-1+3 x+x^2) Sqrt[x+x^4]),x]*)


(* ::Input:: *)
(*int[(-1-2 x+2 x^2)/((1+2 x+4 x^2) Sqrt[x+x^4]),x]*)


(* ::Input:: *)
(*int[(-1-2 x+2 x^2)/((-1+2 x) Sqrt[x+x^4]),x]*)


(* ::Input:: *)
(*int[(-1-2 x+2 x^2)/((1+2 x^2) Sqrt[x+x^4]),x]*)


(* ::Input:: *)
(*int[x/Sqrt[x+x^4],x]*)


(* ::Input:: *)
(*int[Sqrt[x+x^4]/x^3,x]*)


(* ::Input:: *)
(*int[x^6 Sqrt[x+x^4],x]*)


(* ::Input:: *)
(*int[x^3 Sqrt[x+x^4],x]*)


(* ::Input:: *)
(*int[Sqrt[x+x^4],x]*)


(* ::Input:: *)
(*int[(-1-2 x+2 x^2)/((1-x+x^2) Sqrt[x+x^4]),x]*)


(* ::Input:: *)
(*int[1/(x^5 Sqrt[x+x^4]),x]*)


(* ::Input:: *)
(*int[1/(x^2 Sqrt[x+x^4]),x]*)


(* ::Input:: *)
(*int[(Sqrt[-x+x^4] (b+a x^6))/(-d+c x^6),x]*)


(* ::Input:: *)
(*int[((b+a x^3) Sqrt[-x+x^4])/(-d+c x^3),x]*)


(* ::Input:: *)
(*int[Sqrt[-x+x^4]/(-b+a x^6),x]*)


(* ::Input:: *)
(*int[Sqrt[-x+x^4]/(-b+a x^3),x]*)


(* ::Input:: *)
(*int[(x^6 Sqrt[-x+x^4])/(b+a x^6),x]*)


(* ::Input:: *)
(*int[(x^3 Sqrt[-x+x^4])/(-b+a x^3),x]*)


(* ::Input:: *)
(*int[(Sqrt[-x+x^4] (-b+a x^6))/x^6,x]*)


(* ::Input:: *)
(*int[((-b+a x^3) Sqrt[-x+x^4])/x^3,x]*)


(* ::Input:: *)
(*int[(b+a x^3) Sqrt[-x+x^4],x]*)


(* ::Input:: *)
(*int[(1+2 x)/((-1+2 x+2 x^2) Sqrt[-x+x^4]),x]*)


(* ::Input:: *)
(*int[(1+2 x^2)/((-1+2 x+2 x^2) Sqrt[-x+x^4]),x]*)


(* ::Input:: *)
(*int[(3-2 x+2 x^2)/((1+2 x) Sqrt[-x+x^4]),x]*)


(* ::Input:: *)
(*int[(-1-2 x+x^2)/((1+x+2 x^2) Sqrt[-x+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+2 x+2 x^2)/((1-x+3 x^2) Sqrt[-x+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+2 x+2 x^2)/((1+2 x) Sqrt[-x+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+2 x+2 x^2)/((1+2 x^2) Sqrt[-x+x^4]),x]*)


(* ::Input:: *)
(*int[Sqrt[-x+x^4]/x^3,x]*)


(* ::Input:: *)
(*int[x^6 Sqrt[-x+x^4],x]*)


(* ::Input:: *)
(*int[x^3 Sqrt[-x+x^4],x]*)


(* ::Input:: *)
(*int[Sqrt[-x+x^4],x]*)


(* ::Input:: *)
(*int[(-1+2 x+2 x^2)/((-1+x) x Sqrt[-x+x^4]),x]*)


(* ::Input:: *)
(*int[1/(x^5 Sqrt[-x+x^4]),x]*)


(* ::Input:: *)
(*int[1/(x^2 Sqrt[-x+x^4]),x]*)


(* ::Input:: *)
(*int[Sqrt[-x+x^4]/x^6,x]*)


(* ::Input:: *)
(*int[((-q+p x^2) Sqrt[q^2+p^2 x^4])/(b x^3+a (q+p x^2)^3),x]*)


(* ::Input:: *)
(*int[((-q+p x^2) Sqrt[q^2+p^2 x^4] (b x^3+a (q+p x^2)^3))/x^6,x]*)


(* ::Input:: *)
(*int[((-q+p x^2) (a q+b x+a p x^2) Sqrt[q^2+p^2 x^4])/(x^3 (c q+d x+c p x^2)),x]*)


(* ::Input:: *)
(*int[((-q+p x^2) Sqrt[q^2+p^2 x^4])/(x^2 (a q+b x+a p x^2)),x]*)


(* ::Input:: *)
(*int[((-q+p x^2) (a q+b x+a p x^2) Sqrt[q^2+p^2 x^4])/x^4,x]*)


(* ::Input:: *)
(*int[((b+a x^2)^2 (-a q x+b p x^3))/(Sqrt[q+p x^4] (b^4 c+d q^2+4 a b^3 c x^2+(6 a^2 b^2 c+2 d p q) x^4+4 a^3 b c x^6+(a^4 c+d p^2) x^8)),x]*)


(* ::Input:: *)
(*int[((b+a x)^2 (-a q+2 b p x^3+a p x^4))/(Sqrt[q+p x^4] (b^4 c+d q^2+4 a b^3 c x+6 a^2 b^2 c x^2+4 a^3 b c x^3+(a^4 c+2 d p q) x^4+d p^2 x^8)),x]*)


(* ::Input:: *)
(*int[(b c q-a d q+2 b d p x^3+(3 b c p+a d p) x^4+2 a c p x^5)/(Sqrt[q+p x^4] (b^2 f+d^2 e q+(2 a b f+2 c d e q) x+(a^2 f+c^2 e q) x^2+d^2 e p x^4+2 c d e p x^5+c^2 e p x^6)),x]*)


(* ::Input:: *)
(*int[(-b q-2 a q x+2 c p x^3+b p x^4)/(Sqrt[q+p x^4] (c^2 d+e q+2 b c d x+(b^2 d+2 a c d) x^2+2 a b d x^3+(a^2 d+e p) x^4)),x]*)


(* ::Input:: *)
(*int[(-a q x+b p x^3)/(Sqrt[q+p x^4] (b^2 c+d q+2 a b c x^2+(a^2 c+d p) x^4)),x]*)


(* ::Input:: *)
(*int[(-a q+2 b p x^3+a p x^4)/(Sqrt[q+p x^4] (b^2 c+d q+2 a b c x+a^2 c x^2+d p x^4)),x]*)


(* ::Input:: *)
(*int[(x^4 (-q+p x^4) Sqrt[q+p x^4])/(b x^8+a (q+p x^4)^4),x]*)


(* ::Input:: *)
(*int[((-q+p x^4) Sqrt[q+p x^4])/(c x^4+b x^2 (q+p x^4)+a (q+p x^4)^2),x]*)


(* ::Input:: *)
(*int[((-q+p x^4) Sqrt[q+p x^4] (a q+b x^2+a p x^4))/(x^4 (c q+d x^2+c p x^4)),x]*)


(* ::Input:: *)
(*int[((-q+p x^4) Sqrt[q+p x^4])/(b x^4+a (q+p x^4)^2),x]*)


(* ::Input:: *)
(*int[((-q+p x^4) Sqrt[q+p x^4])/(x^2 (a q+b x^2+a p x^4)),x]*)


(* ::Input:: *)
(*int[(1+x^16)/(Sqrt[1+x^4] (-1+x^16)),x]*)


(* ::Input:: *)
(*int[(-1+x^16)/(Sqrt[1+x^4] (1+x^16)),x]*)


(* ::Input:: *)
(*int[(1+3 x^8)/((-1+x^4) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^4] (1+x^6))/(1+x^5),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^4] (1+x^6))/(-1+x^5),x]*)


(* ::Input:: *)
(*int[(1+x^5)/(Sqrt[1+x^4] (-1+x^5)),x]*)


(* ::Input:: *)
(*int[(-1+x^5)/(Sqrt[1+x^4] (1+x^5)),x]*)


(* ::Input:: *)
(*int[(b^10+a^10 x^10)/(Sqrt[b^4+a^4 x^4] (-b^10+a^10 x^10)),x]*)


(* ::Input:: *)
(*int[(1+x^10)/(Sqrt[1+x^4] (-1+x^10)),x]*)


(* ::Input:: *)
(*int[(-b^10+a^10 x^10)/(Sqrt[b^4+a^4 x^4] (b^10+a^10 x^10)),x]*)


(* ::Input:: *)
(*int[(-1+x^10)/(Sqrt[1+x^4] (1+x^10)),x]*)


(* ::Input:: *)
(*int[(b^12+a^12 x^12)/(Sqrt[b^4+a^4 x^4] (-b^12+a^12 x^12)),x]*)


(* ::Input:: *)
(*int[(1+x^12)/(Sqrt[1+x^4] (-1+x^12)),x]*)


(* ::Input:: *)
(*int[(-b^12+a^12 x^12)/(Sqrt[b^4+a^4 x^4] (b^12+a^12 x^12)),x]*)


(* ::Input:: *)
(*int[(-1+x^12)/(Sqrt[1+x^4] (1+x^12)),x]*)


(* ::Input:: *)
(*int[(b^8+a^8 x^8)/(Sqrt[b^4+a^4 x^4] (-b^8+a^8 x^8)),x]*)


(* ::Input:: *)
(*int[(1+x^8)/(Sqrt[1+x^4] (-1+x^8)),x]*)


(* ::Input:: *)
(*int[(-b^8+a^8 x^8)/(Sqrt[b^4+a^4 x^4] (b^8+a^8 x^8)),x]*)


(* ::Input:: *)
(*int[(-1+x^8)/(Sqrt[1+x^4] (1+x^8)),x]*)


(* ::Input:: *)
(*int[((-b^4+a^4 x^4) Sqrt[b^4+a^4 x^4])/(b^8+a^8 x^8),x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^4])/(1+x^8),x]*)


(* ::Input:: *)
(*int[(b^3+a^3 x^3)/((-b^3+a^3 x^3) Sqrt[b^4+a^4 x^4]),x]*)


(* ::Input:: *)
(*int[(1+x^3)/((-1+x^3) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[(-b^3+a^3 x^3)/((b^3+a^3 x^3) Sqrt[b^4+a^4 x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^3)/((1+x^3) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[x^2/((-b+a x^4) Sqrt[b+a x^4]),x]*)


(* ::Input:: *)
(*int[x^2/((-1+x^4) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[(-b^6+a^6 x^6)/(Sqrt[b^4+a^4 x^4] (b^6+a^6 x^6)),x]*)


(* ::Input:: *)
(*int[(-1+x^6)/(Sqrt[1+x^4] (1+x^6)),x]*)


(* ::Input:: *)
(*int[(b^6+a^6 x^6)/(Sqrt[b^4+a^4 x^4] (-b^6+a^6 x^6)),x]*)


(* ::Input:: *)
(*int[(1+x^6)/(Sqrt[1+x^4] (-1+x^6)),x]*)


(* ::Input:: *)
(*int[Sqrt[b+a x^4]/(-b+a x^4),x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^4]/(-1+x^4),x]*)


(* ::Input:: *)
(*int[(1+x^2)/(x (1+x) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[((b+a x^2) Sqrt[b^2+a^2 x^4])/(x^2 (-b+a x^2)),x]*)


(* ::Input:: *)
(*int[((-b+a x^2) Sqrt[b^2+a^2 x^4])/(x^2 (b+a x^2)),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+x) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x)/((1+x) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((-b+a x^2) Sqrt[b^2+a^2 x^4]),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/((b+a x^2) Sqrt[b^2+a^2 x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[b+a x^4]),x]*)


(* ::Input:: *)
(*int[x/Sqrt[b+a x^4],x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^4] (3+x^4) (1+x^4+x^6))/(x^10 (-1-x^4+x^6)),x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^4]/x,x]*)


(* ::Input:: *)
(*int[(-1+x^4)/(x^3 Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[(1+3 x^4)/Sqrt[1+x^4],x]*)


(* ::Input:: *)
(*int[(3+x^4)/(x^4 Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/(x^2 Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[(-b^16+a^16 x^16)/(Sqrt[-b^4+a^4 x^4] (b^16+a^16 x^16)),x]*)


(* ::Input:: *)
(*int[(b^16+a^16 x^16)/(Sqrt[-b^4+a^4 x^4] (-b^16+a^16 x^16)),x]*)


(* ::Input:: *)
(*int[(-1+x^16)/(Sqrt[-1+x^4] (1+x^8+x^16)),x]*)


(* ::Input:: *)
(*int[(-1+x^16)/(Sqrt[-1+x^4] (1-x^8+x^16)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-b^4+a^4 x^4] (b^4+a^4 x^4))/(b^8+a^8 x^8),x]*)


(* ::Input:: *)
(*int[x^8/(Sqrt[-b^4+a^4 x^4] (-b^16+a^16 x^16)),x]*)


(* ::Input:: *)
(*int[x^8/(Sqrt[-1+x^4] (-1+x^16)),x]*)


(* ::Input:: *)
(*int[(b^8+a^8 x^8)/(Sqrt[-b^4+a^4 x^4] (-b^8+a^8 x^8)),x]*)


(* ::Input:: *)
(*int[(-b^8+a^8 x^8)/(Sqrt[-b^4+a^4 x^4] (b^8+a^8 x^8)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-b^4+a^4 x^4] (b^4+a^4 x^4))/(b^8-c x^4+a^8 x^8),x]*)


(* ::Input:: *)
(*int[(b^8+x^4+a^8 x^8)/(Sqrt[-b^4+a^4 x^4] (-b^8+a^8 x^8)),x]*)


(* ::Input:: *)
(*int[x^4/(Sqrt[-b^4+a^4 x^4] (-b^8+a^8 x^8)),x]*)


(* ::Input:: *)
(*int[(-b^12+a^12 x^12)/(Sqrt[-b^4+a^4 x^4] (b^12+a^12 x^12)),x]*)


(* ::Input:: *)
(*int[(b^12+a^12 x^12)/(Sqrt[-b^4+a^4 x^4] (-b^12+a^12 x^12)),x]*)


(* ::Input:: *)
(*int[(b^8+a^8 x^8)/(Sqrt[-b^4+a^4 x^4] (-b^8+a^8 x^8)),x]*)


(* ::Input:: *)
(*int[(-b^8+a^8 x^8)/(Sqrt[-b^4+a^4 x^4] (b^8-c x^4+a^8 x^8)),x]*)


(* ::Input:: *)
(*int[(-b^8+a^8 x^8)/(Sqrt[-b^4+a^4 x^4] (b^8+a^8 x^8)),x]*)


(* ::Input:: *)
(*int[(-2+3 x^4+3 x^8)/(Sqrt[-1+x^4] (1+x^4)),x]*)


(* ::Input:: *)
(*int[(1+3 x^8)/(Sqrt[-1+x^4] (1+x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-b^4+a^4 x^4] (b^4+a^4 x^4))/(b^8+a^8 x^8),x]*)


(* ::Input:: *)
(*int[(-b^4+c^2 x^2+a^4 x^4)/(Sqrt[-b^4+a^4 x^4] (b^4+a^4 x^4)),x]*)


(* ::Input:: *)
(*int[(1-6 x^2+7 x^4)/(Sqrt[-1+x^4] (-1+2 x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(3+2 x^2+x^4)/(Sqrt[-1+x^4] (-1+2 x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(1-2 x^2+3 x^4)/(Sqrt[-1+x^4] (-1+2 x^2+x^4)),x]*)


(* ::Input:: *)
(*int[((-1-3 x+3 x^2) Sqrt[-1+x^4])/(-1+x)^2,x]*)


(* ::Input:: *)
(*int[(-b+a x^4)/(Sqrt[b+a x^4] (b-c^2 x^2+a x^4)),x]*)


(* ::Input:: *)
(*int[(b+a x^4)/(Sqrt[-b+a x^4] (-b+c^2 x^2+a x^4)),x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[-b+a x^4]),x]*)


(* ::Input:: *)
(*int[x/Sqrt[-b+a x^4],x]*)


(* ::Input:: *)
(*int[((-1+x^4) Sqrt[1+x^4] (1+x^2+x^4))/(x^4 (1-x^2+x^4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^4] (1+x^4))/(x^2 (-1+x^2+x^4)),x]*)


(* ::Input:: *)
(*int[x Sqrt[-1+x^4],x]*)


(* ::Input:: *)
(*int[((-1+x^4) (1+x^2+x^4))/(x^4 Sqrt[1+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^16)/(x^8 Sqrt[-1+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^8)/(Sqrt[-1+x^4] (1-2 x^4+x^8)),x]*)


(* ::Input:: *)
(*int[(1+x+x^2)/((-1+x)^2 Sqrt[-1+x^4]),x]*)


(* ::Input:: *)
(*int[(-1+x^8)/(x^4 Sqrt[-1+x^4]),x]*)


(* ::Input:: *)
(*int[((2 c-a x^3) Sqrt[c+b x^2+a x^3])/((c+(-3+b) x^2+a x^3) (c+(-2+b) x^2+a x^3)),x]*)


(* ::Input:: *)
(*int[((-2 c+a x^3) Sqrt[c+b x^2+a x^3])/(c+a x^3)^2,x]*)


(* ::Input:: *)
(*int[(2+x^2)/((-2+x^2) Sqrt[-2 x+2 x^2+x^3]),x]*)


(* ::Input:: *)
(*int[x/((1+x^2) Sqrt[-x-x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) Sqrt[-x-x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[-x-x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1-x+x^2) Sqrt[x+x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) Sqrt[x+x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[x+x^2+x^3]),x]*)


(* ::Input:: *)
(*int[x/((-1+x^2) Sqrt[x+x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(-1+x)/((1+x) Sqrt[x+x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+x) Sqrt[x+x^2+x^3]),x]*)


(* ::Input:: *)
(*int[x^2/(Sqrt[x+x^2+x^3] (-1+x^4)),x]*)


(* ::Input:: *)
(*int[Sqrt[x+x^2+x^3]/(-1+x^4),x]*)


(* ::Input:: *)
(*int[(1-x^2+x^4)/(Sqrt[x+x^2+x^3] (-1+x^4)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/(Sqrt[-x-x^2+x^3] (1+x^4)),x]*)


(* ::Input:: *)
(*int[(1+x^4)/(Sqrt[-x-x^2+x^3] (-1+x^4)),x]*)


(* ::Input:: *)
(*int[Sqrt[-x-x^2+x^3]/(-1+x^4),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[-x-x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(-1+x+x^2)/((1+x^2) Sqrt[-x-x^2+x^3]),x]*)


(* ::Input:: *)
(*int[x^2/(Sqrt[-x-x^2+x^3] (-1+x^4)),x]*)


(* ::Input:: *)
(*int[Sqrt[-x-x^2+x^3]/(-1+x^4),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+2 x+x^2) Sqrt[-x-x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(-1+2 x)/((1+x) Sqrt[-x-x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(2+x)/((-1+x) Sqrt[-x-x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(-1+2 x)/((1+x) Sqrt[-x-x^2+x^3]),x]*)


(* ::Input:: *)
(*int[(-b^6+a^6 x^6)/(Sqrt[b^2 x+a^2 x^3] (b^6+a^6 x^6)),x]*)


(* ::Input:: *)
(*int[(b^6+a^6 x^6)/(Sqrt[b^2 x+a^2 x^3] (-b^6+a^6 x^6)),x]*)


(* ::Input:: *)
(*int[(-b^5+a^5 x^5)/(Sqrt[b^2 x+a^2 x^3] (b^5+a^5 x^5)),x]*)


(* ::Input:: *)
(*int[(b^5+a^5 x^5)/(Sqrt[b^2 x+a^2 x^3] (-b^5+a^5 x^5)),x]*)


(* ::Input:: *)
(*int[(-b^3+a^3 x^3)/(Sqrt[b^2 x+a^2 x^3] (b^3+a^3 x^3)),x]*)


(* ::Input:: *)
(*int[(b^3+a^3 x^3)/(Sqrt[b^2 x+a^2 x^3] (-b^3+a^3 x^3)),x]*)


(* ::Input:: *)
(*int[(-b^2+a^2 x^4)/(Sqrt[-b x+a x^3] (b^2+c x^2+a^2 x^4)),x]*)


(* ::Input:: *)
(*int[(b^2+c x^2+a^2 x^4)/(Sqrt[b x+a x^3] (-b^2+a^2 x^4)),x]*)


(* ::Input:: *)
(*int[x^2/(Sqrt[b x+a x^3] (-b^2+a^2 x^4)),x]*)


(* ::Input:: *)
(*int[((b+a x^2) Sqrt[b x+a x^3])/(x^2 (-b+a x^2)),x]*)


(* ::Input:: *)
(*int[Sqrt[b x+a x^3]/(-b^2+a^2 x^4),x]*)


(* ::Input:: *)
(*int[(-b+a^2 x^2)/((b+2 a b x+a^2 x^2) Sqrt[b x+a^2 x^3]),x]*)


(* ::Input:: *)
(*int[(-b x^2+a x^3)/((b x^2+a x^3) Sqrt[b^2 x+a^2 x^3]),x]*)


(* ::Input:: *)
(*int[(b-c x+a x^2)/((-b+a x^2) Sqrt[b x+a x^3]),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/((b+c x+a x^2) Sqrt[b x+a x^3]),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((-b+a x^2) Sqrt[b x+a x^3]),x]*)


(* ::Input:: *)
(*int[((-7+5 x^2) Sqrt[x+x^3])/(-1+x^2),x]*)


(* ::Input:: *)
(*int[x/((-b+a x^2) Sqrt[b x+a x^3]),x]*)


(* ::Input:: *)
(*int[((-b+a x^2) Sqrt[b x+a x^3])/(b^2 x+2 (-1+a b) x^3+a^2 x^5),x]*)


(* ::Input:: *)
(*int[(-b^2+a^4 x^4)/(Sqrt[b x+a^2 x^3] (b^2+a^4 x^4)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/(Sqrt[x+x^3] (1+x^4)),x]*)


(* ::Input:: *)
(*int[(b+c x+a x^2)/((-b+a x^2) Sqrt[b x+a x^3]),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/((b+c x+a x^2) Sqrt[b x+a x^3]),x]*)


(* ::Input:: *)
(*int[(b+a x)/((-b+a x) Sqrt[b^2 x+a^2 x^3]),x]*)


(* ::Input:: *)
(*int[(-b+a x)/((b+a x) Sqrt[b^2 x+a^2 x^3]),x]*)


(* ::Input:: *)
(*int[(Sqrt[x+x^3] (2-x^2+3 x^4))/x^4,x]*)


(* ::Input:: *)
(*int[(-1+x^4)/(x^2 Sqrt[x+x^3]),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) Sqrt[x+x^3]),x]*)


(* ::Input:: *)
(*int[(b^6+a^6 x^6)/(Sqrt[-b^2 x+a^2 x^3] (-b^6+c x^3+a^6 x^6)),x]*)


(* ::Input:: *)
(*int[(b^6+a^6 x^6)/(Sqrt[-b^2 x+a^2 x^3] (-b^6+a^6 x^6)),x]*)


(* ::Input:: *)
(*int[(-b^6+a^6 x^6)/(Sqrt[-b^2 x+a^2 x^3] (b^6+a^6 x^6)),x]*)


(* ::Input:: *)
(*int[(-2+3 x+x^2)/((-1-2 x+x^2) Sqrt[-x+x^3]),x]*)


(* ::Input:: *)
(*int[(-b^4+a^4 x^4)/(Sqrt[-b^2 x+a^2 x^3] (b^4+a^4 x^4)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/(Sqrt[-x+x^3] (1+x^4)),x]*)


(* ::Input:: *)
(*int[(b^4+a^4 x^4)/(Sqrt[-b^2 x+a^2 x^3] (-b^4+a^4 x^4)),x]*)


(* ::Input:: *)
(*int[(1+x^4)/(Sqrt[-x+x^3] (-1+x^4)),x]*)


(* ::Input:: *)
(*int[(-b+a x^2)/((b+a x^2) Sqrt[-b x+a x^3]),x]*)


(* ::Input:: *)
(*int[x/((b+a x^2) Sqrt[-b x+a x^3]),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((-b+c x+a x^2) Sqrt[-b x+a x^3]),x]*)


(* ::Input:: *)
(*int[(-b+c x+a x^2)/((b+a x^2) Sqrt[-b x+a x^3]),x]*)


(* ::Input:: *)
(*int[((-b+a x^2) Sqrt[-b x+a x^3])/(x^2 (b+a x^2)),x]*)


(* ::Input:: *)
(*int[(-1+x^4)/(x^2 Sqrt[-x+x^3]),x]*)


(* ::Input:: *)
(*int[(1+x^2)/((-1+x^2) Sqrt[-x+x^3]),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((-b+a x^2) Sqrt[-b x+a x^3]),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/(x Sqrt[-b x+a x^3]),x]*)


(* ::Input:: *)
(*int[(-1-2 x+x^2)/((1+2 x+3 x^2) Sqrt[-x+x^3]),x]*)


(* ::Input:: *)
(*int[(x+x^2)/((-1-2 x+x^2) Sqrt[-x+x^3]),x]*)


(* ::Input:: *)
(*int[(-x+x^2)/((-1+2 x+x^2) Sqrt[-x+x^3]),x]*)


(* ::Input:: *)
(*int[Sqrt[-b x+a x^3]/(-b^2+a^2 x^4),x]*)


(* ::Input:: *)
(*int[((7+5 x^2) Sqrt[-x+x^3])/(1+x^2),x]*)


(* ::Input:: *)
(*int[(-7+x+7 x^2)/((1+x^2) Sqrt[-x+x^3]),x]*)


(* ::Input:: *)
(*int[(-1-x+x^2)/((1+x^2) Sqrt[-x+x^3]),x]*)


(* ::Input:: *)
(*int[(-1+x+x^2)/((1+x^2) Sqrt[-x+x^3]),x]*)


(* ::Input:: *)
(*int[(b+a x^2)/((-b+c x+a x^2) Sqrt[-b x+a x^3]),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+2 x+x^2) Sqrt[-x+x^3]),x]*)


(* ::Input:: *)
(*int[(-1+x)/((-1-2 x+x^2) Sqrt[-x+x^3]),x]*)


(* ::Input:: *)
(*int[((b+a x^2)^2 (4 a q x-3 b p x^2+a p x^4))/(Sqrt[q+p x^3] (b^4 c+d q^2+4 a b^3 c x^2+2 d p q x^3+6 a^2 b^2 c x^4+(4 a^3 b c+d p^2) x^6+a^4 c x^8)),x]*)


(* ::Input:: *)
(*int[((b+a x)^2 (-2 a q+3 b p x^2+a p x^3))/(Sqrt[q+p x^3] (b^4 c+d q^2+4 a b^3 c x+6 a^2 b^2 c x^2+(4 a^3 b c+2 d p q) x^3+a^4 c x^4+d p^2 x^6)),x]*)


(* ::Input:: *)
(*int[(2 b c q-2 a d q+3 b d p x^2+5 b c p x^3+a d p x^3+3 a c p x^4)/(Sqrt[q+p x^3] (b^2 f+d^2 e q+(2 a b f+2 c d e q) x+(a^2 f+c^2 e q) x^2+d^2 e p x^3+2 c d e p x^4+c^2 e p x^5)),x]*)


(* ::Input:: *)
(*int[(2 b q+4 a q x-3 c p x^2-b p x^3+a p x^4)/(Sqrt[q+p x^3] (c^2 d+e q+2 b c d x+(b^2 d+2 a c d) x^2+(2 a b d+e p) x^3+a^2 d x^4)),x]*)


(* ::Input:: *)
(*int[(4 a q x-3 b p x^2+a p x^4)/(Sqrt[q+p x^3] (b^2 c+d q+2 a b c x^2+d p x^3+a^2 c x^4)),x]*)


(* ::Input:: *)
(*int[(-2 a q+3 b p x^2+a p x^3)/(Sqrt[q+p x^3] (b^2 c+d q+2 a b c x+a^2 c x^2+d p x^3)),x]*)


(* ::Input:: *)
(*int[(x^4 (-2 q+p x^3) Sqrt[q+p x^3])/(b x^8+a (q+p x^3)^4),x]*)


(* ::Input:: *)
(*int[((-2 q+p x^3) Sqrt[q+p x^3])/(c x^4+b x^2 (q+p x^3)+a (q+p x^3)^2),x]*)


(* ::Input:: *)
(*int[((-2 q+p x^3) Sqrt[q+p x^3])/(b x^4+a (q+p x^3)^2),x]*)


(* ::Input:: *)
(*int[((-2 q+p x^3) Sqrt[q+p x^3])/(x^2 (a q+b x^2+a p x^3)),x]*)


(* ::Input:: *)
(*int[(-2+x^3)/(Sqrt[1+x^3] (4+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[b^2+a^2 x^3] (2 b^2+c x^3+a^2 x^6))/(x^7 (b^2+a^2 x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[b^2+a^2 x^3] (2 b^2+c x^3+a^2 x^6))/(x^7 (-b^2+a^2 x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[b^2+a^2 x^3] (2 b^2+c x^3+a^2 x^6))/(x (b^2+a^2 x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[b^2+a^2 x^3] (2 b^2+c x^3+a^2 x^6))/(x (-b^2+a^2 x^6)),x]*)


(* ::Input:: *)
(*int[(3+x+x^2)/((-2+2 x+x^2) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(3-x+2 x^2)/((-2+2 x+x^2) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(1+x)/((-2+2 x+x^2) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(1-x+x^2)/((-2+2 x+x^2) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(2+x^2)/((-2+2 x+x^2) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(3 b+a x^3)/(x (-b+a x^3) Sqrt[b+a x^3]),x]*)


(* ::Input:: *)
(*int[(3-x+2 x^2)/((-2+2 x+x^2) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(-2+2 x+x^2)/((3-x+2 x^2) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(-4+3 x+x^2)/((-2+x) x Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(-2+2 x+x^2)/((2-4 x+3 x^2) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(-2+2 x+x^2)/((-1-3 x+x^2) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(1+x)/((-2+2 x+x^2) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(-1-2 x+x^2)/((2-x+x^2) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(-2+2 x+x^2)/((3-x+2 x^2) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(-2 b+a x^3)/(Sqrt[b+a x^3] (b+c x^2+a x^3)),x]*)


(* ::Input:: *)
(*int[(-2 b+a x^3)/(Sqrt[b+a x^3] (b-c x^2+a x^3)),x]*)


(* ::Input:: *)
(*int[(2+5 x^3)/(Sqrt[1+x^3] (1+x^2+x^5)),x]*)


(* ::Input:: *)
(*int[(1+x)/((-2+x) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[((2+x^3) (1+x^3+x^6))/(x^7 Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[((2+x^3) (1+x^3+x^6))/(x^4 Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[((2+x^3) (1+x^3+x^6))/(x Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[((-2+x^3) Sqrt[1+x^3] (2+x^2+2 x^3))/(x^4 (1+x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^3] (2+2 x^3+x^6))/(x^7 (-1+x^6)),x]*)


(* ::Input:: *)
(*int[(Sqrt[1+x^3] (2+2 x^3+x^6))/(x (-1+x^6)),x]*)


(* ::Input:: *)
(*int[((-2 b+a x^3) Sqrt[b+a x^3])/(x^2 (b-x^2+a x^3)),x]*)


(* ::Input:: *)
(*int[((-b+a x^3) Sqrt[b+a x^3])/x^7,x]*)


(* ::Input:: *)
(*int[((-b+a x^3) Sqrt[b+a x^3])/x^4,x]*)


(* ::Input:: *)
(*int[((-b+a x^3) Sqrt[b+a x^3])/x,x]*)


(* ::Input:: *)
(*int[(Sqrt[b+a x^3] (2 b+a x^3))/x^7,x]*)


(* ::Input:: *)
(*int[(Sqrt[b+a x^3] (2 b+a x^3))/x^4,x]*)


(* ::Input:: *)
(*int[(Sqrt[b+a x^3] (2 b+a x^3))/x,x]*)


(* ::Input:: *)
(*int[1/(x^10 Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[1/(x^7 Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[1/(x^4 Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^3]/x^7,x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^3]/x^4,x]*)


(* ::Input:: *)
(*int[Sqrt[1+x^3]/x,x]*)


(* ::Input:: *)
(*int[((-2 b+a x^3) Sqrt[b+a x^3])/x^4,x]*)


(* ::Input:: *)
(*int[x^5/Sqrt[b+a x^3],x]*)


(* ::Input:: *)
(*int[x^2/Sqrt[1+x^3],x]*)


(* ::Input:: *)
(*int[x^5 Sqrt[b+a x^3],x]*)


(* ::Input:: *)
(*int[x^2 Sqrt[1+x^3],x]*)


(* ::Input:: *)
(*int[(-4+x^3)/(Sqrt[-1+x^3] (8+x^3)),x]*)


(* ::Input:: *)
(*int[(2+x^3)/((-4+x^3) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[x/((4-2 x+x^2) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[(3-x+x^2)/((-2-2 x+x^2) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[(-1+x)/((-2-2 x+x^2) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[(2 x+x^2)/((-2-2 x+x^2) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[x/(Sqrt[-1+x^3] (8+x^3)),x]*)


(* ::Input:: *)
(*int[(2+x^2)/((-2-2 x+x^2) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[(2-2 x+3 x^2)/((2 x+x^2) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[Sqrt[-b+a x^3]/(x (2 b+a x^3)),x]*)


(* ::Input:: *)
(*int[(2+x^2)/((2 x+x^2) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[(2 b+a x^3)/(Sqrt[-b+a x^3] (-2 b-3 x^2+2 a x^3)),x]*)


(* ::Input:: *)
(*int[(-2-2 x+x^2)/((-1+3 x+x^2) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[(-2-2 x+x^2)/((3-x+x^2) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[(-1+x)/((2+x) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[(2 b+a x^3)/(Sqrt[-b+a x^3] (-b+x^2+a x^3)),x]*)


(* ::Input:: *)
(*int[(-2-2 x+x^2)/((2+x^2) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[(-2-2 x+x^2)/((2 x+x^2) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[-b+a x^3]),x]*)


(* ::Input:: *)
(*int[(2 b+a x^3)/(Sqrt[-b+a x^3] (-2 b-3 x^2+2 a x^3)),x]*)


(* ::Input:: *)
(*int[(-b+4 a x^3)/(x Sqrt[-b+a x^3] (2 b+a x^3)),x]*)


(* ::Input:: *)
(*int[(1+x)/((-2+x) Sqrt[1+x^3]),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^3] (2+x^3) (-1-x^2+x^3)^2)/(x^6 (-2-3 x^2+2 x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^3] (1-x^3+x^6))/(x^10 (2+x^3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^3] (-2+x^3+2 x^6))/x^10,x]*)


(* ::Input:: *)
(*int[(1+x^3)/(x^7 Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[1/(x^7 Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[1/(x^4 Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[Sqrt[-1+x^3]/x^7,x]*)


(* ::Input:: *)
(*int[Sqrt[-1+x^3]/x^4,x]*)


(* ::Input:: *)
(*int[Sqrt[-1+x^3]/x,x]*)


(* ::Input:: *)
(*int[(-2-2 x+x^2)/((1+x+x^2) Sqrt[-1+x^3]),x]*)


(* ::Input:: *)
(*int[(Sqrt[-b+a x^3] (2 b+a x^3))/x^4,x]*)


(* ::Input:: *)
(*int[x^5 Sqrt[-b+a x^3],x]*)


(* ::Input:: *)
(*int[x^2 Sqrt[-b+a x^3],x]*)


(* ::Input:: *)
(*int[x^8/Sqrt[-b+a x^3],x]*)


(* ::Input:: *)
(*int[x^5/Sqrt[-b+a x^3],x]*)


(* ::Input:: *)
(*int[x^2/Sqrt[-b+a x^3],x]*)


(* ::Input:: *)
(*int[((a^2-2 a x+x^2) (-a b c-a b d-a c d+b c d+2 a (b+c+d) x-(3 a+b+c+d) x^2+2 x^3))/((-b+x)^2 (-c+x)^2 (-d+x)^2 Sqrt[(-a+x) (-b+x) (-c+x) (-d+x)] (a-b c d e+(-1+b c e+b d e+c d e) x-(b+c+d) e x^2+e x^3)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-c+x) (-d+x) (a b c+a b d+a c d-b c d-2 a (b+c+d) x+(3 a+b+c+d) x^2-2 x^3))/((-a+x) Sqrt[(-a+x) (-b+x) (-c+x) (-d+x)] (-b c d+a e+(b c+b d+c d-e) x-(b+c+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-a b c-a b d-a c d+b c d+2 a (b+c+d) x-(3 a+b+c+d) x^2+2 x^3)/(Sqrt[(-a+x) (-b+x) (-c+x) (-d+x)] (-b c d+a e+(b c+b d+c d-e) x-(b+c+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-a b c-a b d-a c d+b c d+2 a (b+c+d) x-(3 a+b+c+d) x^2+2 x^3)/(Sqrt[(-a+x) (-b+x) (-c+x) (-d+x)] (a-b c d e+(-1+b c e+b d e+c d e) x-(b+c+d) e x^2+e x^3)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x) (-c+x) (3 a b c-2 (a b+a c+b c) x+(a+b+c) x^2))/(x^2 Sqrt[x (-a+x) (-b+x) (-c+x)] (a b c-(a b+a c+b c) x+(a+b+c) x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x) (-c+x) (a b c-(a+b+c) x^2+2 x^3))/(x Sqrt[x (-a+x) (-b+x) (-c+x)] (-a b c+(a b+a c+b c-d) x-(a+b+c) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(x (-b+x) (-c+x) (-a b c+2 a (b+c) x-(3 a+b+c) x^2+2 x^3))/((-a+x) Sqrt[x (-a+x) (-b+x) (-c+x)] (a d+(b c-d) x-(b+c) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(x (3 a b c-2 (a b+a c+b c) x+(a+b+c) x^2))/(Sqrt[x (-a+x) (-b+x) (-c+x)] (-a b c d+(a b+a c+b c) d x-(a+b+c) d x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[(3 a b c x-2 (a b+a c+b c) x^2+(a+b+c) x^3)/(Sqrt[x (-a+x) (-b+x) (-c+x)] (a b c-(a b+a c+b c) x+(a+b+c) x^2+(-1+d) x^3)),x]*)


(* ::Input:: *)
(*int[(a b c-(a+b+c) x^2+2 x^3)/(Sqrt[x (-a+x) (-b+x) (-c+x)] (-a b c d+(-1+a b d+a c d+b c d) x-(a+b+c) d x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(a b c-(a+b+c) x^2+2 x^3)/(Sqrt[x (-a+x) (-b+x) (-c+x)] (-a b c+(a b+a c+b c-d) x-(a+b+c) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-a b c+2 a (b+c) x-(3 a+b+c) x^2+2 x^3)/(Sqrt[x (-a+x) (-b+x) (-c+x)] (a d+(b c-d) x-(b+c) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-a b c+2 a (b+c) x-(3 a+b+c) x^2+2 x^3)/(Sqrt[x (-a+x) (-b+x) (-c+x)] (a+(-1+b c d) x-(b+c) d x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(1+k^2 x^4)/(Sqrt[(1-x^2) (1-k^2 x^2)] (-1+k^2 x^4)),x]*)


(* ::Input:: *)
(*int[(-1+k^2 x^4)/(Sqrt[(1-x^2) (1-k^2 x^2)] (1+k^2 x^4)),x]*)


(* ::Input:: *)
(*int[(-1+k^(3/2) x^3)/(Sqrt[(1-x^2) (1-k^2 x^2)] (1+k^(3/2) x^3)),x]*)


(* ::Input:: *)
(*int[(1+k^(3/2) x^3)/(Sqrt[(1-x^2) (1-k^2 x^2)] (-1+k^(3/2) x^3)),x]*)


(* ::Input:: *)
(*int[(c+b x^2+c k^2 x^4)/(Sqrt[(1-x^2) (1-k^2 x^2)] (-1+k^2 x^4)),x]*)


(* ::Input:: *)
(*int[(1-2 k^2 x^2+k^2 x^4)/(x^2 Sqrt[(1-x^2) (1-k^2 x^2)] (-1+k^2 x^2)),x]*)


(* ::Input:: *)
(*int[x^2/(Sqrt[(1-x^2) (1-k^2 x^2)] (-1+k^2 x^4)),x]*)


(* ::Input:: *)
(*int[(1+a k x+k x^2)/((-1+k x^2) Sqrt[(1-x^2) (1-k^2 x^2)]),x]*)


(* ::Input:: *)
(*int[(-1+a k x+k x^2)/((1+k x^2) Sqrt[(1-x^2) (1-k^2 x^2)]),x]*)


(* ::Input:: *)
(*int[(1+k x^2)/((-1+c k x+k x^2) Sqrt[(1-x^2) (1-k^2 x^2)]),x]*)


(* ::Input:: *)
(*int[(-1+k x^2)/((1+c k x+k x^2) Sqrt[(1-x^2) (1-k^2 x^2)]),x]*)


(* ::Input:: *)
(*int[(1+k x^2)/((-1+k x^2) Sqrt[(1-x^2) (1-k^2 x^2)]),x]*)


(* ::Input:: *)
(*int[(-1+k x^2)/((1+k x^2) Sqrt[(1-x^2) (1-k^2 x^2)]),x]*)


(* ::Input:: *)
(*int[(-I+Sqrt[k] x)/((I+Sqrt[k] x) Sqrt[(1-x^2) (1-k^2 x^2)]),x]*)


(* ::Input:: *)
(*int[(I+Sqrt[k] x)/((-I+Sqrt[k] x) Sqrt[(1-x^2) (1-k^2 x^2)]),x]*)


(* ::Input:: *)
(*int[(1+Sqrt[k] x)/((-1+Sqrt[k] x) Sqrt[(1-x^2) (1-k^2 x^2)]),x]*)


(* ::Input:: *)
(*int[(-1+Sqrt[k] x)/((1+Sqrt[k] x) Sqrt[(1-x^2) (1-k^2 x^2)]),x]*)


(* ::Input:: *)
(*int[(-1+k^4 x^4)/(Sqrt[(1-x) x (1-k^2 x)] (a+b x^2+a k^4 x^4)),x]*)


(* ::Input:: *)
(*int[(a+b x^2+a k^4 x^4)/(Sqrt[(1-x) x (1-k^2 x)] (-1+k^4 x^4)),x]*)


(* ::Input:: *)
(*int[(1+k^4 x^4)/(Sqrt[(1-x) x (1-k^2 x)] (-1+k^4 x^4)),x]*)


(* ::Input:: *)
(*int[(-1+k^4 x^4)/(Sqrt[(1-x) x (1-k^2 x)] (1+k^4 x^4)),x]*)


(* ::Input:: *)
(*int[(1+k^3 x^3)/(Sqrt[(1-x) x (1-k^2 x)] (-1+k^3 x^3)),x]*)


(* ::Input:: *)
(*int[(-1+k^3 x^3)/(Sqrt[(1-x) x (1-k^2 x)] (1+k^3 x^3)),x]*)


(* ::Input:: *)
(*int[(-a-b x+(a+b k^2) x^2)/(Sqrt[(1-x) x (1-k^2 x)] (1-2 k^2 x+k^2 x^2)),x]*)


(* ::Input:: *)
(*int[(1-2 k^2 x+k^2 x^2)/(x Sqrt[(1-x) x (1-k^2 x)] (-1+k^2 x)),x]*)


(* ::Input:: *)
(*int[(-a-b x+(b+a k^2) x^2)/(Sqrt[(1-x) x (1-k^2 x)] (1-2 x+k^2 x^2)),x]*)


(* ::Input:: *)
(*int[(1-2 x+k^2 x^2)/(Sqrt[(1-x) x (1-k^2 x)] (-1+2 x+(-2+k^2) x^2)),x]*)


(* ::Input:: *)
(*int[(-x+x^2)/(Sqrt[(1-x) x (1-k^2 x)] (1-2 x+k^2 x^2)),x]*)


(* ::Input:: *)
(*int[(1-2 k^2 x+k^2 x^2)/(Sqrt[(1-x) x (1-k^2 x)] (-a-b x+(a k^2+b k^2) x^2)),x]*)


(* ::Input:: *)
(*int[(-1+k^2 x^2)/(Sqrt[(1-x) x (1-k^2 x)] (a+b x+a k^2 x^2)),x]*)


(* ::Input:: *)
(*int[(1+b x+k^2 x^2)/(Sqrt[(1-x) x (1-k^2 x)] (-1+k^2 x^2)),x]*)


(* ::Input:: *)
(*int[(1+k^2 x^2)/(Sqrt[(1-x) x (1-k^2 x)] (-1+k^2 x^2)),x]*)


(* ::Input:: *)
(*int[(-1+k^2 x^2)/(Sqrt[(1-x) x (1-k^2 x)] (1+k^2 x^2)),x]*)


(* ::Input:: *)
(*int[x/(Sqrt[(1-x) x (1-k^2 x)] (-1+k^2 x^2)),x]*)


(* ::Input:: *)
(*int[(-1+k x)/((1+k x) Sqrt[(1-x) x (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[(1+k x)/((-1+k x) Sqrt[(1-x) x (1-k^2 x)]),x]*)


(* ::Input:: *)
(*int[((-a b-a c+3 b c+2 (a-b-c) x+x^2) (a^4-4 a^3 x+6 a^2 x^2-4 a x^3+x^4))/((-b+x) (-c+x) Sqrt[(-a+x) (-b+x) (-c+x)] (a^3+b c d+(-3 a^2-b d-c d) x+(3 a+d) x^2-x^3)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-c+x) (a b+a c-b c-2 a x+x^2))/(Sqrt[(-a+x) (-b+x) (-c+x)] (-a^2+b^2 c^2 d+2 (a-b^2 c d-b c^2 d) x+(-1+b^2 d+4 b c d+c^2 d) x^2-2 (b+c) d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-c+x) (a b+a c-b c-2 a x+x^2))/(Sqrt[(-a+x) (-b+x) (-c+x)] (b^2 c^2-a^2 d-2 (b^2 c+b c^2-a d) x+(b^2+4 b c+c^2-d) x^2-2 (b+c) x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(a (a b+a c-3 b c)+(-2 a^2+a b+a c+3 b c) x+(a-2 b-2 c) x^2+x^3)/(Sqrt[(-a+x) (-b+x) (-c+x)] (-b c-a^3 d+(b+c+3 a^2 d) x-(1+3 a d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(a (a b+a c-3 b c)+(-2 a^2+a b+a c+3 b c) x+(a-2 b-2 c) x^2+x^3)/(Sqrt[(-a+x) (-b+x) (-c+x)] (-a^3-b c d+(3 a^2+b d+c d) x-(3 a+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-c+x) (-a b-a c+3 b c+2 (a-b-c) x+x^2))/((a-x)^2 Sqrt[(-a+x) (-b+x) (-c+x)] (-b c-a^3 d+(b+c+3 a^2 d) x-(1+3 a d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(-a (a b+a c-b c)+(2 a^2+a b+a c-b c) x-3 a x^2+x^3)/((b-x) (-c+x) Sqrt[(-a+x) (-b+x) (-c+x)] (a+b c d-(1+b d+c d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[((-b+x) (-c+x) (a b+a c-b c-2 a x+x^2))/((-a+x) Sqrt[(-a+x) (-b+x) (-c+x)] (b c+a d-(b+c+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(a b+a c-b c-2 a x+x^2)/(Sqrt[(-a+x) (-b+x) (-c+x)] (a+b c d-(1+b d+c d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(a b+a c-b c-2 a x+x^2)/(Sqrt[(-a+x) (-b+x) (-c+x)] (b c+a d-(b+c+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(-a^2 b+a (2 a+b) x-3 a x^2+x^3)/(Sqrt[x (-a+x) (-b+x)] (-a^2-a (-2+b e) x+(-1+b^2 d+a e+b e) x^2-(2 b d+e) x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x) (-a b+x^2))/(Sqrt[x (-a+x) (-b+x)] (a^2 b^2 d-2 a b (a+b) d x+(-1+a^2 d+4 a b d+b^2 d) x^2-2 (a+b) d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(x (-b+x) (a b-2 a x+x^2))/(Sqrt[x (-a+x) (-b+x)] (-a^2 d+a (2 d+b e) x+(b^2-d-a e-b e) x^2+(-2 b+e) x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(a^2 b-a (2 a-b) x-(-a+2 b) x^2+x^3)/(Sqrt[x (-a+x) (-b+x)] (-a^3 d+(b+3 a^2 d) x-(1+3 a d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(a^2 b-a (2 a-b) x-(-a+2 b) x^2+x^3)/(Sqrt[x (-a+x) (-b+x)] (-a^3+(3 a^2+b d) x-(3 a+d) x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(-a b x+x^3)/(Sqrt[x (-a+x) (-b+x)] (a^2 b^2 d-2 a b (a+b) d x+(-1+a^2 d+4 a b d+b^2 d) x^2-2 (a+b) d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x) (-a b+x^2))/(Sqrt[x (-a+x) (-b+x)] (a^2 b^2-2 a b (a+b) x+(a^2+4 a b+b^2-d) x^2-2 (a+b) x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(x (-b+x) (-a b+2 (a-b) x+x^2))/((-a+x)^2 Sqrt[x (-a+x) (-b+x)] (-a^3 d+(b+3 a^2 d) x-(1+3 a d) x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(-a^2 b+a (2 a+b) x-3 a x^2+x^3)/(Sqrt[x (-a+x) (-b+x)] (-a^2+2 a x+(-1+b^2 d) x^2-2 b d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(-a^2 b+a (2 a+b) x-3 a x^2+x^3)/(Sqrt[x (-a+x) (-b+x)] (-a^2 d+2 a d x+(b^2-d) x^2-2 b x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(-a b x+x^3)/(Sqrt[x (-a+x) (-b+x)] (a^2 b^2-2 a b (a+b) x+(a^2+4 a b+b^2-d) x^2-2 (a+b) x^3+x^4)),x]*)


(* ::Input:: *)
(*int[(x^4 (3 a b-2 (a+b) x+x^2))/((-a+x) (-b+x) Sqrt[x (-a+x) (-b+x)] (-a b d+(a+b) d x-d x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(x (-b+x) (a b-2 a x+x^2))/(Sqrt[x (-a+x) (-b+x)] (-a^2+2 a x+(-1+b^2 d) x^2-2 b d x^3+d x^4)),x]*)


(* ::Input:: *)
(*int[(-a^2 b+a (2 a+b) x-3 a x^2+x^3)/(x (-b+x) Sqrt[x (-a+x) (-b+x)] (a-(1+b d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(x (-b+x) (a b-2 a x+x^2))/(Sqrt[x (-a+x) (-b+x)] (-a^2 d+2 a d x+(b^2-d) x^2-2 b x^3+x^4)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x) (3 a b-2 (a+b) x+x^2))/(x^2 Sqrt[x (-a+x) (-b+x)] (-a b+(a+b) x-x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(-a b x+x^3)/((-a+x) (-b+x) Sqrt[x (-a+x) (-b+x)] (a b d-(1+a d+b d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(x (-b+x) (a b-2 a x+x^2))/((-a+x) Sqrt[x (-a+x) (-b+x)] (a d+(b+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[((-a+x) (-b+x) (-a b+x^2))/(x Sqrt[x (-a+x) (-b+x)] (a b-(a+b+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(-Sqrt[a b]+x)/(Sqrt[x (a+x) (b+x)] (Sqrt[a b]+x)),x]*)


(* ::Input:: *)
(*int[(3 a b x-2 (a+b) x^2+x^3)/(Sqrt[x (-a+x) (-b+x)] (-a b+(a+b) x-x^2+d x^3)),x]*)


(* ::Input:: *)
(*int[(x (3 a b-2 (a+b) x+x^2))/(Sqrt[x (-a+x) (-b+x)] (-a b d+(a+b) d x-d x^2+x^3)),x]*)


(* ::Input:: *)
(*int[(a b-x^2)/(Sqrt[x (-a+x) (-b+x)] (a b d-(1+a d+b d) x+d x^2)),x]*)


(* ::Input:: *)
(*int[(a b-x^2)/(Sqrt[x (-a+x) (-b+x)] (a b-(a+b+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(a b-2 a x+x^2)/(Sqrt[x (-a+x) (-b+x)] (a d-(b+d) x+x^2)),x]*)


(* ::Input:: *)
(*int[(a b-2 a x+x^2)/(Sqrt[x (-a+x) (-b+x)] (a-(1+b d) x+d x^2)),x]*)


(* ::Subsection::Closed:: *)
(*regression testing*)


(* ::Subsubsection::Closed:: *)
(*rationalUndeterminedIntegrate*)


(* ::Input:: *)
(*int[(a^2 x^2+b^2)/((a^2 x^2-b^2)^3 (a x^3-b x^2)^(1/3)),x]*)


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


(* ::Input:: *)
(*int[(1+x)/(27+189 x+522 x^2+784 x^3+825 x^4+679 x^5+338 x^6+84 x^7+8 x^8)^(1/3),x]*)


(* ::Input:: *)
(*int[(1+x)/((1+2 x) (27+27 x+36 x^2+28 x^3+9 x^4+x^5)^(1/3)),x]*)


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


(* ::Input:: *)
(*int[Sqrt[(a x^5+2 a x^3+a x-x^4-2 x^2-1)/(a x^5-2 a x^3+a x+x^4-2 x^2+1)],x]*)


(* ::Input:: *)
(*int[Sqrt[(a b^4 x^10+4 a b^3 x^8+6 a b^2 x^6+4 a b x^4+a x^2+b^4 x^8+4 b^3 x^6+6 b^2 x^4+4 b x^2+1)/(c d^4 x^10+4 c d^3 x^8+6 c d^2 x^6+4 c d x^4+c x^2+d^4 x^8+4 d^3 x^6+6 d^2 x^4+4 d x^2+1)]/x,x]*)


(* ::Input:: *)
(*int[(x-a)/((a x^4+3 a x^3+3 a x^2+a x-x^3-3 x^2-3 x-1)/(a x^4-3 a x^3+3 a x^2-a x+x^3-3 x^2+3 x-1))^(1/3),x]*)


(* ::Input:: *)
(*int[(x/(a x^7-3 a x^5+3 a x^3-a x+x^6-3 x^4+3 x^2-1))^(1/3),x]*)


(* ::Input:: *)
(*int[(x/(a x^7-3 a x^5+3 a x^3-a x+x^6-3 x^4+3 x^2-1))^(1/3)/x^3,x]*)


(* ::Input:: *)
(*int[((a x^9-4 a x^7+6 a x^5-4 a x^3+a x+x^8-4 x^6+6 x^4-4 x^2+1)/(b x-c))^(1/4),x]*)


(* ::Input:: *)
(*int[((a x^10+4 a x^8+6 a x^6+4 a x^4+a x^2+x^8+4 x^6+6 x^4+4 x^2+1)/x^2)^(1/4)/x,x]*)


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
(*int[(1-((3 x-1) (4 x+3))^(3/2))/(1+((3 x-1) (4 x+3))^(3/2)),x]*)


(* ::Input:: *)
(*int[(d+f x^2 Sqrt[c+b x+a x^2])/(e+g x^2 Sqrt[c+b x+a x^2]),x]*)


(* ::Input:: *)
(*int[(d+f x^2 (a x^2+b x+c)^(3/2))/(e+g x^2 Sqrt[a x^2+b x+c]),x]*)


(* ::Input:: *)
(*int[(d+f x (a x^2+b x+c)^(3/2))/(e+g x Sqrt[a x^2+b x+c]),x]*)


(* ::Input:: *)
(*int[(a x^2+b x+c)^(3/2)/(1-x Sqrt[a x^2+b x+c]),x]*)


(* ::Input:: *)
(*int[1/(1-x Sqrt[a x^2+b x+c]),x]*)


(* ::Input:: *)
(*int[1/(1-(x+1) Sqrt[a x^2+b x+c]),x]*)


(* ::Input:: *)
(*int[1/(a b c-(a b x+c)^2 Sqrt[a x^2+b x+c]),x]*)


(* ::Input:: *)
(*int[1/((a^3 b^3 x^3+c^3) Sqrt[a x^2+b x+c]),x]*)


(* ::Input:: *)
(*int[(a^2 x^2-b^2 x+a b c)/((b x^2+c)^2 Sqrt[a x^2+b x+c]),x]*)


(* ::Input:: *)
(*int[(a^2 x^2-b^2 x+a b c)/((b x^2+c) Sqrt[a x^2+b x+c]),x]*)


(* ::Input:: *)
(*int[(a x^2+b x+c)^(5/2)/(b x+c)^2,x]*)


(* ::Input:: *)
(*int[(a x^2+b x+c)^(5/2)/(b x+c),x]*)


(* ::Input:: *)
(*int[Sqrt[a x^2+b x+c],x]*)


(* ::Input:: *)
(*int[1/Sqrt[a x^2+b x+c],x]*)


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
(*int[(Sqrt[2 x^2+1]-x^2+(2 x^2+1)^(5/2))/(x^2-x (2 x^2+1)^(3/2)),x]*)


(* ::Subsubsection::Closed:: *)
(*expandIntegrate*)


(* ::Input:: *)
(*int[(a+b x)/((2-x^2) (x^2-1)^(1/4)),x,"Expansion"->True]*)


(* ::Input:: *)
(*int[(2 E^-x^2)/(Sqrt[\[Pi]] Erf[x])-(Erf[Sqrt[3] Tan[x]]+2 E^(-3 Tan[x]^2) Sqrt[3/\[Pi]] x Sec[x]^2)/(1+x Erf[Sqrt[3] Tan[x]]),x,"Expansion"->True]*)


(* ::Subsubsection::Closed:: *)
(*integrateNestedRadicals*)


(* ::Input:: *)
(*int[(x^4 Sqrt[a^2 x^2+b])/(x^2-Sqrt[a x-Sqrt[a^2 x^2+b]]),x]*)


(* ::Input:: *)
(*int[Sqrt[a^2 x^2+b]/(x^2-(a x-Sqrt[a^2 x^2+b])^(1/3)),x]*)


(* ::Input:: *)
(*int[Sqrt[a^2 x^2+b]/(x^2-Sqrt[a x-Sqrt[a^2 x^2+b]]),x]*)


(* ::Input:: *)
(*int[((e+f x) Sqrt[a^2 x^2+b])/((c+d x) (a x-Sqrt[a^2 x^2+b])^(1/3)),x]*)


(* ::Input:: *)
(*int[((e+f x) Sqrt[a^2 x^2+b])/((c+d x) Sqrt[a x-Sqrt[a^2 x^2+b]]),x]*)


(* ::Input:: *)
(*int[1/((c+d x) Sqrt[a^2 x^2+b] Sqrt[a x-Sqrt[a^2 x^2+b]]),x]*)


(* ::Input:: *)
(*int[Sqrt[a x-Sqrt[a^2 x^2+b]]/(a^2 x^2+Sqrt[a^2 x^2+b]),x]*)


(* ::Input:: *)
(*int[x^4/(1-x Sqrt[x^2+1] Sqrt[1-x^2-Sqrt[x^2+1]]),x]*)


(* ::Input:: *)
(*int[x^4/(1-x Sqrt[x^2+1] Sqrt[x-Sqrt[x^2+1]]),x]*)


(* ::Input:: *)
(*int[(1-Sqrt[x-Sqrt[x^2+1]])/(x^4-2 x^2 Sqrt[x^2+1]),x]*)


(* ::Input:: *)
(*int[Sqrt[x-Sqrt[x^2+1]]/(x^2+Sqrt[x^2+1]),x]*)


(* ::Input:: *)
(*int[Sqrt[x-Sqrt[x^2+1]]/(1-Sqrt[x^2+1]),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+x Sqrt[-1+x^2]]/(x Sqrt[-1+x^2]),x]*)


(* ::Input:: *)
(*int[(C[6]+C[7] x)/Sqrt[C[4]+C[5] Sqrt[(C[0]+x C[1])/(C[2]+x C[3])]],x]*)


(* ::Input:: *)
(*int[(C[6]+C[7] x)^2/Sqrt[C[4]+C[5] Sqrt[(C[0]+x C[1])/(C[2]+x C[3])]],x]*)


(* ::Input:: *)
(*int[1/((C[6]+C[7] x) Sqrt[C[4]+C[5] Sqrt[(C[0]+x C[1])/(C[2]+x C[3])]]),x]*)


(* ::Input:: *)
(*int[1/((C[6]+C[7] x)^2 Sqrt[C[4]+C[5] Sqrt[(C[0]+x C[1])/(C[2]+x C[3])]]),x]*)


(* ::Input:: *)
(*int[(C[8]+C[9] x)/((C[6]+C[7] x) Sqrt[C[4]+C[5] Sqrt[(C[0]+x C[1])/(C[2]+x C[3])]]),x]*)


(* ::Input:: *)
(*int[(x^2-1)/((x^2+1) Sqrt[C[4]+C[5] Sqrt[(C[0]+x C[1])/(C[2]+x C[3])]]),x]*)


(* ::Input:: *)
(*int[((x^2-1) Sqrt[C[4]+C[5] Sqrt[(C[0]+x C[1])/(C[2]+x C[3])]])/(x^2+1),x]*)


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
(*nestedQuadraticRadicalIntegrate*)


(* ::Input:: *)
(*int[Sqrt[c x^2-x Sqrt[-b x+a x^2]]/(x^3 Sqrt[-b x+a x^2]),x]*)


(* ::Input:: *)
(*int[Sqrt[a x^2+b x Sqrt[-(a/b^2)+(a^2 x^2)/b^2]]/(x Sqrt[-(a/b^2)+(a^2 x^2)/b^2]),x]*)


(* ::Input:: *)
(*int[(a x^2+b x Sqrt[-(a/b^2)+(a^2 x^2)/b^2])^(1/3)/Sqrt[-(a/b^2)+(a^2 x^2)/b^2],x]*)


(* ::Input:: *)
(*int[(a x^2+b x Sqrt[-(a/b^2)+(a^2 x^2)/b^2])^(1/4)/Sqrt[-(a/b^2)+(a^2 x^2)/b^2],x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[-(a/b^2)+(a^2 x^2)/b^2] Sqrt[a x^2+b x Sqrt[-(a/b^2)+(a^2 x^2)/b^2]]),x]*)


(* ::Input:: *)
(*int[Sqrt[-(a/b^2)+(a^2 x^2)/b^2]/(x Sqrt[a x^2+b x Sqrt[-(a/b^2)+(a^2 x^2)/b^2]]),x]*)


(* ::Input:: *)
(*int[Sqrt[-(a/b^2)+(a^2 x^2)/b^2]/(x^2 Sqrt[a x^2+b x Sqrt[-(a/b^2)+(a^2 x^2)/b^2]]),x]*)


(* ::Input:: *)
(*int[(x^3 Sqrt[-(a/b^2)+(a^2 x^2)/b^2])/Sqrt[a x^2+b x Sqrt[-(a/b^2)+(a^2 x^2)/b^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[-(a/b^2)+(a^2 x^2)/b^2]/Sqrt[a x^2+b x Sqrt[-(a/b^2)+(a^2 x^2)/b^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[-(a/b^2)+(a^2 x^2)/b^2] Sqrt[a x^2+b x Sqrt[-(a/b^2)+(a^2 x^2)/b^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[-(a/b^2)+(a^2 x^2)/b^2] (a x^2+b x Sqrt[-(a/b^2)+(a^2 x^2)/b^2])^(1/3),x]*)


(* ::Input:: *)
(*int[1/Sqrt[x-Sqrt[-1+x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[x^3+x^2 Sqrt[-1+x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+x Sqrt[-1+x^2]]/(x Sqrt[-1+x^2]),x]*)


(* ::Input:: *)
(*int[Sqrt[a x^2+x Sqrt[-b+a^2 x^2]]/(x Sqrt[-b+a^2 x^2]),x]*)


(* ::Input:: *)
(*int[x Sqrt[-1+x^2] Sqrt[x^2+x Sqrt[-1+x^2]],x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2] Sqrt[x^2+x Sqrt[-1+x^2]])/(x^2+1),x]*)


(* ::Input:: *)
(*int[(Sqrt[-1+x^2] Sqrt[x^2+x Sqrt[-1+x^2]])/(x+1),x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+x Sqrt[x+x^2]]/Sqrt[x+x^2],x]*)


(* ::Input:: *)
(*int[Sqrt[x^2+x Sqrt[x+x^2]]/(x Sqrt[x+x^2]),x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[x+x^2] Sqrt[x^2+x Sqrt[x+x^2]]),x]*)


(* ::Input:: *)
(*int[Sqrt[x+x^2]/(x Sqrt[x^2+x Sqrt[x+x^2]]),x]*)


(* ::Input:: *)
(*int[(x Sqrt[x+x^2])/Sqrt[x^2+x Sqrt[x+x^2]],x]*)


(* ::Input:: *)
(*int[(x^2 Sqrt[x+x^2])/Sqrt[x^2+x Sqrt[x+x^2]],x]*)


(* ::Input:: *)
(*int[(x^3 Sqrt[x+x^2])/Sqrt[x^2+x Sqrt[x+x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[-x+x^2]/Sqrt[x^2-x Sqrt[-x+x^2]],x]*)


(* ::Input:: *)
(*int[(Sqrt[-x+x^2] Sqrt[x^2-x Sqrt[-x+x^2]])/x^3,x]*)


(* ::Input:: *)
(*int[Sqrt[x^2-x Sqrt[-x+x^2]]/x^3,x]*)


(* ::Input:: *)
(*int[Sqrt[c x^2-x Sqrt[-b x+a x^2]]/x^3,x]*)


(* ::Input:: *)
(*int[1/(x^3 Sqrt[a x^2+b x Sqrt[-(a/b^2)+(a^2 x^2)/b^2]]),x]*)


(* ::Input:: *)
(*int[((x^4+c) Sqrt[-b+x^2] Sqrt[x+Sqrt[-b+x^2]])/x,x]*)


(* ::Input:: *)
(*int[((c x^4+d) Sqrt[-b+a^2 x^2] Sqrt[a x+Sqrt[-b+a^2 x^2]])/x,x]*)


(* ::Input:: *)
(*int[((c x^4+d) Sqrt[-b+a^2 x^2] Sqrt[a x+Sqrt[-b+a^2 x^2]])/x^2,x]*)


(* ::Input:: *)
(*int[(c x^4+d)/(x (Sqrt[-b+a^2 x^2] (a x+Sqrt[-b+a^2 x^2])^(1/4))),x]*)


(* ::Input:: *)
(*int[x/(Sqrt[-b+a^2 x^2] (a x+Sqrt[-b+a^2 x^2])^(1/4)),x]*)


(* ::Input:: *)
(*int[x^2/(Sqrt[-b+a^2 x^2] (a x+Sqrt[-b+a^2 x^2])^(1/4)),x]*)


(* ::Input:: *)
(*int[((-b+a^2 x^2)^(3/2) (a x+Sqrt[-b+a^2 x^2])^(1/4))/x,x]*)


(* ::Input:: *)
(*int[((-b+a^2 x^2)^(3/2) (a x+Sqrt[-b+a^2 x^2])^(1/4))/x^2,x]*)


(* ::Input:: *)
(*int[(a x+Sqrt[-b+a^2 x^2])^(1/4)/(x (-b+a^2 x^2)^(3/2)),x]*)


(* ::Input:: *)
(*int[(a x+Sqrt[-b+a^2 x^2])^(1/4)/(x^2 (-b+a^2 x^2)^(3/2)),x]*)


(* ::Input:: *)
(*int[((c x^2+d) (a x+Sqrt[-b+a^2 x^2])^(3/4))/(-b+a^2 x^2)^(5/2),x]*)


(* ::Input:: *)
(*int[((c x^2+d) (a x+Sqrt[-b+a^2 x^2])^(5/4))/(-b+a^2 x^2)^(3/2),x]*)


(* ::Input:: *)
(*int[((c x^2+d) (a x+Sqrt[-b+a^2 x^2])^(5/4))/(x (-b+a^2 x^2)^(5/2)),x]*)


(* ::Input:: *)
(*int[((c x^2+d)^2 (a x+Sqrt[-b+a^2 x^2])^(1/4))/((c x^2-d)^2 Sqrt[-b+a^2 x^2]),x]*)


(* ::Input:: *)
(*int[(a x+Sqrt[-b+a^2 x^2])^(1/6)/(x^3 Sqrt[-b+a^2 x^2]),x]*)


(* ::Input:: *)
(*int[1/Sqrt[1+Sqrt[a x+Sqrt[-b+a^2 x^2]]],x]*)


(* ::Input:: *)
(*int[x/Sqrt[1+Sqrt[a x+Sqrt[-b+a^2 x^2]]],x]*)


(* ::Input:: *)
(*int[x^2/Sqrt[1+Sqrt[a x+Sqrt[-b+a^2 x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[-b+a^2 x^2]/Sqrt[c+Sqrt[a x+Sqrt[-b+a^2 x^2]]],x]*)


(* ::Input:: *)
(*int[(Sqrt[-b+a^2 x^2] Sqrt[a x+Sqrt[-b+a^2 x^2]])/Sqrt[c+Sqrt[a x+Sqrt[-b+a^2 x^2]]],x]*)


(* ::Input:: *)
(*int[Sqrt[-b+a^2 x^2]/(Sqrt[a x+Sqrt[-b+a^2 x^2]] Sqrt[c+Sqrt[a x+Sqrt[-b+a^2 x^2]]]),x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[a x+Sqrt[-b+a^2 x^2]] Sqrt[c+Sqrt[a x+Sqrt[-b+a^2 x^2]]]),x]*)


(* ::Input:: *)
(*int[1/((a x+Sqrt[-b+a^2 x^2])^(1/3) (c+(a x+Sqrt[-b+a^2 x^2])^(1/3))^(1/4)),x]*)


(* ::Input:: *)
(*int[1/(Sqrt[-b+a^2 x^2] (a x+Sqrt[-b+a^2 x^2])^(1/3) (c+(a x+Sqrt[-b+a^2 x^2])^(1/3))^(1/4)),x]*)


(* ::Input:: *)
(*int[Sqrt[-b+a^2 x^2]/((a x+Sqrt[-b+a^2 x^2])^(1/3) (c+(a x+Sqrt[-b+a^2 x^2])^(1/3))^(1/4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-b+a^2 x^2] (a x+Sqrt[-b+a^2 x^2])^(1/3))/(c+(a x+Sqrt[-b+a^2 x^2])^(1/3))^(1/4),x]*)


(* ::Input:: *)
(*int[Sqrt[-b+a^2 x^2]/(c+(a x+Sqrt[-b+a^2 x^2])^(1/3))^(1/4),x]*)


(* ::Input:: *)
(*int[(a x+Sqrt[-b+a^2 x^2])^(1/3)/(c+(a x+Sqrt[-b+a^2 x^2])^(1/3))^(1/4),x]*)


(* ::Input:: *)
(*int[1/((a x+Sqrt[-b+a^2 x^2])^(1/4) (c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(Sqrt[-b+a^2 x^2] (a x+Sqrt[-b+a^2 x^2])^(1/4) (c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(1/3)),x]*)


(* ::Input:: *)
(*int[(c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(1/3)/(Sqrt[-b+a^2 x^2] (a x+Sqrt[-b+a^2 x^2])^(1/4)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-b+a^2 x^2] (c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(1/3))/(a x+Sqrt[-b+a^2 x^2])^(1/4),x]*)


(* ::Input:: *)
(*int[1/(Sqrt[-b+a^2 x^2] (c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(2/3)),x]*)


(* ::Input:: *)
(*int[1/(c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(1/3),x]*)


(* ::Input:: *)
(*int[1/(Sqrt[-b+a^2 x^2] (c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(1/3)),x]*)


(* ::Input:: *)
(*int[(c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(1/3),x]*)


(* ::Input:: *)
(*int[Sqrt[-b+a^2 x^2]/(c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(2/3),x]*)


(* ::Input:: *)
(*int[(c+(a x+Sqrt[-b+a^2 x^2])^(3/4))^(4/3)/Sqrt[-b+a^2 x^2],x]*)


(* ::Input:: *)
(*int[Sqrt[-b+a^2 x^2]/(a x^2+x Sqrt[-b+a^2 x^2])^(1/3),x]*)


(* ::Input:: *)
(*int[1/(Sqrt[-b+a^2 x^2] (a x^2+x Sqrt[-b+a^2 x^2])^(1/3)),x]*)


(* ::Input:: *)
(*int[(Sqrt[-b+a^2 x^2] (a x+Sqrt[-b+a^2 x^2])^(3/4))/(c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(2/3),x]*)


(* ::Input:: *)
(*int[1/(Sqrt[-b+a^2 x^2] Sqrt[a x+Sqrt[-b+a^2 x^2]] (c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(Sqrt[-b+a^2 x^2] Sqrt[a x+Sqrt[-b+a^2 x^2]] (c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(1/6)),x]*)


(* ::Input:: *)
(*int[1/(Sqrt[-b+a^2 x^2] (c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(1/6)),x]*)


(* ::Input:: *)
(*int[(c+(a x+Sqrt[-b+a^2 x^2])^(1/4))^(1/6)/Sqrt[-b+a^2 x^2],x]*)


(* ::Input:: *)
(*int[Sqrt[c+Sqrt[a x^2+x Sqrt[-b+a^2 x^2]]]/Sqrt[-b+a^2 x^2],x]*)


(* ::Input:: *)
(*int[(a x+Sqrt[-b x+a^2 x^2])^(3/4)/Sqrt[-b x+a^2 x^2],x]*)


(* ::Input:: *)
(*int[Sqrt[-b x+a^2 x^2]/Sqrt[a x^2+x Sqrt[-b x+a^2 x^2]],x]*)


(* ::Input:: *)
(*int[Sqrt[-b x+a^2 x^2]/(a x^2+x Sqrt[-b x+a^2 x^2])^(3/2),x]*)


(* ::Input:: *)
(*int[(-b x+a^2 x^2)^(3/2)/(a x^2+x Sqrt[-b x+a^2 x^2])^(3/2),x]*)


(* ::Input:: *)
(*int[1/(Sqrt[-b x+a^2 x^2] (a x^2+x Sqrt[-b x+a^2 x^2])^(3/2)),x]*)


(* ::Input:: *)
(*int[1/(x Sqrt[-b x+a^2 x^2] (a x^2+x Sqrt[-b x+a^2 x^2])^(3/2)),x]*)


(* ::Input:: *)
(*int[1/(x^2 Sqrt[-b x+a^2 x^2] (a x^2+x Sqrt[-b x+a^2 x^2])^(3/2)),x]*)


(* ::Input:: *)
(*int[x/(Sqrt[-b x+a^2 x^2] (a x^2+x Sqrt[-b x+a^2 x^2])^(3/2)),x]*)


(* ::Input:: *)
(*int[x^2/(Sqrt[-b x+a^2 x^2] (a x^2+x Sqrt[-b x+a^2 x^2])^(3/2)),x]*)


(* ::Input:: *)
(*int[(x^2 Sqrt[-b x+a^2 x^2])/(a x^2+x Sqrt[-b x+a^2 x^2])^(3/2),x]*)


(* ::Input:: *)
(*int[1/((-b x+a^2 x^2)^(3/2) (a x^2+x Sqrt[-b x+a^2 x^2])^(3/2)),x]*)


(* ::Input:: *)
(*int[1/((-b x+a^2 x^2)^(5/2) (a x^2+x Sqrt[-b x+a^2 x^2])^(3/2)),x]*)


(* ::Subsubsection::Closed:: *)
(*radicandFactorIntegrate*)


(* ::Input:: *)
(*int[1/(1+(9-6 x+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-5+2 x)/(4-4 x+x^2)^(1/4),x]*)


(* ::Input:: *)
(*int[1/((-5+2 x)^2 (4-4 x+x^2)^(1/4)),x]*)


(* ::Input:: *)
(*int[x/(1-4 x+6 x^2-4 x^3+x^4)^(1/5),x]*)


(* ::Input:: *)
(*int[1/(x (1-4 x+6 x^2-4 x^3+x^4)^(1/5)),x]*)


(* ::Input:: *)
(*int[(243-5265 x+47250 x^2-225810 x^3+615255 x^4-954733 x^5+820340 x^6-401440 x^7+112000 x^8-16640 x^9+1024 x^10)^(1/5),x]*)


(* ::Input:: *)
(*int[(256-256 x^2+96 x^4-16 x^6+x^8)^(1/8)/(-1+x^3),x]*)


(* ::Input:: *)
(*int[(1+x)/((-1+x^3) (256-256 x^2+96 x^4-16 x^6+x^8)^(1/8)),x]*)


(* ::Input:: *)
(*int[1/(x^3 (256-256 x^2+96 x^4-16 x^6+x^8)^(1/8)),x]*)


(* ::Input:: *)
(*int[(-1-2 x+x^2+3 x^3)/(-1+3 x-3 x^2+x^3)^(1/4),x]*)


(* ::Input:: *)
(*int[(-1-2 x+x^2+3 x^3) (-1+3 x-3 x^2+x^3)^(1/4),x]*)


(* ::Input:: *)
(*int[(-1+3 x-3 x^2+x^3)^(1/4)/(-1-2 x+x^2+3 x^3),x]*)


(* ::Input:: *)
(*int[1/((-1-2 x+x^2+3 x^3) (-1+3 x-3 x^2+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[1/((-1-2 x+x^2+3 x^3)^4 (-1+3 x-3 x^2+x^3)^(1/4)),x]*)


(* ::Input:: *)
(*int[(-1-2 x+x^2+3 x^3)^4/(-1+3 x-3 x^2+x^3)^(1/4),x]*)


(* ::Input:: *)
(*int[x^2/((x^2+1) (243-5265 x+47250 x^2-225810 x^3+615255 x^4-954733 x^5+820340 x^6-401440 x^7+112000 x^8-16640 x^9+1024 x^10)^(1/5)),x]*)


(* ::Input:: *)
(*int[x^2/((x^2+1) (243-5265 x+47250 x^2-225810 x^3+615255 x^4-954733 x^5+820340 x^6-401440 x^7+112000 x^8-16640 x^9+1024 x^10)^(1/10)),x]*)


(* ::Input:: *)
(*int[1/(1+2 x^4+x^8)^(1/8),x]*)


(* ::Input:: *)
(*int[(-1+x^2)/((1+x^2) (1+5 x^4+10 x^8+10 x^12+5 x^16+x^20)^(1/10)),x]*)


(* ::Subsubsection::Closed:: *)
(*powerExpandIntegrate*)


(* ::Input:: *)
(*int[1/(x^2 (1+x-2 x^2-2 x^3+x^4+x^5)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^2 (-1+2 x+2 x^2-6 x^3+6 x^5-2 x^6-2 x^7+x^8)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^2 (-1+5 x-7 x^2-2 x^3+10 x^4-2 x^5-5 x^6+x^7+x^8)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^2 (-1+5 x-7 x^2-2 x^3+10 x^4-2 x^5-5 x^6+x^7+x^8)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(x^2 (-1-x+5 x^2+2 x^3-10 x^4+2 x^5+7 x^6-5 x^7+x^8)^(1/3)),x]*)


(* ::Input:: *)
(*int[(-1-x+5 x^2+2 x^3-10 x^4+2 x^5+7 x^6-5 x^7+x^8)^(1/3)/x^2,x]*)


(* ::Input:: *)
(*int[1/(x^2 (-4-8 x+11 x^2+17 x^3-20 x^4-7 x^5+16 x^6-7 x^7+x^8)^(1/3)),x]*)


(* ::Input:: *)
(*int[1/(1+2 x-x^2-4 x^3-x^4+2 x^5+x^6)^(1/6),x]*)


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
