(*

results = Timing[ int[#, x, "RationalUndeterminedOnly" -> True] ] & /@ mapleIntegrands;
Count[results, ans_ /; FreeQ[ans, IntegrateAlgebraic]]/Length[mapleIntegrands] // N
results[[All,2]]

*)


mapleIntegrandSolutions = {
 (2*ArcTan[Sqrt[-3 + x^3]/Sqrt[3]])/(3*Sqrt[3]), 
 (Sqrt[2]*ArcTan[Sqrt[-2 + x^3]/Sqrt[2]])/3, Defer[IntegrateAlgebraic][
  (1 + x)/((-2 + x)*Sqrt[-2 + x^3]), x], (2*ArcTan[Sqrt[-1 + x^3]])/3, 
 (2*Sqrt[-1 + x^3])/3, Defer[IntegrateAlgebraic][
  (-1 + x)/((2 + x)*Sqrt[-1 + x^3]), x], (-2*ArcTanh[Sqrt[1 + x^3]])/3, 
 (2*Sqrt[1 + x^3])/3, Defer[IntegrateAlgebraic][
  (1 + x)/((-2 + x)*Sqrt[1 + x^3]), x], 
 -(Sqrt[2]*ArcTanh[Sqrt[2 + x^3]/Sqrt[2]])/3, 
 Defer[IntegrateAlgebraic][(-1 + x)/((2 + x)*Sqrt[2 + x^3]), x], 
 (-2*ArcTanh[Sqrt[3 + x^3]/Sqrt[3]])/(3*Sqrt[3]), 
 Defer[IntegrateAlgebraic][x/((2 + x)*Sqrt[-1 - 2*x + x^3]), x], 
 Defer[IntegrateAlgebraic][(2 + x)/(x*Sqrt[-1 - 2*x + x^3]), x], 
 Defer[IntegrateAlgebraic][(-2 + x)/(x*Sqrt[1 - 2*x + x^3]), x], 
 Defer[IntegrateAlgebraic][x/((-2 + x)*Sqrt[1 - 2*x + x^3]), x], 
 Defer[IntegrateAlgebraic][(-3 + x)/((1 + x)*Sqrt[-2 + x + x^3]), x], 
 Defer[IntegrateAlgebraic][(1 + x)/((-3 + x)*Sqrt[-2 + x + x^3]), x], 
 Defer[IntegrateAlgebraic][(-1 + x)/((3 + x)*Sqrt[2 + x + x^3]), x], 
 Defer[IntegrateAlgebraic][(3 + x)/((-1 + x)*Sqrt[2 + x + x^3]), x], 
 (-2*ArcTanh[(Sqrt[3]*x)/Sqrt[-1 + 3*x + x^3]])/Sqrt[3], 
 (2*ArcTan[Sqrt[1 + 3*x + x^3]/(Sqrt[3]*x)])/Sqrt[3], 
 Defer[IntegrateAlgebraic][(-3 + x)/((-1 + x)*Sqrt[2 + x - 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(-1 + x)/((-3 + x)*Sqrt[2 + x - 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][1/((-1 + x)*Sqrt[-3 + 3*x - 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][x/((-3 + x)*Sqrt[-3 + 3*x - 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][1/((-1 + x)*Sqrt[-2 + 3*x - 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(-2 + x)/((1 + x)*Sqrt[-2 + 3*x - 3*x^2 + x^3]), 
  x], Defer[IntegrateAlgebraic][1/((-1 + x)*Sqrt[1 + 3*x - 3*x^2 + x^3]), x], 
 Sqrt[2/3]*ArcTan[Sqrt[1 + 3*x - 3*x^2 + x^3]/(Sqrt[6]*x)], 
 Defer[IntegrateAlgebraic][1/((-1 + x)*Sqrt[2 + 3*x - 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][1/((-1 + x)*Sqrt[3 + 3*x - 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(-3 + x)/(x*Sqrt[1 - 2*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(-1 + 2*x)/((-2 + x)*Sqrt[1 - 2*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(1 + 2*x)/((-2 + x)*Sqrt[2 - 2*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][x/((-3 + x)*Sqrt[1 - x - 2*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(-2 + x)/(x*Sqrt[-1 + 2*x - 2*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][x/((-2 + x)*Sqrt[-1 + 2*x - 2*x^2 + x^3]), x], 
 -2*ArcTanh[x/Sqrt[-1 + 3*x - 2*x^2 + x^3]], 
 (2*ArcTan[Sqrt[1 + 3*x - 2*x^2 + x^3]/(Sqrt[5]*x)])/Sqrt[5], 
 Defer[IntegrateAlgebraic][(3 + x)/(x*Sqrt[-1 - 2*x - x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(-1 + x)/((2 + x)*Sqrt[1 - 2*x - x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(-3 + 2*x)/((1 + x)*Sqrt[-1 - x - x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(1 + x)/((-2 + x)*Sqrt[-3 + 2*x - x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(-3 + x)/(x*Sqrt[-1 + 2*x - x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(-3 + x)/((1 + x)*Sqrt[-3 + 3*x - x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(1 + x)/((-3 + x)*Sqrt[-3 + 3*x - x^2 + x^3]), x], 
 -(Sqrt[2]*ArcTanh[(Sqrt[2]*x)/Sqrt[-1 + 3*x - x^2 + x^3]]), 
 ArcTan[Sqrt[1 + 3*x - x^2 + x^3]/(2*x)], Defer[IntegrateAlgebraic][
  (1 + x)/((-2 + x)*Sqrt[-1 - 2*x + x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(-3 + x)/(x*Sqrt[1 - 2*x + x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(3 + 2*x)/((-1 + x)*Sqrt[1 - x + x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(3 + x)/(x*Sqrt[1 + 2*x + x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(1 + x)/((-2 + 2*x + x^2)*
    Sqrt[2 + 2*x + x^2 + x^3]), x], Defer[IntegrateAlgebraic][
  (-1 + x)/((2 + x)*Sqrt[3 + 2*x + x^2 + x^3]), x], 
 -ArcTanh[(2*x)/Sqrt[-1 + 3*x + x^2 + x^3]], 
 Sqrt[2]*ArcTan[Sqrt[1 + 3*x + x^2 + x^3]/(Sqrt[2]*x)], 
 Defer[IntegrateAlgebraic][(-1 + x)/((3 + x)*Sqrt[3 + 3*x + x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(3 + x)/((-1 + x)*Sqrt[3 + 3*x + x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(-1 + 2*x)/((2 + x)*Sqrt[-2 + 2*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(3 + x)/(x*Sqrt[-1 + 2*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(1 + 2*x)/((2 + x)*Sqrt[-1 + 2*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][x/((3 + x)*Sqrt[-1 - x + 2*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][x/((2 + x)*Sqrt[1 + 2*x + 2*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(2 + x)/(x*Sqrt[1 + 2*x + 2*x^2 + x^3]), x], 
 (-2*ArcTanh[(Sqrt[5]*x)/Sqrt[-1 + 3*x + 2*x^2 + x^3]])/Sqrt[5], 
 2*ArcTan[Sqrt[1 + 3*x + 2*x^2 + x^3]/x], Defer[IntegrateAlgebraic][
  (1 + x)/((3 + x)*Sqrt[-2 + x + 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(3 + x)/((1 + x)*Sqrt[-2 + x + 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][1/((1 + x)*Sqrt[-3 + 3*x + 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][1/((1 + x)*Sqrt[-2 + 3*x + 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][1/((1 + x)*Sqrt[-1 + 3*x + 3*x^2 + x^3]), x], 
 -(Sqrt[2/3]*ArcTanh[(Sqrt[6]*x)/Sqrt[-1 + 3*x + 3*x^2 + x^3]]), 
 Defer[IntegrateAlgebraic][1/((1 + x)*Sqrt[2 + 3*x + 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][(2 + x)/((-1 + x)*Sqrt[2 + 3*x + 3*x^2 + x^3]), 
  x], Defer[IntegrateAlgebraic][1/((1 + x)*Sqrt[3 + 3*x + 3*x^2 + x^3]), x], 
 Defer[IntegrateAlgebraic][x/((3 + x)*Sqrt[3 + 3*x + 3*x^2 + x^3]), x], 
 Log[x^2 + Sqrt[-3 + x^4]]/2, ArcTan[Sqrt[-2 + x^4]/Sqrt[2]]/(2*Sqrt[2]), 
 Log[x^2 + Sqrt[-2 + x^4]]/2, ArcTan[Sqrt[-1 + x^4]]/2, 
 Log[x^2 + Sqrt[-1 + x^4]]/2, Sqrt[-1 + x^4]/(2 + 2*x^2), 
 -ArcTanh[Sqrt[1 + x^4]]/2, Log[x^2 + Sqrt[1 + x^4]]/2, 
 -(Sqrt[2]*ArcTanh[(Sqrt[2]*x)/(1 + 2*x + x^2 + Sqrt[1 + x^4])]), 
 -(Sqrt[2]*ArcTanh[(Sqrt[2]*x)/(1 - 2*x + x^2 + Sqrt[1 + x^4])]), 
 -(ArcTanh[(-1 + x^2 + Sqrt[1 + x^4])/Sqrt[2]]/Sqrt[2]), 
 -(ArcTan[(Sqrt[2]*x)/Sqrt[1 + x^4]]/Sqrt[2]), 
 Defer[IntegrateAlgebraic][(1 + x^2)/((-1 + x)*Sqrt[1 + x^4]), x], 
 Defer[IntegrateAlgebraic][(1 + x^2)/((1 + x)*Sqrt[1 + x^4]), x], 
 Defer[IntegrateAlgebraic][(1 + x^2)/(x*(1 + x)*Sqrt[1 + x^4]), x], 
 (-2*ArcTanh[(Sqrt[3]*x)/(-1 - x + x^2 + Sqrt[1 + x^4])])/Sqrt[3], 
 -2*ArcTan[x/(1 - x + x^2 + Sqrt[1 + x^4])], 
 -ArcTanh[(Sqrt[2]*x)/(1 - 2*x + x^2 + Sqrt[1 + x^4])]/(2*Sqrt[2]) - 
  (3*ArcTanh[(Sqrt[2]*x)/(1 + 2*x + x^2 + Sqrt[1 + x^4])])/(2*Sqrt[2]), 
 -(ArcTan[(Sqrt[2]*x)/Sqrt[1 + x^4]]/Sqrt[2]) - 
  ArcTanh[(2*Sqrt[2]*x^2)/(1 + 2*x^2 + x^4 + (-1 + x^2)*Sqrt[1 + x^4])]/
   (2*Sqrt[2]), -2*ArcTan[x/(1 + x + x^2 + Sqrt[1 + x^4])], 
 (-3*ArcTanh[(Sqrt[2]*x)/(1 - 2*x + x^2 + Sqrt[1 + x^4])])/(2*Sqrt[2]) - 
  ArcTanh[(Sqrt[2]*x)/(1 + 2*x + x^2 + Sqrt[1 + x^4])]/(2*Sqrt[2]), 
 -ArcTanh[Sqrt[2 + x^4]/Sqrt[2]]/(2*Sqrt[2]), Log[x^2 + Sqrt[2 + x^4]]/2, 
 Log[x^2 + Sqrt[3 + x^4]]/2, ArcTan[(-x^2 + Sqrt[-3 - 3*x^2 + x^4])/Sqrt[3]]/
  Sqrt[3], -Log[3 - 2*x^2 + 2*Sqrt[-3 - 3*x^2 + x^4]]/2, 
 -Log[3 - 2*x^2 + 2*Sqrt[-2 - 3*x^2 + x^4]]/2, 
 -Log[3 - 2*x^2 + 2*Sqrt[-1 - 3*x^2 + x^4]]/2, 
 -2*ArcTan[x/(1 + 2*x + x^2 + Sqrt[1 - 3*x^2 + x^4])], 
 -2*ArcTan[x/(1 - 2*x + x^2 + Sqrt[1 - 3*x^2 + x^4])], 
 Defer[IntegrateAlgebraic][1/((-1 + x)*Sqrt[3 - 3*x^2 + x^4]), x], 
 -Log[3 - 2*x^2 + 2*Sqrt[3 - 3*x^2 + x^4]]/2, 
 -Log[1 - x^2 + Sqrt[-2 - 2*x^2 + x^4]]/2, 
 -Log[1 - x^2 + Sqrt[-1 - 2*x^2 + x^4]]/2, 
 -(ArcTanh[(-x^2 + Sqrt[2 - 2*x^2 + x^4])/Sqrt[2]]/Sqrt[2]), 
 -(ArcTanh[(-x^2 + Sqrt[2 - 2*x^2 + x^4])/Sqrt[2]]/Sqrt[2]) - 
  (3*Log[1 - x^2 + Sqrt[2 - 2*x^2 + x^4]])/2, 
 -(ArcTanh[(-x^2 + Sqrt[3 - 2*x^2 + x^4])/Sqrt[3]]/Sqrt[3]), 
 -Log[1 - x^2 + Sqrt[3 - 2*x^2 + x^4]]/2, Defer[IntegrateAlgebraic][
  x/((-3 + 2*x)*Sqrt[-2 + 3*x - 2*x^2 + x^4]), x], 
 -Log[1 - 2*x^2 + 2*Sqrt[-3 - x^2 + x^4]]/2, 
 ArcTan[(-x^2 + Sqrt[-2 - x^2 + x^4])/Sqrt[2]]/Sqrt[2], 
 -Log[1 - 2*x^2 + 2*Sqrt[-2 - x^2 + x^4]]/2, 
 -ArcTan[x^2 - Sqrt[-1 - x^2 + x^4]], 
 -Log[1 - 2*x^2 + 2*Sqrt[-1 - x^2 + x^4]]/2, 
 ArcTan[1 - x^2 + Sqrt[-1 - x^2 + x^4]], 
 -ArcTan[x^2 - Sqrt[-1 - x^2 + x^4]] - 
  Log[1 - 2*x^2 + 2*Sqrt[-1 - x^2 + x^4]]/2, Defer[IntegrateAlgebraic][
  (1 - x + x^2)/(x*(1 + x)*Sqrt[-1 - x^2 + x^4]), x], 
 ArcTanh[x^2 - Sqrt[1 - x^2 + x^4]], -Log[1 - 2*x^2 + 2*Sqrt[1 - x^2 + x^4]]/
  2, -2*ArcTanh[x/(1 + 2*x + x^2 + Sqrt[1 - x^2 + x^4])], 
 -2*ArcTanh[x/(1 - 2*x + x^2 + Sqrt[1 - x^2 + x^4])], 
 -(ArcTanh[(-1 - x^2 + Sqrt[1 - x^2 + x^4])/Sqrt[3]]/Sqrt[3]), 
 -(ArcTan[(Sqrt[3]*x)/Sqrt[1 - x^2 + x^4]]/Sqrt[3]), 
 Defer[IntegrateAlgebraic][(1 + x^2)/((-1 + x)*Sqrt[1 - x^2 + x^4]), x], 
 ArcTanh[x^2 - Sqrt[1 - x^2 + x^4]] - Log[1 - 2*x^2 + 2*Sqrt[1 - x^2 + x^4]]/
   2, Defer[IntegrateAlgebraic][(1 + x^2)/((-1 + x)*x*Sqrt[1 - x^2 + x^4]), 
  x], Defer[IntegrateAlgebraic][(1 + x^2)/((1 + x)*Sqrt[1 - x^2 + x^4]), x], 
 Defer[IntegrateAlgebraic][(1 + x^2)/(x*(1 + x)*Sqrt[1 - x^2 + x^4]), x], 
 -ArcTanh[x/Sqrt[1 - x^2 + x^4]], 
 -(Sqrt[2]*ArcTanh[(Sqrt[2]*x)/(-1 - x + x^2 + Sqrt[1 - x^2 + x^4])]), 
 -(ArcTan[(Sqrt[3]*x)/Sqrt[1 - x^2 + x^4]]/Sqrt[3]) + 
  ArcTanh[(2*Sqrt[3]*x^2)/(1 + 2*x^2 + x^4 + (-1 + x^2)*Sqrt[1 - x^2 + x^4])]/
   (2*Sqrt[3]), 
 -(Sqrt[2]*ArcTan[(Sqrt[2]*x)/(1 - x + x^2 + Sqrt[1 - x^2 + x^4])]), 
 -(ArcTan[(Sqrt[3]*x)/Sqrt[1 - x^2 + x^4]]/Sqrt[3]) - 
  ArcTanh[(2*Sqrt[3]*x^2)/(1 + 2*x^2 + x^4 + (-1 + x^2)*Sqrt[1 - x^2 + x^4])]/
   (2*Sqrt[3]), 
 -(Sqrt[2]*ArcTan[(Sqrt[2]*x)/(1 + x + x^2 + Sqrt[1 - x^2 + x^4])]), 
 (-3*ArcTanh[x/(1 - 2*x + x^2 + Sqrt[1 - x^2 + x^4])])/2 - 
  ArcTanh[x/(1 + 2*x + x^2 + Sqrt[1 - x^2 + x^4])]/2, 
 -(ArcTanh[(-x^2 + Sqrt[2 - x^2 + x^4])/Sqrt[2]]/Sqrt[2]), 
 -Log[1 - 2*x^2 + 2*Sqrt[2 - x^2 + x^4]]/2, 
 ((-2*I)*ArcTan[(Sqrt[15]*x^2)/(4 + 4*x + x^2 - 
      (4*I)*Sqrt[-1 - 2*x - x^2 + x^4])])/Sqrt[15], 
 (-2*I)*Log[x] + I*Log[I + I*x + Sqrt[-1 - 2*x - x^2 + x^4]], 
 (2*I)*Log[x] - I*Log[I - I*x + Sqrt[-1 + 2*x - x^2 + x^4]], 
 ((2*I)*ArcTan[(Sqrt[15]*x^2)/(4 - 4*x + x^2 - 
      (4*I)*Sqrt[-1 + 2*x - x^2 + x^4])])/Sqrt[15], 
 ArcTan[(-x^2 + Sqrt[-2 + x^2 + x^4])/Sqrt[2]]/Sqrt[2], 
 -Log[-1 - 2*x^2 + 2*Sqrt[-2 + x^2 + x^4]]/2, 
 -ArcTan[x^2 - Sqrt[-1 + x^2 + x^4]], 
 -Log[-1 - 2*x^2 + 2*Sqrt[-1 + x^2 + x^4]]/2, 
 Defer[IntegrateAlgebraic][(-1 + 2*x)/((1 + x)*Sqrt[-1 + x^2 + x^4]), x], 
 Defer[IntegrateAlgebraic][(1 + x + x^2)/((-1 + x)*Sqrt[-1 + x^2 + x^4]), x], 
 ArcTanh[x^2 - Sqrt[1 + x^2 + x^4]], -Log[-1 - 2*x^2 + 2*Sqrt[1 + x^2 + x^4]]/
  2, (-2*ArcTanh[(Sqrt[3]*x)/(1 + 2*x + x^2 + Sqrt[1 + x^2 + x^4])])/Sqrt[3], 
 (-2*ArcTanh[Sqrt[1 + x^2 + x^4]/(Sqrt[3]*(1 - x + x^2))])/Sqrt[3], 
 -(ArcTanh[(1 - x^2 + Sqrt[1 + x^2 + x^4])/Sqrt[3]]/Sqrt[3]), 
 -ArcTan[x/Sqrt[1 + x^2 + x^4]], Defer[IntegrateAlgebraic][
  (1 + x^2)/((-1 + x)*Sqrt[1 + x^2 + x^4]), x], 
 ArcTanh[x^2 - Sqrt[1 + x^2 + x^4]] - Log[-1 - 2*x^2 + 2*Sqrt[1 + x^2 + x^4]]/
   2, Defer[IntegrateAlgebraic][(1 + x^2)/((-1 + x)*x*Sqrt[1 + x^2 + x^4]), 
  x], Defer[IntegrateAlgebraic][(1 + x^2)/((1 + x)*Sqrt[1 + x^2 + x^4]), x], 
 Defer[IntegrateAlgebraic][(1 + x^2)/(x*(1 + x)*Sqrt[1 + x^2 + x^4]), x], 
 -(ArcTanh[(Sqrt[3]*x)/Sqrt[1 + x^2 + x^4]]/Sqrt[3]), 
 ArcTan[(-1 + x^2 + Sqrt[1 + x^2 + x^4])/(3*x)] - 
  ArcTan[(-1 + x^2 + Sqrt[1 + x^2 + x^4])/x] + 
  ArcTanh[(2*x^2)/(1 + 2*x^2 + x^4 + (-1 + x^2)*Sqrt[1 + x^2 + x^4])]/2, 
 -ArcTanh[(2*x)/(-1 + x + x^2 + Sqrt[1 + x^2 + x^4])], 
 ArcTan[(-1 + x^2 + Sqrt[1 + x^2 + x^4])/(3*x)] - 
  ArcTan[(-1 + x^2 + Sqrt[1 + x^2 + x^4])/x] - 
  ArcTanh[(2*x^2)/(1 + 2*x^2 + x^4 + (-1 + x^2)*Sqrt[1 + x^2 + x^4])]/2, 
 Sqrt[1 + x^2 + x^4]/(1 + x + x^2), 
 -(Sqrt[3]*ArcTanh[Sqrt[1 + x^2 + x^4]/(Sqrt[3]*(1 - x + x^2))])/2 + 
  ArcTanh[(Sqrt[3]*Sqrt[1 + x^2 + x^4])/(1 - x + x^2)]/(2*Sqrt[3]), 
 -Log[-1 - 2*x^2 + 2*Sqrt[2 + x^2 + x^4]]/2, 
 -(ArcTanh[(-x^2 + Sqrt[3 + x^2 + x^4])/Sqrt[3]]/Sqrt[3]), 
 -Log[-1 - x^2 + Sqrt[-3 + 2*x^2 + x^4]]/2, 
 ArcTan[(-x^2 + Sqrt[-2 + 2*x^2 + x^4])/Sqrt[2]]/Sqrt[2], 
 -Log[-1 - x^2 + Sqrt[-2 + 2*x^2 + x^4]]/2, 
 -Log[-1 - x^2 + Sqrt[-1 + 2*x^2 + x^4]]/2, 
 -(ArcTanh[(-x^2 + Sqrt[2 + 2*x^2 + x^4])/Sqrt[2]]/Sqrt[2]), 
 -Log[-1 - x^2 + Sqrt[2 + 2*x^2 + x^4]]/2, 
 -(ArcTanh[(-x^2 + Sqrt[3 + 2*x^2 + x^4])/Sqrt[3]]/Sqrt[3]), 
 -Log[-1 - x^2 + Sqrt[3 + 2*x^2 + x^4]]/2, 
 ArcTan[(-x^2 + Sqrt[-2 + 3*x^2 + x^4])/Sqrt[2]]/Sqrt[2], 
 -Log[-3 - 2*x^2 + 2*Sqrt[-2 + 3*x^2 + x^4]]/2, 
 ArcTanh[x^2 - Sqrt[1 + 3*x^2 + x^4]], 
 (-2*ArcTanh[(Sqrt[5]*x)/(1 + 2*x + x^2 + Sqrt[1 + 3*x^2 + x^4])])/Sqrt[5], 
 -(ArcTanh[(-x^2 + Sqrt[2 + 3*x^2 + x^4])/Sqrt[2]]/Sqrt[2]), 
 -Log[-3 - 2*x^2 + 2*Sqrt[3 + 3*x^2 + x^4]]/2, 
 (-2*ArcTan[(1 - 2*x + x^2 - Sqrt[1 - 3*x - 3*x^2 - 3*x^3 + x^4])/
     (Sqrt[7]*x)])/Sqrt[7], Defer[IntegrateAlgebraic][
  1/(x*Sqrt[-2 - 2*x^2 - 3*x^3 + x^4]), x], 
 Sqrt[2/3]*ArcTanh[Sqrt[1 - 3*x - 2*x^2 - 3*x^3 + x^4]/
    (Sqrt[6]*(1 + x + x^2))], 
 -(Sqrt[2/3]*ArcTan[(1 - 2*x + x^2 - Sqrt[1 - 3*x - 2*x^2 - 3*x^3 + x^4])/
     (Sqrt[6]*x)]), 
 (2*ArcTanh[(Sqrt[7]*x)/(1 + 2*x + x^2 - Sqrt[1 - 3*x - x^2 - 3*x^3 + x^4])])/
  Sqrt[7], (-2*ArcTan[(1 - 2*x + x^2 - Sqrt[1 - 3*x - x^2 - 3*x^3 + x^4])/
     (Sqrt[5]*x)])/Sqrt[5], Defer[IntegrateAlgebraic][
  1/(x*Sqrt[3 + 2*x - x^2 - 3*x^3 + x^4]), x], 
 (2*ArcTanh[(3*x)/(1 + 2*x + x^2 - Sqrt[1 - 3*x + x^2 - 3*x^3 + x^4])])/3, 
 (-2*ArcTan[(1 - 2*x + x^2 - Sqrt[1 - 3*x + x^2 - 3*x^3 + x^4])/(Sqrt[3]*x)])/
  Sqrt[3], Defer[IntegrateAlgebraic][
  1/((-2 + 3*x)*Sqrt[-1 + 2*x + x^2 - 3*x^3 + x^4]), x], 
 Sqrt[2/5]*ArcTanh[(Sqrt[2/5]*Sqrt[1 - 3*x + 2*x^2 - 3*x^3 + x^4])/
    (1 + x^2)], 
 -2*ArcTan[(1 - 2*x + x^2 - Sqrt[1 - 3*x + 3*x^2 - 3*x^3 + x^4])/x], 
 -(Sqrt[2]*ArcTan[(1 - 2*x + x^2 - Sqrt[1 - 2*x - 2*x^3 + x^4])/
     (Sqrt[2]*x)]), -Log[1 + 2*x - 2*x^2 + 2*Sqrt[-3 + x - 2*x^3 + x^4]], 
 -Log[1 + 2*x - 2*x^2 + 2*Sqrt[-2 + x - 2*x^3 + x^4]], 
 -Log[1 + 2*x - 2*x^2 + 2*Sqrt[1 + x - 2*x^3 + x^4]], 
 (-2*ArcTanh[(-1 + 4*x - 4*x^2 + 4*Sqrt[1 + x - 2*x^3 + x^4])/Sqrt[21]])/
  Sqrt[21], -Log[1 + 2*x - 2*x^2 + 2*Sqrt[1 + x - 2*x^3 + x^4]], 
 (-2*ArcTanh[(-1 + 4*x - 4*x^2 + 4*Sqrt[2 + x - 2*x^3 + x^4])/Sqrt[37]])/
  Sqrt[37], Defer[IntegrateAlgebraic][
  (3 + x)/((-1 + x)*Sqrt[1 + 2*x - 2*x^3 + x^4]), x], 
 Defer[IntegrateAlgebraic][x/Sqrt[-3 - 3*x - 3*x^2 - 2*x^3 + x^4], x], 
 Defer[IntegrateAlgebraic][x/Sqrt[-2 - 2*x - 3*x^2 - 2*x^3 + x^4], x], 
 (2*ArcTanh[(4 + 4*x + x^2 - Sqrt[-1 - 2*x - 3*x^2 - 2*x^3 + x^4])/
     (Sqrt[23]*(1 + x))])/Sqrt[23], 
 2*ArcTan[(-x^2 + Sqrt[-1 - 2*x - 3*x^2 - 2*x^3 + x^4])/(1 + x)], 
 (-2*ArcTan[(1 - 2*x + x^2 - Sqrt[1 - 2*x - 3*x^2 - 2*x^3 + x^4])/
     (Sqrt[5]*x)])/Sqrt[5], Defer[IntegrateAlgebraic][
  x/Sqrt[-1 - x - 3*x^2 - 2*x^3 + x^4], x], Defer[IntegrateAlgebraic][
  x/Sqrt[3 + 3*x - 3*x^2 - 2*x^3 + x^4], x], Defer[IntegrateAlgebraic][
  (1 + x)/((-2 + x)*Sqrt[3 - 2*x^2 - 2*x^3 + x^4]), x], 
 Defer[IntegrateAlgebraic][
  (2 + x)/((-1 + x)*Sqrt[-3 - 2*x - 2*x^2 - 2*x^3 + x^4]), x], 
 Defer[IntegrateAlgebraic][
  1/((1 + 3*x)*Sqrt[-3 - 2*x - 2*x^2 - 2*x^3 + x^4]), x], 
 ArcTanh[(2*x)/(1 + 2*x + x^2 - Sqrt[1 - 2*x - 2*x^2 - 2*x^3 + x^4])], 
 -ArcTan[(1 - 2*x + x^2 - Sqrt[1 - 2*x - 2*x^2 - 2*x^3 + x^4])/(2*x)], 
 -Log[3 + 2*x - 2*x^2 + 2*Sqrt[-1 + 3*x - 2*x^2 - 2*x^3 + x^4]], 
 -Log[3 + 2*x - 2*x^2 + 2*Sqrt[1 + 3*x - 2*x^2 - 2*x^3 + x^4]], 
 (-2*ArcTanh[(-1 + 4*x - 4*x^2 + 4*Sqrt[3 + 3*x - 2*x^2 - 2*x^3 + x^4])/
     Sqrt[61]])/Sqrt[61], Defer[IntegrateAlgebraic][
  (2 + x)/((-1 + x)*Sqrt[-2 - x^2 - 2*x^3 + x^4]), x], 
 (2*ArcTanh[Sqrt[1 - 2*x - x^2 - 2*x^3 + x^4]/(Sqrt[5]*(1 + x + x^2))])/
  Sqrt[5], (-2*ArcTan[(1 - 2*x + x^2 - Sqrt[1 - 2*x - x^2 - 2*x^3 + x^4])/
     (Sqrt[3]*x)])/Sqrt[3], 
 (2*ArcTan[(-1 + 4*x - 4*x^2 + 4*Sqrt[-2 + 2*x - x^2 - 2*x^3 + x^4])/
     Sqrt[23]])/Sqrt[23], 
 (2*ArcTan[(-1 + 4*x - 4*x^2 + 4*Sqrt[-1 + 2*x - x^2 - 2*x^3 + x^4])/
     Sqrt[7]])/Sqrt[7], 
 (-2*ArcTanh[(-1 + 4*x - 4*x^2 + 4*Sqrt[3 + 2*x - x^2 - 2*x^3 + x^4])/
     Sqrt[57]])/Sqrt[57], Log[-x + x^2 + Sqrt[-2 + x^2 - 2*x^3 + x^4]], 
 Log[-x + x^2 + Sqrt[1 + x^2 - 2*x^3 + x^4]], 
 Log[-x + x^2 + Sqrt[2 + x^2 - 2*x^3 + x^4]], 
 (-2*ArcTanh[(1 - 4*x + 4*x^2 + 4*Sqrt[2 + x^2 - 2*x^3 + x^4])/Sqrt[33]])/
  Sqrt[33], -Log[2 - x + x^2 + Sqrt[3 + x^2 - 2*x^3 + x^4]]/7 + 
  Log[-3 - 2*x + 2*x^2 + 2*Sqrt[3 + x^2 - 2*x^3 + x^4]]/7, 
 Defer[IntegrateAlgebraic][(-2 + 3*x)/Sqrt[3 - 3*x + x^2 - 2*x^3 + x^4], x], 
 (2*ArcTanh[(Sqrt[7]*x)/(1 + 2*x + x^2 - Sqrt[1 - 2*x + x^2 - 2*x^3 + x^4])])/
  Sqrt[7], -2*ArcTan[(1 - 2*x + x^2 - Sqrt[1 - 2*x + x^2 - 2*x^3 + x^4])/x], 
 Defer[IntegrateAlgebraic][(-2 + 3*x)/Sqrt[-2 + 2*x + x^2 - 2*x^3 + x^4], x], 
 Defer[IntegrateAlgebraic][x/((-3 + 2*x)*Sqrt[-2 + 2*x + x^2 - 2*x^3 + x^4]), 
  x], 2*ArcTan[(-x^2 + Sqrt[-1 + 2*x + x^2 - 2*x^3 + x^4])/(-1 + x)], 
 (2*ArcTanh[(Sqrt[3]*x)/(-1 - 2*x + x^2 - Sqrt[1 + 2*x + x^2 - 2*x^3 + 
        x^4])])/Sqrt[3], 
 (2*ArcTan[(Sqrt[7/5]*Sqrt[-2 - x + 2*x^2 - 2*x^3 + x^4])/(2 - x + x^2)])/
  Sqrt[35], 
 (-2*ArcTanh[(-1 + 4*x - 4*x^2 + 4*Sqrt[1 - x + 2*x^2 - 2*x^3 + x^4])/
     Sqrt[13]])/Sqrt[13], 
 -Log[-1 + 2*x - 2*x^2 + 2*Sqrt[2 - x + 2*x^2 - 2*x^3 + x^4]], 
 -Log[1 + x - x^2 + Sqrt[2 - 2*x + 3*x^2 - 2*x^3 + x^4]]/5 + 
  Log[-3 + 2*x - 2*x^2 + 2*Sqrt[2 - 2*x + 3*x^2 - 2*x^3 + x^4]]/5, 
 -RootSum[8 + 16*#1 + 12*#1^2 + #1^4 & , 
    (-Log[x] + Log[1 - x^2 + Sqrt[1 + x - x^3 + x^4] - x*#1] - 2*Log[x]*#1 + 
       2*Log[1 - x^2 + Sqrt[1 + x - x^3 + x^4] - x*#1]*#1)/
      (4 + 6*#1 + #1^3) & ]/2, RootSum[8 + 16*#1 + 12*#1^2 + #1^4 & , 
   (2*Log[x] - 2*Log[1 - x^2 + Sqrt[1 + x - x^3 + x^4] - x*#1] - 
      Log[x]*#1^2 + Log[1 - x^2 + Sqrt[1 + x - x^3 + x^4] - x*#1]*#1^2)/
     (4 + 6*#1 + #1^3) & ]/2, 
 Log[x] - Log[2 + x - 2*x^2 + 2*Sqrt[1 + x - x^3 + x^4]], 
 Sqrt[2]*ArcTanh[(Sqrt[2]*x)/(-1 + x^2 - Sqrt[1 + x - x^3 + x^4])], 
 Sqrt[2]*ArcTanh[(Sqrt[2]*x)/(-1 - x + x^2 - Sqrt[1 + x - x^3 + x^4])], 
 RootSum[8 + 16*#1 + 12*#1^2 + #1^4 & , 
   (3*Log[x] - 3*Log[1 - x^2 + Sqrt[1 + x - x^3 + x^4] - x*#1] + 
      2*Log[x]*#1 - 2*Log[1 - x^2 + Sqrt[1 + x - x^3 + x^4] - x*#1]*#1 - 
      Log[x]*#1^2 + Log[1 - x^2 + Sqrt[1 + x - x^3 + x^4] - x*#1]*#1^2)/
     (4 + 6*#1 + #1^3) & ]/2, 
 2*ArcTan[(-x^2 + Sqrt[-1 + 2*x - x^3 + x^4])/(-1 + x)], 
 -2*Log[x] + Log[-2 - 2*x + x^2 + 2*Sqrt[1 + 2*x - x^3 + x^4]], 
 2*ArcTanh[x/(1 + 2*x + x^2 - Sqrt[1 - x - 3*x^2 - x^3 + x^4])], 
 Defer[IntegrateAlgebraic][1/(x*Sqrt[1 + 2*x - 3*x^2 - x^3 + x^4]), x], 
 (2*ArcTanh[(Sqrt[3]*x)/(1 + 2*x + x^2 - Sqrt[1 - x - x^2 - x^3 + x^4])])/
  Sqrt[3], -2*ArcTan[(1 - 2*x + x^2 - Sqrt[1 - x - x^2 - x^3 + x^4])/x], 
 -ArcTan[(1 - 2*x + x^2 - Sqrt[1 - x - x^2 - x^3 + x^4])/x]/2 - 
  ArcTanh[(Sqrt[3]*x)/(1 + 2*x + x^2 - Sqrt[1 - x - x^2 - x^3 + x^4])]/
   (2*Sqrt[3]), Log[x] - Log[-2 + x - 2*x^2 + 
    2*Sqrt[1 - x - x^2 - x^3 + x^4]], 
 (-2*ArcTan[(1 + x^2 - Sqrt[1 - x - x^2 - x^3 + x^4])/(Sqrt[3]*x)])/Sqrt[3], 
 -ArcTan[(1 - 2*x + x^2 - Sqrt[1 - x - x^2 - x^3 + x^4])/x] + 
  ArcTanh[(Sqrt[3]*x)/(1 + 2*x + x^2 - Sqrt[1 - x - x^2 - x^3 + x^4])]/
   Sqrt[3], -ArcTan[(1 - 2*x + x^2 - Sqrt[1 - x - x^2 - x^3 + x^4])/x]/2 + 
  (Sqrt[3]*ArcTanh[(Sqrt[3]*x)/(1 + 2*x + x^2 - 
       Sqrt[1 - x - x^2 - x^3 + x^4])])/2, 
 -2*ArcTan[(1 + x + x^2 - Sqrt[1 - x - x^2 - x^3 + x^4])/x], 
 (-3*ArcTan[(1 - 2*x + x^2 - Sqrt[1 - x - x^2 - x^3 + x^4])/x])/2 + 
  ArcTanh[(Sqrt[3]*x)/(1 + 2*x + x^2 - Sqrt[1 - x - x^2 - x^3 + x^4])]/
   (2*Sqrt[3]), Log[x] - 
  Log[2 + x - 2*x^2 + 2*Sqrt[1 + x - x^2 - x^3 + x^4]], 
 2*ArcTanh[x/(-1 - x + x^2 - Sqrt[1 + x - x^2 - x^3 + x^4])], 
 (2*ArcTanh[(Sqrt[3]*x)/(-1 + x + x^2 - Sqrt[1 + x - x^2 - x^3 + x^4])])/
  Sqrt[3], RootSum[5 + 16*#1 + 14*#1^2 + #1^4 & , 
   (2*Log[x] - 2*Log[1 - x^2 + Sqrt[1 + x - x^2 - x^3 + x^4] - x*#1] + 
      2*Log[x]*#1 - 2*Log[1 - x^2 + Sqrt[1 + x - x^2 - x^3 + x^4] - x*#1]*
       #1 - Log[x]*#1^2 + Log[1 - x^2 + Sqrt[1 + x - x^2 - x^3 + x^4] - x*#1]*
       #1^2)/(4 + 7*#1 + #1^3) & ]/2, 
 (2*ArcTanh[(Sqrt[5]*x)/(1 + 2*x + x^2 - Sqrt[1 - x + x^2 - x^3 + x^4])])/
  Sqrt[5], 2*ArcTanh[x/(1 - 2*x + x^2 - Sqrt[1 - x + x^2 - x^3 + x^4])], 
 ArcTanh[x/(1 - 2*x + x^2 - Sqrt[1 - x + x^2 - x^3 + x^4])]/2 - 
  ArcTanh[(Sqrt[5]*x)/(1 + 2*x + x^2 - Sqrt[1 - x + x^2 - x^3 + x^4])]/
   (2*Sqrt[5]), Log[x] - Log[-2 + x - 2*x^2 + 
    2*Sqrt[1 - x + x^2 - x^3 + x^4]], 
 ArcTanh[x/(1 - 2*x + x^2 - Sqrt[1 - x + x^2 - x^3 + x^4])] + 
  ArcTanh[(Sqrt[5]*x)/(1 + 2*x + x^2 - Sqrt[1 - x + x^2 - x^3 + x^4])]/
   Sqrt[5], -2*ArcTan[(1 - x + x^2 - Sqrt[1 - x + x^2 - x^3 + x^4])/x], 
 ArcTanh[x/(1 - 2*x + x^2 - Sqrt[1 - x + x^2 - x^3 + x^4])]/2 + 
  (3*ArcTanh[(Sqrt[5]*x)/(1 + 2*x + x^2 - Sqrt[1 - x + x^2 - x^3 + x^4])])/
   (2*Sqrt[5]), 2*ArcTanh[x/(1 + x + x^2 - Sqrt[1 - x + x^2 - x^3 + x^4])], 
 (3*ArcTanh[x/(1 - 2*x + x^2 - Sqrt[1 - x + x^2 - x^3 + x^4])])/2 + 
  ArcTanh[(Sqrt[5]*x)/(1 + 2*x + x^2 - Sqrt[1 - x + x^2 - x^3 + x^4])]/
   (2*Sqrt[5]), -RootSum[13 + 16*#1 + 10*#1^2 + #1^4 & , 
    (-Log[x] + Log[1 - x^2 + Sqrt[1 + x + x^2 - x^3 + x^4] - x*#1] - 
       2*Log[x]*#1 + 2*Log[1 - x^2 + Sqrt[1 + x + x^2 - x^3 + x^4] - x*#1]*
        #1)/(4 + 5*#1 + #1^3) & ]/2, 
 RootSum[13 + 16*#1 + 10*#1^2 + #1^4 & , 
   (3*Log[x] - 3*Log[1 - x^2 + Sqrt[1 + x + x^2 - x^3 + x^4] - x*#1] - 
      Log[x]*#1^2 + Log[1 - x^2 + Sqrt[1 + x + x^2 - x^3 + x^4] - x*#1]*#1^2)/
     (4 + 5*#1 + #1^3) & ]/2, 
 Log[x] - Log[2 + x - 2*x^2 + 2*Sqrt[1 + x + x^2 - x^3 + x^4]], 
 (2*ArcTanh[(Sqrt[3]*x)/(-1 + x^2 - Sqrt[1 + x + x^2 - x^3 + x^4])])/Sqrt[3], 
 RootSum[13 + 16*#1 + 10*#1^2 + #1^4 & , 
   (2*Log[x] - 2*Log[1 - x^2 + Sqrt[1 + x + x^2 - x^3 + x^4] - x*#1] - 
      2*Log[x]*#1 + 2*Log[1 - x^2 + Sqrt[1 + x + x^2 - x^3 + x^4] - x*#1]*
       #1 - Log[x]*#1^2 + Log[1 - x^2 + Sqrt[1 + x + x^2 - x^3 + x^4] - x*#1]*
       #1^2)/(4 + 5*#1 + #1^3) & ]/2, 
 (2*ArcTanh[(Sqrt[5]*x)/(-1 + x + x^2 - Sqrt[1 + x + x^2 - x^3 + x^4])])/
  Sqrt[5], RootSum[13 + 16*#1 + 10*#1^2 + #1^4 & , 
   (4*Log[x] - 4*Log[1 - x^2 + Sqrt[1 + x + x^2 - x^3 + x^4] - x*#1] + 
      2*Log[x]*#1 - 2*Log[1 - x^2 + Sqrt[1 + x + x^2 - x^3 + x^4] - x*#1]*
       #1 - Log[x]*#1^2 + Log[1 - x^2 + Sqrt[1 + x + x^2 - x^3 + x^4] - x*#1]*
       #1^2)/(4 + 5*#1 + #1^3) & ]/2, 
 -2*Log[x] + Log[2 - 2*x + x^2 + 2*Sqrt[1 - 2*x + 2*x^2 - x^3 + x^4]], 
 Sqrt[2/3]*ArcTanh[(Sqrt[2/3]*Sqrt[1 - x + 2*x^2 - x^3 + x^4])/(1 + x^2)], 
 -(Sqrt[2]*ArcTanh[(Sqrt[2]*Sqrt[1 - x + 2*x^2 - x^3 + x^4])/(1 + x^2)]), 
 (2*ArcTanh[(Sqrt[7]*x)/(1 + 2*x + x^2 - Sqrt[1 - x + 3*x^2 - x^3 + x^4])])/
  Sqrt[7], 
 (2*ArcTanh[(Sqrt[3]*x)/(1 - 2*x + x^2 - Sqrt[1 - x + 3*x^2 - x^3 + x^4])])/
  Sqrt[3], -2*Log[x] + Log[2 - 2*x - x^2 + 2*Sqrt[1 - 2*x + x^3 + x^4]], 
 (-2*ArcTanh[(Sqrt[21]*x^2)/(4 - 4*x + x^2 + 4*Sqrt[1 - 2*x + x^3 + x^4])])/
  Sqrt[21], -RootSum[8 - 16*#1 + 12*#1^2 + #1^4 & , 
    (Log[x] - Log[1 - x^2 + Sqrt[1 - x + x^3 + x^4] - x*#1] - 2*Log[x]*#1 + 
       2*Log[1 - x^2 + Sqrt[1 - x + x^3 + x^4] - x*#1]*#1)/
      (-4 + 6*#1 + #1^3) & ]/2, 
 Log[x] - Log[2 - x - 2*x^2 + 2*Sqrt[1 - x + x^3 + x^4]], 
 Sqrt[2]*ArcTanh[(Sqrt[2]*x)/(-1 + x^2 - Sqrt[1 - x + x^3 + x^4])], 
 ArcTanh[(2*x)/(-1 - x + x^2 - Sqrt[1 - x + x^3 + x^4])], 
 RootSum[8 - 16*#1 + 12*#1^2 + #1^4 & , 
   (Log[x] - Log[1 - x^2 + Sqrt[1 - x + x^3 + x^4] - x*#1] + 2*Log[x]*#1 - 
      2*Log[1 - x^2 + Sqrt[1 - x + x^3 + x^4] - x*#1]*#1 - Log[x]*#1^2 + 
      Log[1 - x^2 + Sqrt[1 - x + x^3 + x^4] - x*#1]*#1^2)/
     (-4 + 6*#1 + #1^3) & ]/2, Defer[IntegrateAlgebraic][
  1/(x*Sqrt[1 - 2*x - 3*x^2 + x^3 + x^4]), x], 
 2*ArcTanh[x/(1 - 2*x + x^2 - Sqrt[1 + x - 3*x^2 + x^3 + x^4])], 
 -(Sqrt[2]*ArcTan[(1 + 2*x + x^2 - Sqrt[1 + x - 2*x^2 + x^3 + x^4])/
     (Sqrt[2]*x)]), 
 Sqrt[2]*ArcTanh[(Sqrt[2]*x)/(1 - 2*x + x^2 - 
     Sqrt[1 + x - 2*x^2 + x^3 + x^4])], 
 2*ArcTan[(-x^2 + Sqrt[-1 + 2*x - 2*x^2 + x^3 + x^4])/(-1 + x)], 
 (2*ArcTanh[(4 - 4*x + x^2 - Sqrt[-1 + 2*x - 2*x^2 + x^3 + x^4])/
     (Sqrt[19]*(-1 + x))])/Sqrt[19], Defer[IntegrateAlgebraic][
  (3 + 2*x)/(x*Sqrt[-1 - 2*x - x^2 + x^3 + x^4]), x], 
 -RootSum[5 - 16*#1 + 14*#1^2 + #1^4 & , 
    (Log[x] - Log[1 - x^2 + Sqrt[1 - x - x^2 + x^3 + x^4] - x*#1] - 
       2*Log[x]*#1 + 2*Log[1 - x^2 + Sqrt[1 - x - x^2 + x^3 + x^4] - x*#1]*
        #1)/(-4 + 7*#1 + #1^3) & ]/2, 
 RootSum[5 - 16*#1 + 14*#1^2 + #1^4 & , 
   (Log[x] - Log[1 - x^2 + Sqrt[1 - x - x^2 + x^3 + x^4] - x*#1] - 
      Log[x]*#1^2 + Log[1 - x^2 + Sqrt[1 - x - x^2 + x^3 + x^4] - x*#1]*#1^2)/
     (-4 + 7*#1 + #1^3) & ]/2, 
 2*ArcTanh[x/(-1 + x^2 - Sqrt[1 - x - x^2 + x^3 + x^4])], 
 (2*ArcTanh[(Sqrt[3]*x)/(-1 - x + x^2 - Sqrt[1 - x - x^2 + x^3 + x^4])])/
  Sqrt[3], -2*ArcTan[(1 + 2*x + x^2 - Sqrt[1 + x - x^2 + x^3 + x^4])/x], 
 (2*ArcTanh[(Sqrt[3]*x)/(1 - 2*x + x^2 - Sqrt[1 + x - x^2 + x^3 + x^4])])/
  Sqrt[3], ArcTan[(1 + 2*x + x^2 - Sqrt[1 + x - x^2 + x^3 + x^4])/x]/2 + 
  ArcTanh[(Sqrt[3]*x)/(1 - 2*x + x^2 - Sqrt[1 + x - x^2 + x^3 + x^4])]/
   (2*Sqrt[3]), (-2*ArcTan[(1 + x^2 - Sqrt[1 + x - x^2 + x^3 + x^4])/
     (Sqrt[3]*x)])/Sqrt[3], 
 -ArcTan[(1 + 2*x + x^2 - Sqrt[1 + x - x^2 + x^3 + x^4])/x] + 
  ArcTanh[(Sqrt[3]*x)/(1 - 2*x + x^2 - Sqrt[1 + x - x^2 + x^3 + x^4])]/
   Sqrt[3], -2*ArcTan[(1 - x + x^2 - Sqrt[1 + x - x^2 + x^3 + x^4])/x], 
 (-3*ArcTan[(1 + 2*x + x^2 - Sqrt[1 + x - x^2 + x^3 + x^4])/x])/2 + 
  ArcTanh[(Sqrt[3]*x)/(1 - 2*x + x^2 - Sqrt[1 + x - x^2 + x^3 + x^4])]/
   (2*Sqrt[3]), (-2*ArcTan[(1 + x + x^2 - Sqrt[1 + x - x^2 + x^3 + x^4])/
     (Sqrt[3]*x)])/Sqrt[3], 
 -ArcTan[(1 + 2*x + x^2 - Sqrt[1 + x - x^2 + x^3 + x^4])/x]/2 + 
  (Sqrt[3]*ArcTanh[(Sqrt[3]*x)/(1 - 2*x + x^2 - 
       Sqrt[1 + x - x^2 + x^3 + x^4])])/2, 
 -RootSum[13 - 16*#1 + 10*#1^2 + #1^4 & , 
    (Log[x] - Log[1 - x^2 + Sqrt[1 - x + x^2 + x^3 + x^4] - x*#1] - 
       2*Log[x]*#1 + 2*Log[1 - x^2 + Sqrt[1 - x + x^2 + x^3 + x^4] - x*#1]*
        #1)/(-4 + 5*#1 + #1^3) & ]/2, 
 RootSum[13 - 16*#1 + 10*#1^2 + #1^4 & , 
   (4*Log[x] - 4*Log[1 - x^2 + Sqrt[1 - x + x^2 + x^3 + x^4] - x*#1] - 
      2*Log[x]*#1 + 2*Log[1 - x^2 + Sqrt[1 - x + x^2 + x^3 + x^4] - x*#1]*
       #1 - Log[x]*#1^2 + Log[1 - x^2 + Sqrt[1 - x + x^2 + x^3 + x^4] - x*#1]*
       #1^2)/(-4 + 5*#1 + #1^3) & ]/2, 
 RootSum[13 - 16*#1 + 10*#1^2 + #1^4 & , 
   (2*Log[x] - 2*Log[1 - x^2 + Sqrt[1 - x + x^2 + x^3 + x^4] - x*#1] + 
      2*Log[x]*#1 - 2*Log[1 - x^2 + Sqrt[1 - x + x^2 + x^3 + x^4] - x*#1]*
       #1 - Log[x]*#1^2 + Log[1 - x^2 + Sqrt[1 - x + x^2 + x^3 + x^4] - x*#1]*
       #1^2)/(-4 + 5*#1 + #1^3) & ]/2, 
 2*ArcTanh[x/(1 + 2*x + x^2 - Sqrt[1 + x + x^2 + x^3 + x^4])], 
 (2*ArcTanh[(Sqrt[5]*x)/(1 - 2*x + x^2 - Sqrt[1 + x + x^2 + x^3 + x^4])])/
  Sqrt[5], 
 ArcTanh[(Sqrt[5]*x)/(1 - 2*x + x^2 - Sqrt[1 + x + x^2 + x^3 + x^4])]/
   (2*Sqrt[5]) - ArcTanh[x/(1 + 2*x + x^2 - Sqrt[1 + x + x^2 + x^3 + x^4])]/
   2, -2*ArcTan[(1 + x^2 - Sqrt[1 + x + x^2 + x^3 + x^4])/x], 
 ArcTanh[(Sqrt[5]*x)/(1 - 2*x + x^2 - Sqrt[1 + x + x^2 + x^3 + x^4])]/
   (2*Sqrt[5]) + 
  (3*ArcTanh[x/(1 + 2*x + x^2 - Sqrt[1 + x + x^2 + x^3 + x^4])])/2, 
 -2*ArcTan[(1 + x + x^2 - Sqrt[1 + x + x^2 + x^3 + x^4])/x], 
 Defer[IntegrateAlgebraic][(3 + 2*x)/(x*Sqrt[1 + 2*x + x^2 + x^3 + x^4]), x], 
 Defer[IntegrateAlgebraic][(-1 - 2*x + x^2)/
   ((-1 + x)*Sqrt[1 - x + 2*x^2 + x^3 + x^4]), x], 
 Sqrt[2]*ArcTanh[Sqrt[1 + x + 2*x^2 + x^3 + x^4]/(Sqrt[2]*(1 + x + x^2))], 
 Sqrt[2/3]*ArcTanh[(Sqrt[6]*x)/(1 - 2*x + x^2 - 
     Sqrt[1 + x + 2*x^2 + x^3 + x^4])], 
 (2*ArcTanh[(Sqrt[3]*x)/(1 + 2*x + x^2 - Sqrt[1 + x + 3*x^2 + x^3 + x^4])])/
  Sqrt[3], Log[-1 + 2*x + 2*x^2 + 2*Sqrt[-2 - x + 2*x^3 + x^4]], 
 (2*ArcTan[(1 + 4*x + 4*x^2 + 4*Sqrt[-1 - x + 2*x^3 + x^4])/Sqrt[11]])/
  Sqrt[11], Log[-1 + 2*x + 2*x^2 + 2*Sqrt[1 - x + 2*x^3 + x^4]], 
 (-2*ArcTanh[(1 + 4*x + 4*x^2 + 4*Sqrt[1 - x + 2*x^3 + x^4])/Sqrt[21]])/
  Sqrt[21], -(Sqrt[2]*ArcTan[(1 + 2*x + x^2 - Sqrt[1 + 2*x + 2*x^3 + x^4])/
     (Sqrt[2]*x)]), Sqrt[2/3]*
  ArcTanh[(Sqrt[6]*x)/(1 - 2*x + x^2 - Sqrt[1 + 2*x + 2*x^3 + x^4])], 
 Defer[IntegrateAlgebraic][(-1 - x + x^2)/
   ((1 + x^2)*Sqrt[1 + 2*x + 2*x^3 + x^4]), x], 
 Defer[IntegrateAlgebraic][(1 + x)/((-1 + 2*x + x^2)*
    Sqrt[1 + 2*x + 2*x^3 + x^4]), x], Defer[IntegrateAlgebraic][
  x/Sqrt[3 - 3*x - 3*x^2 + 2*x^3 + x^4], x], Defer[IntegrateAlgebraic][
  x/Sqrt[-2 + 2*x - 3*x^2 + 2*x^3 + x^4], x], 
 Defer[IntegrateAlgebraic][x/Sqrt[-3 + 3*x - 3*x^2 + 2*x^3 + x^4], x], 
 (2*ArcTan[(1 + 4*x + 4*x^2 + 4*Sqrt[-3 - 3*x - 2*x^2 + 2*x^3 + x^4])/
     Sqrt[35]])/Sqrt[35], 
 Log[-3 + 2*x + 2*x^2 + 2*Sqrt[-3 - 3*x - 2*x^2 + 2*x^3 + x^4]], 
 (-2*ArcTanh[(1 + 4*x + 4*x^2 + 4*Sqrt[1 - 3*x - 2*x^2 + 2*x^3 + x^4])/
     Sqrt[29]])/Sqrt[29], 
 (2*ArcTanh[(3*Sqrt[2 - 3*x - 2*x^2 + 2*x^3 + x^4])/
     (Sqrt[5]*(-2 + x + x^2))])/(3*Sqrt[5]), 
 Log[-3 + 2*x + 2*x^2 + 2*Sqrt[2 - 3*x - 2*x^2 + 2*x^3 + x^4]], 
 Log[-3 + 2*x + 2*x^2 + 2*Sqrt[3 - 3*x - 2*x^2 + 2*x^3 + x^4]], 
 Log[-1 + x + x^2 + Sqrt[-3 - 2*x - x^2 + 2*x^3 + x^4]], 
 Log[-1 + x + x^2 + Sqrt[-1 - 2*x - x^2 + 2*x^3 + x^4]], 
 Log[-1 + x + x^2 + Sqrt[2 - 2*x - x^2 + 2*x^3 + x^4]], 
 (-2*ArcTan[(Sqrt[3]*Sqrt[1 + 2*x - x^2 + 2*x^3 + x^4])/(1 - x + x^2)])/
  Sqrt[3], 
 (2*ArcTanh[(Sqrt[5]*x)/(1 - 2*x + x^2 - Sqrt[1 + 2*x - x^2 + 2*x^3 + x^4])])/
  Sqrt[5], (2*ArcTan[(1 + 4*x + 4*x^2 + 4*Sqrt[-3 + x^2 + 2*x^3 + x^4])/
     Sqrt[47]])/Sqrt[47], 
 (2*ArcTan[(1 + 4*x + 4*x^2 + 4*Sqrt[-2 + x^2 + 2*x^3 + x^4])/Sqrt[31]])/
  Sqrt[31], (2*ArcTan[(1 + 4*x + 4*x^2 + 4*Sqrt[-1 + x^2 + 2*x^3 + x^4])/
     Sqrt[15]])/Sqrt[15], 
 (-2*ArcTanh[(1 + 4*x + 4*x^2 + 4*Sqrt[1 + x^2 + 2*x^3 + x^4])/Sqrt[17]])/
  Sqrt[17], Log[x + x^2 + Sqrt[2 + x^2 + 2*x^3 + x^4]], 
 (2*ArcTanh[(4 + 4*x + x^2 - Sqrt[-1 - 2*x + x^2 + 2*x^3 + x^4])/
     (Sqrt[7]*(1 + x))])/Sqrt[7], 
 -2*ArcTan[(1 + 2*x + x^2 - Sqrt[1 + 2*x + x^2 + 2*x^3 + x^4])/x], 
 (2*ArcTanh[(Sqrt[7]*x)/(1 - 2*x + x^2 - Sqrt[1 + 2*x + x^2 + 2*x^3 + x^4])])/
  Sqrt[7], Defer[IntegrateAlgebraic][
  (2 + 3*x)/Sqrt[2 + 2*x + x^2 + 2*x^3 + x^4], x], 
 Defer[IntegrateAlgebraic][(2 + 3*x)/Sqrt[2 + 2*x + x^2 + 2*x^3 + x^4], x], 
 (2*ArcTan[(1 + 4*x + 4*x^2 + 4*Sqrt[-1 + x + 2*x^2 + 2*x^3 + x^4])/
     Sqrt[19]])/Sqrt[19], 
 Log[1 + 2*x + 2*x^2 + 2*Sqrt[-1 + x + 2*x^2 + 2*x^3 + x^4]], 
 Log[1 + 2*x + 2*x^2 + 2*Sqrt[1 + x + 2*x^2 + 2*x^3 + x^4]], 
 Log[1 + 2*x + 2*x^2 + 2*Sqrt[1 + x + 2*x^2 + 2*x^3 + x^4]], 
 (-2*ArcTanh[(1 + 4*x + 4*x^2 + 4*Sqrt[2 + x + 2*x^2 + 2*x^3 + x^4])/
     Sqrt[29]])/Sqrt[29], 
 ArcTanh[(2*Sqrt[2]*x)/(1 - 2*x + x^2 - Sqrt[1 + 3*x + 3*x^3 + x^4])]/
  Sqrt[2], (-2*ArcTan[(1 + 2*x + x^2 - Sqrt[1 + 3*x - 3*x^2 + 3*x^3 + x^4])/
     (Sqrt[7]*x)])/Sqrt[7], 
 -(Sqrt[2/3]*ArcTan[(1 + 2*x + x^2 - Sqrt[1 + 3*x - 2*x^2 + 3*x^3 + x^4])/
     (Sqrt[6]*x)]), Defer[IntegrateAlgebraic][
  1/(x*Sqrt[3 - 2*x - x^2 + 3*x^3 + x^4]), x], 
 (-2*ArcTan[(1 + 2*x + x^2 - Sqrt[1 + 3*x + x^2 + 3*x^3 + x^4])/(Sqrt[3]*x)])/
  Sqrt[3], 
 (2*ArcTanh[(3*x)/(1 - 2*x + x^2 - Sqrt[1 + 3*x + x^2 + 3*x^3 + x^4])])/3, 
 -(Sqrt[2]*ArcTan[(Sqrt[2]*Sqrt[1 + 3*x + 2*x^2 + 3*x^3 + x^4])/(1 + x^2)]), 
 Sqrt[2/5]*ArcTanh[(Sqrt[10]*x)/(1 - 2*x + x^2 - 
     Sqrt[1 + 3*x + 2*x^2 + 3*x^3 + x^4])], (2*ArcTan[Sqrt[-1 + x^5]])/5, 
 (-2*ArcTanh[Sqrt[1 + x^5]])/5, Defer[IntegrateAlgebraic][
  (1 + x)/(x*Sqrt[(1 + x)^3*(1 - x + x^2)]), x], ArcTan[Sqrt[-1 + x^6]]/3, 
 -ArcTanh[Sqrt[1 + x^6]]/3, (-2*ArcTan[x^3 - Sqrt[-1 - x^3 + x^6]])/3, 
 (2*ArcTanh[x^3 - Sqrt[1 - x^3 + x^6]])/3, 
 (-2*ArcTan[x^3 - Sqrt[-1 + x^3 + x^6]])/3, 
 (2*ArcTanh[x^3 - Sqrt[1 + x^3 + x^6]])/3, 
 (-2*ArcTan[x^3 - Sqrt[-1 + 2*x^3 + x^6]])/3};
