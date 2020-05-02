comparisonIntegrandsSolutions = {
  (4*ArcTan[x/Sqrt[1 + x^4]] + Sqrt[2]*ArcTanh[(Sqrt[2]*x)/Sqrt[1 + x^4]])/6, 
 (Sqrt[2]*ArcTan[(Sqrt[2]*x)/Sqrt[1 - x^2 + x^4]] + 
   ArcTanh[x/Sqrt[1 - x^2 + x^4]])/3, ArcTanh[x^2/Sqrt[1 + 2*x^4]]/2, 
 -(x*Sqrt[1 + x^4])/(2*(1 + x^2)^2) - ArcTan[(Sqrt[2]*x)/Sqrt[1 + x^4]]/
   (2*Sqrt[2]), -ArcTan[(2^(1/4)*x)/Sqrt[1 + x^4]]/(2*2^(1/4)) - 
  ArcTanh[(2^(1/4)*x)/Sqrt[1 + x^4]]/(2*2^(1/4)), 
 (-2*x*Sqrt[1 - x^3])/(-1 + x^3), -((x*Sqrt[2 - x^4])/(-2 + x^4)), 
 -(x*Sqrt[-1 + x^2 + x^4])/(2*(-1 + x^4)) - ArcTanh[Sqrt[-1 + x^2 + x^4]/x]/
   2, (-1 + x^4)^(3/2)/(12*x^6) + 
  ((-2*I + Sqrt[2])*ArcTan[(Sqrt[2]*Sqrt[-1 + x^4])/(Sqrt[-2 - I*Sqrt[2]]*
       x^2)])/(16*Sqrt[-2 - I*Sqrt[2]]) + 
  ((2*I + Sqrt[2])*ArcTan[(Sqrt[2]*Sqrt[-1 + x^4])/(Sqrt[-2 + I*Sqrt[2]]*
       x^2)])/(16*Sqrt[-2 + I*Sqrt[2]]), 
 (-ArcTan[x/Sqrt[1 + x^4]] - ArcTanh[x/Sqrt[1 + x^4]])/2, 
 ((Sqrt[2] - I*Sqrt[6])*ArcTan[(Sqrt[1/2 + (I/2)*Sqrt[3]]*x^2)/
      Sqrt[1 + x^4]])/(4*Sqrt[1 + I*Sqrt[3]]) + 
  ((Sqrt[2] + I*Sqrt[6])*ArcTanh[(Sqrt[-1/2 + (I/2)*Sqrt[3]]*x^2)/
      Sqrt[1 + x^4]])/(4*Sqrt[-1 + I*Sqrt[3]]), 
 -((x*Sqrt[1 + x^2 - x^3])/(-1 + x^3)) + ArcTanh[Sqrt[1 + x^2 - x^3]/x], 
 -((x*Sqrt[1 - x^2 + x^3])/(1 + x^3)) + ArcTan[Sqrt[1 - x^2 + x^3]/x], 
 (-3*x + 4*x^3 - 3*x^5)/(3*(1 - x^2 + x^4)^(3/2)), 
 -(x*Sqrt[1 + x^4])/(2*(1 + 3*x^2 + x^4)) - ArcTan[(Sqrt[3]*x)/Sqrt[1 + x^4]]/
   (2*Sqrt[3]), -((x*Sqrt[-1 + x^2 + x^5])/(-1 + x^5)) - 
  ArcTanh[Sqrt[-1 + x^2 + x^5]/x], 
 -ArcTan[(Sqrt[2]*x*Sqrt[1 + x^4])/(1 - x^2 + x^4)]/(2*Sqrt[2]) - 
  ArcTanh[(Sqrt[2]*x*Sqrt[1 + x^4])/(1 + x^2 + x^4)]/(2*Sqrt[2]), 
 ((-2 - 3*x^4 + 2*x^8)*Sqrt[1 + 3*x^4 - 3*x^12 + x^16])/(16*x^8) + Log[x]/8 - 
  Log[-2 - 3*x^4 + 2*x^8 + 2*Sqrt[1 + 3*x^4 - 3*x^12 + x^16]]/32, 
 -2*ArcTanh[Sqrt[1 + x^5]/x], -(x*Sqrt[1 + x^6])/(2*(1 - x^2 + x^6)) - 
  ArcTanh[x/Sqrt[1 + x^6]]/2, -ArcTanh[x/Sqrt[1 + x^6]], 
 -(x*Sqrt[1 - x^2 + x^6])/(2*(1 + x^6)) - ArcTan[x/Sqrt[1 - x^2 + x^6]]/2, 
 -(Sqrt[-1 - x^2 + x^6]*(-5*x - 2*x^3 + 5*x^7))/(8*(-1 + x^6)^2) + 
  (3*ArcTan[Sqrt[-1 - x^2 + x^6]/x])/8, 
 -ArcTan[(Sqrt[2]*x*Sqrt[1 - x^4])/(-1 + x^2 + x^4)]/(2*Sqrt[2]) - 
  ArcTanh[(-1 - x^2 + x^4)/(Sqrt[2]*x*Sqrt[1 - x^4])]/(2*Sqrt[2]), 
 ArcTan[x/Sqrt[1 + x^2 + x^4]], Sqrt[1 + 3*x^2 + x^4]/x - 
  2*ArcTanh[(-1 + x - x^2 + Sqrt[1 + 3*x^2 + x^4])/
     (1 + x + x^2 + Sqrt[1 + 3*x^2 + x^4])], 
 (Sqrt[2 - x^4 + x^6]*(8 - 28*x^4 + 8*x^6 - 3*x^8 - 14*x^10 + 2*x^12))/
   (6*x^6*(2 + x^6)) - (5*ArcTan[x^2/Sqrt[2 - x^4 + x^6]])/2, 
 ((1 - x^4 + x^6)*Sqrt[1 + 2*x^4 + x^6])/(x^2*(1 + x^6)) - 
  (3*ArcTanh[(Sqrt[2]*x^2)/Sqrt[1 + 2*x^4 + x^6]])/Sqrt[2], 
 (2*Sqrt[1 + x^4] - x^4*Sqrt[1 + x^4])/(12*x^6) + 
  ((3 - Sqrt[2])*ArcTanh[x^2/(Sqrt[2 + Sqrt[2]]*Sqrt[1 + x^4])])/
   (8*Sqrt[2 - Sqrt[2]]) + 
  ((-3 - Sqrt[2])*ArcTanh[x^2/Sqrt[2 - Sqrt[2] + (2 - Sqrt[2])*x^4]])/
   (8*Sqrt[2 + Sqrt[2]]), (Sqrt[-1 + x^4]*(1 + 2*x^4))/(6*x^6) - 
  (Sqrt[1 + 1/Sqrt[2]]*ArcTanh[(Sqrt[2 + Sqrt[2]]*Sqrt[-1 + x^4])/x^2])/4 + 
  (Sqrt[1 - 1/Sqrt[2]]*ArcTanh[Sqrt[-2 + Sqrt[2] + (2 - Sqrt[2])*x^4]/x^2])/
   4, Sqrt[1 - x^8 + x^16]/(4*x^4) - ArcTanh[x^4/Sqrt[1 - x^8 + x^16]]/4, 
 Sqrt[2]*ArcTanh[(1 + 2*x + x^2 + Sqrt[2]*Sqrt[-1 - x^4])/
    (-1 + 2*x - x^2 + Sqrt[2]*Sqrt[-1 - x^4])], 
 -((x*Sqrt[-1 - x^2 - x^5])/(1 + x^5)) - ArcTan[Sqrt[-1 - x^2 - x^5]/x], 
 -((x*Sqrt[1 - x^5])/(-1 - x^2 + x^5)) - ArcTan[Sqrt[1 - x^5]/x], 
 Sqrt[1 + x^4]/(1 + x^2) - Sqrt[2]*ArcTan[(Sqrt[2]*x)/Sqrt[1 + x^4]] + 
  ArcTan[(-1 + 2*x - x^2)/Sqrt[1 + x^4]], 
 -(x*Sqrt[-1 - 3*x^2 + x^4])/(2*(-1 - x^2 + x^4)) + 
  ArcTan[Sqrt[-1 - 3*x^2 + x^4]/(Sqrt[2]*x)]/(2*Sqrt[2]), 
 -(x*Sqrt[-1 - 2*x^2 + x^4])/(2*(-1 - x^2 + x^4)) + 
  ArcTan[Sqrt[-1 - 2*x^2 + x^4]/x]/2, 
 -(Sqrt[2]*ArcTan[(Sqrt[2]*x)/Sqrt[1 + x^4]]) + 
  ArcTan[(2*x*Sqrt[1 + x^4])/(1 - x^2 + x^4)]/2, 
 ArcTan[Sqrt[-1 + x^3]/x] - ArcTanh[Sqrt[-1 + x^3]/x], 
 -(x*Sqrt[-1 + 2*x^2 + x^6])/(2*(-1 + x^2 + x^6)) - 
  ArcTanh[Sqrt[-1 + 2*x^2 + x^6]/x]/2, 
 ArcTan[(Sqrt[2]*x)/Sqrt[-1 - x^2 - x^4]]/Sqrt[2], 
 -(Sqrt[1 - 2*x^4]*(1 + x^4))/(6*x^6) - 
  (Sqrt[-1 + Sqrt[2]]*ArcTan[(Sqrt[-1 + Sqrt[2]]*Sqrt[1 - 2*x^4])/x^2])/4 + 
  (Sqrt[1 + Sqrt[2]]*ArcTanh[(Sqrt[1 + Sqrt[2]]*Sqrt[1 - 2*x^4])/x^2])/4, 
 -(ArcTan[(Sqrt[2]*x*Sqrt[1 + x^5])/(1 - x^2 + x^5)]/Sqrt[2]) - 
  ArcTanh[(1 + x^2 + x^5)/(Sqrt[2]*x*Sqrt[1 + x^5])]/Sqrt[2], 
 Sqrt[1 + x^4]/(2*(1 + x + x^2)) + (3*ArcTan[(-1 - 2*x - x^2)/Sqrt[1 + x^4]])/
   4 + ArcTan[(-1 + 2*x - x^2)/Sqrt[1 + x^4]]/4, 
 Sqrt[4 - 5*x^2 + x^4]/x - 4*ArcTanh[(-2 + x + x^2 + Sqrt[4 - 5*x^2 + x^4])/
     (-2 - x + x^2 + Sqrt[4 - 5*x^2 + x^4])] - 
  2*Sqrt[3]*ArcTanh[(2 + x - x^2 + Sqrt[3]*Sqrt[4 - 5*x^2 + x^4])/
     (6 - 3*x - 3*x^2 + Sqrt[3]*Sqrt[4 - 5*x^2 + x^4])], 
 (Sqrt[8 - 7*x^2 + 2*x^4]*(16 - 6*x - 14*x^2 + 3*x^3 + 4*x^4))/(6*x^3) + 
  ArcTanh[(-2*Sqrt[2] + x + Sqrt[2]*x^2 + Sqrt[8 - 7*x^2 + 2*x^4])/
     (-2*Sqrt[2] - x + Sqrt[2]*x^2 + Sqrt[8 - 7*x^2 + 2*x^4])]/Sqrt[2], 
 -(x^2*Sqrt[2 + x^4 - x^6])/(2*(-2 - 2*x^4 + x^6)) - 
  ArcTan[Sqrt[2 + x^4 - x^6]/x^2]/2, 
 ((-I/3)*(-3*I + Sqrt[3])*ArcTan[(Sqrt[1/2 - (I/2)*Sqrt[3]]*x)/
      Sqrt[1 + x^4]])/Sqrt[2 - (2*I)*Sqrt[3]] + 
  ((I/3)*(3*I + Sqrt[3])*ArcTan[(Sqrt[1/2 + (I/2)*Sqrt[3]]*x)/Sqrt[1 + x^4]])/
   Sqrt[2 + (2*I)*Sqrt[3]], 2*ArcTanh[Sqrt[1 - x^3]/x], 
 2*ArcTanh[Sqrt[-1 - x^3]/x], 
 -((x*Sqrt[-1 - 2*x^2 + x^5])/(-1 - x^2 + x^5)) + 
  ArcTan[Sqrt[-1 - 2*x^2 + x^5]/x], -(Sqrt[-1 + 2*x^4]*(1 + 4*x^4))/(6*x^6) + 
  ((-2*Sqrt[1 + Sqrt[2]] + 3*Sqrt[2 + 2*Sqrt[2]])*
    ArcTan[(Sqrt[1 + Sqrt[2]]*Sqrt[-1 + 2*x^4])/x^2])/8 + 
  ((2*Sqrt[-1 + Sqrt[2]] + 3*Sqrt[-2 + 2*Sqrt[2]])*
    ArcTanh[(Sqrt[-1 + Sqrt[2]]*Sqrt[-1 + 2*x^4])/x^2])/8, 
 ArcTanh[(x + Sqrt[3]*(-4 + x + 2*x^5 + 
      2*Sqrt[4 - 2*x + x^2 - 4*x^5 + x^6 + x^10]))/
   (-x + Sqrt[3]*(-4 + x + 2*x^5 + 2*Sqrt[4 - 2*x + x^2 - 4*x^5 + x^6 + 
         x^10]))], (ArcTan[Sqrt[-1 + x^6]/x] - ArcTanh[Sqrt[-1 + x^6]/x])/2, 
 -ArcTan[(Sqrt[2]*x*Sqrt[1 - x^6])/(-1 + x^2 + x^6)]/(2*Sqrt[2]) - 
  ArcTanh[(-1 - x^2 + x^6)/(Sqrt[2]*x*Sqrt[1 - x^6])]/(2*Sqrt[2]), 
 (Sqrt[-1 + x^2 + 2*x^3]*(3 + 4*x^2 - 12*x^3 - 7*x^4 - 8*x^5 + 12*x^6))/
  (15*x^5), -(Sqrt[-1 + x^2 - x^4]*(3*x + 2*x^3 + 3*x^5))/(8*(1 + x^4)^2) + 
  (5*ArcTanh[x/Sqrt[-1 + x^2 - x^4]])/8, Sqrt[-2 - x^3 + x^6]/(3*x^3) + 
  ArcTan[(4 + x^3)/(2*Sqrt[2]*Sqrt[-2 - x^3 + x^6])]/(2*Sqrt[2]) + 
  ((2^(1/4) + 2^(3/4))*ArcTan[(2*2^(1/4) - 5*2^(3/4) + 
       (-4*2^(1/4) + 2^(3/4))*x^3)/(4*Sqrt[-2 - x^3 + x^6])])/6 + 
  ((2^(1/4) - 2^(3/4))*ArcTanh[(-1 - 4*Sqrt[2] + 3*x^3 + Sqrt[2]*x^3 + 
       2*2^(3/4)*Sqrt[-2 - x^3 + x^6])/(-3 - 6*Sqrt[2] + 5*x^3 + 
       Sqrt[2]*x^3 + 2*2^(3/4)*Sqrt[-2 - x^3 + x^6])])/3, 
 (4*(1 + 2*x^4 + x^6 + x^8)*Sqrt[4 + 8*x^4 + x^6 + 4*x^8])/(3*x^9) - 
  2*ArcTanh[(1 + x^3 + x^4 + Sqrt[4 + 8*x^4 + x^6 + 4*x^8])/
     (-1 + x^3 - x^4 + Sqrt[4 + 8*x^4 + x^6 + 4*x^8])], 
 I*(Sqrt[-1 - I]*ArcTan[Sqrt[-1 + x^2 + x^5]/(Sqrt[-1 - I]*x)] - 
   Sqrt[-1 + I]*ArcTan[Sqrt[-1 + x^2 + x^5]/(Sqrt[-1 + I]*x)]), 
 (2*Sqrt[2 + x^2 + x^3]*(12 - 28*x^2 + 12*x^3 + 28*x^4 - 14*x^5 + 3*x^6))/
  (15*x^5), ((1 + 2*x + x^2)*Sqrt[1 + 3*x^2 + x^4])/(x*(1 + x + x^2)) + 
  4*ArcTanh[(-1 + x - x^2 + Sqrt[1 + 3*x^2 + x^4])/
     (-1 - x - x^2 + Sqrt[1 + 3*x^2 + x^4])] - 
  3*Sqrt[2]*ArcTanh[(2*x + Sqrt[2]*Sqrt[1 + 3*x^2 + x^4])/
     (-2 - 2*x^2 + Sqrt[2]*Sqrt[1 + 3*x^2 + x^4])], 
 ArcTanh[x/Sqrt[-1 - x^6]], -(x*Sqrt[-1 + x^2 + x^4 + x^6])/
   (2*(-1 + x^4 + x^6)) - ArcTanh[Sqrt[-1 + x^2 + x^4 + x^6]/x]/2, 
 ((-4 + x^2 + 4*x^3)*Sqrt[2 - x^2 - 4*x^3 + x^4 + x^5 + 2*x^6])/(8*x^4) - 
  (7*Log[x])/(8*Sqrt[2]) + 
  (7*Log[-4*Sqrt[7] + Sqrt[7]*x^2 + 4*Sqrt[7]*x^3 + 
      2*Sqrt[14]*Sqrt[2 - x^2 - 4*x^3 + x^4 + x^5 + 2*x^6]])/(16*Sqrt[2]), 
 Sqrt[-2 + 3*x^4]/(2*x^2) + ((Sqrt[2]*3^(1/4) - Sqrt[2]*3^(3/4))*
    ArcTan[(Sqrt[2]*3^(3/4)*x^2*Sqrt[-2 + 3*x^4])/
      (-2*Sqrt[3] + (-3 + 3*Sqrt[3])*x^4)])/8 + 
  ((-(Sqrt[2]*3^(1/4)) - Sqrt[2]*3^(3/4))*
    ArcTanh[(-2*Sqrt[3] + (3 + 3*Sqrt[3])*x^4)/(Sqrt[2]*3^(3/4)*x^2*
       Sqrt[-2 + 3*x^4])])/8, -2*(ArcTan[Sqrt[-1 - x^2 + x^3]/x] - 
   Sqrt[2]*ArcTan[Sqrt[-1 - x^2 + x^3]/(Sqrt[2]*x)]), 
 -2*(ArcTan[Sqrt[1 - x^2 + x^3]/x] - 
   Sqrt[2]*ArcTan[Sqrt[1 - x^2 + x^3]/(Sqrt[2]*x)]), 
 -2*(ArcTan[Sqrt[2 - x^2 + x^3]/x] - 
   Sqrt[2]*ArcTan[Sqrt[2 - x^2 + x^3]/(Sqrt[2]*x)]), 
 2*ArcTan[Sqrt[2 + x^2 + 2*x^3]/x] - 2*ArcTanh[Sqrt[2 + x^2 + 2*x^3]/x], 
 ArcTan[x/Sqrt[1 - x^2 + x^4]] - 
  Sqrt[2]*ArcTan[(Sqrt[2]*x)/Sqrt[1 - x^2 + x^4]], 
 ArcTanh[Sqrt[-1 + 2*x^2 + x^4]/x] - 
  Sqrt[2]*ArcTanh[Sqrt[-1 + 2*x^2 + x^4]/(Sqrt[2]*x)], 
 ArcTan[x/Sqrt[2 + x^4]] - Sqrt[2]*ArcTan[(Sqrt[2]*x)/Sqrt[2 + x^4]], 
 (Sqrt[4 + 2*x + 2*x^2 + x^3 + x^4]*(32 - 44*x + x^2 - 22*x^3 + 8*x^4))/
   (24*x^3) - (45*ArcTanh[(-2 - x^2 + Sqrt[4 + 2*x + 2*x^2 + x^3 + x^4])/
      (-2 - x - x^2 + Sqrt[4 + 2*x + 2*x^2 + x^3 + x^4])])/8, 
 -ArcTanh[x/Sqrt[1 + x^2 + x^4]] + ArcTanh[x/(Sqrt[2]*Sqrt[1 + x^2 + x^4])]/
   Sqrt[2], Sqrt[2]*ArcTan[Sqrt[-1 - x^2 + x^3]/(Sqrt[2]*x)], 
 -(Sqrt[-1 - x^2 + x^6]*(-3*x + 2*x^3 + 3*x^7))/(8*(-1 + x^6)^2) + 
  (5*ArcTan[Sqrt[-1 - x^2 + x^6]/x])/8, 
 (2*Sqrt[2 - x^4] - 13*x^4*Sqrt[2 - x^4])/(12*x^6) + 
  ((12 + 7*Sqrt[3])*ArcTan[Sqrt[2 - x^4]/(Sqrt[2 + Sqrt[3]]*x^2)])/
   (6*Sqrt[2 + Sqrt[3]]) + 
  ((12 - 7*Sqrt[3])*ArcTan[Sqrt[-2/(-2 + Sqrt[3]) + x^4/(-2 + Sqrt[3])]/x^2])/
   (6*Sqrt[2 - Sqrt[3]]), 
 I*(Sqrt[1 - I]*ArcTan[Sqrt[1 - x^2 + x^5]/(Sqrt[1 - I]*x)] - 
   Sqrt[1 + I]*ArcTan[Sqrt[1 - x^2 + x^5]/(Sqrt[1 + I]*x)]), 
 -((x*Sqrt[-1 + x^2 + x^4 + x^5])/(-1 + x^4 + x^5)) - 
  ArcTanh[Sqrt[-1 + x^2 + x^4 + x^5]/x], 
 (Sqrt[-2 + x^6]*(-2 + 9*x^2 + x^6))/(6*x^3) - 
  (3*ArcTanh[Sqrt[-2 + x^6]/(Sqrt[2]*x)])/Sqrt[2], 
 (Sqrt[2 + 3*x^2 + x^6]*(4 + 3*x^2 + 2*x^6))/(4*x*(2 + x^2 + x^6)) - 
  (3*ArcTanh[(Sqrt[2]*x)/Sqrt[2 + 3*x^2 + x^6]])/(4*Sqrt[2]), 
 ((-1 + 4*x^3 + x^6)*Sqrt[1 + x^12])/(6*x^6) - 
  (4*ArcTanh[(-2 + x^3 + 2*x^6 + Sqrt[3]*Sqrt[1 + x^12])/
      (3*x^3 + Sqrt[3]*Sqrt[1 + x^12])])/Sqrt[3] - 3*Log[x] + 
  Log[Sqrt[2]*(-1 + x^6 + Sqrt[1 + x^12])], 
 ((-2 - 7*x^4 + x^6)*Sqrt[4 + 28*x^4 - 4*x^6 + 48*x^8 - 14*x^10 + x^12])/
   (4*x^8) - Log[x] + 
  Log[2 + 7*x^4 - x^6 + Sqrt[4 + 28*x^4 - 4*x^6 + 48*x^8 - 14*x^10 + x^12]]/
   4, -2*ArcTan[Sqrt[-1 - x^2 - x^3]/x], 
 (Sqrt[-2 + 3*x^4 + 2*x^5]*(-2 + 15*x^4 + 2*x^5))/(3*x^6) - 
  8*ArcTanh[Sqrt[-2 + 3*x^4 + 2*x^5]/(2*x^2)], 
 -2*(ArcTanh[Sqrt[-1 + x^2 - x^5]/x] - 
   Sqrt[2]*ArcTanh[Sqrt[-1 + x^2 - x^5]/(Sqrt[2]*x)]), 
 2*ArcTanh[Sqrt[1 + x^2 + x^5]/x] - 
  2*Sqrt[2]*ArcTanh[Sqrt[1 + x^2 + x^5]/(Sqrt[2]*x)], 
 ArcTan[x/Sqrt[1 - x^2 + x^6]] - 
  Sqrt[2]*ArcTan[(Sqrt[2]*x)/Sqrt[1 - x^2 + x^6]], 
 Sqrt[1 + x + 2*x^4 + x^5 + x^8]/(4*x) - 
  (Sqrt[3]*ArcTan[(2 - x + 2*x^4)/(2*Sqrt[3]*
       Sqrt[1 + x + 2*x^4 + x^5 + x^8])])/16 - 
  ArcTanh[(-1 - x^4 + Sqrt[1 + x + 2*x^4 + x^5 + x^8])/
     (-1 - x - x^4 + Sqrt[1 + x + 2*x^4 + x^5 + x^8])]/8, 
 (Sqrt[1 + x^2 + x^4 + x^6 + x^8]*(8 + 26*x^2 + 65*x^4 + 26*x^6 + 8*x^8))/
   (48*x^6) + (65*Log[x])/16 + 2*Sqrt[5]*Log[-1 + x^2] - 
  (65*Log[-2 - x^2 - 2*x^4 + 2*Sqrt[1 + x^2 + x^4 + x^6 + x^8]])/32 - 
  Sqrt[5]*Log[5 + 5*x^4 + 2*Sqrt[5]*Sqrt[1 + x^2 + x^4 + x^6 + x^8]], 
 Sqrt[2]*ArcTan[Sqrt[-1 - x^2 + x^5]/(Sqrt[2]*x)], 
 ((1 - 4*x^3 + x^6)*Sqrt[2 + 3*x^6 + 2*x^12])/(6*x^6) - 
  (2*ArcTan[x^3/Sqrt[2 + 3*x^6 + 2*x^12]])/3 - Log[x]/(2*Sqrt[2]) + 
  Log[Sqrt[2] + Sqrt[2]*x^6 - Sqrt[2 + 3*x^6 + 2*x^12]]/(6*Sqrt[2]), 
 -((1 + 3*x + x^2)*Sqrt[1 + 3*x^2 + x^4])/(5*(1 + x + x^2 + x^3 + x^4)) + 
  ((-3*Sqrt[2] + Sqrt[10])*ArcTanh[(1 + Sqrt[5] + 3*x - Sqrt[5]*x + x^2 + 
       Sqrt[5]*x^2 + Sqrt[10 - 2*Sqrt[5]]*Sqrt[1 + 3*x^2 + x^4])/
      (-3 + Sqrt[5] + x + Sqrt[5]*x - 3*x^2 + Sqrt[5]*x^2 + 
       Sqrt[10 - 2*Sqrt[5]]*Sqrt[1 + 3*x^2 + x^4])])/(5*Sqrt[5 - Sqrt[5]]) + 
  ((-3*Sqrt[2] - Sqrt[10])*ArcTanh[(3 + Sqrt[5] - x + Sqrt[5]*x + 3*x^2 + 
       Sqrt[5]*x^2 - Sqrt[10 + 2*Sqrt[5]]*Sqrt[1 + 3*x^2 + x^4])/
      (-1 + Sqrt[5] - 3*x - Sqrt[5]*x - x^2 + Sqrt[5]*x^2 - 
       Sqrt[10 + 2*Sqrt[5]]*Sqrt[1 + 3*x^2 + x^4])])/(5*Sqrt[5 + Sqrt[5]]), 
 Sqrt[1 - 2*x^3 - x^4 + x^6]/x^2 + 
  (4*Sqrt[1 - 2*x^3 - x^4 + x^6]*
    ArcTanh[(x + Sqrt[2]*(Sqrt[-1 - x^2 + x^3] + Sqrt[-1 + x^2 + x^3]))/
      (-x + Sqrt[2]*(Sqrt[-1 - x^2 + x^3] + Sqrt[-1 + x^2 + x^3]))])/
   (Sqrt[-1 - x^2 + x^3]*Sqrt[-1 + x^2 + x^3]), 
 ((8 + x^2 + 14*x^5)*Sqrt[16 + 4*x^2 + x^4 + 56*x^5 + 7*x^7 + 49*x^10])/
   (4*x^4) + (3*Log[x])/4 - 
  (3*Log[Sqrt[3]*(-8 - x^2 - 14*x^5 + 2*Sqrt[16 + 4*x^2 + x^4 + 56*x^5 + 
          7*x^7 + 49*x^10])])/8, 
 -Sqrt[1 + 2*x^3 - 2*x^9 + x^12]/(3*(-1 - x^3 + x^6)) - Log[x] + 
  Log[-1 - x^3 + x^6 + Sqrt[1 + 2*x^3 - 2*x^9 + x^12]]/3, 
 (Sqrt[4 + 2*x^4 - 4*x^7 - x^8 - x^11 + x^14]*(-128 - 96*x^4 + 192*x^7 + 
     76*x^8 + 96*x^11 + 23*x^12 - 96*x^14 - 38*x^15 - 24*x^18 + 16*x^21))/
   (64*x^16) - (75*Log[x])/32 + 
  (75*Log[-4 - x^4 + 2*x^7 + 2*Sqrt[4 + 2*x^4 - 4*x^7 - x^8 - x^11 + x^14]])/
   128, (Sqrt[2 - x^3 + x^6 + 8*x^7 - 2*x^10 + 8*x^14]*
    (128 - 96*x^3 + 164*x^6 + 768*x^7 - 37*x^9 - 384*x^10 + 328*x^13 + 
     1536*x^14 - 384*x^17 + 1024*x^21))/(256*x^12) + 
  (441*Log[x])/(512*Sqrt[2]) - 
  (147*Log[-4*Sqrt[7] + Sqrt[7]*x^3 - 8*Sqrt[7]*x^7 + 
      2*Sqrt[14]*Sqrt[2 - x^3 + x^6 + 8*x^7 - 2*x^10 + 8*x^14]])/
   (512*Sqrt[2]), Sqrt[2]*ArcTanh[Sqrt[2 + 2*x^2 - x^3]/(Sqrt[2]*x)], 
 -ArcTan[Sqrt[-2 - x^2 + 2*x^4]/x] + 
  Sqrt[2]*ArcTan[Sqrt[-2 - x^2 + 2*x^4]/(Sqrt[2]*x)], 
 -2*ArcTan[Sqrt[1 - x^2 - x^5]/x], ArcTanh[x/Sqrt[-1 + x^2 - x^6]], 
 (Sqrt[1 - x^4]*(-2 + 5*x^4))/(48*x^6) - 
  (Sqrt[1/5 + 1/Sqrt[5]]*ArcTan[(2*Sqrt[1 - x^4])/(Sqrt[-1 + Sqrt[5]]*x^2)])/
   32 - (Sqrt[-1/5 + 1/Sqrt[5]]*ArcTanh[(2*Sqrt[1 - x^4])/
      (Sqrt[1 + Sqrt[5]]*x^2)])/32, 
 (Sqrt[1 - x^4 - x^6]*(-1 + 4*x^4 + x^6))/(3*x^6) - 
  Sqrt[2]*ArcTan[Sqrt[1 - x^4 - x^6]/(Sqrt[2]*x^2)], 
 (2*Sqrt[-1 - x^4 + x^7])/x^2 + 2*ArcTan[Sqrt[-1 - x^4 + x^7]/x^2] - 
  4*Sqrt[2]*ArcTan[Sqrt[-1 - x^4 + x^7]/(Sqrt[2]*x^2)], 
 -ArcTan[Sqrt[2 + x^2 - x^4]/x], 
 ((1 - 5*x^4 + 2*x^5)*Sqrt[2 - 7*x^4 + 4*x^5])/(6*x^6) + 
  ArcTan[Sqrt[2 - 7*x^4 + 4*x^5]/(2*Sqrt[2]*x^2)]/Sqrt[2], 
 ((5*Sqrt[2/(3 - Sqrt[5])] - 3*Sqrt[10/(3 - Sqrt[5])])*
    ArcTan[(Sqrt[3/2 + Sqrt[5]/2]*Sqrt[-1 - x^2 + x^3])/x])/5 + 
  2*Sqrt[2/(5*(3 - Sqrt[5]))]*
   ArcTan[(Sqrt[2/(3 + Sqrt[5])]*Sqrt[-1 - x^2 + x^3])/x], 
 (Sqrt[4 + 2*x^3 + 4*x^4 - 2*x^6 + x^7 + x^8]*(32 + 4*x^3 + 32*x^4 - 19*x^6 + 
     2*x^7 + 8*x^8))/(24*x^9) + (27*Log[x])/16 - 
  (9*Log[-4 - x^3 - 2*x^4 + 2*Sqrt[4 + 2*x^3 + 4*x^4 - 2*x^6 + x^7 + x^8]])/
   16, ((-3*Sqrt[-1 - I*Sqrt[3]] + 3*Sqrt[-1 + I*Sqrt[3]] + 
    I*Sqrt[-3 - (3*I)*Sqrt[3]] + I*Sqrt[-3 + (3*I)*Sqrt[3]])*
   ArcTan[(-(Sqrt[-2 - (2*I)*Sqrt[3]]*x*Sqrt[-1 + x^4]) + 
      Sqrt[-2 + (2*I)*Sqrt[3]]*x*Sqrt[-1 + x^4])/
     (-2 + Sqrt[-1 - I*Sqrt[3]]*Sqrt[-1 + I*Sqrt[3]]*x^2 + 2*x^4)])/
  (6*Sqrt[-1 + I*Sqrt[3]]*Sqrt[-2 - (2*I)*Sqrt[3]]), 
 -(Sqrt[-1 + x^2 - x^4]*(33*x - 10*x^3 + 58*x^5 - 10*x^7 + 33*x^9))/
   (48*(1 + x^4)^3) + (5*ArcTanh[x/Sqrt[-1 + x^2 - x^4]])/16, 
 -2*ArcTan[Sqrt[-1 + x^2 - x^5]/x], 
 (Sqrt[4 - 3*x^2 + x^4] + 8*x*ArcTanh[(-2 + x + x^2 + Sqrt[4 - 3*x^2 + x^4])/
      (2 + x - x^2 + Sqrt[4 - 3*x^2 + x^4])] - 
   5*x*ArcTanh[(-2 + x + x^2 + Sqrt[4 - 3*x^2 + x^4])/
      (-2 - x + x^2 + Sqrt[4 - 3*x^2 + x^4])] - 
   5*Sqrt[5]*x*ArcTanh[(-2 + 3*x + x^2 + Sqrt[5]*Sqrt[4 - 3*x^2 + x^4])/
      (6 + x - 3*x^2 + Sqrt[5]*Sqrt[4 - 3*x^2 + x^4])])/(2*x), 
 (Sqrt[6 + 4*x^2 + 3*x^4]*(42 + 16*x^2 + 21*x^4))/(189*x^3) + 
  (4*ArcTanh[(3*Sqrt[3/7]*x)/Sqrt[6 + 4*x^2 + 3*x^4]])/(7*Sqrt[21]), 
 (Sqrt[1 - x^4]*(-3*x + 5*x^3 + 3*x^5))/(8*(-1 + x^2 + x^4)^2) + 
  (11*ArcTanh[Sqrt[1 - x^4]/x])/8, ArcTan[Sqrt[1 - 3*x^2 - 2*x^4]/x] - 
  Sqrt[2]*ArcTan[Sqrt[1 - 3*x^2 - 2*x^4]/(Sqrt[2]*x)], 
 2*ArcTanh[Sqrt[-1 + x^2 + x^3 + x^4]/x] - 
  2*Sqrt[2]*ArcTanh[Sqrt[-1 + x^2 + x^3 + x^4]/(Sqrt[2]*x)], 
 (Sqrt[-16 - 19*x^4 + 4*x^5]*(-16 - 13*x^4 + 4*x^5))/(3*x^6) - 
  2*Sqrt[13/3]*ArcTan[(Sqrt[3/13]*Sqrt[-16 - 19*x^4 + 4*x^5])/x^2], 
 (Sqrt[-3 + x^2 - 8*x^5]*(3 - 11*x^2 + 8*x^5))/(27*x^3) + 
  (20*Sqrt[5/3]*ArcTan[(Sqrt[3/5]*Sqrt[-3 + x^2 - 8*x^5])/(2*x)])/27, 
 (I/2)*(Sqrt[-1 - I]*ArcTan[Sqrt[-1 + x^2 + x^4 + x^6]/(Sqrt[-1 - I]*x)] - 
   Sqrt[-1 + I]*ArcTan[Sqrt[-1 + x^2 + x^4 + x^6]/(Sqrt[-1 + I]*x)]), 
 (42*Sqrt[-6 + x^2 - 3*x^3])/x - 88*ArcTanh[Sqrt[-6 + x^2 - 3*x^3]/(2*x)] + 
  2*ArcTanh[Sqrt[-6 + x^2 - 3*x^3]/x], ArcTanh[Sqrt[1 - 2*x^3 - x^4]/x], 
 -ArcTan[Sqrt[1 - 3*x^2 + 2*x^4]/x] + 
  Sqrt[2]*ArcTan[Sqrt[1 - 3*x^2 + 2*x^4]/(Sqrt[2]*x)], 
 (Sqrt[1 - 2*x^2 + x^4]*(3 - 7*x^2 + 13*x^4 - 7*x^6 + 3*x^8))/(15*x^5) - 
  (Sqrt[7/3]*ArcTan[(Sqrt[3/7]*Sqrt[1 - 2*x^2 + x^4])/x])/3, 
 ArcTanh[Sqrt[-1 + x^2 + x^4 + x^6]/x] - 
  Sqrt[2]*ArcTanh[Sqrt[-1 + x^2 + x^4 + x^6]/(Sqrt[2]*x)], 
 (Sqrt[3 + x^2 + 2*x^3]*(324 - 84*x^2 + 432*x^3 + 61*x^4 - 56*x^5 + 144*x^6))/
   (135*x^5) - (25*ArcTanh[(Sqrt[3]*Sqrt[3 + x^2 + 2*x^3])/x])/(27*Sqrt[3]), 
 -2*(ArcTan[Sqrt[1 - x^2 + x^4 + x^5]/x] - 
   Sqrt[2]*ArcTan[Sqrt[1 - x^2 + x^4 + x^5]/(Sqrt[2]*x)]), 
 (Sqrt[3 + 4*x^2 - 7*x^5]*(-120 - 37*x^2 + 280*x^5))/(288*x^3) - 
  (41*Sqrt[35/2]*ArcTanh[(2*Sqrt[2/35]*Sqrt[3 + 4*x^2 - 7*x^5])/x])/192, 
 -(Sqrt[1 + x^2 - x^3 - x^6]*(-5*x - 2*x^3 + 5*x^4 + 5*x^7))/
   (4*(-1 + x^3 + x^6)^2) + (3*ArcTanh[Sqrt[1 + x^2 - x^3 - x^6]/x])/4, 
 (Sqrt[1 + x^2 + x^4]*(9*x + 2*x^3 + 9*x^5))/(8*(1 + x^4)^2) + 
  (31*ArcTanh[x/Sqrt[1 + x^2 + x^4]])/8 - 
  3*Sqrt[2]*ArcTanh[(Sqrt[2]*x)/Sqrt[1 + x^2 + x^4]], 
 2*ArcTanh[x/Sqrt[-1 + x^3 - x^6]], Sqrt[-1 + x^2 + 2*x^3]/x + 
  ArcTanh[Sqrt[-1 + x^2 + 2*x^3]/x] - 
  Sqrt[2]*ArcTanh[(Sqrt[2]*Sqrt[-1 + x^2 + 2*x^3])/x], 
 (49*Sqrt[196 - 389*x^4 + 196*x^8])/(8*x^2) - 
  2*Sqrt[3]*ArcTanh[(-1 + 3*x^2 + x^4 + 
      Sqrt[3]*Sqrt[196 - 389*x^4 + 196*x^8])/(1 + 3*x^2 - x^4 + 
      Sqrt[3]*Sqrt[196 - 389*x^4 + 196*x^8])] - 
  (41*Sqrt[61]*ArcTanh[(94 + 7*x^2 - 94*x^4 + 
       Sqrt[61]*Sqrt[196 - 389*x^4 + 196*x^8])/(102 + 5*x^2 - 102*x^4 + 
       Sqrt[61]*Sqrt[196 - 389*x^4 + 196*x^8])])/8 + (287*Log[x])/8 - 
  (287*Log[Sqrt[3]*(-14 + 14*x^4 + Sqrt[196 - 389*x^4 + 196*x^8])])/16, 
 ((-4 - 7*x^2 + x^5)*Sqrt[128 - x^4 - 64*x^5 + 8*x^10])/(4*x^4) + 
  (7*ArcTanh[(-24 - 2*x^2 + 6*x^5 + Sqrt[128 - x^4 - 64*x^5 + 8*x^10])/
      (-8 + 2*x^5 + Sqrt[128 - x^4 - 64*x^5 + 8*x^10])])/2 + 
  (29*Log[x])/(4*Sqrt[2]) - 
  (29*Log[-8*Sqrt[2] + 2*Sqrt[2]*x^5 + Sqrt[128 - x^4 - 64*x^5 + 8*x^10]])/
   (8*Sqrt[2]), Sqrt[2]*ArcTanh[Sqrt[1 + x^2 - x^3 - x^6]/(Sqrt[2]*x)], 
 ((-32 + 23*x^4 + 16*x^6)*Sqrt[32 - 14*x^4 - 32*x^6 - 4*x^8 + 7*x^10 + 
      8*x^12])/(32*x^8) + ArcTan[(14 + 8*x^4 - 7*x^6)/
    (4*Sqrt[32 - 14*x^4 - 32*x^6 - 4*x^8 + 7*x^10 + 8*x^12])] - 
  (65*Log[x])/(32*Sqrt[2]) + 
  (65*Log[-32*Sqrt[2] + 7*Sqrt[2]*x^4 + 16*Sqrt[2]*x^6 - 
      8*Sqrt[32 - 14*x^4 - 32*x^6 - 4*x^8 + 7*x^10 + 8*x^12]])/(128*Sqrt[2]), 
 ArcTan[Sqrt[1 - x^2 - x^4 - x^6]/x] - 
  Sqrt[2]*ArcTan[Sqrt[1 - x^2 - x^4 - x^6]/(Sqrt[2]*x)], 
 I*(Sqrt[-1 - I]*ArcTan[Sqrt[-1 + x^2 - x^3 + x^5]/(Sqrt[-1 - I]*x)] - 
   Sqrt[-1 + I]*ArcTan[Sqrt[-1 + x^2 - x^3 + x^5]/(Sqrt[-1 + I]*x)]), 
 ((30 + 5*x^2 + 24*x^5)*Sqrt[25 - x^4 + 40*x^5 + 16*x^10])/(36*x^4) - 
  (5*Sqrt[13]*ArcTanh[(65 - 13*x^2 + 52*x^5 + 
       Sqrt[13]*Sqrt[25 - x^4 + 40*x^5 + 16*x^10])/(5 + x^2 + 4*x^5 + 
       Sqrt[13]*Sqrt[25 - x^4 + 40*x^5 + 16*x^10])])/108 - Log[x]/108 + 
  Log[-5 - 4*x^5 + Sqrt[25 - x^4 + 40*x^5 + 16*x^10]]/216, 
 ((144 - 17*x^2 + 24*x^4)*Sqrt[144 + 6*x^2 + 47*x^4 + x^6 + 4*x^8])/
   (128*x^4) + 
  (5*ArcTan[(72 - 7*x^2 + 12*x^4)/(4*Sqrt[2]*
       Sqrt[144 + 6*x^2 + 47*x^4 + x^6 + 4*x^8])])/(32*Sqrt[2]) - 
  (111*Log[x])/256 + 
  (111*Log[-48 - x^2 - 8*x^4 + 4*Sqrt[144 + 6*x^2 + 47*x^4 + x^6 + 4*x^8]])/
   512, (2*(-12 + x^4 + 6*x^5)*Sqrt[12 - 2*x^4 - 12*x^5 + x^9 + 3*x^10])/
   (3*x^8) + (16*Sqrt[12 - 2*x^4 - 12*x^5 + x^9 + 3*x^10]*Log[x])/
   (3*Sqrt[3]*Sqrt[-2 + x^5]*Sqrt[-6 + x^4 + 3*x^5]) - 
  (8*Sqrt[12 - 2*x^4 - 12*x^5 + x^9 + 3*x^10]*
    Log[Sqrt[3]*Sqrt[-2 + x^5] + Sqrt[-6 + x^4 + 3*x^5]])/
   (3*Sqrt[3]*Sqrt[-2 + x^5]*Sqrt[-6 + x^4 + 3*x^5]), 
 (5*Sqrt[49 - 84*x^7 - 6*x^8 + 36*x^14])/(8*x^4) + 
  ((-35*Sqrt[17] - 2*Sqrt[119])*ArcTan[(-7*Sqrt[7] - 12*x^4 + 6*Sqrt[7]*x^7)/
      (Sqrt[17]*Sqrt[49 - 84*x^7 - 6*x^8 + 36*x^14])])/224 + 
  ((35*Sqrt[17] - 2*Sqrt[119])*ArcTan[(-7*Sqrt[7] + 12*x^4 + 6*Sqrt[7]*x^7)/
      (Sqrt[17]*Sqrt[49 - 84*x^7 - 6*x^8 + 36*x^14])])/224 - Log[x]/2 + 
  Log[-7 + 6*x^7 + Sqrt[49 - 84*x^7 - 6*x^8 + 36*x^14]]/8, 
 ((-560 - 289*x^4 + 490*x^8)*Sqrt[320 + 56*x^4 - 565*x^8 - 49*x^12 + 
      245*x^16])/(80*x^8) + 
  6*Sqrt[7]*ArcTanh[(8 - 136*Sqrt[5] - x^4 + 3*Sqrt[5]*x^4 - 7*x^8 + 
      119*Sqrt[5]*x^8 - 2*Sqrt[35]*Sqrt[320 + 56*x^4 - 565*x^8 - 49*x^12 + 
         245*x^16])/(-8 - 136*Sqrt[5] + x^4 + 3*Sqrt[5]*x^4 + 7*x^8 + 
      119*Sqrt[5]*x^8 - 2*Sqrt[35]*Sqrt[320 + 56*x^4 - 565*x^8 - 49*x^12 + 
         245*x^16])] - (3037*Log[x])/(40*Sqrt[5]) + 
  (3037*Log[-80*Sqrt[5] - 7*Sqrt[5]*x^4 + 70*Sqrt[5]*x^8 + 
      10*Sqrt[320 + 56*x^4 - 565*x^8 - 49*x^12 + 245*x^16]])/(160*Sqrt[5]), 
 2*ArcTanh[Sqrt[1 + 2*x^2 - x^3 - x^4]/x], Sqrt[-4 - 15*x^4 - x^5]/x^2 + 
  ((-9*Sqrt[2] - 92*Sqrt[6])*ArcTan[(Sqrt[2/(17 - Sqrt[3])]*
       Sqrt[-4 - 15*x^4 - x^5])/x^2])/(6*Sqrt[17 - Sqrt[3]]) + 
  ((-9*Sqrt[2] + 92*Sqrt[6])*ArcTan[(Sqrt[2/(17 + Sqrt[3])]*
       Sqrt[-4 - 15*x^4 - x^5])/x^2])/(6*Sqrt[17 + Sqrt[3]]), 
 (4*Sqrt[-3 + x^5]*(-6 + 9*x^4 + 2*x^5))/(3*x^6) + 
  ((-36*6^(1/4) + 17*6^(3/4))*ArcTan[Sqrt[-3 + x^5]/(6^(1/4)*x^2)])/6 + 
  ((-36*6^(1/4) - 17*6^(3/4))*ArcTanh[Sqrt[-3 + x^5]/(6^(1/4)*x^2)])/6, 
 (7*Sqrt[-64 - 84*x^8 - 25*x^16])/(8*x^4) + 
  (2*ArcTan[(2*x^4)/Sqrt[-64 - 84*x^8 - 25*x^16]])/5 + 
  (27*Sqrt[41]*ArcTan[(Sqrt[41]*x^4)/(2*Sqrt[-64 - 84*x^8 - 25*x^16])])/80 + 
  ((27*I)/80)*Sqrt[41]*ArcTanh[(-2368*I - (2985*I)*x^8 - (925*I)*x^16 + 
      Sqrt[41]*(40 + 25*x^8)*Sqrt[-64 - 84*x^8 - 25*x^16])/
     (-1856*I - (2395*I)*x^8 - (725*I)*x^16 + Sqrt[41]*(40 + 25*x^8)*
       Sqrt[-64 - 84*x^8 - 25*x^16])] + ((27*I)/4)*Log[x] - 
  ((27*I)/16)*Log[-8*I - (5*I)*x^8 + Sqrt[-64 - 84*x^8 - 25*x^16]], 
 (3*(-1 + x^4 + 4*x^8)*Sqrt[3 - 20*x^8 + 48*x^16])/(16*x^8) + 
  ArcTanh[(-Sqrt[3] + 2*x^4 + 4*Sqrt[3]*x^8 + Sqrt[3 - 20*x^8 + 48*x^16])/
    (Sqrt[3] + 2*x^4 - 4*Sqrt[3]*x^8 + Sqrt[3 - 20*x^8 + 48*x^16])] - 
  (7*Sqrt[19]*ArcTanh[(-5 + 7*x^4 + 20*x^8 + 
       Sqrt[19]*Sqrt[3 - 20*x^8 + 48*x^16])/(-1 + 9*x^4 + 4*x^8 + 
       Sqrt[19]*Sqrt[3 - 20*x^8 + 48*x^16])])/16 - (15*Sqrt[3]*Log[x])/8 + 
  (15*Sqrt[3]*Log[-Sqrt[3] + 4*Sqrt[3]*x^8 + Sqrt[3 - 20*x^8 + 48*x^16]])/32, 
 2*ArcTanh[Sqrt[-2 + x^2 - 2*x^3 + 2*x^4]/x] - 
  2*Sqrt[2]*ArcTanh[Sqrt[-2 + x^2 - 2*x^3 + 2*x^4]/(Sqrt[2]*x)], 
 (2*Sqrt[-4 + x^2 + 3*x^3]*(-4 + 4*x^2 + 3*x^3))/(3*x^3) - 
  (2*(-5 + 2*Sqrt[5])*ArcTanh[(2*Sqrt[-4 + x^2 + 3*x^3])/((-1 + Sqrt[5])*x)])/
   5 - (2*(5 + 2*Sqrt[5])*ArcTanh[(2*Sqrt[-4 + x^2 + 3*x^3])/
      ((1 + Sqrt[5])*x)])/5, (Sqrt[-1 + 4*x^2 - 3*x^4]*(1 + 4*x^2 + 3*x^4))/
   (4*x^3) + ((-369*Sqrt[976 + 122*Sqrt[3]] + 8*Sqrt[2928 + 366*Sqrt[3]])*
    ArcTanh[(Sqrt[2/(8 - Sqrt[3])]*Sqrt[-1 + 4*x^2 - 3*x^4])/x])/5856 + 
  ((-8*Sqrt[2928 - 366*Sqrt[3]] - 369*Sqrt[976 - 122*Sqrt[3]])*
    ArcTanh[(Sqrt[2/(8 + Sqrt[3])]*Sqrt[-1 + 4*x^2 - 3*x^4])/x])/5856, 
 ((48 + 35*x^2 + 72*x^4)*Sqrt[16 - 14*x^2 + 49*x^4 - 21*x^6 + 36*x^8])/
   (64*x^4) + (7*Sqrt[3/2]*ArcTan[(Sqrt[3/2]*(4 + x^2 + 6*x^4))/
      (2*Sqrt[16 - 14*x^2 + 49*x^4 - 21*x^6 + 36*x^8])])/8 - 
  (267*Log[x])/128 + 
  (267*Log[-16 + 7*x^2 - 24*x^4 + 4*Sqrt[16 - 14*x^2 + 49*x^4 - 21*x^6 + 
         36*x^8]])/256, (Sqrt[-4 + 2*x^2 + 3*x^4]*(-16 + 29*x^2 + 12*x^4))/
   (9*x^3) + (((-11*I)*Sqrt[6 + (2*I)*Sqrt[3]] - 45*Sqrt[18 + (6*I)*Sqrt[3]])*
    ArcTan[(Sqrt[3/2]*Sqrt[-4 + 2*x^2 + 3*x^4])/(Sqrt[-3 + I*Sqrt[3]]*x)])/
   72 + (((11*I)*Sqrt[-6 + (2*I)*Sqrt[3]] - 45*Sqrt[-18 + (6*I)*Sqrt[3]])*
    ArcTanh[(Sqrt[3/2]*Sqrt[-4 + 2*x^2 + 3*x^4])/(Sqrt[3 + I*Sqrt[3]]*x)])/
   72, ((-6 - 3*x^2 + 8*x^4)*Sqrt[9 - 23*x^4 + 16*x^8])/(8*x^4) - 
  (3*Sqrt[3]*ArcTan[(72 - 190*x^4 + 128*x^8 + Sqrt[3]*(15 - 20*x^4)*
        Sqrt[9 - 23*x^4 + 16*x^8])/(9 - 20*x^4 + 16*x^8)])/8 + 
  (3*Sqrt[3]*ArcTanh[(3*x^4 + 2*Sqrt[3]*x^2*Sqrt[9 - 23*x^4 + 16*x^8])/
      (36 - 92*x^4 + 64*x^8 + 2*Sqrt[3]*x^2*Sqrt[9 - 23*x^4 + 16*x^8])])/16 - 
  (5*Log[x])/4 + (5*Log[-3 + 4*x^4 + Sqrt[9 - 23*x^4 + 16*x^8]])/8, 
 (4*Sqrt[8 + 35*x^4 + 32*x^8])/(5*x^2) - 
  (5*Sqrt[7/2]*ArcTanh[(4 + 4*Sqrt[2] + 6*x^2 - Sqrt[2]*x^2 + 8*x^4 + 
       8*Sqrt[2]*x^4 + Sqrt[14]*Sqrt[8 + 35*x^4 + 32*x^8])/
      (4 - 4*Sqrt[2] + 6*x^2 + Sqrt[2]*x^2 + 8*x^4 - 8*Sqrt[2]*x^4 + 
       Sqrt[14]*Sqrt[8 + 35*x^4 + 32*x^8])])/3 + 
  (Sqrt[83]*ArcTanh[(-8 + 5*Sqrt[2] + 15*x^2 + Sqrt[2]*x^2 - 16*x^4 + 
       10*Sqrt[2]*x^4 + Sqrt[83]*Sqrt[8 + 35*x^4 + 32*x^8])/
      (-8 - 5*Sqrt[2] + 15*x^2 - Sqrt[2]*x^2 - 16*x^4 - 10*Sqrt[2]*x^4 + 
       Sqrt[83]*Sqrt[8 + 35*x^4 + 32*x^8])])/75 - (43*Log[x])/(25*Sqrt[2]) + 
  (43*Log[2*Sqrt[6] + 4*Sqrt[6]*x^4 + Sqrt[3]*Sqrt[8 + 35*x^4 + 32*x^8]])/
   (50*Sqrt[2]), ((4 + 3*x^2 + 10*x^4)*Sqrt[4 + 22*x^4 + 25*x^8])/(4*x^4) + 
  ((-9*Sqrt[14] + 20*Sqrt[21])*ArcTanh[(-10 + Sqrt[6]*x^2 - 25*x^4 + 
       Sqrt[21]*Sqrt[4 + 22*x^4 + 25*x^8])/(-2 + 3*Sqrt[6]*x^2 - 5*x^4 + 
       Sqrt[21]*Sqrt[4 + 22*x^4 + 25*x^8])])/24 + 
  ((-9*Sqrt[14] - 20*Sqrt[21])*ArcTanh[(2 + 3*Sqrt[6]*x^2 + 5*x^4 + 
       Sqrt[21]*Sqrt[4 + 22*x^4 + 25*x^8])/(10 + Sqrt[6]*x^2 + 25*x^4 + 
       Sqrt[21]*Sqrt[4 + 22*x^4 + 25*x^8])])/24 + 7*Log[x] - 
  (7*Log[Sqrt[2]*(-2 - 5*x^4 + Sqrt[4 + 22*x^4 + 25*x^8])])/2, 
 ((-14 - 12*x^2 + 35*x^4)*Sqrt[4 - 21*x^4 + 25*x^8])/(12*x^4) + 
  ((-25*Sqrt[2] - 6*Sqrt[6])*ArcTan[(-2 + Sqrt[3]*x^2 + 5*x^4)/
      (Sqrt[2]*Sqrt[4 - 21*x^4 + 25*x^8])])/36 + 
  ((25*Sqrt[2] - 6*Sqrt[6])*ArcTan[(2*Sqrt[2] + Sqrt[6]*x^2 - 5*Sqrt[2]*x^4)/
      (2*Sqrt[4 - 21*x^4 + 25*x^8])])/36 - (29*Log[x])/18 + 
  (29*Log[-2 + 5*x^4 + Sqrt[4 - 21*x^4 + 25*x^8]])/36, 
 (Sqrt[-64 - 112*x^5 - 7*x^8 - 49*x^10]*(1024 - 144*x^4 + 1792*x^5 + 75*x^8 - 
     126*x^9 + 784*x^10))/(64*x^12) - ((393*I)/256)*
   ArcTanh[(96 + 9*x^4 + 84*x^5 - (12*I)*Sqrt[-64 - 112*x^5 - 7*x^8 - 
         49*x^10])/(192 - 103*x^4 + 168*x^5)] - 
  ((407*I)/256)*ArcTanh[((32 + 520*I) + (3 - 42*I)*x^4 + (28 + 455*I)*x^5 + 
      33*Sqrt[-64 - 112*x^5 - 7*x^8 - 49*x^10])/((-32 + 520*I) - 
      (3 + 42*I)*x^4 - (28 - 455*I)*x^5 + 
      33*Sqrt[-64 - 112*x^5 - 7*x^8 - 49*x^10])] - 
  ((407*I)/256)*ArcTanh[(11616*x^4 + 1089*x^8 + 10164*x^9 + 
      (5324*I)*x^4*Sqrt[-64 - 112*x^5 - 7*x^8 - 49*x^10])/
     (114688 - 1728*x^4 + 200704*x^5 + 13471*x^8 - 1512*x^9 + 87808*x^10)] - 
  ((393*I)/64)*Log[x] + ((393*I)/512)*Log[256 + 24*x^4 + 448*x^5 + 14*x^8 + 
     21*x^9 + 196*x^10 + (32*I + (3*I)*x^4 + (28*I)*x^5)*
      Sqrt[-64 - 112*x^5 - 7*x^8 - 49*x^10]], 
 ((5 - x^4 + 7*x^8)*Sqrt[25 + 67*x^8 + 49*x^16])/(24*x^8) + 
  ((34*Sqrt[65] - 5*Sqrt[78])*ArcTan[(-5*Sqrt[65] + 3*Sqrt[78]*x^4 - 
       7*Sqrt[65]*x^8)/(13*Sqrt[25 + 67*x^8 + 49*x^16])])/1440 + 
  ((-34*Sqrt[65] - 5*Sqrt[78])*ArcTan[(5*Sqrt[65] + 3*Sqrt[78]*x^4 + 
       7*Sqrt[65]*x^8)/(13*Sqrt[25 + 67*x^8 + 49*x^16])])/1440 + 
  (4*Log[x])/9 - Log[-5 - 7*x^8 + Sqrt[25 + 67*x^8 + 49*x^16]]/9, 
 (Sqrt[9 + 18*x^2 - 22*x^4 - 18*x^6 + 9*x^8]*(36 - 45*x^2 - 217*x^4 + 
     45*x^6 + 36*x^8))/(12*x^6) - 
  2*ArcTan[(-9 + 4*x^2 + 9*x^4)/(2*Sqrt[9 + 18*x^2 - 22*x^4 - 18*x^6 + 
        9*x^8])] + (157*Log[x])/2 - 
  (157*Log[-3 - 3*x^2 + 3*x^4 + Sqrt[9 + 18*x^2 - 22*x^4 - 18*x^6 + 9*x^8]])/
   4, (Sqrt[3 - 2*x^4 + 4*x^7]*(18 + 75*x^4 + 48*x^7 + 14*x^8 + 100*x^11 + 
     32*x^14))/(18*x^6*(3 + x^4 + 4*x^7)) - 
  (4*ArcTan[Sqrt[3 - 2*x^4 + 4*x^7]/(Sqrt[3]*x^2)])/Sqrt[3], 
 Sqrt[-3 + 5*x^2 + 6*x^3]/(2*x) - 
  (3*Sqrt[15/2]*ArcTanh[(Sqrt[2/15]*Sqrt[-3 + 5*x^2 + 6*x^3])/x])/2 + 
  2*Sqrt[2]*ArcTanh[Sqrt[-3 + 5*x^2 + 6*x^3]/(2*Sqrt[2]*x)], 
 (Sqrt[-1 + x^2 + x^4]*(-56 + 37*x^2 + 56*x^4))/(1152*x^3) + 
  (173*Sqrt[29/6]*ArcTanh[(2*Sqrt[6/29]*Sqrt[-1 + x^2 + x^4])/x])/25344 + 
  (Sqrt[3]*ArcTanh[(2*Sqrt[-1 + x^2 + x^4])/(Sqrt[3]*x)])/704, 
 (Sqrt[-30 + 3*x^2 - 10*x^3]*(2520 + 2748*x^2 + 1680*x^3 - 663*x^4 + 
     916*x^5 + 280*x^6))/(30*x^3*(6 - x^2 + 2*x^3)) - 
  (121*ArcTan[Sqrt[-30 + 3*x^2 - 10*x^3]/(Sqrt[2]*x)])/(2*Sqrt[2]), 
 (Sqrt[-1 + x^2 + 6*x^5]*(882 + 3766*x^2 + 467*x^4 - 10584*x^5 - 22596*x^7 + 
     31752*x^10))/(5145*x^5) - 
  (1023*Sqrt[2/7]*ArcTanh[(Sqrt[7/2]*Sqrt[-1 + x^2 + 6*x^5])/(3*x)])/343, 
 Sqrt[7 - 6*x^2 - 8*x^6]/(2*x) + 
  (7*Sqrt[11/2]*ArcTan[(Sqrt[2/11]*Sqrt[7 - 6*x^2 - 8*x^6])/x])/2 - 
  4*Sqrt[5]*ArcTan[Sqrt[7 - 6*x^2 - 8*x^6]/(Sqrt[5]*x)], 
 Sqrt[-1 - 4*x^2 + 8*x^6]/(5*x) + 
  ((-693 + 947*Sqrt[11])*ArcTan[(Sqrt[5/(24 - Sqrt[11])]*
       Sqrt[-1 - 4*x^2 + 8*x^6])/x])/(110*Sqrt[120 - 5*Sqrt[11]]) - 
  ((693 + 947*Sqrt[11])*ArcTan[(Sqrt[5/(24 + Sqrt[11])]*
       Sqrt[-1 - 4*x^2 + 8*x^6])/x])/(110*Sqrt[120 + 5*Sqrt[11]]), 
 (7*Sqrt[1 - 2*x^2 + 2*x^6])/(64*x) + 
  ((220 - 157*Sqrt[2])*ArcTan[(Sqrt[30 - Sqrt[2]]*x)/
      (4*Sqrt[1 - 2*x^2 + 2*x^6])])/(512*Sqrt[30 - Sqrt[2]]) + 
  ((220 + 157*Sqrt[2])*ArcTan[(Sqrt[30 + Sqrt[2]]*x)/
      (4*Sqrt[1 - 2*x^2 + 2*x^6])])/(512*Sqrt[30 + Sqrt[2]]), 
 ((3 - 8*x^4 + 3*x^8)*Sqrt[21 + 43*x^8 + 21*x^16])/(8*x^8) + 
  (7*ArcTanh[(-Sqrt[21] + x^4 - Sqrt[21]*x^8 + Sqrt[21 + 43*x^8 + 21*x^16])/
      (Sqrt[21] + x^4 + Sqrt[21]*x^8 + Sqrt[21 + 43*x^8 + 21*x^16])])/2 - 
  Sqrt[15/2]*ArcTanh[(-18 + 4*x^4 - 18*x^8 + 
      Sqrt[30]*Sqrt[21 + 43*x^8 + 21*x^16])/(-24 + 2*x^4 - 24*x^8 + 
      Sqrt[30]*Sqrt[21 + 43*x^8 + 21*x^16])] - (13*Sqrt[3/7]*Log[x])/2 + 
  (13*Sqrt[3/7]*Log[Sqrt[21] + Sqrt[21]*x^8 - Sqrt[21 + 43*x^8 + 21*x^16]])/
   8, ArcTan[(Sqrt[1 - x^6]*(-1 + x^6))/x^3]/3 - 
  ArcTanh[(-1 - x^2 + x^6)/(Sqrt[3]*x*Sqrt[1 - x^6])]/Sqrt[3], 
 (Sqrt[7 - 3*x^2 + 8*x^5]*(98 - 119*x^2 - 48*x^4 + 224*x^5 - 136*x^7 + 
     128*x^10))/(3*x^3*(7 + 2*x^2 + 8*x^5)) + 
  (27*ArcTan[Sqrt[7 - 3*x^2 + 8*x^5]/(Sqrt[5]*x)])/Sqrt[5], 
 (4*Sqrt[-1 - x^2 + 2*x^5] - 3*Sqrt[22]*x*
    ArcTan[(2*Sqrt[2/11]*Sqrt[-1 - x^2 + 2*x^5])/x] + 
   4*Sqrt[7]*x*ArcTan[(2*Sqrt[-1 - x^2 + 2*x^5])/(Sqrt[7]*x)])/(64*x), 
 (2*Sqrt[5 + 6*x^5]*(10 - 3*x^4 + 12*x^5))/(3*x^6) + 
  ((37*2^(1/4) + 4*2^(3/4))*ArcTan[Sqrt[5 + 6*x^5]/(2^(3/4)*x^2)])/4 + 
  ((-37*2^(1/4) + 4*2^(3/4))*ArcTanh[Sqrt[5 + 6*x^5]/(2^(3/4)*x^2)])/4, 
 (4*Sqrt[3 + 9*x^4 - 5*x^6])/(3*x^2) + 
  (7*(4*I + 5*Sqrt[3])*ArcTan[(Sqrt[3/2]*Sqrt[3 + 9*x^4 - 5*x^6])/
      (Sqrt[-12 + I*Sqrt[3]]*x^2)])/(6*Sqrt[-24 + (2*I)*Sqrt[3]]) - 
  (7*(-4*I + 5*Sqrt[3])*ArcTanh[(Sqrt[3/2]*Sqrt[3 + 9*x^4 - 5*x^6])/
      (Sqrt[12 + I*Sqrt[3]]*x^2)])/(6*Sqrt[24 + (2*I)*Sqrt[3]]), 
 ((-4 - 9*x^4 + 4*x^7)*Sqrt[6 - 13*x^4 - 12*x^7 + 3*x^8 + 13*x^11 + 6*x^14])/
   (8*x^8) + (5*Sqrt[11/3]*ArcTan[(-18 - 29*x^4 + 18*x^7)/
      (4*Sqrt[33]*Sqrt[6 - 13*x^4 - 12*x^7 + 3*x^8 + 13*x^11 + 6*x^14])])/3 + 
  (217*Log[x])/(12*Sqrt[6]) - 
  (217*Log[-12 + 13*x^4 + 12*x^7 + 2*Sqrt[6]*
       Sqrt[6 - 13*x^4 - 12*x^7 + 3*x^8 + 13*x^11 + 6*x^14]])/(48*Sqrt[6]), 
 (Sqrt[180 + 60*x^3 - 8*x^4 + 5*x^6]*(1800 - 390*x^2 + 600*x^3 + 146*x^4 - 
     65*x^5 + 50*x^6))/(250*x^6) + 
  (113*Sqrt[39/5]*ArcTan[(Sqrt[5/39]*(6 + 8*x^2 + x^3))/
      Sqrt[180 + 60*x^3 - 8*x^4 + 5*x^6]])/125 + (294*Log[x])/(125*Sqrt[5]) - 
  (147*Log[6*Sqrt[5] + Sqrt[5]*x^3 - Sqrt[180 + 60*x^3 - 8*x^4 + 5*x^6]])/
   (125*Sqrt[5]), ((4 - 4*x^2 + x^4)*Sqrt[4 + 7*x^2 + x^4])/(2*x^3) + 
  ((-143*Sqrt[2] - 14*Sqrt[11])*ArcTan[(Sqrt[-2 + Sqrt[11/2]]*x)/
      Sqrt[4 + 7*x^2 + x^4]])/(88*Sqrt[-4 + Sqrt[22]]) + 
  ((-143*Sqrt[2] + 14*Sqrt[11])*ArcTanh[(Sqrt[2 + Sqrt[11/2]]*x)/
      Sqrt[4 + 7*x^2 + x^4]])/(88*Sqrt[4 + Sqrt[22]]), 
 -(((-3 - 12*x^4 + 5*x^5)*Sqrt[-9 - 24*x^4 + 30*x^5 - 17*x^8 + 40*x^9 - 
       25*x^10])/x^8) + 
  (6*I)*ArcTanh[(12 - 20*x^5 - (4*I)*Sqrt[-9 - 24*x^4 + 30*x^5 - 17*x^8 + 
         40*x^9 - 25*x^10])/(-24 - 17*x^4 + 40*x^5)] - (24*I)*Log[x] + 
  (3*I)*Log[18 + 24*x^4 - 60*x^5 + x^8 - 40*x^9 + 50*x^10 + 
     (6*I - (10*I)*x^5)*Sqrt[-9 - 24*x^4 + 30*x^5 - 17*x^8 + 40*x^9 - 
        25*x^10]], 
 (2*(-12*Sqrt[-6 + x^4 + 3*x^7] + 41*x^4*Sqrt[-6 + x^4 + 3*x^7] + 
     6*x^7*Sqrt[-6 + x^4 + 3*x^7]))/(81*x^6) + 
  (76*ArcTanh[(2*Sqrt[-6 + x^4 + 3*x^7])/((-3 + Sqrt[5])*x^2)])/
   (81*(101 + 45*Sqrt[5])) - (2*(10163 + 4545*Sqrt[5])*
    ArcTanh[(2*Sqrt[-6 + x^4 + 3*x^7])/((3 + Sqrt[5])*x^2)])/
   (81*(101 + 45*Sqrt[5])), x/Sqrt[-1 - x^2 - x^4] - 
  ((2*I)*ArcTan[(Sqrt[-1/2 - (I/2)*Sqrt[3]]*x)/Sqrt[-1 - x^2 - x^4]])/
   Sqrt[-3/2 - ((3*I)/2)*Sqrt[3]] + 
  ((2*I)*ArcTan[(Sqrt[-1/2 + (I/2)*Sqrt[3]]*x)/Sqrt[-1 - x^2 - x^4]])/
   Sqrt[-3/2 + ((3*I)/2)*Sqrt[3]], 
 ((70 + 23*x^2 + 60*x^4)*Sqrt[49 + 85*x^4 + 36*x^8])/(32*x^4) - 
  Sqrt[5]*ArcTanh[(21 - x^2 + 18*x^4 + Sqrt[5]*Sqrt[49 + 85*x^4 + 36*x^8])/
     (7 + 3*x^2 + 6*x^4 + Sqrt[5]*Sqrt[49 + 85*x^4 + 36*x^8])] - 
  (7*Sqrt[17]*ArcTanh[(21 + 5*x^2 + 18*x^4 + 
       Sqrt[17]*Sqrt[49 + 85*x^4 + 36*x^8])/(-35 + 3*x^2 - 30*x^4 + 
       Sqrt[17]*Sqrt[49 + 85*x^4 + 36*x^8])])/64 + (161*Log[x])/64 - 
  (161*Log[-7 - 6*x^4 + Sqrt[49 + 85*x^4 + 36*x^8]])/128, 
 ((-147 - 55*x^2 + 168*x^4)*Sqrt[49 - 113*x^4 + 64*x^8])/(72*x^4) + 
  ((845*Sqrt[2555 - 73*Sqrt[73]] - 64313*Sqrt[35 - Sqrt[73]])*
    ArcTan[(Sqrt[70 + 2*Sqrt[73]]*Sqrt[49 - 113*x^4 + 64*x^8])/
      (7 - 7*Sqrt[73] - 12*x^2 + (-8 + 8*Sqrt[73])*x^4)])/1513728 + 
  ((64313*Sqrt[35 + Sqrt[73]] + 845*Sqrt[2555 + 73*Sqrt[73]])*
    ArcTan[(Sqrt[70 - 2*Sqrt[73]]*Sqrt[49 - 113*x^4 + 64*x^8])/
      (-7 - 7*Sqrt[73] + 12*x^2 + (8 + 8*Sqrt[73])*x^4)])/1513728 + 
  (17*Log[x])/216 - (17*Log[-7 + 8*x^4 + Sqrt[49 - 113*x^4 + 64*x^8]])/432, 
 ((-3 + 4*x^8)*Sqrt[63 - 170*x^8 + 112*x^16])/(8*x^8) - 
  (Sqrt[5]*ArcTanh[(-24 - 3*x^4 + 32*x^8 + 
       Sqrt[5]*Sqrt[63 - 170*x^8 + 112*x^16])/(-18 - x^4 + 24*x^8 + 
       Sqrt[5]*Sqrt[63 - 170*x^8 + 112*x^16])])/7 + 
  (Sqrt[31]*ArcTanh[(51 - 5*x^4 - 68*x^8 + 
       Sqrt[31]*Sqrt[63 - 170*x^8 + 112*x^16])/(75 - 11*x^4 - 100*x^8 + 
       Sqrt[31]*Sqrt[63 - 170*x^8 + 112*x^16])])/28 - 
  (5*Log[x])/(2*Sqrt[7]) + 
  (5*Log[-3*Sqrt[7] + 4*Sqrt[7]*x^8 + Sqrt[63 - 170*x^8 + 112*x^16]])/
   (8*Sqrt[7]), (Sqrt[1 - x^2 + x^5]*(2 + 7*x^2 + 2*x^5))/(18*x^3) + 
  (Sqrt[3 - Sqrt[6]]*(-66 + 25*Sqrt[6])*
    ArcTan[(Sqrt[6/(3 - Sqrt[6])]*Sqrt[1 - x^2 + x^5])/x])/
   (72*(-3 + Sqrt[6])) + 
  ((-66 - 25*Sqrt[6])*ArcTan[(Sqrt[6/(3 + Sqrt[6])]*Sqrt[1 - x^2 + x^5])/x])/
   (72*Sqrt[3 + Sqrt[6]]), (Sqrt[-2 + x^2 + 2*x^3]*(-28 + 53*x^2 + 28*x^3))/
   (6*x^3) + ((95*Sqrt[21 + 7*Sqrt[2]] - 62*Sqrt[42 + 14*Sqrt[2]])*
    ArcTanh[(Sqrt[6/7 + (2*Sqrt[2])/7]*Sqrt[-2 + x^2 + 2*x^3])/x])/56 + 
  ((-62*Sqrt[42 - 14*Sqrt[2]] - 95*Sqrt[21 - 7*Sqrt[2]])*
    ArcTanh[(Sqrt[2/(3 + Sqrt[2])]*Sqrt[-2 + x^2 + 2*x^3])/x])/56, 
 (Sqrt[3 - 8*x^2 - 6*x^6]*(-3 + 11*x^2 + 6*x^6))/(9*x^3) + 
  ((-369*Sqrt[2/(65 - Sqrt[41])] + 62*Sqrt[82/(65 - Sqrt[41])])*
    ArcTan[(2*Sqrt[2/(65 - Sqrt[41])]*Sqrt[3 - 8*x^2 - 6*x^6])/x])/123 + 
  ((-369*Sqrt[2/(65 + Sqrt[41])] - 62*Sqrt[82/(65 + Sqrt[41])])*
    ArcTan[(2*Sqrt[2/(65 + Sqrt[41])]*Sqrt[3 - 8*x^2 - 6*x^6])/x])/123, 
 ((56 + 27*x^4 + 28*x^8)*Sqrt[28 + 6*x^4 + 30*x^8 + 3*x^12 + 7*x^16])/
   (112*x^8) - 
  ArcTanh[(-6 + 3*x^4 - 3*x^8 + 3*Sqrt[28 + 6*x^4 + 30*x^8 + 3*x^12 + 
         7*x^16])/(-10 + 2*x^4 - 5*x^8 + 
      3*Sqrt[28 + 6*x^4 + 30*x^8 + 3*x^12 + 7*x^16])]/2 - 
  (Sqrt[17/3]*ArcTanh[(30 + 9*x^4 + 15*x^8 + 
       Sqrt[51]*Sqrt[28 + 6*x^4 + 30*x^8 + 3*x^12 + 7*x^16])/
      (22 + 10*x^4 + 11*x^8 + Sqrt[51]*Sqrt[28 + 6*x^4 + 30*x^8 + 3*x^12 + 
          7*x^16])])/32 + (69*Log[x])/(112*Sqrt[7]) - 
  (69*Log[-28*Sqrt[47] - 3*Sqrt[47]*x^4 - 14*Sqrt[47]*x^8 + 
      2*Sqrt[329]*Sqrt[28 + 6*x^4 + 30*x^8 + 3*x^12 + 7*x^16]])/
   (448*Sqrt[7]), (Sqrt[-7 - 6*x^2 - 3*x^3]*(14 + 57*x^2 + 6*x^3))/(6*x^3) - 
  (3*(119 + 33*Sqrt[17])*ArcTan[(2*Sqrt[-7 - 6*x^2 - 3*x^3])/
      (Sqrt[-1 + Sqrt[17]]*x)])/(68*Sqrt[-1 + Sqrt[17]]) - 
  (3*(-119 + 33*Sqrt[17])*ArcTanh[(2*Sqrt[-7 - 6*x^2 - 3*x^3])/
      (Sqrt[1 + Sqrt[17]]*x)])/(68*Sqrt[1 + Sqrt[17]]), 
 ((-15 - 14*x^4 + 21*x^7)*Sqrt[25 - 70*x^7 + 7*x^8 + 49*x^14])/x^8 + 
  4*Sqrt[2]*ArcTanh[(-5 + 3*x^4 + 7*x^7 + 
      Sqrt[2]*Sqrt[25 - 70*x^7 + 7*x^8 + 49*x^14])/
     (4*x^4 + Sqrt[2]*Sqrt[25 - 70*x^7 + 7*x^8 + 49*x^14])] + 
  26*Sqrt[11]*ArcTanh[(5 + 9*x^4 - 7*x^7 + 
      Sqrt[11]*Sqrt[25 - 70*x^7 + 7*x^8 + 49*x^14])/
     (15 + 5*x^4 - 21*x^7 + Sqrt[11]*Sqrt[25 - 70*x^7 + 7*x^8 + 49*x^14])] - 
  184*Log[x] + 
  46*Log[Sqrt[7]*(-5 + 7*x^7 + Sqrt[25 - 70*x^7 + 7*x^8 + 49*x^14])], 
 -((x*Sqrt[1 - x^2 - x^3 + x^6])/(1 - x^3 + x^6)) + 
  3*ArcTan[Sqrt[1 - x^2 - x^3 + x^6]/x] - 
  Sqrt[2]*ArcTan[Sqrt[1 - x^2 - x^3 + x^6]/(Sqrt[2]*x)]};
