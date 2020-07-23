# A simple method for computing some pseudo-elliptic integrals

This is a Mathematica prototype for solving _some_ pseudo-elliptic integrals in terms of elementary functions. Relative to the algebraic case of the Risch-Trager-Bronstein algorithm, its very simple, however it is not a decision process. 

This method should be easy enough to port to other CAS. We use **SolveAlways** to equate coefficients of powers of _x_ and compute the undeterminted coefficients in a system of equations, but this could easily be hand-coded and reduced to a system of linear equations.

The package AlgebraicIntegrateHeuristic.m exposes one function: **solveAlgebraicIntegral[**_integrand_, _variable_**]** and returns a list containing {_the rational part of the integrand (unintegrated)_, _the unintegrated part_, _the integrated part_}. 

This method can solve some interesting pseudo-elliptic integrals. Below are some examples, many more are in the source code. 

```
In[1261]:= IntegrateAlgebraic[(1 + x^2)/((1 - x^2) Sqrt[1 + x^4]), x]

Out[1261]= ArcTanh[(Sqrt[2] x)/Sqrt[1 + x^4]]/Sqrt[2]
```

```
In[1260]:= IntegrateAlgebraic[(1 + x^6)/((1 - x^6) Sqrt[1 - x^2 + x^4]), x]

Out[1260]= 1/3 Sqrt[2] ArcTan[(Sqrt[2] x)/Sqrt[1 - x^2 + x^4]] + 1/3 ArcTanh[x/Sqrt[1 - x^2 + x^4]]
```

```
In[1259]:= IntegrateAlgebraic[1/(x^4 + 1)^(1/4), x]

Out[1259]= 1/2 ArcTan[x/(1 + x^4)^(1/4)] + 1/2 ArcTanh[x/(1 + x^4)^(1/4)]
```

```
In[1258]:= IntegrateAlgebraic[((-1 + x^4 - x^5)^(1/4) (-4 + x^5))/x^6, x]

Out[1258]= -((4 (-1 + x^4 - x^5)^(5/4))/(5 x^5))
```

```
In[1257]:= IntegrateAlgebraic[((3 + 2 x) (1 + x + x^3)^(1/3))/(x^2 (1 + x)), x]

Out[1257]= -((3 (1 + x + x^3)^(1/3))/x) + Sqrt[3] ArcTan[(x + 2 (1 + x + x^3)^(1/3))/(Sqrt[3] x)] - 
 Log[-x + (1 + x + x^3)^(1/3)] + 1/2 Log[x^2 + x (1 + x + x^3)^(1/3) + (1 + x + x^3)^(2/3)]
```

```
In[1256]:= IntegrateAlgebraic[(1 - x^2)^2/((x^2 + 1) (x^4 + 6 x^2 + 1)^(3/4)), x]

Out[1256]= -ArcTan[(-1 + x)/(1 + 6 x^2 + x^4)^(1/4)] - ArcTan[(1 + x)/(1 + 6 x^2 + x^4)^(1/4)] + 
 ArcTanh[(-1 + x)/(1 + 6 x^2 + x^4)^(1/4)] + ArcTanh[(1 + x)/(1 + 6 x^2 + x^4)^(1/4)]
```

```
In[1254]:= IntegrateAlgebraic[((x^2 - 1) Sqrt[x^4 + x^2 + 1])/((x^2 + 1) (x^4 + x^3 + x^2 + x + 1)), x]
AlgebraicIntegrateHeuristic`Private`RationalSubstitution

Out[1254]= -2 ArcTan[Sqrt[1 + x^2 + x^4]/(1 - x + x^2)] + 
 Sqrt[2/5 (1 + Sqrt[5])] ArcTan[Sqrt[1 + x^2 + x^4]/(Sqrt[2 + Sqrt[5]] (1 - x + x^2))] + 
 Sqrt[2/5 (-1 + Sqrt[5])] ArcTanh[(Sqrt[-2 + Sqrt[5]] Sqrt[1 + x^2 + x^4])/(1 + x + x^2)]

Out[1255]= (-1 - x^2)/x
```

Below we see that the symbol ```AlgebraicIntegrateHeuristic`Private`RationalSubstitution``` stores the substitution used in computing the integral, incase the interested reader would like to see the substitution that simplified the integral.

```
In[1252]:= IntegrateAlgebraic[((1 - x^3)^(2/3) (-1 + 4 x^3))/(x^6 (-2 + 3 x^3)), x]
AlgebraicIntegrateHeuristic`Private`RationalSubstitution

Out[1252]= ((1 - x^3)^(2/3) (-4 + 29 x^3))/(40 x^5) - (5 ArcTan[(Sqrt[3] x)/(x + 2 2^(1/3) (1 - x^3)^(1/3))])/(4 2^(2/3) Sqrt[3]) + (5 Log[-x + 2^(1/3) (1 - x^3)^(1/3)])/(12 2^(2/3)) - (5 Log[x^2 + 2^(1/3) x (1 - x^3)^(1/3) + 2^(2/3) (1 - x^3)^(2/3)])/(24 2^(2/3))

Out[1253]= (1 + x^3)/x^3
```

```
In[1250]:= IntegrateAlgebraic[((-1 + x^3 - x^5 - 2 x^7)^(2/3) (1 - x^3 + x^5 + 2 x^7) (-3 + 2 x^5 + 8 x^7))/x^9, x]
AlgebraicIntegrateHeuristic`Private`RationalSubstitution

Out[1250]= (3 (-1 + x^3 - x^5 - 2 x^7)^(8/3))/(8 x^8)

Out[1251]= (-1 - x^5 - 2 x^7)/x^3
```

```
In[1246]:= IntegrateAlgebraic[((2 + x - x^3 - x^4)^(2/3) (6 + 2 x + x^4) (-2 - x + x^3 + x^4))/(x^6 (-2 - x + 2 x^3 + x^4)), x]
AlgebraicIntegrateHeuristic`Private`RationalSubstitution


Out[1246]= -(((12 + 6 x + 9 x^3 - 6 x^4) (2 + x - x^3 - x^4)^(2/3))/(10 x^5)) - Sqrt[3] ArcTan[(x + 2 (2 + x - x^3 - x^4)^(1/3))/(Sqrt[3] x)] - 
 Log[-x + (2 + x - x^3 - x^4)^(1/3)] + 1/2 Log[x^2 + x (2 + x - x^3 - x^4)^(1/3) + (2 + x - x^3 - x^4)^(2/3)]

Out[1247]= (2 + x - x^4)/x^3
```

We now have some additional specific methods for integrals of the form ```Integrate[a[x]/(b[x] Sqrt[r[x]])] == c Log[p[x] + q[x] Sqrt[r[x]]]```. For example, the following integral was posted on sci.math.symbolic by Henri Cohen in 1993 and was solved by Manuel Bronstein using AXIOM: 

```
In[1244]:= IntegrateAlgebraic[x/Sqrt[-71 - 96 x + 10 x^2 + x^4], x]

Out[1244]= -(1/8) Log[-10001 - 3124 x^2 + 1408 x^3 - 54 x^4 + 128 x^5 - 20 x^6 - x^8 + Sqrt[-71 - 96 x + 10 x^2 + x^4] (781 - 528 x + 27 x^2 - 80 x^3 + 15 x^4 + x^6)]
```

Here are a number of similar integrals I came up with

```
In[1243]:= IntegrateAlgebraic[x/Sqrt[1 + 4 x + 3 x^2 - 2 x^3 + x^4], x]

Out[1243]= 1/3 ArcTanh[x^3/((1 + x) Sqrt[1 + 4 x + 3 x^2 - 2 x^3 + x^4])] + ArcTanh[x^2/(-1 - 2 x + Sqrt[1 + 4 x + 3 x^2 - 2 x^3 + x^4])]
```

```
In[1242]:= IntegrateAlgebraic[(1 + 3 x)/Sqrt[-1 - 4 x - 5 x^2 - 2 x^3 + x^4], x]

Out[1242]= -(1/2) Log[11 - 4 x - 24 x^2 - 16 x^3 + 38 x^4 - 16 x^5 + 2 x^6 + (2 + 18 x - 30 x^2 + 14 x^3 - 2 x^4) Sqrt[-1 - 4 x - 5 x^2 - 2 x^3 + x^4]]
```

```
In[1241]:= IntegrateAlgebraic[(x^2 - x)/Sqrt[-2 x + 4 x^2 - 2 x^3 + x^4 - 2 x^5 + x^6], x]

Out[1241]= 2/3 ArcTanh[((-1 + x) x^2)/Sqrt[-2 x + 4 x^2 - 2 x^3 + x^4 - 2 x^5 + x^6]]
```

```
In[1240]:= IntegrateAlgebraic[(-1 - 2 x + 3 x^2)/Sqrt[-3 - 2 x - x^2 + 4 x^3 - x^4 - 2 x^5 + x^6], x]

Out[1240]= 2 ArcTanh[Sqrt[-3 - 2 x - x^2 + 4 x^3 - x^4 - 2 x^5 + x^6]/(-1 - x - x^2 + x^3)]
```

We can now solve some integrals which require a linear rational substitution to simplify the radicand prior to the usual call to solveAlgebraicIntegral. For example 

```
In[1239]:= IntegrateAlgebraic[1/((x + 1) (x^4 + 6 x^2 + 1)^(1/4)), x]

Out[1239]= ArcTan[(-1 + x)/(2^(1/4) (1 + 6 x^2 + x^4)^(1/4))]/(2 2^(3/4)) + ArcTanh[(-1 + x)/(2^(1/4) (1 + 6 x^2 + x^4)^(1/4))]/(2 2^(3/4))
```

```
In[1238]:= IntegrateAlgebraic[Sqrt[x^4 + 6 x^2 + 1]/((x - 1) (x + 1)^3), x]

Out[1238]= Sqrt[1 + 6 x^2 + x^4]/(4 (1 + x)^2) - ArcTanh[(2 Sqrt[2] x)/(1 - 2 x + x^2 + Sqrt[1 + 6 x^2 + x^4])]/(2 Sqrt[2])
```

```
In[1237]:= IntegrateAlgebraic[Sqrt[x^4 + 6 x^2 + 1]/(x (x^2 + 1)), x]

Out[1237]= 2 ArcTan[1/2 (-1 - x^2 + Sqrt[1 + 6 x^2 + x^4])] - ArcTanh[2 + x^2 - Sqrt[1 + 6 x^2 + x^4]] - 1/2 Log[1 - x^2 + Sqrt[1 + 6 x^2 + x^4]]
```

We can now solve some integrals containing nested radicals. For example 

```
In[780]:= IntegrateAlgebraic[((1 + x^2) Sqrt[x^2 + Sqrt[1 + x^4]])/((-1 + x^2) Sqrt[1 + x^4]), x]

Out[780]= 
Sqrt[2 (-1 + Sqrt[2])]ArcTan[(Sqrt[2/(1 + Sqrt[2])] x Sqrt[x^2 + Sqrt[1 + x^4]])/(1 + x^2 + Sqrt[1 + x^4])] + 
 Sqrt[2] ArcTanh[(Sqrt[2] x Sqrt[x^2 + Sqrt[1 + x^4]])/(1 + x^2 + Sqrt[1 + x^4])] - 
 Sqrt[2 (1 + Sqrt[2])] ArcTanh[(Sqrt[2/(-1 + Sqrt[2])] x Sqrt[x^2 + Sqrt[1 + x^4]])/(1 + x^2 + Sqrt[1 + x^4])]
```

