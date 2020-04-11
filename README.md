# A simple method for computing some pseudo-elliptic integrals

This is a prototype method for solving _some_ pseudo-elliptic integrals. Relative to the algebraic case of the Risch-Trager-Bronstein algorithm, its very simple, however it is not a decision process. 

This method should be easy enough to port to other CAS. We use **SolveAlways** to equate coefficients of powers of _x_ and compute the undeterminted coefficients in a system of equations, but this could easily be hand-coded and reduced to a system of linear equations.

The package AlgebraicIntegrateHeuristic.m exposes one function: **solveAlgebraicIntegral[**_integrand_, _variable_**]** and returns a list containing {_the rational part of the integrand (unintegrated)_, _the unintegrated part_, _the integrated part_}. 

This method can solve some interesting pseudo-elliptic integrals. Below are some examples, many more are in the source code. 

```
In[290]:= solveAlgebraicIntegral[(1 + x^2)/((1 - x^2) Sqrt[1 + x^4]), x]

Out[290]= {0, 0, ArcTanh[Sqrt[1 + x^4]/(Sqrt[2] x)]/Sqrt[2]}
```

```
In[291]:= solveAlgebraicIntegral[(1 + x^6)/((1 - x^6) Sqrt[1 - x^2 + x^4]), x]

Out[291]= {0, 0, -(1/3) Sqrt[2] ArcTan[Sqrt[1 - x^2 + x^4]/(Sqrt[2] x)] - 1/6 Log[x - Sqrt[1 - x^2 + x^4]] + 1/6 Log[x + Sqrt[1 - x^2 + x^4]]}
```

```
In[1974]:= solveAlgebraicIntegral[1/(x^4 + 1)^(1/4), x]

Out[1974]= {0, 0, -(1/2) ArcTan[(1 + x^4)^(1/4)/x] - 1/4 Log[x - (1 + x^4)^(1/4)] + 1/4 Log[x + (1 + x^4)^(1/4)]}
```

```
In[1990]:= solveAlgebraicIntegral[((-1 + x^4 - x^5)^(1/4) (-4 + x^5))/x^6, x]

Out[1990]= {0, 0, ((-1 + x^4 - x^5)^(1/4) (4/5 - (4 x^4)/5 + (4 x^5)/5))/x^5}
```

```
In[1991]:= solveAlgebraicIntegral[((3 + 2 x) (1 + x + x^3)^(1/3))/(x^2 (1 + x)), x]

Out[1991]= {0, 0, -((3 (1 + x + x^3)^(1/3))/x) + Sqrt[3] ArcTan[(x/Sqrt[3] + (2 (1 + x + x^3)^(1/3))/Sqrt[3])/x] + Log[x] - Log[x^2]/2 - Log[x - (1 + x + x^3)^(1/3)] + 1/2 Log[x^2 + x (1 + x + x^3)^(1/3) + (1 + x + x^3)^(2/3)]}
```

```
In[293]:= int[((x^2 - 1) Sqrt[x^4 + x^2 + 1])/((x^2 + 1) (x^4 + x^3 + x^2 + x + 1)), x]

Out[293]= {0, 0, ArcTan[Sqrt[1 + x^2 + x^4]/x] + Sqrt[2/(5 (-1 + Sqrt[5]))] ArcTan[(Sqrt[-2 + 2 Sqrt[5]] Sqrt[1 + x^2 + x^4])/(-1 + Sqrt[5] - 2 x - x^2 + Sqrt[5] x^2)] + 1/5 Sqrt[(15 - 5 Sqrt[5])/(-1 + Sqrt[5])] Log[2 + x + Sqrt[5] x + 2 x^2] - 1/5 Sqrt[(15 - 5 Sqrt[5])/(-1 + Sqrt[5])] Log[1 + Sqrt[5] + 2 x + x^2 + Sqrt[5] x^2 - Sqrt[2 + 2 Sqrt[5]] Sqrt[1 + x^2 + x^4]]}
```

Below we see that the variable ```AlgebraicIntegrateHeuristic`Private`RationalSubstitution``` stores the substitution used in computing the integral, incase the interested reader would like to see the substitution that simplified the integral.

```
In[294]:= integrand = ((-1 + x^4) (1 + x^2 + x^4) Sqrt[1 - x^2 + x^4])/(1 + x^4)^3;
solveAlgebraicIntegral[integrand, x]
D[% // Last, x] - integrand // Simplify
AlgebraicIntegrateHeuristic`Private`RationalSubstitution

Out[295]= {0, 0, (Sqrt[1 - x^2 + x^4] (-((3 x)/8) - x^3/4 - (3 x^5)/8))/(1 + x^4)^2 + 5/8 ArcTan[Sqrt[1 - x^2 + x^4]/x]}

Out[296]= 0

Out[297]= (-1 - x^2)/x
```
