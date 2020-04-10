# A simple method for computing some pseudo-elliptic integrals

This is a prototype method for solving _some_ pseudo-elliptic integrals. Relative to the algebraic case of the Risch-Trager-Bronstein algorithm, its very simple, however it is not a decision process. 

The package AlgebraicIntegrateHeuristic.m exposes one function: **solveAlgebraicIntegral[**_integrand_, _variable_**]** and returns a list containing {_the rational part of the integrand (unintegrated)_, _the unintegrated part_, _the integrated part_}. 

```
In[294]:= integrand = ((-1 + x^4) (1 + x^2 + x^4) Sqrt[1 - x^2 + x^4])/(1 + x^4)^3;
solveAlgebraicIntegral[integrand, x]
D[% // Last, x] - integrand // Simplify
AlgebraicIntegrateHeuristic`Private`RationalSubstitution

Out[295]= {0, 0, (Sqrt[1 - x^2 + x^4] (-((3 x)/8) - x^3/4 - (3 x^5)/8))/(1 + x^4)^2 + 5/8 ArcTan[Sqrt[1 - x^2 + x^4]/x]}

Out[296]= 0

Out[297]= (-1 - x^2)/x
```
