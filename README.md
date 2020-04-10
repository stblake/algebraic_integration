# A simple method for computing some pseudo-elliptic integrals. 

This is a prototype method for solving _some_ pseudo-elliptic integrals. Relative to the algebraic case of the Risch-Trager-Bronstein algorithm, its very simple, however it is not a decision process. 

Here's an example integral. The result returned is a list containing {/the rational part of the integrand (unintegrated)/, /the unintegrated part/, /the integrated part/}. 

In[5026]:= solveAlgebraicIntegral[((-1 + x^4) (1 + x^2 + x^4) Sqrt[1 - x^2 + x^4])/(1 + x^4)^3, x]
AlgebraicIntegrateHeuristic`Private`RationalSubstitution
D[%% // Last, x] - ((-1 + x^4) (1 + x^2 + x^4) Sqrt[1 - x^2 + x^4])/(1 + x^4)^3 // Simplify

Out[5026]= {0, 0, (Sqrt[1 - x^2 + x^4] (-((3 x)/8) - x^3/4 - (3 x^5)/8))/(1 + x^4)^2 + 5/8 ArcTan[Sqrt[1 - x^2 + x^4]/x]}

Out[5027]= (-1 - x^2)/x

Out[5028]= 0
