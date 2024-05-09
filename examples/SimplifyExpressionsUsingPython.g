#! @Chapter Examples and Tests

#! @Section Simplify Expressions using Python

LoadPackage( "MachineLearningForCAP" );

#! @Example
SimplifyExpressionUsingPython(
  [ "Sin(x)^2+Cos(x)^2", "(x+y)^2+(x-y)^2" ], [ "x", "y" ] );
#! [ "1", "2*x^2 + 2*y^2" ]
JacobianMatrixUsingPython( [ "Sin(x)+y^3", "x*y" ], [ "x", "y" ], [ 1 ] );
#! [ [ "Cos(x)" ], [ "y" ] ]
LaTeXOutputUsingPython( [ "Cos(x+y)^2+(x-y)^2" ], [ "x", "y" ] );
#! [ "\\left(x - y\\right)^{2} + \\cos^{2}{\\left(x + y \\right)}" ]
Smooth := CategoryOfSmoothMaps( );
#! Smooth
f := PostCompose( Smooth.Exp, Smooth.Log );;
Display( f );
#! ℝ^1 -> ℝ^1
#!
#! Exp(Log(x1))
Display( SimplifyMorphism( f, infinity ) );
#! ℝ^1 -> ℝ^1
#!
#! x1
#! @EndExample
