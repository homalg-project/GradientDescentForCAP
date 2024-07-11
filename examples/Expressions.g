#! @Chapter Examples and Tests

#! @Section Expressions

LoadPackage( "MachineLearning" );

#! @Example
vars := [ "x", "y", "z" ];
#! [ "x", "y", "z" ]
e1 := Expression( vars, "x + Sin( y ) * Log( z )" );
#! x + Sin( y ) * Log( z )
e2 := Expression( vars, "( x * y + Sin( z ) ) ^ 2" );
#! (x * y + Sin( z )) ^ 2
CategoriesOfObject( e1 );
#! [ "IsExtAElement", "IsNearAdditiveElement", "IsNearAdditiveElementWithZero",
#!   "IsNearAdditiveElementWithInverse", "IsAdditiveElement", "IsExtLElement",
#!   "IsExtRElement", "IsMultiplicativeElement", "IsExpression" ]
KnownAttributesOfObject( e1 );
#! [ "String", "Variables" ]
String( e1 );
#! "x + Sin( y ) * Log( z )"
Variables( e1 );
#! [ "x", "y", "z" ]
e1 + e2;
#! x + Sin( y ) * Log( z ) + (x * y + Sin( z )) ^ 2
e1 * e2;
#! (x + Sin( y ) * Log( z )) * (x * y + Sin( z )) ^ 2
e := Sin( e1 ) / e2;
#! Sin( (x + Sin( y ) * Log( z )) ) / (x * y + Sin( z )) ^ 2
f := AsFunction( e );
#! function( vec ) ... end
Display( f );
#! function ( vec )
#!     local x, y, z;
#!     Assert( 0, IsDenseList( vec ) and Length( vec ) = 3 );
#!     x := vec[1];
#!     y := vec[2];
#!     z := vec[3];
#!     return Sin( (x + Sin( y ) * Log( z )) ) / (x * y + Sin( z )) ^ 2;
#! end
x := [ 3, 2, 4 ];
#! [ 3, 2, 4 ]
f( x );
#! -0.032725
dummy_input := ConvertToExpressions( [ "x1", "x2", "x3" ] );
#! [ x1, x2, x3 ]
f( dummy_input );
#! Sin( (x1 + Sin( x2 ) * Log( x3 )) ) / (x1 * x2 + Sin( x3 )) ^ 2
AssignExpressions( dummy_input );
#! #I  MakeReadWriteGlobal: x1 already read-write
#! #I  MakeReadWriteGlobal: x2 already read-write
#! #I  MakeReadWriteGlobal: x3 already read-write
x1;
#! x1
Variables( x1 );
#! [ "x1", "x2", "x3" ]
[ [ x1, x2 ] ] * [ [ x3 ], [ -x3 ] ];
#! [ [ x1 * x3 + x2 * (- x3) ] ]
e := Sin( x1 ) / Cos( x1 ) + Sin( x2 ) ^ 2 + Cos( x2 ) ^ 2;
#! Sin( x1 ) / Cos( x1 ) + Sin( x2 ) ^ 2 + Cos( x2 ) ^ 2
SimplifyExpressionUsingPython( [ e ] );
#! [ "Tan(x1) + 1" ]
Diff( e, 1 )( dummy_input );
#! Sin( x1 ) ^ 2 / Cos( x1 ) ^ 2 + 1
LazyDiff( e, 1 )( dummy_input );;
# Diff( [ "x1", "x2", "x3" ],
#  "(((Sin(x1))/(Cos(x1)))+((Sin(x2))^(2)))+((Cos(x2))^(2))", 1 )( [ x1, x2, x3 ] );
JacobianMatrixUsingPython( [ x1*Cos(x2)+Exp(x3), x1*x2*x3 ], [ 1, 2, 3 ] );
#! [ [ "Cos(x2)", "-x1*Sin(x2)", "Exp(x3)" ], [ "x2*x3", "x1*x3", "x1*x2" ] ]
LaTeXOutputUsingPython( e );
#! "\\frac{\\sin{\\left(x_{1} \\right)}}{\\cos{\\left(x_{1} \\right)}}
#! + \\sin^{2}{\\left(x_{2} \\right)} + \\cos^{2}{\\left(x_{2} \\right)}"
sigmoid := Expression( [ "x" ], "Exp(x)/(1+Exp(x))" );
#! Exp( x ) / (1 + Exp( x ))
sigmoid := AsFunction( sigmoid );
#! function( vec ) ... end
sigmoid( [ 0 ] );
#! 0.5
points := List( 0.1 * [ -20 .. 20 ], x -> [ x, sigmoid( [ x ] ) ] );
#! [ [ -2., 0.119203 ], [ -1.9, 0.130108 ], [ -1.8, 0.141851 ], [ -1.7, 0.154465 ],
#!   [ -1.6, 0.167982 ], [ -1.5, 0.182426 ], [ -1.4, 0.197816 ], [ -1.3, 0.214165 ],
#!   [ -1.2, 0.231475 ], [ -1.1, 0.24974 ], [ -1., 0.268941 ], [ -0.9, 0.28905 ],
#!   [ -0.8, 0.310026 ], [ -0.7, 0.331812 ],  [ -0.6, 0.354344 ], [ -0.5, 0.377541 ],
#!   [ -0.4, 0.401312 ], [ -0.3, 0.425557 ], [ -0.2, 0.450166 ], [ -0.1, 0.475021 ],
#!   [ 0., 0.5 ], [ 0.1, 0.524979 ], [ 0.2, 0.549834 ], [ 0.3, 0.574443 ],
#!   [ 0.4, 0.598688 ], [ 0.5, 0.622459 ], [ 0.6, 0.645656 ], [ 0.7, 0.668188 ],
#!   [ 0.8, 0.689974 ], [ 0.9, 0.71095 ], [ 1., 0.731059 ], [ 1.1, 0.75026 ],
#!   [ 1.2, 0.768525 ], [ 1.3, 0.785835 ], [ 1.4, 0.802184 ],  [ 1.5, 0.817574 ],
#!   [ 1.6, 0.832018 ], [ 1.7, 0.845535 ], [ 1.8, 0.858149 ], [ 1.9, 0.869892 ],
#!   [ 2., 0.880797 ] ]
labels := List( points, point -> SelectBasedOnCondition( point[2] < 0.5, 0, 1 ) );
#! [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#!   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
ScatterPlotUsingPython( points, labels : size := "100", action := "save" );;
# e.g, dir("/tmp/gaptempdirX7Qsal/")
#! @EndExample
