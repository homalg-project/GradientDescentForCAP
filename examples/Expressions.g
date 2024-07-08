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
#! @EndExample
