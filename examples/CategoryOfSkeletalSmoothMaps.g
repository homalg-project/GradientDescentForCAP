#! @Chapter Examples and Tests

#! @Section Category of Smooth Maps

LoadPackage( "MachineLearningForCAP" );

#! @Example
Smooth := CategoryOfSkeletalSmoothMaps( );
#! SkeletalSmoothMaps
R2 := ObjectConstructor( Smooth, 2 );
#! ℝ^2
R2 = Smooth.2;
#! true
r := RandomMorphism( R2, R2, 3 );
#! ℝ^2 -> ℝ^2
Display( r );
#! ℝ^2 -> ℝ^2
#!
#! ‣ 0.766 * x1 ^ 1 + 0.234 * x2 ^ 2 + 1.
#! ‣ 0.388 * x1 ^ 1 + 0.459 * x2 ^ 2 + 0.278
f := MorphismConstructor( Smooth,
        Smooth.2,
        Pair(
          x -> [ x[1] ^ 2 + Sin( x[2] ), Exp( x[1] ) + 3 * x[2] ],
          x -> [ [ 2 * x[1], Cos( x[2] ) ], [ Exp( x[1] ), 3 ] ] ),
        Smooth.2 );
#! ℝ^2 -> ℝ^2
Display( f );
#! ℝ^2 -> ℝ^2
#!
#! ‣ x1 ^ 2 + Sin( x2 )
#! ‣ Exp( x1 ) + 3 * x2
dummy_input := ConvertToExpressions( [ "x1", "x2" ] );
#! [ x1, x2 ]
Map( f )( dummy_input );
#! [ x1 ^ 2 + Sin( x2 ), Exp( x1 ) + 3 * x2 ]
JacobianMatrix( f )( dummy_input );
#! [ [ 2 * x1, Cos( x2 ) ], [ Exp( x1 ), 3 ] ]
x := [ 0.2, 0.3 ];
#! [ 0.2, 0.3 ]
Map( f )( x );
#! [ 0.33552, 2.1214 ]
JacobianMatrix( f )( x );
#! [ [ 0.4, 0.955336 ], [ 1.2214, 3 ] ]
g := MorphismConstructor( Smooth,
        Smooth.2,
        Pair(
          x -> [ 3 * x[1], Exp( x[2] ), x[1] ^ 3 + Log( x[2] ) ],
          x -> [ [ 3, 0 ],
                 [ 0, Exp( x[2] ) ],
                 [ 3 * x[1] ^ 2, 1 / x[2] ] ] ),
        Smooth.3 );
#! ℝ^2 -> ℝ^3
Display( g );
#! ℝ^2 -> ℝ^3
#!
#! ‣ 3 * x1
#! ‣ Exp( x2 )
#! ‣ x1 ^ 3 + Log( x2 )
Map( g )( dummy_input );
#! [ 3 * x1, Exp( x2 ), x1 ^ 3 + Log( x2 ) ]
JacobianMatrix( g )( dummy_input );
#! [ [ 3, 0 ], [ 0, Exp( x2 ) ], [ 3 * x1 ^ 2, 1 / x2 ] ]
h := PostCompose( g, f );
#! ℝ^2 -> ℝ^3
Display( h );
#! ℝ^2 -> ℝ^3
#!
#! ‣ 3 * (x1 ^ 2 + Sin( x2 ))
#! ‣ Exp( Exp( x1 ) + 3 * x2 )
#! ‣ (x1 ^ 2 + Sin( x2 )) ^ 3 + Log( (Exp( x1 ) + 3 * x2) )
x;
#! [ 0.2, 0.3 ]
Map( h )( x );
#! [ 1.00656, 8.34283, 0.789848 ]
Eval( h, x );
#! [ 1.00656, 8.34283, 0.789848 ]
Map( g )( Map( f )( x ) );
#! [ 1.00656, 8.34283, 0.789848 ]
JacobianMatrix( h )( x );
#! [ [ 1.2, 2.86601 ], [ 10.19, 25.0285 ], [ 0.710841, 1.7368 ] ]
EvalJacobianMatrix( h, x );
#! [ [ 1.2, 2.86601 ], [ 10.19, 25.0285 ], [ 0.710841, 1.7368 ] ]
JacobianMatrix( g )( Map( f )( x ) ) * JacobianMatrix( f )( x );
#! [ [ 1.2, 2.86601 ], [ 10.19, 25.0285 ], [ 0.710841, 1.7368 ] ]
IsCongruentForMorphisms( Smooth, f + f, 2 * f );
#! true
IsCongruentForMorphisms( Smooth, 2 * f - f, f );
#! true
s := SimplifyMorphism( h, infinity );
#! ℝ^2 -> ℝ^3
Display( s );
#! ℝ^2 -> ℝ^3
#!
#! ‣ 3 * x1 ^ 2 + 3 * Sin( x2 )
#! ‣ Exp( 3 * x2 + Exp( x1 ) )
#! ‣ (x1 ^ 2 + Sin( x2 )) ^ 3 + Log( (3 * x2 + Exp( x1 )) )
R2 := Smooth.( 2 );
#! ℝ^2
R3 := Smooth.( 3 );
#! ℝ^3
DirectProduct( R2, R3 );
#! ℝ^5
p1 := ProjectionInFactorOfDirectProduct( [ R2, R3 ], 1 );
#! ℝ^5 -> ℝ^2
Display( p1 );
#! ℝ^5 -> ℝ^2
#!
#! ‣ x1
#! ‣ x2
p2 := ProjectionInFactorOfDirectProduct( [ R2, R3 ], 2 );
#! ℝ^5 -> ℝ^3
Display( p2 );
#! ℝ^5 -> ℝ^3
#!
#! ‣ x3
#! ‣ x4
#! ‣ x5
u := UniversalMorphismIntoDirectProduct( Smooth, [ f, g ] );
#! ℝ^2 -> ℝ^5
Display( u );
#! ℝ^2 -> ℝ^5
#!
#! ‣ x1 ^ 2 + Sin( x2 )
#! ‣ Exp( x1 ) + 3 * x2
#! ‣ 3 * x1
#! ‣ Exp( x2 )
#! ‣ x1 ^ 3 + Log( x2 )
PreCompose( Smooth, u, p1 ) = f;
#! true
PreCompose( Smooth, u, p2 ) = g;
#! true
d := DirectProductFunctorial( Smooth, [ f, g ] );
#! ℝ^4 -> ℝ^5
Display( d );
#! ℝ^4 -> ℝ^5
#!
#! ‣ x1 ^ 2 + Sin( x2 )
#! ‣ Exp( x1 ) + 3 * x2
#! ‣ 3 * x3
#! ‣ Exp( x4 )
#! ‣ x3 ^ 3 + Log( x4 )
Rf := ReverseDifferential( f );
#! ℝ^4 -> ℝ^2
Display( Rf );
#! ℝ^4 -> ℝ^2
#!
#! ‣ x3 * (2 * x1) + x4 * Exp( x1 )
#! ‣ x3 * Cos( x2 ) + x4 * 3
Display( Smooth.Sqrt );
#! ℝ^1 -> ℝ^1
#!
#! ‣ Sqrt( x1 )
Display( Smooth.Exp );
#! ℝ^1 -> ℝ^1
#!
#! ‣ Exp( x1 )
Display( Smooth.Log );
#! ℝ^1 -> ℝ^1
#!
#! ‣ Log( x1 )
Display( Smooth.Sin );
#! ℝ^1 -> ℝ^1
#!
#! ‣ Sin( x1 )
Display( Smooth.Cos );
#! ℝ^1 -> ℝ^1
#!
#! ‣ Cos( x1 )
Display( Smooth.Constant( [ 1.2, 3.4 ] ) );
#! ℝ^0 -> ℝ^2
#!
#! ‣ 1.2
#! ‣ 3.4
Display( Smooth.Zero( 2, 3 ) );
#! ℝ^2 -> ℝ^3
#!
#! ‣ 0
#! ‣ 0
#! ‣ 0
Display( Smooth.IdFunc( 2 ) );
#! ℝ^2 -> ℝ^2
#!
#! ‣ x1
#! ‣ x2
Display( Smooth.Relu( 2 ) );
#! ℝ^2 -> ℝ^2
#!
#! ‣ Relu( x1 )
#! ‣ Relu( x2 )
Display( Smooth.Mul( 3 ) );
#! ℝ^3 -> ℝ^1
#!
#! ‣ x1 * x2 * x3
Display( Smooth.Sum( 3 ) );
#! ℝ^3 -> ℝ^1
#!
#! ‣ x1 + x2 + x3
Display( Smooth.Mean( 3 ) );
#! ℝ^3 -> ℝ^1
#!
#! ‣ (x1 + x2 + x3) / 3
Display( Smooth.Variance( 3 ) );
#! ℝ^3 -> ℝ^1
#!
#! ‣ ((x1 - (x1 + x2 + x3) / 3) ^ 2 + (x2 - (x1 + x2 + x3) / 3) ^ 2
#!    + (x3 - (x1 + x2 + x3) / 3) ^ 2) / 3
Display( Smooth.StandardDeviation( 3 ) );
#! ℝ^3 -> ℝ^1
#!
#! ‣ Sqrt( ((x1 - (x1 + x2 + x3) / 3) ^ 2 + (x2 - (x1 + x2 + x3) / 3) ^ 2
#!   + (x3 - (x1 + x2 + x3) / 3) ^ 2) / 3 )
Display( Smooth.Power( 6 ) );
#! ℝ^1 -> ℝ^1
#!
#! ‣ x1 ^ 6
Display( Smooth.PowerBase( 6 ) );
#! ℝ^1 -> ℝ^1
#!
#! ‣ 6 ^ x1
Display( Smooth.Sigmoid( 2 ) );
#! ℝ^2 -> ℝ^2
#!
#! ‣ 1 / (1 + Exp( (- x1) ))
#! ‣ 1 / (1 + Exp( (- x2) ))
Display( Smooth.Softmax( 2 ) );
#! ℝ^2 -> ℝ^2
#!
#! ‣ Exp( x1 ) / (Exp( x1 ) + Exp( x2 ))
#! ‣ Exp( x2 ) / (Exp( x1 ) + Exp( x2 ))
Display( Smooth.QuadraticLoss( 2 ) );
#! ℝ^4 -> ℝ^1
#!
#! ‣ ((x1 - x3) ^ 2 + (x2 - x4) ^ 2) / 2
Display( Smooth.BinaryCrossEntropyLoss( 1 ) );
#! ℝ^2 -> ℝ^1
#!
#! ‣ - (x2 * Log( x1 ) + (1 - x2) * Log( (1 - x1) ))
Display( Smooth.SigmoidBinaryCrossEntropyLoss( 1 ) );
#! ℝ^2 -> ℝ^1
#!
#! ‣ Log( 1 + Exp( (- x1) ) ) + (1 - x2) * x1
Display( Smooth.CrossEntropyLoss( 3 ) );
#! ℝ^6 -> ℝ^1
#!
#! ‣ (- (Log( x1 ) * x4 + Log( x2 ) * x5 + Log( x3 ) * x6)) / 3
Display( Smooth.SoftmaxCrossEntropyLoss( 3 ) );
#! ℝ^6 -> ℝ^1
#!
#! ‣ ((Log( Exp( x1 ) + Exp( x2 ) + Exp( x3 ) ) - x1) * x4
#!   + (Log( Exp( x1 ) + Exp( x2 ) + Exp( x3 ) ) - x2) * x5
#!   + (Log( Exp( x1 ) + Exp( x2 ) + Exp( x3 ) ) - x3) * x6) / 3
Display( Smooth.AffineTransformation( 2, 3 ) );
#! ℝ^11 -> ℝ^3
#!
#! ‣ x1 * x10 + x2 * x11 + x3
#! ‣ x4 * x10 + x5 * x11 + x6
#! ‣ x7 * x10 + x8 * x11 + x9
Display( Smooth.PolynomialTransformation( 2, 3, 2 ) );
#! ℝ^20 -> ℝ^3
#!
#! ‣ x1 * x19 ^ 2 + x2 * (x19 ^ 1 * x20 ^ 1) + x3 * x19 ^ 1
#!   + x4 * x20 ^ 2 + x5 * x20 ^ 1 + x6 * 1
#! ‣ x7 * x19 ^ 2 + x8 * (x19 ^ 1 * x20 ^ 1) + x9 * x19 ^ 1
#!   + x10 * x20 ^ 2 + x11 * x20 ^ 1 + x12 * 1
#! ‣ x13 * x19 ^ 2 + x14 * (x19 ^ 1 * x20 ^ 1) + x15 * x19 ^ 1
#!   + x16 * x20 ^ 2 + x17 * x20 ^ 1 + x18 * 1
#! @EndExample
