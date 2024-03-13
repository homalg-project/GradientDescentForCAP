#! @Chapter Examples and Tests

#! Section Category of Smooth Maps

LoadPackage( "MachineLearningForCAP" );

#! @Example
Smooth := SmoothCategory( );
#! Smooth
R2 := ObjectConstructor( Smooth, 2 );
#! ℝ^2
R2 = Smooth.2;
#! true
f := MorphismConstructor( Smooth,
        Smooth.2,
        Pair(
          [ x -> x[1] ^ 2 + Sin( x[2] ), x -> Exp( x[1] ) + 3 * x[2] ],
          [ [ x -> 2 * x[1], x -> Cos( x[2] ) ], [ x -> Exp( x[1] ), x -> 3 ] ]
        ),
        Smooth.2 );
#! ℝ^2 -> ℝ^2
Display( f );
#! ℝ^2 -> ℝ^2
#!
#! ((x1)^(2))+(Sin(x2))
#! (Exp(x1))+((3)*(x2))
dummy_input := AsListOfExpressions( [ "x1", "x2" ] );
#! [ x1, x2 ]
Eval( f, dummy_input );
#! [ ((x1)^(2))+(Sin(x2)), (Exp(x1))+((3)*(x2)) ]
EvalJacobianMatrix( f, dummy_input );
#! [ [ (2)*(x1), Cos(x2) ], [ Exp(x1), 3 ] ]
x := [ 0.2, 0.3 ];
#! [ 0.2, 0.3 ]
Eval( f, x );
#! [ 0.33552, 2.1214 ]
EvalJacobianMatrix( f, x );
#! [ [ 0.4, 0.955336 ], [ 1.2214, 3 ] ]
g := MorphismConstructor( Smooth,
        Smooth.2,
        Pair(
          [ x -> 3*x[1], x -> Exp( x[2] ), x -> x[1] ^ 3 + Log( x[2] ) ],
          [ [ x -> 3., x -> 0. ],
            [ x -> 0., x -> Exp( x[2] ) ],
            [ x -> 3 * x[1] ^ 2, x -> 1 / x[2] ] ]
        ),
        Smooth.3 );
#! ℝ^2 -> ℝ^3
Display( g );
#! ℝ^2 -> ℝ^3
#!
#! (3)*(x1)
#! Exp(x2)
#! ((x1)^(3))+(Log(x2))
Eval( g, dummy_input );
#! [ (3)*(x1), Exp(x2), ((x1)^(3))+(Log(x2)) ]
EvalJacobianMatrix( g, dummy_input );
#! [ [ 3., 0. ], [ 0., Exp(x2) ], [ (3)*((x1)^(2)), (1)/(x2) ] ]
g_after_f := PostCompose( g, f );
#! ℝ^2 -> ℝ^3
Display( g_after_f );
#! ℝ^2 -> ℝ^3
#!
#! (3)*(((x1)^(2))+(Sin(x2)))
#! Exp((Exp(x1))+((3)*(x2)))
#! ((((x1)^(2))+(Sin(x2)))^(3))+(Log((Exp(x1))+((3)*(x2))))
Eval( g_after_f, x );
#! [ 1.00656, 8.34283, 0.789848 ]
Eval( g, Eval( f, x ) );
#! [ 1.00656, 8.34283, 0.789848 ]
EvalJacobianMatrix( g_after_f, x );
#! [ [ 1.2, 2.86601 ], [ 10.19, 25.0285 ], [ 0.710841, 1.7368 ] ]
EvalJacobianMatrix( g, Eval( f, x ) ) * EvalJacobianMatrix( f, x );
#! [ [ 1.2, 2.86601 ], [ 10.19, 25.0285 ], [ 0.710841, 1.7368 ] ]
#! @EndExample
