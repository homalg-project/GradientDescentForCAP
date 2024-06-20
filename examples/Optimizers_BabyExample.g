LoadPackage( "MachineLearningForCAP" );

Smooth := CategoryOfSkeletalSmoothMaps( );
Lenses := CategoryOfLenses( Smooth );
Para := CategoryOfParametrisedMorphisms( Smooth );

iota := EmbeddingIntoCategoryOfLenses( Smooth, Lenses );

l := MorphismConstructor( Lenses,
        ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 1 ) ] ),
        [ Smooth.IdFunc( 1 ), Smooth.Constant( 1, [ -1 ] ) ],
        ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 0 ) ] ) );

f := PreCompose( Smooth,
        DirectProductFunctorial( Smooth, [ Smooth.IdFunc( 1 ) ^ 2, Smooth.IdFunc( 1 ) ^ 2 ] ),
        Smooth.Sum( 2 ) );

f := ApplyFunctor( iota, f );

## Adam Optimizer

optimizer := Lenses.DefaultAdamOptimizer( 2 );

u := PreComposeList( Lenses, [ optimizer, f, l ] );

# x = [ time (initial value is 1), m_vec (initial value is zero) , v_vec (initial value is zero), initial_value_for_x ]

x := [ 1, 0, 0, 0, 0, 30, 10 ];

for i in [ 1 .. 1000 ] do
  x := Eval( PutMorphism( u ), x );
od;

## Gradient Descent Optimizer

optimizer := Lenses.DefaultGradientDescentOptimizer( 2 );

u := PreComposeList( Lenses, [ optimizer, f, l ] );

x := [ 30, 10 ];

for i in [ 1 .. 1000 ] do
  x := Eval( PutMorphism( u ), x );
od;

## Gradient Descent with Momentum Optimizer

optimizer := Lenses.GradientDescentWithMomentumOptimizer( 2 )( 0.1, 0.3 );

u := PreComposeList( Lenses, [ optimizer, f, l ] );

x := [ 0, 0, 30, 10 ];

for i in [ 1 .. 1000 ] do
  x := Eval( PutMorphism( u ), x );
od;

## Adagrad Optimizer

optimizer := Lenses.AdagradOptimizer( 2 )( 1, 1.e-8 );

u := PreComposeList( Lenses, [ optimizer, f, l ] );

x := [ 0, 0, 30, 10 ];

for i in [ 1 .. 1000 ] do
  x := Eval( PutMorphism( u ), x );
od;
