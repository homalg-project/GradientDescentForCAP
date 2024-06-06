LoadPackage( "MachineLearningForCAP" );

Smooth := CategoryOfSmoothMaps( );
Lenses := CategoryOfLenses( Smooth );
Para := CategoryOfParametrisedMorphisms( Smooth );

iota := EmbeddingIntoCategoryOfLenses( Smooth, Lenses );

a := Lenses.AdamOptimizer( 2 );

f := PreCompose( Smooth,
        DirectProductFunctorial( Smooth, [ Smooth.IdFunc( 1 ) ^ 2, Smooth.IdFunc( 1 ) ^ 2 ] ),
        Smooth.Sum( 2 ) );

f := ApplyFunctor( iota, f );

l := MorphismConstructor( Lenses,
        ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 1 ) ] ),
        [ Smooth.IdFunc( 1 ), Smooth.Constant( 1, [ 1 ] ) ],
        ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 0 ) ] ) );

u := PreComposeList( Lenses, [ a, f, l ] );

# x = [ time (initial value is 1), m_vec (initial value is zero) , v_vec (initial value is zero), initial_value_for_x ]

x := [ 1, 0, 0, 0, 0, 30, 10 ];

for i in [ 1 .. 10000 ] do
  x := Eval( PutMorphism( u ), x );
od;


