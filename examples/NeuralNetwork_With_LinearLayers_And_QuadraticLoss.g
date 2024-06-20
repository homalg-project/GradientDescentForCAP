LoadPackage( "MachineLearningForCAP" );



Smooth := CategoryOfSkeletalSmoothMaps( );
Lenses := CategoryOfLenses( Smooth );
Para := CategoryOfParametrisedMorphisms( Smooth );

perfect_weights := [ 2, -3, 1 ];

dataset_size := 100;
batch_size := 5;
Assert( 0, Int( dataset_size / batch_size ) <> fail );

noise := 0;

dataset :=
  List( [ 1 .. dataset_size ],
    function ( i )
      local x1, x2, error;
      
      x1 := Random( [ -0.01, 0.01 ] ) * Random( [ 1 .. 100 ] );
      x2 := Random( [ -0.01, 0.01 ] ) * Random( [ 1 .. 100 ] );
      
      error := Random( [ 0.01, 0.01 ] ) * Random( [ 1 .. 100 ] );
      
      return [ x1, x2, [ x1, x2 ] * perfect_weights{[ 1, 2 ]} + perfect_weights[3] + noise * error ];
      
    end );

# R^2 ------> R^1

linear_layer_1 := DirectProductFunctorial( Smooth, [ Smooth.LinearLayer( 2, 1 ), Smooth.IdFunc( 1 ) ] );
loss := Smooth.QuadraticLoss( 1 );

model := PreCompose( Smooth, linear_layer_1, loss );

model :=
  MorphismConstructor( Para,
    ObjectConstructor( Para, Smooth.( 3 ) ),
    Pair( Smooth.( 3 ), model ),
    ObjectConstructor( Para, Smooth.( 1 ) ) );

model := AdjustToBatchSize( model, batch_size );

model := SwitchSourceAndParameterObject( model );

batches :=
  List( [ 1 .. Int( dataset_size / batch_size ) ],
    i -> SmoothMorphism( Smooth,
            Smooth.( 0 ),
            Concatenation( dataset{[1 + batch_size * (i-1) .. batch_size * (i)]} ),
            Smooth.( 3 * batch_size ) ) );

models := List( batches, batch -> ReparametriseMorphism( model, batch ) );

models := List( models, model -> ParametrisedMorphism( model ) );

R := EmbeddingIntoCategoryOfLenses( Smooth, Lenses );

models := List( models, model -> ApplyFunctor( R, model ) );

#?optimizer := Lenses.GradientDescentOptimizer( 3 )( 0.1 );
optimizer := Lenses.DefaultAdamOptimizer( 3 );

learning_rate :=
    MorphismConstructor( Lenses,
        ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 1 ) ] ),
        [ Smooth.IdFunc( 1 ), Smooth.Constant( 1, [ -1 ] ) ],
        ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 0 ) ] ) );

models := List( models, model -> PreCompose( Lenses, optimizer, PreCompose( Lenses, model, learning_rate ) ) );

random_weights := [ 0.1, 0.1, 0.1 ];

#?x := random_weights;
x := Concatenation( [ 1, 0, 0, 0, 0, 0, 0 ], random_weights );

train :=
  function( models, x, nr_epocs )
    local i, model;
     
    for i in [ 1 .. nr_epocs ] do
      
      for model in models do
        
        x := PutMorphism( model )( x );
        
      od;
      
      #if IsZero( i mod 5 ) then
        
        loss := Sum( models, model -> GetMorphism( model )( x ) ) / Length( models );
        
        Print( "Epoch: ", String( i ), ",  loss: ", String( loss[1] ), "\n" );
        
      #fi;
      
    od;
    
    return x;
    
  end;

# x := train( models, x, 50 );
