LoadPackage( "MachineLearningForCAP" );


Smooth := CategoryOfSmoothMaps( );
Lenses := CategoryOfLenses( Smooth );
Para := CategoryOfParametrisedMorphisms( Smooth );

# we create two classes (two circles: the first has radius 1 and second has radius 2)

dataset_size := 1000;
batch_size := 1;
Assert( 0, Int( dataset_size / batch_size ) <> fail );

noise := 1;
error := 0.2;

dataset :=
  List( [ 1 .. dataset_size ],
    function ( i )
      local r, x1, x2;
      
      r := Random( [ 1, 2 ] );
      
      x1 := 0.01 * r * Random( [ -100 .. 100 ] );
      x2 := Random( [-1, 1 ] ) * Sqrt( r ^ 2 - x1 ^ 2 ) + noise * error;
      
      return Concatenation( [ x1, x2 ], IdentityMat( 2 )[r] );
      
    end );

input_size := 2;

hidden_size_1 := 4;
hidden_size_2 := 4;

output_size := 2;

linear_layer_1 := Para.LinearLayer( input_size, hidden_size_1 );
relu_1 := Para.Relu( hidden_size_1 );

linear_layer_2 := Para.LinearLayer( hidden_size_1, hidden_size_2 );
relu_2 := Para.Relu( hidden_size_2 );

linear_layer_3 := Para.LinearLayer( hidden_size_2, output_size );
softmax := Para.Softmax( output_size );

f := PreComposeList( Para, [ linear_layer_1, relu_1, linear_layer_2, relu_2, linear_layer_3, softmax ] );

P := ParameterObject( f );

nr_parameters := RankOfObject( P );

forward := DirectProductFunctorial( Smooth, [ ParametrisedMorphism( f ), Smooth.IdFunc( output_size ) ] );
loss := Smooth.CrossEntropyLoss( 2 );

model := PreCompose( Smooth, forward, loss );

model :=
  MorphismConstructor( Para,
    ObjectConstructor( Para, Smooth.( input_size + output_size ) ),
    Pair( P, model ),
    ObjectConstructor( Para, Smooth.( 1 ) ) );

model := AdjustToBatchSize( model, batch_size );

model := SwitchSourceAndParameterObject( model );

batches :=
  List( [ 1 .. Int( dataset_size / batch_size ) ],
    i -> SmoothMorphism( Smooth,
            Smooth.( 0 ),
            Concatenation( dataset{[1 + batch_size * ( i - 1 ) .. batch_size * ( i )]} ),
            Smooth.( ( input_size + output_size )  * batch_size ) ) );

models := List( batches, batch -> ParametrisedMorphism( ReparametriseMorphism( model, batch ) ) );

R := EmbeddingIntoCategoryOfLenses( Smooth, Lenses );

models := List( models, model -> ApplyFunctor( R, model ) );

#optimizer := Lenses.DefaultAdamOptimizer( nr_parameters );
optimizer := Lenses.GradientDescentOptimizer( nr_parameters )( 0.025 );

learning_rate :=
    MorphismConstructor( Lenses,
        ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 1 ) ] ),
        [ Smooth.IdFunc( 1 ), Smooth.Constant( 1, [ -1 ] ) ],
        ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 0 ) ] ) );

models := List( models, model -> PreCompose( Lenses, optimizer, PreCompose( Lenses, model, learning_rate ) ) );

random_weights :=
  [ 1.86147, -2.29119, 0.209532, 0.317703, 0.692188, -2.46147, 2.39119,
    0.390468, -0.617703, -0.192188, -1.35576, -1.17715, -1.25967, 1.89876,
    1.36656, 1.82858, 2.09602, 1.91308, -1.4349, -0.319786, 0.423307, 0.59688,
    -0.704382, 0.461489, -0.907284, 0.121786, -0.0616471, -0.222958,
    0.299378, -1.31333, -2.41947, -0.289648, -0.0026164, 0.760027, 2.1042,
    -0.752433, 1.27907, -1.84755, -0.0521345, -0.279145, 0.0442088, 2.22709 ];

t := [ 1 ];
m := ListWithIdenticalEntries( nr_parameters, 0 );
v := m;

#w := Concatenation( t, m, v, random_weights );
w := random_weights;

train :=
  function( nr_epocs )
    local i, model;
    
    MachineLearningForCAP.MOD := "train";
    
    for i in [ 1 .. nr_epocs ] do
      
      for model in models do
        
        w := PutMorphism( model )( w );
        
      od;
      
      #if IsZero( i mod 5 ) then
        
        loss := Sum( models, model -> GetMorphism( model )( w ) ) / Length( models );
        
        Print( "Epoch: ", String( i ), ",  loss: ", String( loss[1] ), "\n" );
        
      #fi;
      
    od;
    
    MachineLearningForCAP.MOD := "basic";
    
  end;

f := ReparametriseMorphism( f, SmoothMorphism( Smooth, Smooth.( 0 ), w, Smooth.( nr_parameters ) ) );
f := ParametrisedMorphism( f );

points := [ ];
labels := [ ];

for x in 0.1 * [ -30 .. 30  ] do
  for y in 0.1 * [ -30 .. 30 ] do
      
      l := PositionMaximum( f( [ x , y ] ) );
      
      Add( points, [ x, y ] );
      Add( labels, l );
  od;
od;

