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

# classes are 0 or 1
dataset :=
  List( [ 1 .. dataset_size ],
    function ( i )
      local r, x1, x2;
      
      r := Random( [ 1, 2 ] );
      
      x1 := 0.01 * r * Random( [ -100 .. 100 ] );
      x2 := Random( [-1, 1 ] ) * Sqrt( r ^ 2 - x1 ^ 2 ) + noise * error;
      
      return [ x1, x2, r - 1 ];
      
    end );

input_size := 2;

hidden_size_1 := 6;
hidden_size_2 := 6;

output_size := 1;

linear_layer_1 := Para.LinearLayer( input_size, hidden_size_1 );
relu_1 := Para.Relu( hidden_size_1 );

linear_layer_2 := Para.LinearLayer( hidden_size_1, hidden_size_2 );
relu_2 := Para.Relu( hidden_size_2 );

linear_layer_3 := Para.LinearLayer( hidden_size_2, output_size );
sigmoid := Para.Sigmoid( output_size );

f := PreComposeList( Para, [ linear_layer_1, relu_1, linear_layer_2, relu_2, linear_layer_3 ] );

P := ParameterObject( f );

nr_parameters := RankOfObject( P );

forward := DirectProductFunctorial( Smooth, [ ParametrisedMorphism( f ), Smooth.IdFunc( output_size ) ] );
loss := Smooth.SigmoidBinaryCrossEntropyLoss;

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

optimizer := Lenses.AdamOptimizer( nr_parameters )( 0.9, 0.999, 0.001, 1.e-8 );
#optimizer := Lenses.GradientDescentOptimizer( nr_parameters )( 0.01 );

learning_rate :=
    MorphismConstructor( Lenses,
        ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 1 ) ] ),
        [ Smooth.IdFunc( 1 ), Smooth.Constant( 1, [ -1 ] ) ],
        ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 0 ) ] ) );

models := List( models, model -> PreCompose( Lenses, optimizer, PreCompose( Lenses, model, learning_rate ) ) );

random_weights := List( [ 1 .. nr_parameters ], i -> 0.001 * Random( [ -100 .. 100 ] ) );

t := [ 1 ];
m := ListWithIdenticalEntries( nr_parameters, 0 );
v := m;

w := Concatenation( t, m, v, random_weights );
#w := random_weights;

train :=
  function( nr_epocs )
    local i, model;
     
    for i in [ 1 .. nr_epocs ] do
      
      for model in models do
        
        w := PutMorphism( model )( w );
        
      od;
      
      #if IsZero( i mod 5 ) then
        
        loss := Sum( models, model -> GetMorphism( model )( w ) ) / Length( models );
        
        Print( "Epoch: ", String( i ), ",  loss: ", String( loss[1] ), "\n" );
        
      #fi;
      
    od;
    
  end;


train_points := List( dataset, example -> example{[ 1, 2 ]} );
train_labels := List( dataset, example -> example[3] );

## Visualize

w_ := w{[ Length( w ) - nr_parameters + 1 .. Length( w ) ]};

predict := PreCompose( Smooth,
            ParametrisedMorphism( ReparametriseMorphism( f, Smooth.Constant( w_ ) ) ),
            Smooth.Sigmoid( 1 ) );

test_points := [ ];
test_labels := [ ];

for x in 0.1 * [ -30 .. 30  ] do
  for y in 0.1 * [ -30 .. 30 ] do
      
      Add( test_points, [ x, y ] );
      Add( test_labels, predict( [ x, y ] ) );
      
  od;
od;

ScatterPlotUsingPython( train_points, train_labels, test_points, test_labels );
