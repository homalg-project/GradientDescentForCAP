LoadPackage( "MachineLearningForCAP" );


Smooth := SkeletalSmoothMaps;
Lenses := CategoryOfLenses( Smooth );
Para := CategoryOfParametrisedMorphisms( Smooth );


## The function we are trying minimize
f := LossMorphismOfNeuralNetwork( Para, 2, [], 1, "IdFunc" );

## One epoch update of the parameters
optimizer := Lenses.AdamOptimizer();

training_examples_path := "training_examples.txt";

batch_size := 5;

one_epoch_update := OneEpochUpdateLens( f, optimizer, training_examples_path, batch_size );

## initialize the parameters and apply updates nr_epochs times
w := [ 1, 0, 0, 0, 0, 0, 0, 0.21, -0.31, 0.7 ];
nr_epochs := 50;

w := Fit( one_epoch_update, nr_epochs, w );
