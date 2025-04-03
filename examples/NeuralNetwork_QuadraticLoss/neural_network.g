LoadPackage( "GradientDescentForCAP" );


Smooth := SkeletalSmoothMaps;
Lenses := CategoryOfLenses( Smooth );
Para := CategoryOfParametrisedMorphisms( Smooth );

## the perfect weights are [ 2, -3, 1 ]

## The function we are trying minimize
f := LossMorphismOfNeuralNetwork( Para, 2, [], 1, "IdFunc" );

## One epoch update of the parameters
optimizer := Lenses.AdamOptimizer();

training_examples_path := "data/training_examples.txt";

batch_size := 5;

one_epoch_update := OneEpochUpdateLens( f, optimizer, training_examples_path, batch_size );

## initialize the parameters and apply updates nr_epochs times
w := [ 1, 0, 0, 0, 0, 0, 0, 0.21, -0.31, 0.7 ];
nr_epochs := 50;

w := Fit( one_epoch_update, nr_epochs, w );

# after 5 epochs w = [ 1021, -0.00236067, -0.00633157, 0.000258869, 0.0135747, 0.0500079, 0.0310695, 2.00197, -2.99162, 0.997524 ]
#                                                                                                    ca. 2    ca. -3    ca. 1
