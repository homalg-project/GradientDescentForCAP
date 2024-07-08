gap> Smooth := SkeletalSmoothMaps;;
gap> Lenses := CategoryOfLenses( Smooth );;
gap> Para := CategoryOfParametrisedMorphisms( Smooth );;
gap> f := LossMorphismOfNeuralNetwork( Para, 2, [ 5, 5 ], 4, "Softmax" );;
The total number of layers is 4

Creating a morphism from layer 1 to 2 with 15 parameters
Creating a morphism from layer 2 to 3 with 30 parameters
Creating a morphism from layer 3 to 4 with 24 parameters
gap> optimizer := Lenses.GradientDescentOptimizer( : learning_rate := 0.01 );;
gap> training_examples_path := SelectBasedOnCondition( IsExistingFile( "data-2.txt" ), "data-2.txt", "tst/data-2.txt" );;
gap> batch_size := 1;;
gap> one_epoch_update := OneEpochUpdateLens( f, optimizer, training_examples_path, batch_size );;
gap> w1 := [[-0.61789644,  0.56407845, -0.5965204,  -0.85063416,  0.770488  ], [-0.13079625,  0.47618425,  0.8807312,   0.24377191,  0.18529081], [0.,           0.,          0.,          0.,          0.        ] ];;
gap> w1 := Concatenation( TransposedMat( w1 ) );;
gap> w2 := [[-0.52913845,  0.524745,    0.67446196, -0.13036567, -0.5108599 ], [-0.12336099,  0.7475884,  -0.18031466,  0.30409217, -0.5017855 ], [-0.5523451,   0.74021363, -0.38746935, -0.2771675,   0.6162708 ], [-0.24399745,  0.523523,    0.31327105, -0.5376833,  -0.4945482 ], [ 0.33063114, -0.10083395,  0.13537377,  0.671383,   -0.2012583 ], [ 0.,          0.,          0.,          0.,          0.        ] ];;
gap> w2 := Concatenation( TransposedMat( w2 ) );;
gap> w3 := [[-0.05885905, -0.81396204,  0.00370395, -0.42547446], [-0.39928403,  0.56314194,  0.6614479 ,  0.5060446 ], [ 0.6662301,  -0.2800727 ,  0.1187852 , -0.27065504], [ 0.15874296, -0.6039741 , -0.7533438 , -0.33242884], [ 0.26578736, -0.45036432, -0.61879224,  0.8060001 ], [ 0.,          0.,          0.,          0.,       ] ];;
gap> w3 := Concatenation( TransposedMat( w3 ) );;
gap> w := Concatenation( [ w3, w2, w1 ] );;
gap> nr_epochs := 3;;
gap> w := Fit( one_epoch_update, nr_epochs, w );;
Epoch  0/3 - loss = 0.36090265698232782
Epoch  1/3 - loss = 0.21968044680244986
Epoch  2/3 - loss = 0.17883151112398918
Epoch  3/3 - loss = 0.15935373922266871
