LoadPackage( "MachineLearningForCAP" );


Smooth := SkeletalSmoothMaps;
Lenses := CategoryOfLenses( Smooth );
Para := CategoryOfParametrisedMorphisms( Smooth );


## The function we are trying minimize
f := LossMorphismOfNeuralNetwork( Para, 2, [ 5, 5 ], 4, "Softmax" );

## One epoch update of the parameters
optimizer := Lenses.GradientDescentOptimizer( : learning_rate := 0.01 );

training_examples_path := "data/training_examples.txt";

batch_size := 1;

one_epoch_update := OneEpochUpdateLens( f, optimizer, training_examples_path, batch_size );

## Initialize the parameters and apply updates nr_epochs times

# initial values for the parameters of the first affine transformation w1 (as 3x5 matrix)
w1 := [[-0.61789644,  0.56407845, -0.5965204,  -0.85063416,  0.770488  ],
       [-0.13079625,  0.47618425,  0.8807312,   0.24377191,  0.18529081],
       [0.,           0.,          0.,          0.,          0.        ] ];

# as vector
w1 := Concatenation( TransposedMat( w1 ) );

# initial weights for the parameters of the second affine transformation w2 (as 6x5 matrix)
w2 :=
[[-0.52913845,  0.524745,    0.67446196, -0.13036567, -0.5108599 ],
 [-0.12336099,  0.7475884,  -0.18031466,  0.30409217, -0.5017855 ],
 [-0.5523451,   0.74021363, -0.38746935, -0.2771675,   0.6162708 ],
 [-0.24399745,  0.523523,    0.31327105, -0.5376833,  -0.4945482 ],
 [ 0.33063114, -0.10083395,  0.13537377,  0.671383,   -0.2012583 ],
 [ 0.,          0.,          0.,          0.,          0.        ] ];

# as vector
w2 := Concatenation( TransposedMat( w2 ) );

# initial weights for the parameters of the third affine transformation w3 (as 6x4 matrix)
w3 :=
[[-0.05885905, -0.81396204,  0.00370395, -0.42547446],
 [-0.39928403,  0.56314194,  0.6614479 ,  0.5060446 ],
 [ 0.6662301,  -0.2800727 ,  0.1187852 , -0.27065504],
 [ 0.15874296, -0.6039741 , -0.7533438 , -0.33242884],
 [ 0.26578736, -0.45036432, -0.61879224,  0.8060001 ],
 [ 0.,          0.,          0.,          0.,       ] ];

# as vector
w3 := Concatenation( TransposedMat( w3 ) );

# creating a vector of initial values
w := Concatenation( [ w3, w2, w1 ] );

nr_epochs := 50;

w := Fit( one_epoch_update, nr_epochs, w );

# After 50 epochs:
# w = [ 0.927523, -1.16534, 3.09767, 1.21408, 0.666635, -1.43307, -0.985489, 0.871045, -1.92995, 0.786445, -1.90583, 0.40558, -0.0727751, 2.23415, 0.54885, -2.54374, -2.19966, -0.398129, -1.16385, -0.608512, -1.48229, -0.987787, 3.44148, 1.42562, -0.465934, -0.356098, -1.33342, -0.236309, 0.961528, 0.644209, 0.809773, -0.881621, 2.03238, -0.870562, -1.20672, 1.29646, 2.97375, -0.133015, -1.56653, 2.90988, 0.817293, 1.46626, -0.262231, 0.301989, -0.500305, -1.36048, 2.25753, 1.28782, -0.0197388, -3.45074, 1.58903, -0.815923, -1.0852, 2.2728, -2.66226, 1.12052, 1.03489, 0.085673, 3.31336, 0.29301, 0.110178,  2.22798, 2.15017, -1.25682, 2.86108, -1.89215, 2.74446, 1.19491, 1.01804 ]
