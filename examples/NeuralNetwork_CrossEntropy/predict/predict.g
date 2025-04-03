LoadPackage( "GradientDescentForCAP" );


Smooth := SkeletalSmoothMaps;
Para := CategoryOfParametrisedMorphisms( Smooth );


## The function we are trying minimize
predict := PredictionMorphismOfNeuralNetwork( Para, 2, [ 5, 5 ], 4, "Softmax" );

## After 50 epochs, we got the following weights:
w := [ 0.927523, -1.16534, 3.09767, 1.21408, 0.666635, -1.43307, -0.985489, 0.871045,
       -1.92995, 0.786445, -1.90583, 0.40558, -0.0727751, 2.23415, 0.54885, -2.54374,
       -2.19966, -0.398129, -1.16385, -0.608512, -1.48229, -0.987787, 3.44148, 1.42562,
       -0.465934, -0.356098, -1.33342, -0.236309, 0.961528, 0.644209, 0.809773, -0.881621,
       2.03238, -0.870562, -1.20672, 1.29646, 2.97375, -0.133015, -1.56653, 2.90988,
       0.817293, 1.46626, -0.262231, 0.301989, -0.500305, -1.36048, 2.25753, 1.28782,
       -0.0197388, -3.45074, 1.58903, -0.815923, -1.0852, 2.2728, -2.66226, 1.12052,
       1.03489, 0.085673, 3.31336, 0.29301, 0.110178,  2.22798, 2.15017, -1.25682, 2.86108,
       -1.89215, 2.74446, 1.19491, 1.01804 ];

## Let us use w to predict:
predict_using_w := UnderlyingMorphism( ReparametriseMorphism( predict, Smooth.Constant( w ) ) );

## create inputs:
inputs := Cartesian( 0.01 * [ -100 .. 100 ], 0.01 * [ -100 .. 100 ] );
predictions := List( inputs, x -> PositionMaximum( predict_using_w( x ) ) );


ScatterPlotUsingPython( inputs, predictions );
