gap> LoadPackage( "GradientDescentForCAP" );
true

gap> Para := CategoryOfParametrisedMorphisms( SkeletalSmoothMaps );
CategoryOfParametrisedMorphisms( SkeletalSmoothMaps )

gap> training_set := [ [ 1, 2.9 ], [ 2, 5.1 ], [ 3, 7.05 ] ];
[ [ 1, 2.9 ], [ 2, 5.1 ], [ 3, 7.05 ] ]

gap> input_dim := 1;; output_dim := 1;; hidden_dims := [ ];;

gap> f := PredictionMorphismOfNeuralNetwork( Para, input_dim, hidden_dims, output_dim, "IdFunc" );;

gap> Display( f );
ℝ^1 -> ℝ^1 defined by:

Underlying Object:
-----------------
ℝ^2

Underlying Morphism:
-------------------
ℝ^3 -> ℝ^1

‣ x1 * x3 + x2

gap> parameters := [ 2, 1 ];; x := [ 2 ];;

gap> Eval( f, [ parameters, x ] );
[ 5 ]

gap> ell := LossMorphismOfNeuralNetwork( Para, input_dim, hidden_dims, output_dim, "IdFunc" );;

gap> Display( ell );
ℝ^2 -> ℝ^1 defined by:

Underlying Object:
-----------------
ℝ^2

Underlying Morphism:
-------------------
ℝ^4 -> ℝ^1

‣ (x1 * x3 + x2 - x4) ^ 2 / 1

gap> Lenses := CategoryOfLenses( SkeletalSmoothMaps );
CategoryOfLenses( SkeletalSmoothMaps )

gap> optimizer := Lenses.GradientDescentOptimizer( : learning_rate := 0.01 );;

gap> batch_size := 1;;

gap> one_epoch_update := OneEpochUpdateLens( ell, optimizer, training_set, batch_size );;

gap> parameters := [ 0.1, -0.1 ];;

gap> nr_epochs := 15;;

gap> parameters := Fit( one_epoch_update, nr_epochs, parameters );
Epoch  0/15 - loss = 26.777499999999993
Epoch  1/15 - loss = 13.002145872163197
Epoch  2/15 - loss = 6.3171942035316935
Epoch  3/15 - loss = 3.0722513061917534
Epoch  4/15 - loss = 1.4965356389126505
Epoch  5/15 - loss = 0.73097379078374358
Epoch  6/15 - loss = 0.35874171019291579
Epoch  7/15 - loss = 0.1775574969062125
Epoch  8/15 - loss = 0.089228700384937534
Epoch  9/15 - loss = 0.046072054531129378
Epoch 10/15 - loss = 0.024919378473509772
Epoch 11/15 - loss = 0.014504998499450883
Epoch 12/15 - loss = 0.0093448161379050161
Epoch 13/15 - loss = 0.0067649700132868147
Epoch 14/15 - loss = 0.0054588596501628835
Epoch 15/15 - loss = 0.0047859930295160499
[ 2.08995, 0.802632 ]

gap> theta := SkeletalSmoothMaps.Constant( parameters );
ℝ^0 -> ℝ^2

gap> Display( theta );
ℝ^0 -> ℝ^2

‣ 2.08995
‣ 0.802632

gap> f_theta := ReparametriseMorphism( f, theta );;

gap> Display( f_theta );
ℝ^1 -> ℝ^1 defined by:

Underlying Object:
-----------------
ℝ^0

Underlying Morphism:
-------------------
ℝ^1 -> ℝ^1

‣ 2.08995 * x1 + 0.802632

gap> f_theta := UnderlyingMorphism( f_theta );
ℝ^1 -> ℝ^1

gap> Eval( f_theta, [ 1 ] );
[ 2.89259 ]

gap> Eval( f_theta, [ 2 ] );
[ 4.98254 ]

gap> Eval( f_theta, [ 3 ] );
[ 7.07249 ]
