gap> LoadPackage( "GradientDescentForCAP" );
true

gap> Para := CategoryOfParametrisedMorphisms( SkeletalSmoothMaps );
CategoryOfParametrisedMorphisms( SkeletalSmoothMaps )

gap> class_1 := Concatenation( List( [ -2 .. 3 ], i -> [ [ i, i - 1, 1, 0, 0 ], [ i + 1, i - 1, 1, 0, 0 ] ] ) );;
gap> class_2 := Concatenation( List( [ -3 .. -1 ], i -> List( [ i + 1 .. - i - 1 ], j -> [ i, j, 0, 1, 0 ] ) ) );;
gap> class_3 := Concatenation( List( [ 1 .. 3 ], i -> List( [ - i + 1 .. i - 1 ], j -> [ j, i, 0, 0, 1 ] ) ) );;

gap> training_set := Concatenation( class_1, class_2, class_3 );
[ [ -2, -3, 1, 0, 0 ], [ -1, -3, 1, 0, 0 ], [ -1, -2, 1, 0, 0 ], [ 0, -2, 1, 0, 0 ], [ 0, -1, 1, 0, 0 ],
  [ 1, -1, 1, 0, 0 ], [ 1, 0, 1, 0, 0 ], [ 2, 0, 1, 0, 0 ], [ 2, 1, 1, 0, 0 ], [ 3, 1, 1, 0, 0 ],
  [ 3, 2, 1, 0, 0 ], [ 4, 2, 1, 0, 0 ], [ -3, -2, 0, 1, 0 ], [ -3, -1, 0, 1, 0 ], [ -3, 0, 0, 1, 0 ],
  [ -3, 1, 0, 1, 0 ], [ -3, 2, 0, 1, 0 ], [ -2, -1, 0, 1, 0 ], [ -2, 0, 0, 1, 0 ], [ -2, 1, 0, 1, 0 ],
  [ -1, 0, 0, 1, 0 ], [ 0, 1, 0, 0, 1 ], [ -1, 2, 0, 0, 1 ], [ 0, 2, 0, 0, 1 ], [ 1, 2, 0, 0, 1 ],
  [ -2, 3, 0, 0, 1 ], [ -1, 3, 0, 0, 1 ], [ 0, 3, 0, 0, 1 ], [ 1, 3, 0, 0, 1 ], [ 2, 3, 0, 0, 1 ] ]

gap> input_dim := 2;; output_dim := 3;; hidden_dims := [ ];;

gap> f := PredictionMorphismOfNeuralNetwork( Para, input_dim, hidden_dims, output_dim, "Softmax" );;

gap> input := ConvertToExpressions( [ "theta_1", "theta_2", "theta_3", "theta_4", "theta_5", "theta_6", "theta_7", "theta_8", "theta_9", "x1", "x2" ] );;

gap> Display( f : dummy_input := input );
ℝ^2 -> ℝ^3 defined by:

Underlying Object:
-----------------
ℝ^9

Underlying Morphism:
-------------------
ℝ^11 -> ℝ^3

‣ Exp( (theta_1 * x1 + theta_2 * x2 + theta_3) )
    / (Exp( theta_1 * x1 + theta_2 * x2 + theta_3 ) + Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) ) + Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) ))
‣ Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) )
    / (Exp( theta_1 * x1 + theta_2 * x2 + theta_3 ) + Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) ) + Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) ))
‣ Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) )
    / (Exp( theta_1 * x1 + theta_2 * x2 + theta_3 ) + Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) ) + Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) ))

gap> parameters := [ 0.1, -0.1, 0, 0.1, 0.2, 0, -0.2, 0.3, 0 ];;
gap> x := [ 1, 2 ];;

gap> prediction_x := Eval( f, [ parameters, x ] );
[ 0.223672, 0.407556, 0.368772 ]

gap> PositionMaximum( prediction_x );
2

gap> ell := LossMorphismOfNeuralNetwork( Para, input_dim, hidden_dims, output_dim, "Softmax" );;

gap> input := ConvertToExpressions( [ "theta_1", "theta_2", "theta_3", "theta_4", "theta_5", "theta_6", "theta_7", "theta_8", "theta_9", "x1", "x2", "y1", "y2", "y3" ] );;

gap> Display( ell : dummy_input := input );
ℝ^5 -> ℝ^1 defined by:

Underlying Object:
-----------------
ℝ^9

Underlying Morphism:
-------------------
ℝ^14 -> ℝ^1

‣ ((Log( Exp( theta_1 * x1 + theta_2 * x2 + theta_3 ) + Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) ) + Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) ) ) - (theta_1 * x1 + theta_2 * x2 + theta_3)) * y1
  + (Log( Exp( theta_1 * x1 + theta_2 * x2 + theta_3 ) + Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) ) + Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) ) ) - (theta_4 * x1 + theta_5 * x2 + theta_6)) * y2
  + (Log( Exp( theta_1 * x1 + theta_2 * x2 + theta_3 ) + Exp( (theta_4 * x1 + theta_5 * x2 + theta_6) ) + Exp( (theta_7 * x1 + theta_8 * x2 + theta_9) ) ) - (theta_7 * x1 + theta_8 * x2 + theta_9)) * y3) / 3

gap> Lenses := CategoryOfLenses( SkeletalSmoothMaps );
CategoryOfLenses( SkeletalSmoothMaps )

gap> optimizer := Lenses.AdamOptimizer( : learning_rate := 0.01, beta_1 := 0.9, beta_2 := 0.999 );;

gap> batch_size := 1;;

gap> one_epoch_update := OneEpochUpdateLens( ell, optimizer, training_set, batch_size );
(ℝ^28, ℝ^28) -> (ℝ^1, ℝ^0) defined by:

Get Morphism:
----------
ℝ^28 -> ℝ^1

Put Morphism:
----------
ℝ^28 -> ℝ^28

gap> parameters := [ 0.1, -0.1, 0, 0.1, 0.2, 0, -0.2, 0.3, 0 ];;

gap> w := Concatenation( [ 1 ], 0 * parameters, 0 * parameters, parameters );;

gap> Eval( GetMorphism( one_epoch_update ), w );
[ 0.345836 ]

gap> new_w := Eval( PutMorphism( one_epoch_update ), w );
[ 31, 0.104642, -0.355463, -0.197135, -0.109428, -0.147082, 0.00992963,
  0.00478679, 0.502546, 0.187206, 0.0105493, 0.00642903, 0.00211548,
  0.00660062, 0.00274907, 0.00110985, 0.00278786, 0.0065483, 0.00112838,
  5.45195, -1.26208, 3.82578, -5.40639, -0.952146, -3.42835, -2.79496, 3.09008, -6.80672 ]

gap> nr_epochs := 4;;

gap> w := Fit( one_epoch_update, nr_epochs, w );
Epoch 0/4 - loss = 0.34583622811001763
Epoch 1/4 - loss = 0.6449437167091393
Epoch 2/4 - loss = 0.023811108587716449
Epoch 3/4 - loss = 0.0036371652708073405
Epoch 4/4 - loss = 0.0030655216725219204
[ 121, -4.57215e-05, -0.00190417, -0.0014116, -0.00181528, 0.00108949, 0.00065518, 0.001861, 0.000814679,
  0.000756424, 0.0104885, 0.00846858, 0.0022682, 0.00784643, 0.00551702, 0.0014626, 0.00351408, 0.00640225,
  0.00115053, 5.09137, -4.83379, 3.06257, -5.70976, 0.837175, -4.23622, -1.71171, 5.54301, -4.80856 ]

gap> theta := SplitDenseList( w, [ 19, 9 ] )[2];
[ 5.09137, -4.83379, 3.06257, -5.70976, 0.837175, -4.23622, -1.71171, 5.54301, -4.80856 ]

gap> theta := SkeletalSmoothMaps.Constant( theta );
ℝ^0 -> ℝ^9

gap> f_theta := ReparametriseMorphism( f, theta );;

gap> Display( f_theta );
ℝ^2 -> ℝ^3 defined by:

Underlying Object:
-----------------
ℝ^0

Underlying Morphism:
-------------------
ℝ^2 -> ℝ^3

‣ Exp( (5.09137 * x1 + (- 4.83379) * x2 + 3.06257) )
    / (Exp( 5.09137 * x1 + (- 4.83379) * x2 + 3.06257 )
        + Exp( ((- 5.70976) * x1 + 0.837175 * x2 + (- 4.23622)) )
          + Exp( ((- 1.71171) * x1 + 5.54301 * x2 + (- 4.80856)) ))
‣ Exp( ((- 5.70976) * x1 + 0.837175 * x2 + (- 4.23622)) )
    / (Exp( 5.09137 * x1 + (- 4.83379) * x2 + 3.06257 )
          + Exp( ((- 5.70976) * x1 + 0.837175 * x2 + (- 4.23622)) )
            + Exp( ((- 1.71171) * x1 + 5.54301 * x2 + (- 4.80856)) ))
‣ Exp( ((- 1.71171) * x1 + 5.54301 * x2 + (- 4.80856)) )
    / (Exp( 5.09137 * x1 + (- 4.83379) * x2 + 3.06257 )
          + Exp( ((- 5.70976) * x1 + 0.837175 * x2 + (- 4.23622)) )
            + Exp( ((- 1.71171) * x1 + 5.54301 * x2 + (- 4.80856)) ))

gap> f_theta := UnderlyingMorphism( f_theta );
ℝ^2 -> ℝ^3

gap> predictions_vec := Eval( f_theta, [ 1, -1 ] );
[ 1., 4.74723e-11, 1.31974e-11 ]

gap> PositionMaximum( predictions_vec );
1

gap> predictions_vec := Eval( f_theta, [ 1, 3 ] );
[ 7.13122e-08, 2.40484e-08, 1. ]

gap> PositionMaximum( predictions_vec );
3
