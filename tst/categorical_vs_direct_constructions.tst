gap> Smooth := SkeletalSmoothMaps;;
gap> Lenses := CategoryOfLenses( Smooth );;
gap> Para := CategoryOfParametrisedMorphisms( Smooth );;
gap> Assert( 0, Para.Softmax_( 3 ) = Para.Softmax( 3 ) );
gap> Assert( 0, Para.Sigmoid_( 3 ) = Para.Sigmoid( 3 ) );
gap> Assert( 0, Para.QuadraticLoss_( 3 ) = Para.QuadraticLoss( 3 ) );
gap> Assert( 0, Para.CrossEntropyLoss_( 3 ) = Para.CrossEntropyLoss( 3 ) );
gap> Assert( 0, Para.SoftmaxCrossEntropyLoss_( 3 ) = Para.SoftmaxCrossEntropyLoss( 3 ) );
gap> Assert( 0, Para.QuadraticLoss_( 3 ) = Para.QuadraticLoss( 3 ) );
gap> Assert( 0, Para.QuadraticLoss_( 3 ) = Para.QuadraticLoss( 3 ) );
gap> Assert( 0, Para.SigmoidBinaryCrossEntropyLoss_( 1 ) = Para.SigmoidBinaryCrossEntropyLoss( 1 ) );
gap> Assert( 0, Para.AffineTransformation_( 3, 4 ) = Para.AffineTransformation( 3, 4 ) );
gap> Eval( Smooth.PolynomialTransformation( 2, 3, 4 ), [ 1 .. 47 ] );
[ 122341573, 479204128, 836066683 ]
gap> EvalJacobianMatrix( Smooth.PolynomialTransformation( 2, 3, 4 ), [ 1 .. 47 ] );
[ [ 4477456, 4574792, 97336, 4674244, 99452, 2116, 4775858, 101614, 2162, 46, 4879681, 103823, 2209, 47, 1, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2585050, 7818740 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4477456, 4574792, 97336, 4674244, 99452, 2116, 4775858, 101614, 2162, 46,
      4879681, 103823, 2209, 47, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 17701630, 23262470 ],
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4477456, 4574792, 97336,
      4674244, 99452, 2116, 4775858, 101614, 2162, 46, 4879681, 103823, 2209, 47, 1, 32818210, 38706200 ] ]
gap> Assert( 0, Lenses.GradientDescentOptimizer_( )( 3 ) = Lenses.GradientDescentOptimizer( )( 3 ) );
gap> Assert( 0, Lenses.GradientDescentWithMomentumOptimizer_( )( 3 ) = Lenses.GradientDescentWithMomentumOptimizer( )( 3 ) );
gap> Assert( 0, Lenses.AdagradOptimizer_( )( 3 ) = Lenses.AdagradOptimizer( )( 3 ) );
gap> Assert( 0, Lenses.AdamOptimizer_( )( 3 ) = Lenses.AdamOptimizer( )( 3 ) );
