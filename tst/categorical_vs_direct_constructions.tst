gap> Smooth := SkeletalSmoothMaps;;
gap> Lenses := CategoryOfLenses( Smooth );;
gap> Para := CategoryOfParametrisedMorphisms( Smooth );;
gap> Assert( 0, Para.Softmax_( 3 ) = Para.Softmax( 3 ) );
gap> Assert( 0, Para.Sigmoid_( 3 ) = Para.Sigmoid( 3 ) );
gap> Assert( 0, Para.QuadraticLoss_( 3 ) = Para.QuadraticLoss( 3 ) );
gap> Assert( 0, Para.CrossEntropyLoss_( 3 ) = Para.CrossEntropyLoss( 3 ) );
gap> Assert( 0, Para.SoftmaxCrossEntropyLoss_( 3 ) = Para.SoftmaxCrossEntropyLoss( 3 ) );
gap> Assert( 0, Para.QuadraticLoss_( 3 ) = Para.QuadraticLoss( 3 ) );
gap> Assert( 0, Para.SigmoidBinaryCrossEntropyLoss_( 1 ) = Para.SigmoidBinaryCrossEntropyLoss( 1 ) );
gap> Assert( 0, Para.AffineTransformation_( 3, 4 ) = Para.AffineTransformation( 3, 4 ) );
gap> l := Para.AffineTransformation( 3, 4 );
ℝ^3 -> ℝ^4 defined by:

Parameter Object:
-----------------
ℝ^16

Parametrised Morphism:
----------------------
ℝ^19 -> ℝ^4
gap> dummy_input := DummyInputForAffineTransformation( 3, 4, "w", "b", "x" );;
gap> Display( l : dummy_input := dummy_input );
ℝ^3 -> ℝ^4 defined by:

Parameter Object:
-----------------
ℝ^16

Parametrised Morphism:
----------------------
ℝ^19 -> ℝ^4

‣ w1_1 * x1 + w2_1 * x2 + w3_1 * x3 + b_1
‣ w1_2 * x1 + w2_2 * x2 + w3_2 * x3 + b_2
‣ w1_3 * x1 + w2_3 * x2 + w3_3 * x3 + b_3
‣ w1_4 * x1 + w2_4 * x2 + w3_4 * x3 + b_4
gap> LaTeXOutput( UnderlyingMorphism( l ) : dummy_input := dummy_input );
"\\begin{array}{c}\n\\mathbb{R}^{19}\\rightarrow\\mathbb{R}^{4}\\\\ \n \\hline \\\\ \n \\left( \\begin{array}{l}\nb_{1} + w_{1 1} x_{1} + w_{2 1} x_{2} + w_{3 1} x_{3} \\\\ \n b_{2} + w_{1 2} x_{1} + w_{2 2} x_{2} + w_{3 2} x_{3} \\\\ \n b_{3} + w_{1 3} x_{1} + w_{2 3} x_{2} + w_{3 3} x_{3} \\\\ \n b_{4} + w_{1 4} x_{1} + w_{2 4} x_{2} + w_{3 4} x_{3}\n\\end{array} \\right)\\\\ \n \\\\ \n \\hline \\\\ \n \\left( \\begin{array}{lllllllllllllllllll}\nx_{1} & x_{2} & x_{3} & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & w_{1 1} & w_{2 1} & w_{3 1} \\\\ \n 0 & 0 & 0 & 0 & x_{1} & x_{2} & x_{3} & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & w_{1 2} & w_{2 2} & w_{3 2} \\\\ \n 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & x_{1} & x_{2} & x_{3} & 1 & 0 & 0 & 0 & 0 & w_{1 3} & w_{2 3} & w_{3 3} \\\\ \n 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & x_{1} & x_{2} & x_{3} & 1 & w_{1 4} & w_{2 4} & w_{3 4}\n\\end{array} \\right)\n\\end{array}"
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
