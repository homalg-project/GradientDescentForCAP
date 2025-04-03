LoadPackage( "GradientDescentForCAP" );


# the function f(x1,x2) = sin(x1)^2 + log(x2)^2 has local miminima at the points (πk, 1) where k ∈ ℤ

Smooth := CategoryOfSkeletalSmoothMaps( );
Lenses := CategoryOfLenses( Smooth );
Para := CategoryOfParametrisedMorphisms( Smooth );

f := PreCompose( Smooth,
        DirectProductFunctorial( Smooth, [ Smooth.Sin ^ 2, Smooth.Log ^ 2 ] ),
        Smooth.Sum( 2 ) );

f := MorphismConstructor( Para,
        ObjectConstructor( Para, Smooth.( 0 ) ),
        Pair( Smooth.( 2 ), f ),
        ObjectConstructor( Para, Smooth.( 1 ) ) );

optimizer := Lenses.AdamOptimizer( );

# there is only one training example in R^0 which is the trivial vector []
training_examples := [ [] ];

# what else :)
batch_size := 1;

one_epoch_update := OneEpochUpdateLens( f, optimizer, training_examples, batch_size );

# initial value for w
w := [ 1, 0, 0, 0, 0, 1.58, 0.1 ];

nr_epochs := 5000;

w := Fit( one_epoch_update, nr_epochs, w );

# after 5000 epoch the found point is [ bla bla, 3.14159, 1 ]
