#! @Chapter Examples and Tests

#! @Section Category of Lenses

LoadPackage( "GradientDescentForCAP" );

#! @Example
Smooth := CategoryOfSkeletalSmoothMaps( );
#! SkeletalSmoothMaps
Lenses := CategoryOfLenses( Smooth );
#! CategoryOfLenses( SkeletalSmoothMaps )
A := ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 2 ) ] );
#! (ℝ^1, ℝ^2)
IsWellDefined( A );
#! true
CapCategory( A );
#! CategoryOfLenses( SkeletalSmoothMaps )
A_datum := ObjectDatum( A );
#! [ ℝ^1, ℝ^2 ]
CapCategory( A_datum[1] );
#! SkeletalSmoothMaps
B := ObjectConstructor( Lenses, [ Smooth.( 3 ), Smooth.( 4 ) ] );
#! (ℝ^3, ℝ^4)
get := RandomMorphism( Smooth.( 1 ), Smooth.( 3 ), 5 );
#! ℝ^1 -> ℝ^3
put := RandomMorphism( Smooth.( 1 + 4 ), Smooth.( 2 ), 5 );
#! ℝ^5 -> ℝ^2
f := MorphismConstructor( Lenses, A, [ get, put ], B );
#! (ℝ^1, ℝ^2) -> (ℝ^3, ℝ^4) defined by:
#!
#! Get Morphism:
#! ----------
#! ℝ^1 -> ℝ^3
#!
#! Put Morphism:
#! ----------
#! ℝ^5 -> ℝ^2
MorphismDatum( f );
#! [ ℝ^1 -> ℝ^3, ℝ^5 -> ℝ^2 ]
IsWellDefined( f );
#! true
Display( f );
#! (ℝ^1, ℝ^2) -> (ℝ^3, ℝ^4) defined by:
#!
#! Get Morphism:
#! ------------
#! ℝ^1 -> ℝ^3
#!
#! ‣ 0.766 * x1 ^ 4 + 0.234
#! ‣ 1. * x1 ^ 4 + 0.388
#! ‣ 0.459 * x1 ^ 4 + 0.278
#!
#! Put Morphism:
#! ------------
#! ℝ^5 -> ℝ^2
#!
#! ‣ 0.677 * x1 ^ 5 + 0.19 * x2 ^ 4 + 0.659 * x3 ^ 4
#! + 0.859 * x4 ^ 5 + 0.28 * x5 ^ 1 + 0.216
#! ‣ 0.37 * x1 ^ 5 + 0.571 * x2 ^ 4 + 0.835 * x3 ^ 4
#! + 0.773 * x4 ^ 5 + 0.469 * x5 ^ 1 + 0.159
id_A := IdentityMorphism( Lenses, A );
#! (ℝ^1, ℝ^2) -> (ℝ^1, ℝ^2) defined by:
#!
#! Get Morphism:
#! ----------
#! ℝ^1 -> ℝ^1
#!
#! Put Morphism:
#! ----------
#! ℝ^3 -> ℝ^2
Display( id_A );
#! (ℝ^1, ℝ^2) -> (ℝ^1, ℝ^2) defined by:
#!
#! Get Morphism:
#! ------------
#! ℝ^1 -> ℝ^1
#!
#! ‣ x1
#!
#! Put Morphism:
#! ------------
#! ℝ^3 -> ℝ^2
#!
#! ‣ x2
#! ‣ x3
IsCongruentForMorphisms( PreCompose( id_A, f ), f );
#! true
TensorUnit( Lenses );
#! (ℝ^0, ℝ^0)
TensorProductOnObjects( A, B );
#! (ℝ^4, ℝ^6)
f1 := RandomMorphism( A, B, 5 );
#! (ℝ^1, ℝ^2) -> (ℝ^3, ℝ^4) defined by:
#!
#! Get Morphism:
#! ----------
#! ℝ^1 -> ℝ^3
#!
#! Put Morphism:
#! ----------
#! ℝ^5 -> ℝ^2
f2 := RandomMorphism( A, B, 5 );
#! (ℝ^1, ℝ^2) -> (ℝ^3, ℝ^4) defined by:
#!
#! Get Morphism:
#! ----------
#! ℝ^1 -> ℝ^3
#!
#! Put Morphism:
#! ----------
#! ℝ^5 -> ℝ^2
f3 := RandomMorphism( A, B, 5 );
#! (ℝ^1, ℝ^2) -> (ℝ^3, ℝ^4) defined by:
#!
#! Get Morphism:
#! ----------
#! ℝ^1 -> ℝ^3
#!
#! Put Morphism:
#! ----------
#! ℝ^5 -> ℝ^2
f1_f2 := TensorProductOnMorphisms( Lenses, f1, f2 );
#! (ℝ^2, ℝ^4) -> (ℝ^6, ℝ^8) defined by:
#!
#! Get Morphism:
#! ----------
#! ℝ^2 -> ℝ^6
#!
#! Put Morphism:
#! ----------
#! ℝ^10 -> ℝ^4
f2_f3 := TensorProductOnMorphisms( Lenses, f2, f3 );
#! (ℝ^2, ℝ^4) -> (ℝ^6, ℝ^8) defined by:
#!
#! Get Morphism:
#! ----------
#! ℝ^2 -> ℝ^6
#!
#! Put Morphism:
#! ----------
#! ℝ^10 -> ℝ^4
t1 := TensorProductOnMorphisms( Lenses, f1_f2, f3 );
#! (ℝ^3, ℝ^6) -> (ℝ^9, ℝ^12) defined by:
#!
#! Get Morphism:
#! ----------
#! ℝ^3 -> ℝ^9
#!
#! Put Morphism:
#! ----------
#! ℝ^15 -> ℝ^6
t2 := TensorProductOnMorphisms( Lenses, f1, f2_f3 );
#! (ℝ^3, ℝ^6) -> (ℝ^9, ℝ^12) defined by:
#!
#! Get Morphism:
#! ----------
#! ℝ^3 -> ℝ^9
#!
#! Put Morphism:
#! ----------
#! ℝ^15 -> ℝ^6
IsCongruentForMorphisms( t1, t2 );
#! true
Display( Braiding( A, B ) );
#! (ℝ^4, ℝ^6) -> (ℝ^4, ℝ^6) defined by:
#!
#! Get Morphism:
#! ------------
#! ℝ^4 -> ℝ^4
#!
#! ‣ x2
#! ‣ x3
#! ‣ x4
#! ‣ x1
#!
#! Put Morphism:
#! ------------
#! ℝ^10 -> ℝ^6
#!
#! ‣ x9
#! ‣ x10
#! ‣ x5
#! ‣ x6
#! ‣ x7
#! ‣ x8
Display( PreCompose( Braiding( A, B ), BraidingInverse( A, B ) ) );
#! (ℝ^4, ℝ^6) -> (ℝ^4, ℝ^6) defined by:
#!
#! Get Morphism:
#! ------------
#! ℝ^4 -> ℝ^4
#!
#! ‣ x1
#! ‣ x2
#! ‣ x3
#! ‣ x4
#!
#! Put Morphism:
#! ------------
#! ℝ^10 -> ℝ^6
#!
#! ‣ x5
#! ‣ x6
#! ‣ x7
#! ‣ x8
#! ‣ x9
#! ‣ x10
R := EmbeddingIntoCategoryOfLenses( Smooth, Lenses );
#! Embedding functor into category of lenses
SourceOfFunctor( R );
#! SkeletalSmoothMaps
RangeOfFunctor( R );
#! CategoryOfLenses( SkeletalSmoothMaps )
f := Smooth.Softmax( 2 );
#! ℝ^2 -> ℝ^2
Display( f );
#! ℝ^2 -> ℝ^2
#!
#! ‣ Exp( x1 ) / (Exp( x1 ) + Exp( x2 ))
#! ‣ Exp( x2 ) / (Exp( x1 ) + Exp( x2 ))
Rf := ApplyFunctor( R, f );
#! (ℝ^2, ℝ^2) -> (ℝ^2, ℝ^2) defined by:
#!
#! Get Morphism:
#! ----------
#! ℝ^2 -> ℝ^2
#!
#! Put Morphism:
#! ----------
#! ℝ^4 -> ℝ^2
Display( Rf );
#! (ℝ^2, ℝ^2) -> (ℝ^2, ℝ^2) defined by:
#!
#! Get Morphism:
#! ------------
#! ℝ^2 -> ℝ^2
#!
#! ‣ Exp( x1 ) / (Exp( x1 ) + Exp( x2 ))
#! ‣ Exp( x2 ) / (Exp( x1 ) + Exp( x2 ))
#!
#! Put Morphism:
#! ------------
#! ℝ^4 -> ℝ^2
#!
#! ‣ x3 *
#!   ((Exp( x1 ) + Exp( x2 ) - Exp( x1 )) * (Exp( x1 ) / (Exp( x1 ) + Exp( x2 )) ^ 2))
#!   + x4 * ((- Exp( x1 )) * (Exp( x2 ) / (Exp( x1 ) + Exp( x2 )) ^ 2))
#! ‣ x3 * ((- Exp( x2 )) * (Exp( x1 ) / (Exp( x1 ) + Exp( x2 )) ^ 2)) +
#!   x4 *
#!   ((Exp( x1 ) + Exp( x2 ) - Exp( x2 )) * (Exp( x2 ) / (Exp( x1 ) + Exp( x2 )) ^ 2))
Display( Lenses.GradientDescentOptimizer( :learning_rate := 0.01 )( 2 ) );
#! (ℝ^2, ℝ^2) -> (ℝ^2, ℝ^2) defined by:
#!
#! Get Morphism:
#! ------------
#! ℝ^2 -> ℝ^2
#!
#! ‣ x1
#! ‣ x2
#!
#! Put Morphism:
#! ------------
#! ℝ^4 -> ℝ^2
#!
#! ‣ x1 + 0.01 * x3
#! ‣ x2 + 0.01 * x4
Display( Lenses.GradientDescentWithMomentumOptimizer(
            :learning_rate := 0.01, momentum := 0.9 )( 2 ) );
#! (ℝ^4, ℝ^4) -> (ℝ^2, ℝ^2) defined by:
#!
#! Get Morphism:
#! ------------
#! ℝ^4 -> ℝ^2
#!
#! ‣ x3
#! ‣ x4
#!
#! Put Morphism:
#! ------------
#! ℝ^6 -> ℝ^4
#!
#! ‣ 0.9 * x1 + 0.01 * x5
#! ‣ 0.9 * x2 + 0.01 * x6
#! ‣ x3 + (0.9 * x1 + 0.01 * x5)
#! ‣ x4 + (0.9 * x2 + 0.01 * x6)
Display( Lenses.AdagradOptimizer( :learning_rate := 0.01 )( 2 ) );
#! (ℝ^4, ℝ^4) -> (ℝ^2, ℝ^2) defined by:
#!
#! Get Morphism:
#! ------------
#! ℝ^4 -> ℝ^2
#!
#! ‣ x3
#! ‣ x4
#!
#! Put Morphism:
#! ------------
#! ℝ^6 -> ℝ^4
#!
#! ‣ x1 + x5 ^ 2
#! ‣ x2 + x6 ^ 2
#! ‣ x3 + 0.01 * x5 / (1.e-07 + Sqrt( (x1 + x5 ^ 2) ))
#! ‣ x4 + 0.01 * x6 / (1.e-07 + Sqrt( (x2 + x6 ^ 2) ))
Display( Lenses.AdamOptimizer(
          :learning_rate := 0.01, beta_1 := 0.9, beta_2 := 0.999 )( 2 ) );
#! (ℝ^7, ℝ^7) -> (ℝ^2, ℝ^2) defined by:
#!
#! Get Morphism:
#! ------------
#! ℝ^7 -> ℝ^2
#!
#! ‣ x6
#! ‣ x7
#!
#! Put Morphism:
#! ------------
#! ℝ^9 -> ℝ^7
#!
#! ‣ x1 + 1
#! ‣ 0.9 * x2 + 0.1 * x8
#! ‣ 0.9 * x3 + 0.1 * x9
#! ‣ 0.999 * x4 + 0.001 * x8 ^ 2
#! ‣ 0.999 * x5 + 0.001 * x9 ^ 2
#! ‣ x6 + 0.01 / (1 - 0.999 ^ x1)
#!   * ((0.9 * x2 + 0.1 * x8) /
#!      (1.e-07 + Sqrt( (0.999 * x4 + 0.001 * x8 ^ 2) / (1 - 0.999 ^ x1) )))
#! ‣ x7 + 0.01 / (1 - 0.999 ^ x1)
#!   * ((0.9 * x3 + 0.1 * x9) /
#!      (1.e-07 + Sqrt( (0.999 * x5 + 0.001 * x9 ^ 2) / (1 - 0.999 ^ x1) )))
#! @EndExample
