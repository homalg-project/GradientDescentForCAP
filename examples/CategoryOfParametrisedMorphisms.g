#! @Chapter Examples and Tests

#! @Section Category of Parametrised Morphisms

LoadPackage( "GradientDescentForCAP" );

#! @Example
Smooth := CategoryOfSkeletalSmoothMaps( );
#! SkeletalSmoothMaps
Para := CategoryOfParametrisedMorphisms( Smooth );
#! CategoryOfParametrisedMorphisms( SkeletalSmoothMaps )
R1 := Smooth.( 1 );
#! ℝ^1
R2 := Smooth.( 2 );
#! ℝ^2
R3 := Smooth.( 3 );
#! ℝ^3
R1 / Para;
#! ℝ^1
Para.( 1 );
#! ℝ^1
IsEqualForObjects( Para.( 1 ), R1 / Para );
#! true
f := Smooth.Softmax( 3 );
#! ℝ^3 -> ℝ^3
f := MorphismConstructor( Para, R1 / Para, [ R2, f ], R3 / Para );
#! ℝ^1 -> ℝ^3 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^2
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^3 -> ℝ^3
Display( f );
#! ℝ^1 -> ℝ^3 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^2
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^3 -> ℝ^3
#!
#! ‣ Exp( x1 ) / (Exp( x1 ) + Exp( x2 ) + Exp( x3 ))
#! ‣ Exp( x2 ) / (Exp( x1 ) + Exp( x2 ) + Exp( x3 ))
#! ‣ Exp( x3 ) / (Exp( x1 ) + Exp( x2 ) + Exp( x3 ))
IsWellDefined( f );
#! true
r := DirectProductFunctorial( Smooth, [ Smooth.Sqrt, Smooth.Cos ] );
#! ℝ^2 -> ℝ^2
Display( r );
#! ℝ^2 -> ℝ^2
#!
#! ‣ Sqrt( x1 )
#! ‣ Cos( x2 )
g := ReparametriseMorphism( f, r );
#! ℝ^1 -> ℝ^3 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^2
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^3 -> ℝ^3
Display( g );
#! ℝ^1 -> ℝ^3 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^2
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^3 -> ℝ^3
#!
#! ‣ Exp( Sqrt( x1 ) ) / (Exp( Sqrt( x1 ) ) + Exp( Cos( x2 ) ) + Exp( x3 ))
#! ‣ Exp( Cos( x2 ) ) / (Exp( Sqrt( x1 ) ) + Exp( Cos( x2 ) ) + Exp( x3 ))
#! ‣ Exp( x3 ) / (Exp( Sqrt( x1 ) ) + Exp( Cos( x2 ) ) + Exp( x3 ))
l := Para.AffineTransformation( 3, 2 );
#! ℝ^3 -> ℝ^2 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^8
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^11 -> ℝ^2
h := PreCompose( g, l );
#! ℝ^1 -> ℝ^2 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^10
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^11 -> ℝ^2
Display( h );
#! ℝ^1 -> ℝ^2 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^10
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^11 -> ℝ^2
#!
#! ‣ x1 * (Exp( Sqrt( x9 ) ) / (Exp( Sqrt( x9 ) ) + Exp( Cos( x10 ) ) + Exp( x11 )))
#!   + x2 * (Exp( Cos( x10 ) ) / (Exp( Sqrt( x9 ) ) + Exp( Cos( x10 ) ) + Exp( x11 )))
#!   + x3 * (Exp( x11 ) / (Exp( Sqrt( x9 ) ) + Exp( Cos( x10 ) ) + Exp( x11 ))) + x4
#! ‣ x5 * (Exp( Sqrt( x9 ) ) / (Exp( Sqrt( x9 ) ) + Exp( Cos( x10 ) ) + Exp( x11 )))
#!   + x6 * (Exp( Cos( x10 ) ) / (Exp( Sqrt( x9 ) ) + Exp( Cos( x10 ) ) + Exp( x11 )))
#!   + x7 * (Exp( x11 ) / (Exp( Sqrt( x9 ) ) + Exp( Cos( x10 ) ) + Exp( x11 ))) + x8
constants := [ 0.91, 0.24, 0.88, 0.59, 0.67, 0.05, 0.85, 0.31, 0.76, 0.04 ];;
r := Smooth.Constant( constants );
#! ℝ^0 -> ℝ^10
t := ReparametriseMorphism( h, r );
#! ℝ^1 -> ℝ^2 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^0
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^1 -> ℝ^2
Display( t );
#! ℝ^1 -> ℝ^2 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^0
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^1 -> ℝ^2
#!
#! ‣ 0.91 * (2.39116 / (5.10727 + Exp( x1 ))) + 0.24 * (2.71611 / (5.10727 + Exp( x1 )))
#!   + 0.88 * (Exp( x1 ) / (5.10727 + Exp( x1 ))) + 0.59
#! ‣ 0.67 * (2.39116 / (5.10727 + Exp( x1 ))) + 0.05 * (2.71611 / (5.10727 + Exp( x1 )))
#!   + 0.85 * (Exp( x1 ) / (5.10727 + Exp( x1 ))) + 0.31
s := SimplifyMorphism( t, infinity );
#! ℝ^1 -> ℝ^2 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^0
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^1 -> ℝ^2
Display( s );
#! ℝ^1 -> ℝ^2 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^0
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^1 -> ℝ^2
#!
#! ‣ (1.47 * Exp( x1 ) + 5.84111) / (Exp( x1 ) + 5.10727)
#! ‣ (1.16 * Exp( x1 ) + 3.32114) / (Exp( x1 ) + 5.10727)
iota := NaturalEmbeddingIntoCategoryOfParametrisedMorphisms( Smooth, Para );
#! Natural embedding into category of parametrised morphisms
ApplyFunctor( iota, Smooth.( 1 ) );
#! ℝ^1
psi := ApplyFunctor( iota, Smooth.Sum( 2 ) );
#! ℝ^2 -> ℝ^1 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^0
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^2 -> ℝ^1
Print( DisplayString( psi ) );
#! ℝ^2 -> ℝ^1 defined by:
#!
#! Underlying Object:
#! -----------------
#! ℝ^0
#!
#! Underlying Morphism:
#! -------------------
#! ℝ^2 -> ℝ^1
#!
#! ‣ x1 + x2
#! @EndExample
