#! @Chapter Examples and Tests

#! @Section Category of Parametrised Morphisms

LoadPackage( "MachineLearningForCAP" );

#! @Example
Smooth := CategoryOfSmoothMaps( );
#! Smooth
Para := CategoryOfParametrisedMorphisms( Smooth );
#! CategoryOfParametrisedMorphisms( Smooth )
R1 := Smooth.( 1 );
#! ℝ^1
R2 := Smooth.( 2 );
#! ℝ^2
R3 := Smooth.( 3 );
#! ℝ^3
f := Smooth.Softmax( 3 );
#! ℝ^3 -> ℝ^3
f := MorphismConstructor( Para, R1 / Para, [ R2, f ], R3 / Para );
#! ℝ^1 -> ℝ^3 defined by:
#!
#! Parameter Object:
#! -----------------
#! ℝ^2
#!
#! Parametrised Morphism:
#! ----------------------
#! ℝ^3 -> ℝ^3
Display( f );
#! ℝ^1 -> ℝ^3 defined by:
#!
#! Parameter Object:
#! -----------------
#! ℝ^2
#!
#! Parametrised Morphism:
#! ----------------------
#! ℝ^3 -> ℝ^3
#!
#! ‣ Exp( x1 ) / (Exp( x1 ) + Exp( x2 ) + Exp( x3 ))
#! ‣ Exp( x2 ) / (Exp( x1 ) + Exp( x2 ) + Exp( x3 ))
#! ‣ Exp( x3 ) / (Exp( x1 ) + Exp( x2 ) + Exp( x3 ))
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
#! Parameter Object:
#! -----------------
#! ℝ^2
#!
#! Parametrised Morphism:
#! ----------------------
#! ℝ^3 -> ℝ^3
Display( g );
#! ℝ^1 -> ℝ^3 defined by:
#!
#! Parameter Object:
#! -----------------
#! ℝ^2
#!
#! Parametrised Morphism:
#! ----------------------
#! ℝ^3 -> ℝ^3
#!
#! ‣ Exp( Sqrt( x1 ) ) / (Exp( Sqrt( x1 ) ) + Exp( Cos( x2 ) ) + Exp( x3 ))
#! ‣ Exp( Cos( x2 ) ) / (Exp( Sqrt( x1 ) ) + Exp( Cos( x2 ) ) + Exp( x3 ))
#! ‣ Exp( x3 ) / (Exp( Sqrt( x1 ) ) + Exp( Cos( x2 ) ) + Exp( x3 ))
l := Para.LinearLayer( 3, 2 );
#! ℝ^3 -> ℝ^2 defined by:
#!
#! Parameter Object:
#! -----------------
#! ℝ^8
#!
#! Parametrised Morphism:
#! ----------------------
#! ℝ^11 -> ℝ^2
h := PreCompose( g, l );
#! ℝ^1 -> ℝ^2 defined by:
#!
#! Parameter Object:
#! -----------------
#! ℝ^10
#!
#! Parametrised Morphism:
#! ----------------------
#! ℝ^11 -> ℝ^2
Display( h );
#! ℝ^1 -> ℝ^2 defined by:
#!
#! Parameter Object:
#! -----------------
#! ℝ^10
#!
#! Parametrised Morphism:
#! ----------------------
#! ℝ^11 -> ℝ^2
#!
#! ‣ Exp( Sqrt( x9 ) ) / (Exp( Sqrt( x9 ) ) + Exp( Cos( x10 ) ) + Exp( x11 )) * x1
#! + Exp( Cos( x10 ) ) / (Exp( Sqrt( x9 ) ) + Exp( Cos( x10 ) ) + Exp(x11 )) * x2 +
#! Exp( x11 ) / (Exp( Sqrt( x9 ) ) + Exp( Cos( x10 ) ) + Exp( x11 )) * x3 + x4
#! ‣ Exp( Sqrt( x9 ) ) / (Exp( Sqrt( x9 ) ) + Exp( Cos( x10 ) ) + Exp( x11 )) * x5
#! + Exp( Cos( x10 ) ) / (Exp( Sqrt( x9 ) ) + Exp( Cos( x10 ) ) + Exp(x11 )) * x6 +
#! Exp( x11 ) / (Exp( Sqrt( x9 ) ) + Exp( Cos( x10 ) ) + Exp( x11 )) * x7 + x8
constants := [ 0.91, 0.24, 0.88, 0.59, 0.67, 0.05, 0.85, 0.31, 0.76, 0.04 ];;
r := SmoothMorphism( Smooth, Smooth.( 0 ), constants, Smooth.( 10 ) );
#! ℝ^0 -> ℝ^10
t := ReparametriseMorphism( h, r );
#! ℝ^1 -> ℝ^2 defined by:
#!
#! Parameter Object:
#! -----------------
#! ℝ^0
#!
#! Parametrised Morphism:
#! ----------------------
#! ℝ^1 -> ℝ^2
Display( t );
#! ℝ^1 -> ℝ^2 defined by:
#!
#! Parameter Object:
#! -----------------
#! ℝ^0
#!
#! Parametrised Morphism:
#! ----------------------
#! ℝ^1 -> ℝ^2
#!
#! ‣ 2.39116 / (5.10727 + Exp( x1 )) * 0.91 + 2.71611 / (5.10727 + Exp( x1 )) * 0.24
#!   + Exp( x1 ) / (5.10727 + Exp( x1 )) * 0.88 + 0.59
#! ‣ 2.39116 / (5.10727 + Exp( x1 )) * 0.67 + 2.71611 / (5.10727 + Exp( x1 )) * 0.05
#!   + Exp( x1 ) / (5.10727 + Exp( x1 )) * 0.85 + 0.31
s := SimplifyMorphism( t, infinity );
#! ℝ^1 -> ℝ^2 defined by:
#!
#! Parameter Object:
#! -----------------
#! ℝ^0
#!
#! Parametrised Morphism:
#! ----------------------
#! ℝ^1 -> ℝ^2
Display( s );
#! ℝ^1 -> ℝ^2 defined by:
#!
#! Parameter Object:
#! -----------------
#! ℝ^0
#!
#! Parametrised Morphism:
#! ----------------------
#! ℝ^1 -> ℝ^2
#!
#! ‣ (1.47 * Exp( x1 ) + 5.84111) / (Exp( x1 ) + 5.10727)
#! ‣ (1.16 * Exp( x1 ) + 3.32114) / (Exp( x1 ) + 5.10727)
#! @EndExample
