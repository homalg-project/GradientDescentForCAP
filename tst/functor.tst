

gap> Smooth := SkeletalSmoothMaps;;
gap> Lenses := CategoryOfLenses( Smooth );;
gap> Para := CategoryOfParametrisedMorphisms( Smooth );;
gap> Para_Lenses := CategoryOfParametrisedMorphisms( Lenses );;
gap> ell := LossMorphismOfNeuralNetwork( Para, 2, [], 1, "IdFunc" );;
gap> dummy_input := ConvertToExpressions( [ "w1", "w2", "b1", "x1", "x2", "y" ] );
[ w1, w2, b1, x1, x2, y ]
gap> Display( ell : dummy_input := dummy_input );
ℝ^3 -> ℝ^1 defined by:

Parameter Object:
-----------------
ℝ^3

Parametrised Morphism:
----------------------
ℝ^6 -> ℝ^1

‣ (w1 * x1 + w2 * x2 + b1 - y) ^ 2 / 1
gap> R := EmbeddingIntoCategoryOfParametrisedMorphisms( Para, Para_Lenses );
Embedding into category of parametrised morphisms
gap> Rf := ApplyFunctor( R, ell );
(ℝ^3, ℝ^3) -> (ℝ^1, ℝ^1) defined by:

Parameter Object:
-----------------
(ℝ^3, ℝ^3)

Parametrised Morphism:
----------------------
(ℝ^6, ℝ^6) -> (ℝ^1, ℝ^1) defined by:

Get Morphism:
----------
ℝ^6 -> ℝ^1

Put Morphism:
----------
ℝ^7 -> ℝ^6
gap> Display( Rf );
(ℝ^3, ℝ^3) -> (ℝ^1, ℝ^1) defined by:

Parameter Object:
-----------------
(ℝ^3, ℝ^3)

Parametrised Morphism:
----------------------
(ℝ^6, ℝ^6) -> (ℝ^1, ℝ^1) defined by:

Get Morphism:
------------
ℝ^6 -> ℝ^1

‣ (x1 * x4 + x2 * x5 + x3 - x6) ^ 2 / 1

Put Morphism:
------------
ℝ^7 -> ℝ^6

‣ x7 * (2 * (x1 * x4 + x2 * x5 + x3 - x6) / 1 * (1 * (x4 * 1 + 0 + 0 + 0 + 0)) + 0)
‣ x7 * (2 * (x1 * x4 + x2 * x5 + x3 - x6) / 1 * (1 * (0 + x5 * 1 + 0 + 0 + 0)) + 0)
‣ x7 * (2 * (x1 * x4 + x2 * x5 + x3 - x6) / 1 * (1 * (0 + 0 + 1 + 0 + 0)) + 0)
‣ x7 * (2 * (x1 * x4 + x2 * x5 + x3 - x6) / 1 * (1 * (0 + 0 + 0 + x1 * 1 + 0)) + 0)
‣ x7 * (2 * (x1 * x4 + x2 * x5 + x3 - x6) / 1 * (1 * (0 + 0 + 0 + 0 + x2 * 1)) + 0)
‣ x7 * (0 + 2 * (x6 - (x1 * x4 + x2 * x5 + x3)) / 1 * 1)
