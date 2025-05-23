# SPDX-License-Identifier: GPL-2.0-or-later
# GradientDescentForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#

DeclareCategory( "IsCategoryOfLenses",
        IsCapCategory );

#! @Description
#!  The &GAP; category of objects in path categories.
DeclareCategory( "IsObjectInCategoryOfLenses",
        IsCapCategoryObject );

#! @Description
#!  The &GAP; category of morphisms in path categories.
DeclareCategory( "IsMorphismInCategoryOfLenses",
        IsCapCategoryMorphism );

DeclareOperation( "CategoryOfLenses", [ IsCapCategory ] );

DeclareAttribute( "UnderlyingPairOfObjects", IsObjectInCategoryOfLenses );
DeclareAttribute( "UnderlyingPairOfMorphisms", IsMorphismInCategoryOfLenses );
DeclareAttribute( "GetMorphism", IsMorphismInCategoryOfLenses );
DeclareAttribute( "PutMorphism", IsMorphismInCategoryOfLenses );

DeclareOperation( "EmbeddingIntoCategoryOfLenses", [ IsCapCategory, IsCategoryOfLenses ] );
