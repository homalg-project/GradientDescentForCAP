# SPDX-License-Identifier: GPL-2.0-or-later
# GradientDescentForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#


DeclareCategory( "IsCategoryOfParametrisedMorphisms",
        IsCapCategory );

DeclareCategory( "IsObjectInCategoryOfParametrisedMorphisms",
        IsCapCategoryObject );

DeclareCategory( "IsMorphismInCategoryOfParametrisedMorphisms",
        IsCapCategoryMorphism );


DeclareOperation( "CategoryOfParametrisedMorphisms", [ IsCapCategory ] );

DeclareAttribute( "UnderlyingCategory", IsCategoryOfParametrisedMorphisms );

DeclareAttribute( "UnderlyingObject", IsObjectInCategoryOfParametrisedMorphisms );

DeclareAttribute( "UnderlyingObject", IsMorphismInCategoryOfParametrisedMorphisms );
DeclareAttribute( "UnderlyingMorphism", IsMorphismInCategoryOfParametrisedMorphisms );

DeclareOperation( "ReparametriseMorphism", [ IsMorphismInCategoryOfParametrisedMorphisms, IsCapCategoryMorphism ] );
DeclareOperation( "SwitchSourceAndUnderlyingObject", [ IsMorphismInCategoryOfParametrisedMorphisms ] );

DeclareOperation( "AdjustToBatchSize", [ IsMorphismInCategoryOfParametrisedMorphisms, IsInt ] );
DeclareOperation( "NaturalEmbeddingIntoCategoryOfParametrisedMorphisms", [ IsCapCategory, IsCategoryOfParametrisedMorphisms ] );
DeclareOperation( "EmbeddingIntoCategoryOfParametrisedMorphisms", [ IsCategoryOfParametrisedMorphisms, IsCategoryOfParametrisedMorphisms ] );

