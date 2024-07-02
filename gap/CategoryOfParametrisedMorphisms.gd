# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
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

DeclareAttribute( "ParameterObject", IsMorphismInCategoryOfParametrisedMorphisms );
DeclareAttribute( "ParametrisedMorphism", IsMorphismInCategoryOfParametrisedMorphisms );

DeclareOperation( "ReparametriseMorphism", [ IsMorphismInCategoryOfParametrisedMorphisms, IsCapCategoryMorphism ] );
DeclareOperation( "SwitchSourceAndParameterObject", [ IsMorphismInCategoryOfParametrisedMorphisms ] );

DeclareOperation( "AdjustToBatchSize", [ IsMorphismInCategoryOfParametrisedMorphisms, IsInt ] );
DeclareOperation( "NaturalEmbeddingIntoCategoryOfParametrisedMorphisms", [ IsCapCategory, IsCategoryOfParametrisedMorphisms ] );
DeclareOperation( "EmbeddingIntoCategoryOfParametrisedMorphisms", [ IsCategoryOfParametrisedMorphisms, IsCategoryOfParametrisedMorphisms ] );

