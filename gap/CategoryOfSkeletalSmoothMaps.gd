# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#


DeclareCategory( "IsCategoryOfSkeletalSmoothMaps",
        IsCapCategory );

DeclareCategory( "IsObjectInCategoryOfSkeletalSmoothMaps",
        IsCapCategoryObject );

DeclareCategory( "IsMorphismInCategoryOfSkeletalSmoothMaps",
        IsCapCategoryMorphism );


DeclareGlobalFunction( "CategoryOfSkeletalSmoothMaps" );

DeclareGlobalVariable( "SkeletalSmoothMaps" );

DeclareOperation( "SmoothMorphism",
    [ IsCategoryOfSkeletalSmoothMaps, IsObjectInCategoryOfSkeletalSmoothMaps, IsDenseList, IsObjectInCategoryOfSkeletalSmoothMaps ] );

DeclareAttribute( "RankOfObject", IsObjectInCategoryOfSkeletalSmoothMaps );

DeclareAttribute( "Map", IsMorphismInCategoryOfSkeletalSmoothMaps );
DeclareAttribute( "JacobianMatrix", IsMorphismInCategoryOfSkeletalSmoothMaps );

DeclareOperation( "Eval", [ IsMorphismInCategoryOfSkeletalSmoothMaps, IsDenseList ] );
DeclareOperation( "EvalJacobianMatrix", [ IsMorphismInCategoryOfSkeletalSmoothMaps, IsDenseList ] );

DeclareGlobalVariable( "MachineLearningForCAP" );

DeclareGlobalFunction( "DummyInputStringsForAffineTransformation" );
DeclareGlobalFunction( "DummyInputForAffineTransformation" );
DeclareGlobalFunction( "DummyInputStringsForPolynomialTransformation" );
DeclareGlobalFunction( "DummyInputForPolynomialTransformation" );
