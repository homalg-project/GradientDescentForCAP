# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#


DeclareCategory( "IsCategoryOfSmoothMaps",
        IsCapCategory );

DeclareCategory( "IsObjectInCategoryOfSmoothMaps",
        IsCapCategoryObject );

DeclareCategory( "IsMorphismInCategoryOfSmoothMaps",
        IsCapCategoryMorphism );


DeclareGlobalFunction( "CategoryOfSmoothMaps" );

DeclareOperation( "SmoothMorphism",
    [ IsCategoryOfSmoothMaps, IsObjectInCategoryOfSmoothMaps, IsDenseList, IsObjectInCategoryOfSmoothMaps ] );

DeclareAttribute( "RankOfObject", IsObjectInCategoryOfSmoothMaps );

DeclareAttribute( "Map", IsMorphismInCategoryOfSmoothMaps );
DeclareAttribute( "JacobianMatrix", IsMorphismInCategoryOfSmoothMaps );

DeclareOperation( "Eval", [ IsMorphismInCategoryOfSmoothMaps, IsDenseList ] );
DeclareOperation( "EvalJacobianMatrix", [ IsMorphismInCategoryOfSmoothMaps, IsDenseList ] );

DeclareGlobalVariable( "MachineLearningForCAP" );

DeclareGlobalFunction( "DummyInputStringsForLinearLayer" );
DeclareGlobalFunction( "DummyInputForLinearLayer" );
