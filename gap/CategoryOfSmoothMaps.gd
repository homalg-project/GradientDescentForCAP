# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#


DeclareCategory( "IsSmoothCategory",
        IsCapCategory );

DeclareCategory( "IsSmoothCategoryObject",
        IsCapCategoryObject );

DeclareCategory( "IsSmoothCategoryMorphism",
        IsCapCategoryMorphism );


DeclareGlobalFunction( "SmoothCategory" );

DeclareAttribute( "RankOfObject", IsSmoothCategoryObject );

DeclareAttribute( "UnderlyingMaps", IsSmoothCategoryMorphism );
DeclareAttribute( "JacobianMatrix", IsSmoothCategoryMorphism );

DeclareOperation( "Eval", [ IsSmoothCategoryMorphism, IsDenseList ] );
DeclareOperation( "EvalJacobianMatrix", [ IsSmoothCategoryMorphism, IsDenseList ] );
