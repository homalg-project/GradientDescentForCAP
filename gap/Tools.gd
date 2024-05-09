# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#


DeclareAttribute( "Relu", IsFloat );
DeclareOperation( "Relu", [ IsFloat ] );

DeclareGlobalFunction( "SimplifyExpressionUsingPython" );
DeclareGlobalFunction( "JacobianMatrixUsingPython" );
DeclareGlobalFunction( "LaTeXOutputUsingPython" );
DeclareGlobalFunction( "KroneckerDelta" );
DeclareOperation( "SimplifyExpressionUsingPython", [ IsDenseList, IsDenseList ] );
DeclareOperation( "JacobianMatrixUsingPython", [ IsDenseList, IsDenseList, IsDenseList ] );
DeclareOperation( "LazyJacobianMatrix", [ IsDenseList, IsDenseList, IsDenseList ] );
DeclareOperation( "LaTeXOutputUsingPython", [ IsDenseList, IsDenseList ] );
DeclareOperation( "LazyDiff", [ IsString, IsDenseList, IsPosInt ] );
