# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#


DeclareOperation( "Relu", [ IsFloat ] );
DeclareOperation( "Enumerate", [ IsDenseList ] );

DeclareGlobalFunction( "KroneckerDelta" );
DeclareGlobalFunction( "MultiplyMatrices" );

DeclareOperation( "SimplifyExpressionUsingPython", [ IsDenseList, IsDenseList ] );
DeclareOperation( "JacobianMatrixUsingPython", [ IsDenseList, IsDenseList, IsDenseList ] );
DeclareOperation( "LazyJacobianMatrix", [ IsDenseList, IsDenseList, IsDenseList ] );
DeclareOperation( "LaTeXOutputUsingPython", [ IsDenseList, IsDenseList ] );
DeclareOperation( "LazyDiff", [ IsDenseList, IsString, IsPosInt ] );
DeclareOperation( "Diff", [ IsDenseList, IsString, IsPosInt ] );
