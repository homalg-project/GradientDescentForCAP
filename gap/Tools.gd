# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#


DeclareOperation( "Relu", [ IsFloat ] );
DeclareOperation( "Enumerate", [ IsDenseList ] );

DeclareGlobalFunction( "KroneckerDelta" );

DeclareOperation( "SimplifyExpressionUsingPython", [ IsDenseList, IsDenseList ] );
DeclareOperation( "JacobianMatrixUsingPython", [ IsDenseList, IsDenseList, IsDenseList ] );
DeclareOperation( "LazyJacobianMatrix", [ IsDenseList, IsDenseList, IsDenseList ] );
DeclareOperation( "LaTeXOutputUsingPython", [ IsDenseList, IsDenseList ] );
DeclareOperation( "LazyDiff", [ IsString, IsDenseList, IsPosInt ] );
DeclareOperation( "Diff", [ IsString, IsDenseList, IsPosInt ] );
