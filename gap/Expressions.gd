# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#

DeclareCategory( "IsExpression", IsObject );


DeclareGlobalFunction( "Expression" );

DeclareAttribute( "Variables", IsExpression );
DeclareAttribute( "String", IsExpression );

DeclareGlobalFunction( "AsListOfExpressions" );
DeclareGlobalFunction( "DummyInput" );
