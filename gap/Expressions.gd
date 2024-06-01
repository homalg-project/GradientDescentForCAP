# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#

DeclareCategory( "IsExpression", IsObject );


DeclareGlobalFunction( "Expression" );

DeclareAttribute( "Variables", IsExpression );
DeclareAttribute( "String", IsExpression );
DeclareAttribute( "AsFunction", IsExpression );
DeclareOperation( "AsFunction", [ IsDenseList, IsString ] );

DeclareOperation( "DummyInputStrings", [ IsString, IsInt ] );
DeclareOperation( "DummyInput", [ IsString, IsInt ] );

DeclareGlobalFunction( "ConvertToExpressions" );
DeclareGlobalFunction( "AssignExpressions" );

