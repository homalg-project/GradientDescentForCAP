# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#

DeclareCategory( "IsExpression", IsNearAdditiveElementWithInverse and IsAdditiveElement and IsMultiplicativeElement );
DeclareCategory( "IsConstantExpression", IsExpression );

DeclareOperation( "Expression", [ IsDenseList, IsString ] );
DeclareOperation( "Expression", [ IsString ] );

DeclareAttribute( "Variables", IsExpression );
DeclareAttribute( "String", IsExpression );
DeclareAttribute( "AsFunction", IsExpression );
DeclareOperation( "AsFunction", [ IsDenseList, IsString ] );

DeclareOperation( "DummyInputStrings", [ IsString, IsInt ] );
DeclareOperation( "DummyInput", [ IsString, IsInt ] );

DeclareGlobalFunction( "ConvertToConstantExpressions" );
DeclareGlobalFunction( "ConvertToExpressions" );
DeclareGlobalFunction( "AssignExpressions" );

DeclareGlobalVariable( "LIST_OF_GLOBAL_CONSTANT_EXPRESSIONS" );
