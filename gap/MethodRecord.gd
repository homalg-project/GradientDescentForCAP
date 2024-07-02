# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#



DeclareGlobalVariable( "CRDC_INTERNAL_METHOD_NAME_RECORD" );

DeclareOperation( "ReverseDifferentialWithGivenObjects",
      [ IsCapCategoryObject, IsCapCategoryMorphism, IsCapCategoryObject ] );

DeclareAttribute( "ReverseDifferential", IsCapCategoryMorphism );

DeclareOperation( "MultiplicationForMorphisms", [ IsCapCategoryMorphism, IsCapCategoryMorphism ] );
