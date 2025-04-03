# SPDX-License-Identifier: GPL-2.0-or-later
# GradientDescentForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#



DeclareOperation( "LogitsMorphismOfNeuralNetwork", [ IsCategoryOfParametrisedMorphisms, IsPosInt, IsDenseList, IsPosInt ] );
DeclareOperation( "PredictionMorphismOfNeuralNetwork", [ IsCategoryOfParametrisedMorphisms, IsPosInt, IsDenseList, IsPosInt, IsString ] );
DeclareOperation( "LossMorphismOfNeuralNetwork", [ IsCategoryOfParametrisedMorphisms, IsPosInt, IsDenseList, IsPosInt, IsString ] );
