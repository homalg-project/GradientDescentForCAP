# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#

DeclareOperation( "OneEpochUpdateLens", [ IsMorphismInCategoryOfParametrisedMorphisms, IsFunction, IsDenseList, IsPosInt ] );
DeclareOperation( "Fit", [ IsMorphismInCategoryOfLenses, IsPosInt, IsDenseList ] );
