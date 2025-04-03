# SPDX-License-Identifier: GPL-2.0-or-later
# GradientDescentForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#

DeclareOperation( "OneEpochUpdateLens", [ IsMorphismInCategoryOfParametrisedMorphisms, IsFunction, IsDenseList, IsPosInt ] );
DeclareOperation( "Fit", [ IsMorphismInCategoryOfLenses, IsPosInt, IsDenseList ] );
