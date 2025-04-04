# SPDX-License-Identifier: GPL-2.0-or-later
# GradientDescentForCAP: Exploring categorical machine learning in CAP
#
# Declarations
#
# THIS FILE IS AUTOMATICALLY GENERATED, SEE CAP_project/CAP/gap/MethodRecordTools.gi

#! @Chapter operations for machine learning in CAP

#! @Section Add-methods

#! @BeginGroup
#! @Description
#! The arguments are a category $C$ and a function $F$.
#! This operation adds the given function $F$
#! to the category for the basic operation `MultiplicationForMorphisms`.
#! Optionally, a weight (default: 100) can be specified which should roughly correspond
#! to the computational complexity of the function (lower weight = less complex = faster execution).
#! $F: ( alpha, beta ) \mapsto \mathtt{MultiplicationForMorphisms}(alpha, beta)$.
#! @Returns nothing
#! @Arguments C, F
DeclareOperation( "AddMultiplicationForMorphisms",
                  [ IsCapCategory, IsFunction ] );

#! @Arguments C, F, weight
DeclareOperation( "AddMultiplicationForMorphisms",
                  [ IsCapCategory, IsFunction, IsInt ] );
#! @EndGroup


#! @BeginGroup
#! @Description
#! The arguments are a category $C$ and a function $F$.
#! This operation adds the given function $F$
#! to the category for the basic operation `ReverseDifferential`.
#! Optionally, a weight (default: 100) can be specified which should roughly correspond
#! to the computational complexity of the function (lower weight = less complex = faster execution).
#! $F: ( alpha ) \mapsto \mathtt{ReverseDifferential}(alpha)$.
#! @Returns nothing
#! @Arguments C, F
DeclareOperation( "AddReverseDifferential",
                  [ IsCapCategory, IsFunction ] );

#! @Arguments C, F, weight
DeclareOperation( "AddReverseDifferential",
                  [ IsCapCategory, IsFunction, IsInt ] );
#! @EndGroup


#! @BeginGroup
#! @Description
#! The arguments are a category $C$ and a function $F$.
#! This operation adds the given function $F$
#! to the category for the basic operation `ReverseDifferentialWithGivenObjects`.
#! Optionally, a weight (default: 100) can be specified which should roughly correspond
#! to the computational complexity of the function (lower weight = less complex = faster execution).
#! $F: ( source, alpha, range ) \mapsto \mathtt{ReverseDifferentialWithGivenObjects}(source, alpha, range)$.
#! @Returns nothing
#! @Arguments C, F
DeclareOperation( "AddReverseDifferentialWithGivenObjects",
                  [ IsCapCategory, IsFunction ] );

#! @Arguments C, F, weight
DeclareOperation( "AddReverseDifferentialWithGivenObjects",
                  [ IsCapCategory, IsFunction, IsInt ] );
#! @EndGroup

