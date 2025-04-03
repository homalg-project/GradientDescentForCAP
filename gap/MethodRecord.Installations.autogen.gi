# SPDX-License-Identifier: GPL-2.0-or-later
# GradientDescentForCAP: Exploring categorical machine learning in CAP
#
# Implementations
#
# THIS FILE IS AUTOMATICALLY GENERATED, SEE CAP_project/CAP/gap/MethodRecordTools.gi

## MultiplicationForMorphisms
InstallMethod( AddMultiplicationForMorphisms,
               [ IsCapCategory, IsFunction ],
               
  function( category, func )
    
    AddCapOperation( "MultiplicationForMorphisms", category, func, -1 );
    
end );

InstallMethod( AddMultiplicationForMorphisms,
               [ IsCapCategory, IsFunction, IsInt ],
               
    FunctionWithNamedArguments(
        [
            [ "IsPrecompiledDerivation", false ],
        ],
        function( CAP_NAMED_ARGUMENTS, category, func, weight )
            
            AddCapOperation( "MultiplicationForMorphisms", category, func, weight : IsPrecompiledDerivation := CAP_NAMED_ARGUMENTS.IsPrecompiledDerivation );
            
        end
    )
);

## ReverseDifferential
InstallMethod( AddReverseDifferential,
               [ IsCapCategory, IsFunction ],
               
  function( category, func )
    
    AddCapOperation( "ReverseDifferential", category, func, -1 );
    
end );

InstallMethod( AddReverseDifferential,
               [ IsCapCategory, IsFunction, IsInt ],
               
    FunctionWithNamedArguments(
        [
            [ "IsPrecompiledDerivation", false ],
        ],
        function( CAP_NAMED_ARGUMENTS, category, func, weight )
            
            AddCapOperation( "ReverseDifferential", category, func, weight : IsPrecompiledDerivation := CAP_NAMED_ARGUMENTS.IsPrecompiledDerivation );
            
        end
    )
);

## ReverseDifferentialWithGivenObjects
InstallMethod( AddReverseDifferentialWithGivenObjects,
               [ IsCapCategory, IsFunction ],
               
  function( category, func )
    
    AddCapOperation( "ReverseDifferentialWithGivenObjects", category, func, -1 );
    
end );

InstallMethod( AddReverseDifferentialWithGivenObjects,
               [ IsCapCategory, IsFunction, IsInt ],
               
    FunctionWithNamedArguments(
        [
            [ "IsPrecompiledDerivation", false ],
        ],
        function( CAP_NAMED_ARGUMENTS, category, func, weight )
            
            AddCapOperation( "ReverseDifferentialWithGivenObjects", category, func, weight : IsPrecompiledDerivation := CAP_NAMED_ARGUMENTS.IsPrecompiledDerivation );
            
        end
    )
);
