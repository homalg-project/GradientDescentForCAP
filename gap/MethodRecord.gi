# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Implementations
#



## CRDC ~ Cartesian Reverse Differential Category
##
InstallValue( CRDC_INTERNAL_METHOD_NAME_RECORD, rec(

ReverseDifferentialWithGivenObjects := rec(
  filter_list := [ "category", "object", "morphism", "object" ],
  input_arguments_names := [ "cat", "source", "alpha", "range" ],
  output_source_getter_string := "source",
  output_range_getter_string := "range",
  return_type := "morphism",
  compatible_with_congruence_of_morphisms := true
),
  
  ReverseDifferential := rec(
  filter_list := [ "category", "morphism", ],
  input_arguments_names := [ "cat", "alpha" ],
  output_source_getter_string := "DirectProduct( cat, [ Source( alpha ), Target( alpha ) ] )",
  output_source_getter_preconditions := [ [ "DirectProduct", 1 ] ],
  output_range_getter_string := "Source( alpha )",
  output_range_getter_preconditions := [ ],
  return_type := "morphism",
  compatible_with_congruence_of_morphisms := true,
),

MultiplicationForMorphisms := rec(
  filter_list := [ "category", "morphism", "morphism" ],
  input_arguments_names := [ "cat", "alpha", "beta" ],
  output_source_getter_string := "Source( alpha )",
  output_source_getter_preconditions := [ ],
  output_range_getter_string := "Range( alpha )",
  output_range_getter_preconditions := [ ],
  
  pre_function := function( cat, morphism_1, morphism_2 )
    
    if not IsEqualForObjects( cat, Source( morphism_1 ), Source( morphism_2 ) ) then
        
        return [ false, "sources are not equal" ];
        
    fi;
        
    if not IsEqualForObjects( cat, Range( morphism_1 ), Range( morphism_2 ) ) then
        
        return [ false, "ranges are not equal" ];
        
    fi;
    
    return [ true ];
    
  end,
  return_type := "morphism",
  compatible_with_congruence_of_morphisms := true,
),

) );

CAP_INTERNAL_ENHANCE_NAME_RECORD( CRDC_INTERNAL_METHOD_NAME_RECORD );

CAP_INTERNAL_GENERATE_DOCUMENTATION_FROM_METHOD_NAME_RECORD(
    CRDC_INTERNAL_METHOD_NAME_RECORD,
    "MachineLearningForCAP",
    "MethodRecord.autogen.gd",
    "operations for machine learning in CAP",
    "Add-methods"
);

CAP_INTERNAL_REGISTER_METHOD_NAME_RECORD_OF_PACKAGE( CRDC_INTERNAL_METHOD_NAME_RECORD, "MachineLearningForCAP" );

CAP_INTERNAL_INSTALL_ADDS_FROM_RECORD( CRDC_INTERNAL_METHOD_NAME_RECORD );
