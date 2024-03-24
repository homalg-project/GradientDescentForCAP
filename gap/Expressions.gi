# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Implementations
#
DeclareRepresentation( "IsExpressionRep",
  IsExpression and IsAttributeStoringRep, [ ] );

##
BindGlobal( "TypeOfExpression",
  NewType( NewFamily( "FamilyOfExpressions", IsObject ), IsExpressionRep ) );

##
InstallGlobalFunction( Expression,
  
  function ( string )
    
    return ObjectifyWithAttributes( rec( ), TypeOfExpression,
                String, string );
    
end );

##
InstallOtherMethod( ViewString,
      [ IsExpression ],
  
  e -> String( e )
);

##
InstallGlobalFunction( AsListOfExpressions,
  
  list_of_strings -> List( list_of_strings, str -> Expression( str ) )
);

##
InstallGlobalFunction( DummyInput,
  
  { var, n } -> AsListOfExpressions( List( [ 1 .. n ], i -> Concatenation( var, "", String( i ), "" ) ) )
);

## Apply Functions on Expressions

##
InstallOtherMethod( Sin,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "Sin(", String( e ), ")" ) )
);

##
InstallOtherMethod( Cos,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "Cos(", String( e ), ")" ) )
);

##
InstallOtherMethod( Log,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "Log(", String( e ), ")" ) )
);

##
InstallOtherMethod( Exp,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "Exp(", String( e ), ")" ) )
);

##
InstallOtherMethod( Sqrt,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "Sqrt(", String( e ), ")" ) )
);

##
InstallOtherMethod( SignFloat,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "SignFloat(", String( e ), ")" ) )
);

##
InstallOtherMethod( Relu,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "Relu(", String( e ), ")" ) )
);

## Binary Operations on Expressions

##
InstallOtherMethod( \+,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Concatenation( "(", String( a ), ")+(", String( b ), ")" ) )
);

##
InstallOtherMethod( \-,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Concatenation( "(", String( a ), ")-(", String( b ), ")" ) )
);

##
InstallOtherMethod( \*,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Concatenation( "(", String( a ), ")*(", String( b ), ")" ) )
);

##
InstallOtherMethod( \/,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Concatenation( "(", String( a ), ")/(", String( b ), ")" ) )
);

##
InstallOtherMethod( \^,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Concatenation( "(", String( a ), ")^(", String( b ), ")" ) )
);

## Operations with Floats

##
InstallOtherMethod( \+,
        [ IsObject, IsExpression ],
  
  { a, b } -> Expression( String( a ) ) * b
);

##
InstallOtherMethod( \-,
        [ IsObject, IsExpression ],
  
  { a, b } -> Expression( String( a ) ) - b
);

##
InstallOtherMethod( \*,
        [ IsObject, IsExpression ],
  
  { a, b } -> Expression( String( a ) ) * b
);

##
InstallOtherMethod( \/,
        [ IsObject, IsExpression ],
  
  { a, b } -> Expression( String( a ) ) / b
);

##
InstallOtherMethod( \^,
        [ IsObject, IsExpression ],
  
  { a, b } -> Expression( String( a ) ) ^ b
);

##
InstallOtherMethod( \+,
        [ IsExpression, IsObject ],
  
  { a, b } -> a + Expression( String( b ) )
);

##
InstallOtherMethod( \-,
        [ IsExpression, IsObject ],
  
  { a, b } -> a - Expression( String( b ) )
);

##
InstallOtherMethod( \*,
        [ IsExpression, IsObject ],
  
  { a, b } -> a * Expression( String( b ) )
);

##
InstallOtherMethod( \/,
        [ IsExpression, IsObject ],
  
  { a, b } -> a / Expression( String( b ) )
);

##
InstallOtherMethod( \^,
        [ IsExpression, IsObject ],
  
  { a, b } -> a ^ Expression( String( b ) )
);

