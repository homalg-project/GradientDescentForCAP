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
                UnderlyingString, string );
    
end );

##
InstallOtherMethod( ViewString,
      [ IsExpression ],
  
  e -> UnderlyingString( e )
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
  
  e -> Expression( Concatenation( "Sin(", UnderlyingString( e ), ")" ) )
);

##
InstallOtherMethod( Cos,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "Cos(", UnderlyingString( e ), ")" ) )
);

##
InstallOtherMethod( Log,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "Log(", UnderlyingString( e ), ")" ) )
);

##
InstallOtherMethod( Exp,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "Exp(", UnderlyingString( e ), ")" ) )
);

##
InstallOtherMethod( Sqrt,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "Sqrt(", UnderlyingString( e ), ")" ) )
);

##
InstallOtherMethod( SignFloat,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "SignFloat(", UnderlyingString( e ), ")" ) )
);

##
InstallOtherMethod( Relu,
          [ IsExpression ],
  
  e -> Expression( Concatenation( "Relu(", UnderlyingString( e ), ")" ) )
);

## Binary Operations on Expressions

##
InstallOtherMethod( \+,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Concatenation( "(", UnderlyingString( a ), ")+(", UnderlyingString( b ), ")" ) )
);

##
InstallOtherMethod( \-,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Concatenation( "(", UnderlyingString( a ), ")-(", UnderlyingString( b ), ")" ) )
);

##
InstallOtherMethod( \*,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Concatenation( "(", UnderlyingString( a ), ")*(", UnderlyingString( b ), ")" ) )
);

##
InstallOtherMethod( \/,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Concatenation( "(", UnderlyingString( a ), ")/(", UnderlyingString( b ), ")" ) )
);

##
InstallOtherMethod( \^,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Concatenation( "(", UnderlyingString( a ), ")^(", UnderlyingString( b ), ")" ) )
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

