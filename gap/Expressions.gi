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
  
  function ( variables, string ) # list of variables, string
    
    return ObjectifyWithAttributes( rec( ), TypeOfExpression,
                Variables, variables,
                String, string );
    
end );

##
InstallOtherMethod( ViewString,
      [ IsExpression ],
  
  function ( e )
    local head, tail, str;
    
    head := Concatenation( "function ( ", JoinStringsWithSeparator( Variables( e ), ", " ), " ) return " );
    
    tail := "; end";
    
    str := String( EvalString( Concatenation( head, String( e ), tail ) ) );
    
    return str{[ Length( head ) + 1 .. Length( str ) - 5 ]};
    
end );

##
InstallGlobalFunction( AsListOfExpressions,
  
  variables -> List( variables, var -> Expression( variables, var ) )
);

##
InstallGlobalFunction( DummyInput,
  
  { var, n } -> AsListOfExpressions( List( [ 1 .. n ], i -> Concatenation( var, "", String( i ), "" ) ) )
);

## Apply Functions on Expressions

##
InstallOtherMethod( Sin,
          [ IsExpression ],
  
  e -> Expression( Variables( e ), Concatenation( "Sin(", String( e ), ")" ) )
);

##
InstallOtherMethod( Cos,
          [ IsExpression ],
  
  e -> Expression( Variables( e ), Concatenation( "Cos(", String( e ), ")" ) )
);

##
InstallOtherMethod( Log,
          [ IsExpression ],
  
  e -> Expression( Variables( e ), Concatenation( "Log(", String( e ), ")" ) )
);

##
InstallOtherMethod( Exp,
          [ IsExpression ],
  
  e -> Expression( Variables( e ), Concatenation( "Exp(", String( e ), ")" ) )
);

##
InstallOtherMethod( Sqrt,
          [ IsExpression ],
  
  e -> Expression( Variables( e ), Concatenation( "Sqrt(", String( e ), ")" ) )
);

##
InstallOtherMethod( SignFloat,
          [ IsExpression ],
  
  e -> Expression( Variables( e ), Concatenation( "SignFloat(", String( e ), ")" ) )
);

##
InstallOtherMethod( Relu,
          [ IsExpression ],
  
  e -> Expression( Variables( e ), Concatenation( "Relu(", String( e ), ")" ) )
);

## Binary Operations on Expressions

##
InstallOtherMethod( \+,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Variables( a ), Concatenation( "(", String( a ), ")+(", String( b ), ")" ) )
);

##
InstallOtherMethod( \-,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Variables( a ), Concatenation( "(", String( a ), ")-(", String( b ), ")" ) )
);

##
InstallOtherMethod( \*,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Variables( a ), Concatenation( "(", String( a ), ")*(", String( b ), ")" ) )
);

##
InstallOtherMethod( \/,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Variables( a ), Concatenation( "(", String( a ), ")/(", String( b ), ")" ) )
);

##
InstallOtherMethod( \^,
        [ IsExpression, IsExpression ],
  
  { a, b } -> Expression( Variables( a ), Concatenation( "(", String( a ), ")^(", String( b ), ")" ) )
);

## Operations with Floats

##
InstallOtherMethod( \+,
        [ IsObject, IsExpression ],
  
  { a, b } -> Expression( Variables( b ), String( a ) ) * b
);

##
InstallOtherMethod( \-,
        [ IsObject, IsExpression ],
  
  { a, b } -> Expression( Variables( b ), String( a ) ) - b
);

##
InstallOtherMethod( \*,
        [ IsObject, IsExpression ],
  
  { a, b } -> Expression( Variables( b ), String( a ) ) * b
);

##
InstallOtherMethod( \/,
        [ IsObject, IsExpression ],
  
  { a, b } -> Expression( Variables( b ), String( a ) ) / b
);

##
InstallOtherMethod( \^,
        [ IsObject, IsExpression ],
  
  { a, b } -> Expression( Variables( b ), String( a ) ) ^ b
);

##
InstallOtherMethod( \+,
        [ IsExpression, IsObject ],
  
  { a, b } -> a + Expression( Variables( a ), String( b ) )
);

##
InstallOtherMethod( \-,
        [ IsExpression, IsObject ],
  
  { a, b } -> a - Expression( Variables( a ), String( b ) )
);

##
InstallOtherMethod( \*,
        [ IsExpression, IsObject ],
  
  { a, b } -> a * Expression( Variables( a ), String( b ) )
);

##
InstallOtherMethod( \/,
        [ IsExpression, IsObject ],
  
  { a, b } -> a / Expression( Variables( a ), String( b ) )
);

##
InstallOtherMethod( \^,
        [ IsExpression, IsObject ],
  
  { a, b } -> a ^ Expression( Variables( a ), String( b ) )
);

