# SPDX-License-Identifier: GPL-2.0-or-later
# GradientDescentForCAP: Exploring categorical machine learning in CAP
#
# Implementations
#

##
InstallValue( LIST_OF_GLOBAL_CONSTANT_EXPRESSIONS, [ ] );

##
DeclareRepresentation( "IsExpressionRep",
  IsExpression and IsAttributeStoringRep, [ ] );

##
DeclareRepresentation( "IsConstantExpressionRep",
  IsConstantExpression and IsAttributeStoringRep, [ ] );

##
BindGlobal( "TypeOfExpression",
  NewType( NewFamily( "FamilyOfExpressions", IsObject ), IsExpressionRep ) );

##
BindGlobal( "TypeOfConstantExpression",
  NewType( NewFamily( "FamilyOfConstantExpressions", IsObject ), IsConstantExpressionRep ) );

##
InstallMethod( Expression,
          [ IsDenseList, IsString ],
  
  function ( variables, string )
    
    return ObjectifyWithAttributes( rec( ), TypeOfExpression,
                Variables, variables,
                String, string );
    
end );

##
InstallMethod( Expression,
          [ IsString ],
  
  function ( string )
    local constant;
    
    constant := ObjectifyWithAttributes( rec( ), TypeOfConstantExpression,
                      Variables, [ ],
                      String, string );
    
    MakeReadWriteGlobal( string );
    
    DeclareSynonym( string, constant );
    
    Add( LIST_OF_GLOBAL_CONSTANT_EXPRESSIONS, constant );
    
    return constant;
    
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
InstallGlobalFunction( ConvertToExpressions,
  
  variables -> List( variables, var -> Expression( variables, var ) )
);

##
InstallGlobalFunction( ConvertToConstantExpressions,
  
  constants -> List( constants, c -> Expression( c ) )
);

##
InstallMethod( DummyInputStrings,
          [ IsString, IsInt ],
  
  function ( var, r )
    
    return List( [ 1 .. r ], i -> Concatenation( var, String( i ) ) );
    
end );

##
InstallMethod( DummyInput,
          [ IsString, IsInt ],
  
  function ( var, r )
    
    return ConvertToExpressions( DummyInputStrings( var, r ) );
    
end );

##
InstallOtherMethod( DummyInput,
          [ IsString, IsMorphismInCategoryOfSkeletalSmoothMaps ],
  
  function ( var, f )
    
    return DummyInput( var, RankOfObject( Source( f ) ) );
    
end );

##
InstallOtherMethod( DummyInput,
          [ IsMorphismInCategoryOfSkeletalSmoothMaps ],
  
  function ( f )
    
    return DummyInput( "x", f );
    
end );

##
InstallGlobalFunction( AssignExpressions,
  
  function ( vars )
    local func;
    
    func :=
      function ( e )
        local name;
        
        name := String( e );
        
        MakeReadWriteGlobal( name );
        
        DeclareSynonym( name, e );
        
        return true;
        
    end;
    
    List( vars, e -> func( e ) );
    
end );

##
InstallOtherMethod( AsFunction,
          [ IsDenseList, IsString ],
  
  function ( vars, str )
    
    return
      EvalString(
        Concatenation(
          "function( vec )",
          SelectBasedOnCondition( Length( vars ) <> 0, Concatenation( " local ", JoinStringsWithSeparator( vars, ", " ), ";" ), " " ),
          Concatenation( "Assert( 0, IsDenseList( vec ) and Length( vec ) = ", String( Length( vars ) ), " );" ),
          Concatenation( ListN( [ 1 .. Length( vars ) ], i -> Concatenation( vars[i], " := vec[", String( i ), "]; " ) ) ),
          Concatenation( "return ", str, "; end" ) ) );
    
end );

##
InstallMethod( AsFunction,
          [ IsExpression ],
  
  e -> AsFunction( Variables( e ), String( e ) )
);

##
InstallOtherMethod( CallFuncList,
      [ IsExpression, IsDenseList ],
  
  { e, L } -> AsFunction( e )( L[1] )
);

##
InstallOtherMethod( Eval,
      [ IsExpression ],
  
  e -> EvalString( String( e ) )
);

##
InstallOtherMethod( Eval,
      [ IsDenseList ],
  
  l -> List( l, Eval )
);

## Apply Functions on Expressions
##

for op in [ "Sin", "Cos", "Tan", "Cot", "Tanh", "Coth", "Log", "Exp", "Sqrt", "Square", "AbsoluteValue", "SignFloat", "Relu" ] do
  
  ##
  InstallOtherMethod( EvalString( op ),
        [ IsExpression ],
    
    EvalString( ReplacedString( "e -> Expression( Variables( e ), Concatenation( \"op\", \"(\", String( e ), \")\" ) )", "op", op ) )
  );
  
od;

## Operations on Expressions

##
InstallOtherMethod( ZeroImmutable,
        [ IsExpression ],
  
  a -> Expression( Variables( a ), "0" )
);

##
InstallOtherMethod( AdditiveInverseMutable,
        [ IsExpression ],
  
  a -> Expression( Variables( a ), Concatenation( "-(", String( a ), ")" ) )
);

for op in [ "+", "-", "*", "/", "^" ] do
  
  ##
  InstallOtherMethod( EvalString( Concatenation( "\\", op ) ),
          [ IsExpression, IsExpression ],
    
    EvalString( ReplacedString(
      "{ a, b } -> Expression( Unique( Concatenation( Variables( a ), Variables( b ) ) ), Concatenation( \"(\", String( a ), \")op(\", String( b ), \")\" ) )", "op", op ) )
  );
  
  ##
  InstallOtherMethod( EvalString( Concatenation( "\\", op ) ),
          [ IsFloat, IsExpression ],
    
    EvalString( ReplacedString( "{ a, b } -> Expression( Variables( b ), ViewString( a ) ) op b", "op", op ) )
  );
  
  ##
  InstallOtherMethod( EvalString( Concatenation( "\\", op ) ),
          [ IsExpression, IsFloat ],
    
    EvalString( ReplacedString( "{ a, b } -> a op Expression( Variables( a ), ViewString( b ) )", "op", op ) )
  );
  
  ##
  InstallOtherMethod( EvalString( Concatenation( "\\", op ) ),
          [ IsRat, IsExpression ],
    
    EvalString( ReplacedString( "{ a, b } -> Expression( Variables( b ), String( a ) ) op b", "op", op ) )
  );
  
  ##
  InstallOtherMethod( EvalString( Concatenation( "\\", op ) ),
          [ IsExpression, IsRat ],
    
    EvalString( ReplacedString( "{ a, b } -> a op Expression( Variables( a ), String( b ) )", "op", op ) )
  );
  
  ##
  InstallOtherMethod( EvalString( Concatenation( "\\", op ) ),
          [ IsDenseList, IsExpression ],
    
    EvalString( ReplacedString( "{ l, b } -> List( l, a -> a op b )", "op", op ) )
  );
  
  ##
  InstallOtherMethod( EvalString( Concatenation( "\\", op ) ),
          [ IsExpression, IsDenseList ],
    
    EvalString( ReplacedString( "{ a, l } -> List( l, b -> a op b )", "op", op ) )
  );
  
od;
