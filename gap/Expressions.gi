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
InstallGlobalFunction( ConvertToExpressions,
  
  variables -> List( variables, var -> Expression( variables, var ) )
);

##
InstallMethod( DummyInput,
          [ IsString, IsInt ],
  
  function ( var, r )
    
    return ConvertToExpressions( List( [ 1 .. r ], i -> Concatenation( var, String( i ) ) ) );
    
end );

##
InstallOtherMethod( DummyInput,
          [ IsString, IsMorphismInCategoryOfSmoothMaps ],
  
  function ( var, f )
    
    return DummyInput( var, RankOfObject( Source( f ) ) );
    
end );

##
InstallOtherMethod( DummyInput,
          [ IsMorphismInCategoryOfSmoothMaps ],
  
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
          Concatenation( "function( vec ) local ", JoinStringsWithSeparator( vars, ", " ), ";" ),
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
InstallOtherMethod( AdditiveInverseMutable,
        [ IsExpression ],
  
  a -> Expression( Variables( a ), Concatenation( "-(", String( a ), ")" ) )
);

for op in [ "+", "-", "*", "/", "^" ] do
  
  ##
  InstallOtherMethod( EvalString( Concatenation( "\\", op ) ),
          [ IsExpression, IsExpression ],
    
    EvalString( ReplacedString( "{ a, b } -> Expression( Variables( a ), Concatenation( \"(\", String( a ), \")op(\", String( b ), \")\" ) )", "op", op ) )
  );

  ##
  InstallOtherMethod( EvalString( Concatenation( "\\", op ) ),
          [ IsObject, IsExpression ],
    
    EvalString( ReplacedString( "{ a, b } -> Expression( Variables( b ), String( a ) ) op b", "op", op ) )
  );
  
  ##
  InstallOtherMethod( EvalString( Concatenation( "\\", op ) ),
          [ IsExpression, IsObject ],
    
    EvalString( ReplacedString( "{ a, b } -> a op Expression( Variables( a ), String( b ) )", "op", op ) )
  );
  
od;
