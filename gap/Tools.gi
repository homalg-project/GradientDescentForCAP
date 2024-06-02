# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Implementations
#

InfoPython := NewInfoClass( "InfoPython" );

SetInfoLevel( InfoPython, 0 );

BindGlobal( "GAP_PYTHON_DIC",
  [ [ "Sin", "Cos", "Tan", "Cot", "Tanh", "Coth", "Log", "Exp", "^", "Sqrt", "AbsoluteValue", "Maximum", "Minimum", "SignFloat" ],
    [ "sin", "cos", "tan", "cot", "tanh", "coth", "log", "exp", "**", "sqrt", "Abs", "max", "min", "sign" ] ] );

for op in [ "Sin", "Cos", "Tan", "Cot", "Tanh", "Coth", "Log", "Exp", "Sqrt","Square", "AbsoluteValue", "SignFloat", "Relu" ] do
  
  ## 
  InstallOtherMethod( EvalString( op ),
        [ IsRat ],
    
    EvalString( ReplacedString( "n -> op( Float( n ) )", "op", op ) )
  );
  
od;

##
InstallGlobalFunction( SelectBasedOnCondition,
  
  function ( bool, x, y )
    
    if bool then
        return x;
    else
        return y;
    fi;
    
end );

##
InstallMethod( Relu,
      [ IsFloat ],
  
  a -> Maximum( a, 0. )
);

##
InstallMethod( Enumerate,
          [ IsDenseList ],
  
  function ( l )
    
    return ListN( [ 1 .. Length( l ) ], l, { i, l_i } -> [ i, l_i ] );
    
end );

##
InstallGlobalFunction( MultiplyMatrices,
  
  function ( m_1, mat_1, n_1, m_2, mat_2, n_2 )
    
    Assert( 0, n_1 = m_2 );
    
    if m_1 = 0 or n_2 = 0 then
        return ListWithIdenticalEntries( m_1, [ ] );
    elif n_1 = 0 then
        return ListWithIdenticalEntries( m_1, ListWithIdenticalEntries( n_2, 0 ) );
    else
        return mat_1 * mat_2;
    fi;
    
end );

##
InstallGlobalFunction( KroneckerDelta,
  
  function ( a, b )
    
    if a = b then
        return 1;
    else
        return 0;
    fi;
    
end );

##
InstallMethod( Diff,
        [ IsDenseList, IsString, IsPosInt ],
  
  function ( vars, str, i )
    
    Assert( 0, i <= Length( vars ) );
    
    return x -> JacobianMatrix( vars, [ str ], [ i ] )( x )[1][1];
    
end );

##
InstallOtherMethod( Diff,
        [ IsExpression, IsPosInt ],
  
  { e, i } -> Diff( Variables( e ), String( e ), i )
);

##
InstallMethod( LazyDiff,
        [ IsDenseList, IsString, IsPosInt ],
  
  function ( vars, str, i )
    
    Assert( 0, i <= Length( vars ) );
    
    return
      function ( vec )
        local vec_vars;
        
        if ForAll( vec, e -> IsFloat( e ) or IsRat( e ) ) then
            vec := List( vec, e -> Expression( [ ], String( e ) ) );
        fi;
        
        # obviously
        Assert( 0, IsDenseList( vec ) and Length( vars ) = Length( vec ) );
        
        # all entries of vec must be expressions defined by the same variables
        Assert( 0, Length( Set( List( vec, Variables ) ) ) = 1 );
        
        if not IsEmpty( vec ) then
            vec_vars := Variables( vec[1] );
        else
            vec_vars := [ ];
        fi;
        
        return
          Expression( vec_vars, Concatenation( "Diff( ", String( vars ), ", \"", String( str ), "\" , ", String( i ), ")( ", String( vec ), " )" ) );
        
      end;
      
end );

##
InstallOtherMethod( LazyDiff,
        [ IsExpression, IsPosInt ],
  
  { e, i } -> LazyDiff( Variables( e ), String( e ), i )
);

##
InstallOtherMethod( LazyDiff,
        [ IsExpression, IsInt ],
  
  { e, i } -> LazyDiff( Variables( e ), String( e ), i )
);

##
InstallMethod( SimplifyExpressionUsingPython,
          [ IsDenseList, IsDenseList ],
  
  function ( vars, exps )
    local constants, dir, input_path, input_file, output_path, import, symbols, functions, g_ops, p_ops, define_exps, simplify, write_output, stream, err, output_file, outputs, j, i, exp, o;
    
    if not ( ForAll( exps, IsString ) and ForAll( vars, IsString ) ) then
        TryNextMethod( );
    fi;
    
    constants := List( LIST_OF_GLOBAL_CONSTANT_EXPRESSIONS, String );
    
    vars := Concatenation( vars, constants );
    
    # create a copy
    exps := List( [ 1 .. Length( exps ) ], i -> exps[i] );
    
    dir := DirectoryTemporary( );
    
    input_path := Filename( dir, "expression.py" );
    
    Info( InfoPython, 1, input_path );
    
    input_file := IO_File( input_path, "w" );
    
    output_path := Filename( dir, "output.txt" );
    
    import := "from sympy import *;\n";
    symbols := Concatenation( JoinStringsWithSeparator( vars, ", " ), " = symbols( '", JoinStringsWithSeparator( vars, " " ), "' );\n" );
    functions := "max, min, Relu = Function('max'), Function('min'), Function('Relu'); \n";
    
    g_ops := GAP_PYTHON_DIC[1];
    p_ops := GAP_PYTHON_DIC[2];
    
    for j in [ 1 .. Length( exps ) ] do
      for i in [ 1 .. Length( g_ops ) ] do
         exps[j] := ReplacedString( exps[j], g_ops[i], p_ops[i] );
      od;
    od;
    
    define_exps := Concatenation( "exps = [", JoinStringsWithSeparator( exps, ", " ), "];\n" );
    
    simplify := "output = [simplify(exp) for exp in exps];\n";
    
    write_output := Concatenation( "with open('", output_path, "', 'w') as f:\n     for o in output:\n         f.write(str(o)+'\\n')\n" );
    
    IO_Write( input_file, Concatenation( import, symbols, functions, define_exps, simplify, write_output ) );
    
    IO_Close( input_file );
    
    stream := IO_Popen3( IO_FindExecutable( "python" ), [ input_path ] );
    
    err := Concatenation( IO_ReadLines( stream.stderr ) );
    
    IO_ReadLines( stream.stdout );
    
    IO_Close( stream.stdin );
    
    IO_Close( stream.stdout );
    
    IO_Close( stream.stderr );
    
    if not IsEmpty( err ) then
      
      Error( err, "\n" );
      
    fi;
    
    output_file := IO_File( output_path, "r" );
    
    outputs := IO_ReadLines( output_file );
    
    IO_Close( output_file );
    
    Assert( 0, Length( outputs ) = Length( exps ) );
    
    for j in [ 1 .. Length( outputs ) ] do
      
      outputs[j] := ReplacedString( outputs[j], "\n", "" );
      
      for i in [ 1 .. Length( g_ops ) ] do
         outputs[j] := ReplacedString( outputs[j], p_ops[i], g_ops[i] );
      od;
      
    od;
    
    return outputs;
    
end );

##
InstallOtherMethod( SimplifyExpressionUsingPython,
          [ IsDenseList ],
  
  exps -> SimplifyExpressionUsingPython( Variables( exps[1] ), List( exps, String ) )
);

##
InstallMethod( JacobianMatrixUsingPython,
          [ IsDenseList, IsDenseList, IsDenseList ],
  
  function ( vars, exps, indices )
    local constants, dir, input_path, input_file, output_path, import, symbols, g_ops, p_ops, define_exps, simplify, write_output, stream, err, output_file, outputs, j, i;
    
    constants := List( LIST_OF_GLOBAL_CONSTANT_EXPRESSIONS, String );
    
    vars := Concatenation( vars, constants );
    
    exps := List( [ 1 .. Length( exps ) ], i -> exps[i] );
    
    dir := DirectoryTemporary( );
    
    input_path := Filename( dir, "expression.py" );
    
    Info( InfoPython, 1, input_path );
    
    input_file := IO_File( input_path, "w" );
    
    output_path := Filename( dir, "output.txt" );
    
    import := "from sympy import *;\n";
    symbols := Concatenation( JoinStringsWithSeparator( vars, ", " ), " = symbols( '", JoinStringsWithSeparator( vars, " " ), "' );\n" );
    
    g_ops := GAP_PYTHON_DIC[1];
    p_ops := GAP_PYTHON_DIC[2];
    
    for j in [ 1 .. Length( exps ) ] do
      for i in [ 1 .. Length( g_ops ) ] do
         exps[j] := ReplacedString( exps[j], g_ops[i], p_ops[i] );
      od;
    od;
    
    define_exps := Concatenation( "exps = Matrix([", JoinStringsWithSeparator( exps, ", " ), "]);\n" );
    
    simplify := Concatenation( "output = exps.jacobian([", JoinStringsWithSeparator( vars{indices}, "," ), "]);\n" );
    
    write_output := Concatenation( "with open('", output_path, "', 'w') as f:\n     for o in output:\n         f.write(str(o)+'\\n')\n" );
    
    IO_Write( input_file, Concatenation( import, symbols, define_exps, simplify, write_output ) );
    
    IO_Close( input_file );
    
    stream := IO_Popen3( IO_FindExecutable( "python" ), [ input_path ] );
    
    err := Concatenation( IO_ReadLines( stream.stderr ) );
    
    IO_ReadLines( stream.stdout );
    
    IO_Close( stream.stdin );
    
    IO_Close( stream.stdout );
    
    IO_Close( stream.stderr );
    
    if not IsEmpty( err ) then
      
      Error( err, "\n" );
      
    fi;
    
    output_file := IO_File( output_path, "r" );
    
    outputs := IO_ReadLines( output_file );
    
    IO_Close( output_file );
    
    Assert( 0, Length( outputs ) = Length( indices ) * Length( exps ) );
    
    for j in [ 1 .. Length( outputs ) ] do
      
      outputs[j] := ReplacedString( outputs[j], "\n", "" );
      
      for i in [ 1 .. Length( g_ops ) ] do
         outputs[j] := ReplacedString( outputs[j], p_ops[i], g_ops[i] );
      od;
      
    od;
    
    outputs := List( [ 0 .. Length( exps ) - 1 ], i -> outputs{[ i * Length( indices ) + 1 .. ( i + 1 ) * Length( indices ) ]} );
    
    return outputs;
    
end );

##
InstallOtherMethod( JacobianMatrixUsingPython,
          [ IsDenseList, IsDenseList ],
  
  { exps, indices } -> JacobianMatrixUsingPython( Variables( exps[1] ), List( exps, String ), indices )
);

##
InstallOtherMethod( JacobianMatrix,
          [ IsDenseList, IsDenseList, IsDenseList ],
  
  function ( vars, exps, indices )
    local jacobian_matrix;
    
    jacobian_matrix := JacobianMatrixUsingPython( vars, exps, indices );
    
    jacobian_matrix :=
      Concatenation( "[", JoinStringsWithSeparator( List( jacobian_matrix, row -> Concatenation( "[", JoinStringsWithSeparator( row, ", " ), "]" ) ), ", " ), "]" );
    
    return AsFunction( vars, jacobian_matrix );
    
end );

##
InstallOtherMethod( JacobianMatrix,
        [ IsDenseList, IsDenseList ],
  
  function ( exps, indices )
    
    if not ForAll( exps, IsExpression ) then
        TryNextMethod( );
    fi;
    
    return JacobianMatrix( Variables( exps[1] ), List( exps, String ), indices );
    
end );

##
InstallMethod( LazyJacobianMatrix,
          [ IsDenseList, IsDenseList, IsDenseList ],
  
  function ( vars, exps, indices )
    
    return x -> List( exps, exp -> List( indices, index -> LazyDiff( vars, exp, index )( x ) ) );
    
end );

##
InstallOtherMethod( LazyJacobianMatrix,
          [ IsDenseList, IsDenseList ],
  
  { exps, indices } -> LazyJacobianMatrix( Variables( exps[1] ), List( exps, String ), indices )
);

InstallMethod( LaTeXOutputUsingPython,
          [ IsDenseList, IsDenseList ],
  
  function ( vars, exps )
    local constants, evaluate, dir, input_path, input_file, output_path, import, symbols, functions, g_ops, p_ops, define_exps, simplify, write_output, stream, err, output_file, outputs, j, i, exp, o;
    
    exps := List( [ 1 .. Length( exps ) ], i -> exps[i] );
    
    constants := List( LIST_OF_GLOBAL_CONSTANT_EXPRESSIONS, String );
    
    vars := Concatenation( vars, constants );
    
    dir := DirectoryTemporary( );
    
    input_path := Filename( dir, "expression.py" );
    
    Info( InfoPython, 1, input_path );
    
    input_file := IO_File( input_path, "w" );
    
    output_path := Filename( dir, "output.txt" );
    
    import := "from sympy import *;\n";
    symbols := Concatenation( JoinStringsWithSeparator( vars, ", " ), " = symbols( '", JoinStringsWithSeparator( vars, " " ), "' );\n" );
    functions := "max, min, Relu = Function('max'), Function('min'), Function('Relu'); \n";
    
    g_ops := GAP_PYTHON_DIC[1];
    p_ops := GAP_PYTHON_DIC[2];
    
    for j in [ 1 .. Length( exps ) ] do
      for i in [ 1 .. Length( g_ops ) ] do
         exps[j] := ReplacedString( exps[j], g_ops[i], p_ops[i] );
      od;
    od;
    
    define_exps := Concatenation( "exps = [", JoinStringsWithSeparator( exps, ", " ), "];\n" );
    
    simplify := "output = [printing.latex(exp) for exp in exps];\n";
    
    write_output := Concatenation( "with open('", output_path, "', 'w') as f:\n     for o in output:\n         f.write(str(o)+'\\n')\n" );
    
    IO_Write( input_file, Concatenation( import, symbols, functions, define_exps, simplify, write_output ) );
    
    IO_Close( input_file );
    
    stream := IO_Popen3( IO_FindExecutable( "python" ), [ input_path ] );
    
    err := Concatenation( IO_ReadLines( stream.stderr ) );
    
    IO_ReadLines( stream.stdout );
    
    IO_Close( stream.stdin );
    
    IO_Close( stream.stdout );
    
    IO_Close( stream.stderr );
    
    if not IsEmpty( err ) then
      
      Error( err, "\n" );
      
    fi;
    
    output_file := IO_File( output_path, "r" );
    
    outputs := IO_ReadLines( output_file );
    
    IO_Close( output_file );
    
    Assert( 0, Length( outputs ) = Length( exps ) );
    
    for j in [ 1 .. Length( outputs ) ] do
      
      outputs[j] := ReplacedString( outputs[j], "\n", "" );
      
    od;
    
    return outputs;
    
end );

##
InstallOtherMethod( LaTeXOutputUsingPython,
          [ IsDenseList ],
  
  exps -> LaTeXOutputUsingPython( Variables( exps[1] ), List( exps, String ) )
);

##
InstallOtherMethod( LaTeXOutputUsingPython,
          [ IsExpression ],
  
  e -> LaTeXOutputUsingPython( [ e ] )[1]
);
