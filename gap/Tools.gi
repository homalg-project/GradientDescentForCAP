# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Implementations
#


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
        return ListWithIdenticalEntries( m_1, ListWithIdenticalEntries( n_2, 0. ) );
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
        [ IsString, IsDenseList, IsPosInt ],
  
  function ( str, vars, i )
    
    Assert( 0, i <= Length( vars ) );
    
    return AsFunction( Expression( vars, JacobianMatrixUsingPython( [ str ], vars, [ i ] )[1][1] ) );
    
end );

##
InstallOtherMethod( Diff,
        [ IsExpression, IsPosInt ],
  
  { e, i } -> Diff( String( e ), Variables( e ), i )
);

##
InstallMethod( LazyDiff,
        [ IsString, IsDenseList, IsPosInt ],
  
  function ( str, vars, i )
    
    Assert( 0, i <= Length( vars ) );
    
    return
      function ( vec )
        local vec_vars;
        
        if not ForAll( vec, IsExpression ) then
            vec := List( vec, e -> Expression( [ ], e ) );
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
          Expression(
            vec_vars,
            Concatenation(
                "Diff( \"",
                str,
                "\", [",
                JoinStringsWithSeparator( List( vars, var -> Concatenation( "\"", var, "\"" ) ), ", " ),
                "], ",
                String( i ),
                ")( [",
                JoinStringsWithSeparator( List( vec, String ), ", " ),
                "] )" ) );
        
      end;
      
end );

##
InstallOtherMethod( LazyDiff,
        [ IsExpression, IsPosInt ],
  
  { e, i } -> LazyDiff( String( e ), Variables( e ), i )
);

##
InstallOtherMethod( LazyDiff,
        [ IsExpression, IsInt ],
  
  { e, i } -> LazyDiff( String( e ), Variables( e ), i )
);

##
InstallMethod( SimplifyExpressionUsingPython,
          [ IsDenseList, IsDenseList ],
  
  function ( exps, vars )
    local dir, input_path, input_file, output_path, import, symbols, functions, g_ops, p_ops, define_exps, simplify, write_output, stream, err, output_file, outputs, j, i, exp, o;
    
    if not ( ForAll( exps, IsString ) and ForAll( vars, IsString ) ) then
        TryNextMethod( );
    fi;
    
    exps := List( [ 1 .. Length( exps ) ], i -> exps[i] );
    
    dir := DirectoryTemporary( );
    
    input_path := Filename( dir, "expression.py" );
    
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
  
  exps -> SimplifyExpressionUsingPython( List( exps, String ), Variables( exps[1] ) )
);

##
InstallMethod( JacobianMatrixUsingPython,
          [ IsDenseList, IsDenseList, IsDenseList ],
  
  function ( exps, vars, indices )
    local dir, input_path, input_file, output_path, import, symbols, g_ops, p_ops, define_exps, simplify, write_output, stream, err, output_file, outputs, j, i;
    
    exps := List( [ 1 .. Length( exps ) ], i -> exps[i] );
    
    dir := DirectoryTemporary( );
    
    input_path := Filename( dir, "expression.py" );
    
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
  
  { exps, indices } -> JacobianMatrixUsingPython( List( exps, String ), Variables( exps[1] ), indices )
);

##
InstallMethod( LazyJacobianMatrix,
          [ IsDenseList, IsDenseList, IsDenseList ],
  
  function ( exps, vars, indices )
    
    return List( exps, exp -> List( indices, i -> LazyDiff( exp, vars, i ) ) );
    
end );

##
InstallOtherMethod( LazyJacobianMatrix,
          [ IsDenseList, IsDenseList ],
  
  { exps, indices } -> LazyJacobianMatrix( List( exps, String ), Variables( exps[1] ), indices )
);

InstallMethod( LaTeXOutputUsingPython,
          [ IsDenseList, IsDenseList ],
  
  function ( exps, vars )
    local dir, input_path, input_file, output_path, import, symbols, functions, g_ops, p_ops, define_exps, simplify, write_output, stream, err, output_file, outputs, j, i;
    
    exps := List( [ 1 .. Length( exps ) ], i -> exps[i] );
    
    dir := DirectoryTemporary( );
    
    input_path := Filename( dir, "expression.py" );
    
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
  
  exps -> LaTeXOutputUsingPython( List( exps, String ), Variables( exps[1] ) )
);

##
InstallOtherMethod( LaTeXOutputUsingPython,
          [ IsExpression ],
  
  e -> LaTeXOutputUsingPython( [ e ] )[1]
);
