# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Implementations
#

##
InstallMethod( Relu,
      [ IsFloat ],
  
  a -> Maximum( a, 0. )
);

##
InstallOtherMethod( Relu,
    [ IsObject ],
  
  function ( a )
    
    return Relu( Float( a ) );
    
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
InstallGlobalFunction( SimplifyExpressionUsingPython,
  
  function ( exps, x )
    local dir, input_path, input_file, output_path, import, symbols, functions, g_ops, p_ops, define_exps, simplify, write_output, stream, err, output_file, outputs, j, i, exp, o;
    
    exps := List( [ 1 .. Length( exps ) ], i -> exps[i] );
    
    dir := DirectoryTemporary( );
    
    input_path := Filename( dir, "expression.py" );
    
    input_file := IO_File( input_path, "w" );
    
    output_path := Filename( dir, "output.txt" );
    
    import := "from sympy import *;\n";
    symbols := Concatenation( JoinStringsWithSeparator( x, ", " ), " = symbols( '", JoinStringsWithSeparator( x, " " ), "' );\n" );
    functions := "max, min = Function('max'), Function('min'); \n";
    
    g_ops := [ "Sin", "Cos", "Log", "Exp", "^", "Sqrt", "AbsoluteValue", "Maximum", "Minimum", "SignFloat" ];
    p_ops := [ "sin", "cos", "log", "exp", "**", "sqrt", "Abs", "max", "min", "sign" ];
    
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

InstallGlobalFunction( JacobianMatrixUsingPython,
  
  function ( exps, x )
    local dir, input_path, input_file, output_path, import, symbols, g_ops, p_ops, define_exps, simplify, write_output, stream, err, output_file, outputs, j, i;
    
    exps := List( [ 1 .. Length( exps ) ], i -> exps[i] );
    
    dir := DirectoryTemporary( );
    
    input_path := Filename( dir, "expression.py" );
    
    input_file := IO_File( input_path, "w" );
    
    output_path := Filename( dir, "output.txt" );
    
    import := "from sympy import *;\n";
    symbols := Concatenation( JoinStringsWithSeparator( x, ", " ), " = symbols( '", JoinStringsWithSeparator( x, " " ), "' );\n" );
    
    g_ops := [ "Sin", "Cos", "Log", "Exp", "^", "Sqrt", "AbsoluteValue" ];
    p_ops := [ "sin", "cos", "log", "exp", "**", "sqrt", "Abs" ];
    
    for j in [ 1 .. Length( exps ) ] do
      for i in [ 1 .. 7 ] do
         exps[j] := ReplacedString( exps[j], g_ops[i], p_ops[i] );
      od;
    od;
    
    define_exps := Concatenation( "exps = Matrix([", JoinStringsWithSeparator( exps, ", " ), "]);\n" );
    
    simplify := Concatenation( "output = exps.jacobian([", JoinStringsWithSeparator( x, "," ), "]);\n" );
    
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
    
    Assert( 0, Length( outputs ) = Length( x ) * Length( exps ) );
    
    for j in [ 1 .. Length( outputs ) ] do
      
      outputs[j] := ReplacedString( outputs[j], "\n", "" );
      
      for i in [ 1 .. 7 ] do
         outputs[j] := ReplacedString( outputs[j], p_ops[i], g_ops[i] );
      od;
      
    od;
    
    outputs := List( [ 0 .. Length( exps ) - 1 ], i -> outputs{[ i * Length( x ) + 1 .. ( i + 1 ) * Length( x ) ]} );
    
    return outputs;
    
end );

InstallGlobalFunction( LaTeXOutputUsingPython,
  
  function ( exps, x )
    local dir, input_path, input_file, output_path, import, symbols, g_ops, p_ops, define_exps, simplify, write_output, stream, err, output_file, outputs, j, i;
    
    exps := List( [ 1 .. Length( exps ) ], i -> exps[i] );
    
    dir := DirectoryTemporary( );
    
    input_path := Filename( dir, "expression.py" );
    
    input_file := IO_File( input_path, "w" );
    
    output_path := Filename( dir, "output.txt" );
    
    import := "from sympy import *;\n";
    symbols := Concatenation( JoinStringsWithSeparator( x, ", " ), " = symbols( '", JoinStringsWithSeparator( x, " " ), "' );\n" );
    
    g_ops := [ "Sin", "Cos", "Log", "Exp", "^", "Sqrt", "AbsoluteValue" ];
    p_ops := [ "sin", "cos", "log", "exp", "**", "sqrt", "Abs" ];
    
    for j in [ 1 .. Length( exps ) ] do
      for i in [ 1 .. 7 ] do
         exps[j] := ReplacedString( exps[j], g_ops[i], p_ops[i] );
      od;
    od;
    
    define_exps := Concatenation( "exps = [", JoinStringsWithSeparator( exps, ", " ), "];\n" );
    
    simplify := "output = [printing.latex(exp) for exp in exps];\n";
    
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
    
    Assert( 0, Length( outputs ) = Length( exps ) );
    
    for j in [ 1 .. Length( outputs ) ] do
      
      outputs[j] := ReplacedString( outputs[j], "\n", "" );
      
    od;
    
    return outputs;
    
end );
