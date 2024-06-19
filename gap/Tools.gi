# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Implementations
#

InfoPython := NewInfoClass( "InfoPython" );

SetInfoLevel( InfoPython, 0 );

BindGlobal( "GAP_PYTHON_DIC",
  [ [ "Sin", "Cos", "Tan", "Cot", "Tanh", "Coth", "Log", "Exp", "^", "Sqrt", "AbsoluteValue", "Maximum", "Minimum", "SignFloat", "Relu" ],
    [ "sin", "cos", "tan", "cot", "tanh", "coth", "log", "exp", "**", "sqrt", "Abs", "max", "min", "sign", "relu" ] ] );

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
    functions := "max, min, relu = Function('max'), Function('min'), Function('relu'); \n";
    
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
InstallOtherMethod( JacobianMatrix,
          [ IsDenseList, IsFunction, IsDenseList ],
  
  function ( vars, map_func, indices )
    
    return JacobianMatrix( map_func( ConvertToExpressions( vars ) ), indices );
    
end );


##
InstallMethod( LazyJacobianMatrix,
          [ IsDenseList, IsDenseList, IsDenseList ],
  
  function ( vars, exps, indices )
    
    return vec -> List( exps, exp -> List( indices, index -> LazyDiff( vars, exp, index )( vec ) ) );
    
end );

##
InstallOtherMethod( LazyJacobianMatrix,
          [ IsDenseList, IsDenseList ],
  
  { exps, indices } -> LazyJacobianMatrix( Variables( exps[1] ), List( exps, String ), indices )
);

##
InstallOtherMethod( LazyJacobianMatrix,
          [ IsDenseList, IsFunction, IsDenseList ],
  
  function ( vars, map_func, indices )
    local exps;
    
    # dirty hack to prevent evaluating the function each time on a dummy input!
    exps := fail;
    
    return
      function( vec )
         
        if exps = fail then
          exps := map_func( ConvertToExpressions( vars ) );
        fi;
        
        return LazyJacobianMatrix( exps, indices )( vec );
        
      end;
      
end );

InstallMethod( LaTeXOutputUsingPython,
          [ IsDenseList, IsDenseList ],
  
  function ( vars, exps )
    local constants, evaluate, dir, input_path, input_file, output_path, import, symbols, functions, g_ops, p_ops, define_exps, simplify, write_output, stream, err, output_file, outputs, j, i, exp, o;
    
    exps := List( [ 1 .. Length( exps ) ], i -> exps[i] );
    
    constants := List( LIST_OF_GLOBAL_CONSTANT_EXPRESSIONS, String );
    
    evaluate := CAP_INTERNAL_RETURN_OPTION_OR_DEFAULT( "evaluate", true );
    
    evaluate := SelectBasedOnCondition( evaluate, "True", "False" );
    
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
      
      exps[j] := Concatenation( "parse_expr(\"", exps[j], "\", evaluate=", evaluate, ")" );
      
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


## if the gap function is very large then compiling it using cython takes forever ...
##
InstallMethod( AsCythonFunction,
          [ IsDenseList, IsDenseList, IsDenseList ],
  
  function ( vars, function_names, functions )
    local nr_functions, g_ops, p_ops, dir, cython_functions, import, setup_py_path, setup_py, build_py_path, build_py, stream, err, j, i;
    
    nr_functions := Length( functions );
    
    Assert( 0, Length( vars ) = nr_functions and Length( function_names ) = nr_functions );
    
    g_ops := GAP_PYTHON_DIC[1];
    p_ops := GAP_PYTHON_DIC[2];
    
    for j in [ 1 .. nr_functions ] do
       
      for i in [ 1 .. Length( g_ops ) ] do
          functions[j] := ReplacedString( functions[j], g_ops[i], p_ops[i] );
      od;
      
    od;
    
    dir := DirectoryTemporary( );
    
    Info( InfoPython, 1, dir );
    
    cython_functions := Filename( dir, "cython_functions.pyx" );
    
    cython_functions := IO_File( cython_functions, "w" );
    
    import := Concatenation(
                "cimport cython;\n",
                "from sympy import *;\n\n",
                "@cython.boundscheck(False)\n",
                "@cython.wraparound(False)\n\n" );
    
    IO_Write( cython_functions, import );
    
    IO_Write( cython_functions,
          Concatenation(
                  "def relu(double x):\n     ",
                  "return max( x, 0 )",
                  "\n\n" ) );
    
    for i in [ 1 .. nr_functions ] do
      
      IO_Write( cython_functions,
          Concatenation(
                  "def ",
                  Concatenation( function_names[i], "_" ),
                  "(",
                  JoinStringsWithSeparator( List( vars[i], var -> Concatenation( "double ", var ) ), ", " ),
                  "):\n     ",
                  "return ",
                  functions[i],
                  "\n\n" ) );
      
      IO_Write( cython_functions,
          Concatenation(
                  "def ",
                  function_names[i],
                  "(vec):\n     ",
                  "return ",
                  function_names[i],
                  "_(*vec)",
                  "\n\n" ) );
      
    od;
    
    IO_Close( cython_functions );
    
    setup_py_path := Filename( dir, "setup.py" );
    
    setup_py := IO_File( setup_py_path, "w" );
    
    IO_Write( setup_py,
      Concatenation(
            "from setuptools import setup;\n",
            "from Cython.Build import cythonize;\n\n",
            "setup( ext_modules = cythonize(\"cython_functions.pyx\", compiler_directives={'language_level': '3'} ) )" ) );
    
    IO_Close( setup_py );
    
    build_py_path := Filename( dir, "build.py" );
    
    build_py := IO_File( build_py_path, "w" );
    
    IO_Write( build_py,
      Concatenation(
        "import subprocess\n",
        "import os\n\n",
        "working_dir = \"",
        Filename( dir, "" ),
        "\"\n\n",
        "subprocess.run([\"python\", \"setup.py\", \"build_ext\", \"--inplace\"], cwd=working_dir)" ) );
    
    IO_Close( build_py );
    
    stream := IO_Popen3( IO_FindExecutable( "python" ), [ build_py_path ] );
    
    err := Concatenation( IO_ReadLines( stream.stderr ) );
    
    IO_ReadLines( stream.stdout );
    
    IO_Close( stream.stdin );
    
    IO_Close( stream.stdout );
    
    IO_Close( stream.stderr );
    
    if not IsEmpty( err ) then
      
      Error( err, "\n" );
      
    fi;
    
    Print(
      Concatenation( "cd ", Filename( dir, "" ), "\n\n" ),
      "start python!\n\n",
      "from cython_functions import ",
      JoinStringsWithSeparator( function_names, ", " ),
      ";\n\n# ",
      "w = [ ", String( Length( vars[1] ) ), " entries :) ]\n\n# ",
      function_names[1],
      "(w)" );
    
end );

##
BindGlobal( "ScatterPlotUsingPython",
  
  function ( train_points, train_labels, test_points, test_labels )
    local dir, path, file, stream, err, p;
    
    dir := DirectoryTemporary( );
    
    Info( InfoPython, 1, dir );
    
    path := Filename( dir, "plot.py" );
    
    file := IO_File( path, "w" );
    
    IO_Write( file,
      Concatenation(
        "import matplotlib.pyplot as plt\n",
        "import matplotlib.patches as patches\n\n",
        
        "train_points =", String( train_points ), "\n",
        "train_labels =", String( train_labels ), "\n",
        
        "test_points =", String( test_points ), "\n",
        "test_labels =", String( test_labels ), "\n",
        
        "# Convert the train_points to separate x and y lists\n",
        "x1 = [p[0] for p in train_points]\n",
        "y1 = [p[1] for p in train_points]\n",
        
        "x2 = [p[0] for p in test_points]\n",
        "y2 = [p[1] for p in test_points]\n",
        
        "# Create a figure and an axes\n",
        "fig, ax = plt.subplots()\n",
        
        "# Create a scatter plot\n",
        "scatter1 = plt.scatter(x1, y1, c=train_labels, cmap='viridis', marker='v', s=100, label='training data')\n",
        "scatter2 = plt.scatter(x2, y2, c=test_labels, cmap='viridis', marker='o', s=20, label='test data')\n",
       
        "# Set the limits of the plot based on min and max values of x and y\n",
        "plt.xlim(min(x1+x2) - 0.1, max(x1+x2) + 0.1)\n",
        "plt.ylim(min(y1+y2) - 0.1, max(y1+y2) + 0.1)\n",
        
        "plt.xlabel('X-axis')\n",
        "plt.ylabel('Y-axis')\n",
        "plt.title('Scatter Plot using Matplotlib')\n",
        "plt.colorbar(label='Label')\n",
        "plt.legend()\n",
        "plt.show()\n" ) );
    
    IO_Close( file );
    
    stream := IO_Popen3( IO_FindExecutable( "python" ), [ path, "&" ] );
    
    err := Concatenation( IO_ReadLines( stream.stderr ) );
    
    IO_ReadLines( stream.stdout );
    
    IO_Close( stream.stdin );
    
    IO_Close( stream.stdout );
    
    IO_Close( stream.stderr );
    
    if not IsEmpty( err ) then
      
      Error( err, "\n" );
      
    fi;
    
    return true;
    
end );
