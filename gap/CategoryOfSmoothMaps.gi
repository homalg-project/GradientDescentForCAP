# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Implementations
#


InstallGlobalFunction( CategoryOfSmoothMaps,
  
  function ( )
    local name, Smooth;
    
    name := "Smooth";
    
    Smooth := CreateCapCategory( name,
                  IsCategoryOfSmoothMaps,
                  IsObjectInCategoryOfSmoothMaps,
                  IsMorphismInCategoryOfSmoothMaps,
                  IsCapCategoryTwoCell
                  : overhead := false
                  );
    
    Smooth!.is_computable := false;
    
    SetIsCartesianCategory( Smooth, true );
    SetIsStrictMonoidalCategory( Smooth, true );
    SetIsSymmetricMonoidalCategory( Smooth, true );
    SetIsLinearCategoryOverCommutativeRing( Smooth, true );
    
    ## Usually we need the field of reals or floats
    SetCommutativeRingOfLinearCategory( Smooth, Rationals );
    
    ##
    AddObjectConstructor( Smooth,
      
      function ( Smooth, n )
        
        return CreateCapCategoryObjectWithAttributes( Smooth,
                    RankOfObject, n );
        
    end );
    
    ##
    AddObjectDatum( Smooth,
      
      function ( Smooth, V )
        
        return RankOfObject( V );
        
    end );
    
    ##
    AddIsWellDefinedForObjects( Smooth,
      
      function ( Smooth, V )
        
        return RankOfObject( V ) >= 0;
        
    end );
    
    ##
    AddIsEqualForObjects( Smooth,
      
      function ( Smooth, U, V )
        
        return RankOfObject( U ) = RankOfObject( V );
        
    end );
    
    # f:R^m -> R^n is defined by a list of functions [f_1:R^m -> R, ..., f_n:R^m -> R]
    AddMorphismConstructor( Smooth,
      
      function ( Smooth, source, datum, target )
        local maps, jacobian_matrix;
        
        if Length( datum[1] ) <> RankOfObject( target ) then
            Error( "wrong input: the length of 'datum[1]' must be equal to the rank of 'target'!\n" );
        fi;
        
        if Length( datum[2] ) <> RankOfObject( target ) then
            Error( "wrong input: the length of 'datum[2]' must be equal to the rank of 'target'!\n" );
        fi;
        
        if not ForAll( datum[2], d -> Length( d ) = RankOfObject( source ) ) then
            Error( "wrong input: the length of each row in 'datum[2]' must be equal to the rank of 'source'!\n" );
        fi;
        
        maps := datum[1];
        jacobian_matrix := datum[2];
        
        return CreateCapCategoryMorphismWithAttributes( Smooth,
                  source, target,
                  UnderlyingMaps, maps,
                  JacobianMatrix, jacobian_matrix );
        
    end );
    
    ##
    AddMorphismDatum( Smooth,
      
      function ( Smooth, f )
        
        return Pair( UnderlyingMaps( f ), JacobianMatrix( f ) );
        
    end );
    
    ##
    AddIsWellDefinedForMorphisms( Smooth,
      
      function ( Smooth, f )
        local maps, jacobian_matrix;
        
        maps := UnderlyingMaps( f );
        jacobian_matrix := JacobianMatrix( f );
        
        return Length( maps ) = RankOfObject( Target( f ) ) and
                Length( jacobian_matrix ) = RankOfObject( Target( f ) ) and
                  ForAll( jacobian_matrix, df -> Length( df ) = RankOfObject( Source( f ) ) );
        
    end );
    
    ##
    InstallOtherMethod( IsCongruentForMorphisms,
              [ IsCategoryOfSmoothMaps, IsMorphismInCategoryOfSmoothMaps, IsMorphismInCategoryOfSmoothMaps ],
      
      function ( Smooth, f, g )
        local rank_S, 100_random_inputs;
        
        rank_S := RankOfObject( Source( f ) );
        
        100_random_inputs := List( [ 1 .. 100 ], i -> List( [ 1 .. rank_S ], j -> 0.01 * Random( [ 1 .. 100 ] ) ) );
        
        # Maybe:
        return ForAll( 100_random_inputs, x -> ForAll( ListN( Eval( f, x ), Eval( g, x ), { a, b } -> a - b < 1.e-10 ), IdFunc ) );
        
    end );
    
    ##
    InstallOtherMethod( IsEqualForMorphisms,
              [ IsCategoryOfSmoothMaps, IsMorphismInCategoryOfSmoothMaps, IsMorphismInCategoryOfSmoothMaps ],
      
      function ( Smooth, f, g )
        
        return UnderlyingMaps( f ) = UnderlyingMaps( g ) and JacobianMatrix( f ) = JacobianMatrix( g );
        
    end );
    
    ##
    AddIdentityMorphism( Smooth,

      function ( Smooth, V )
        local rank_V, maps, jacobian_matrix;

        rank_V := RankOfObject( V );

        maps := List( [ 1 .. rank_V ], i -> x -> x[i] );

        jacobian_matrix :=
          List( [ 1 .. rank_V ],
            i -> List( [ 1 .. rank_V ],
              function ( j )
                if i = j then
                    return x -> 1.;
                else
                    return x -> 0.;
                fi;
              end ) );

        return MorphismConstructor( Smooth, V, Pair( maps, jacobian_matrix ), V );

    end );

    ##       f        g
    ## R^s ----> R^m ----> R^t
    ##
    ## J(gof)_{x} = J(g)_{f(x)} * J(f)_{x} where * is the usual matrix multiplication
    ##
    AddPreCompose( Smooth,

      function ( Smooth, f, g )
        local f_maps, g_maps, maps, jacobian_matrix_f, jacobian_matrix_g, jacobian_matrix;

        f_maps := UnderlyingMaps( f );
        g_maps := UnderlyingMaps( g );

        maps := List( [ 1 .. RankOfObject( Target( g ) ) ], i -> x -> g_maps[i]( List( f_maps, m -> m( x ) ) ) );

        jacobian_matrix_f := JacobianMatrix( f );
        jacobian_matrix_g := JacobianMatrix( g );

        # chain rule for jacobian matrices
        jacobian_matrix :=
              List( [ 1 .. RankOfObject( Target( g ) ) ],
                i -> List( [ 1 .. RankOfObject( Source( f ) ) ],
                  j -> x -> Sum( [ 1 .. RankOfObject( Target( f ) ) ],
                    k ->  jacobian_matrix_g[i][k]( List( f_maps, m -> m( x ) ) ) * jacobian_matrix_f[k][j](x) ) ) );

        return MorphismConstructor( Smooth, Source( f ), Pair( maps, jacobian_matrix ), Target( g ) );

    end );
    
    ##
    AddSumOfMorphisms( Smooth,
      
      function ( Smooth, S, morphisms, T )
        local rank_S, rank_T, nr_morphisms, underlying_maps, maps, underlying_jacobian_matrices, jacobian_matrix;
        
        rank_S := RankOfObject( S );
        rank_T := RankOfObject( T );
        
        nr_morphisms := Length( morphisms );
        
        underlying_maps := List( morphisms, f -> UnderlyingMaps( f ) );
        maps := List( [ 1 .. rank_T ], i -> x -> Sum( List( [ 1 .. nr_morphisms ], k -> underlying_maps[k][i]( x ) ) ) );
        
        underlying_jacobian_matrices := List( morphisms, f -> JacobianMatrix( f ) );
        
        jacobian_matrix :=
          List( [ 1 .. rank_T ], i ->
            List( [ 1 .. rank_S ], j ->
              x -> Sum( List( [ 1 .. nr_morphisms ], k -> underlying_jacobian_matrices[k][i][j]( x ) ) ) ) );
        
        return MorphismConstructor( Smooth, S, Pair( maps, jacobian_matrix ), T );
        
    end );
    
    ## f, g: R^m -> R^n
    ##
    ## J_fg(x) = Diag(g_1(x), g_2(x), ..., g_n(x)) * J_f(x) + Diag(f_1(x), f_2(x), ..., f_n(x)) * J_g(x)
    ##
    AddMultiplicationForMorphisms( Smooth,
      
      function ( Smooth, f, g )
        local rank_S, rank_T, underlying_maps_f, underlying_maps_g, jacobian_matrix_f, jacobian_matrix_g, maps, jacobian_matrix;
        
        rank_S := RankOfObject( Source( f ) );
        rank_T := RankOfObject( Target( f ) );
        
        underlying_maps_f := UnderlyingMaps( f );
        underlying_maps_g := UnderlyingMaps( g );
        
        jacobian_matrix_f := JacobianMatrix( f );
        jacobian_matrix_g := JacobianMatrix( g );
        
        maps := List( [ 1 .. rank_T ], i -> x -> underlying_maps_f[i](x) * underlying_maps_g[i](x) );
        
        jacobian_matrix :=
          List( [ 1 .. rank_T ], i ->
            List( [ 1 .. rank_S ], j ->
              x -> underlying_maps_g[i](x) * jacobian_matrix_f[i][j](x) + underlying_maps_f[i](x) * jacobian_matrix_g[i][j](x) ) );
        
        return MorphismConstructor( Smooth, Source( f ), Pair( maps, jacobian_matrix ), Target( f ) );
        
    end );
     
    ##
    AddSimplifyMorphism( Smooth,
      
      function ( Smooth, f, i )
        local V, W, rank_V, rank_W, x, all, maps, jacobian_matrix;
        
        V := Source( f );
        W := Target( f );
        
        rank_V := RankOfObject( V );
        rank_W := RankOfObject( W );
        
        all := JoinStringsWithSeparator(
                    SimplifyExpressionUsingPython(
                      List( Concatenation( Eval( f ), Concatenation( EvalJacobianMatrix( f ) ) ), String ),
                      List( DummyInput( "x", rank_V ), String ) ),
                    "&" );
        
        for i in [ 1 .. rank_V ] do
            all := ReplacedString( all, Concatenation( "x", String( i ) ), Concatenation( "x[", String( i ), "]" ) );
        od;
        
        all := SplitString( all, "&" );
        
        maps := List( all{[ 1 .. rank_W ]}, m -> EvalString( Concatenation( "x -> ", m ) ) );
        
        jacobian_matrix := List( all{[rank_W + 1 .. rank_W + rank_W * rank_V ]}, m -> EvalString( Concatenation( "x -> ", m ) ) );
        
        jacobian_matrix := List( [ 0 .. rank_W - 1 ], i -> jacobian_matrix{[ i * rank_V + 1 .. ( i + 1 ) * rank_V ]} );
        
        return MorphismConstructor( Smooth, V, Pair( maps, jacobian_matrix ), W );
        
    end );
    
    Finalize( Smooth );
    
    return Smooth;
    
end );

##
InstallMethod( Eval,
          [ IsMorphismInCategoryOfSmoothMaps, IsDenseList ],
  
  function ( f, x )
    local rank;
    
    rank := RankOfObject( Source( f ) );
    
    if Length( x ) <> rank then
        Error( "the input must be a list of length ", rank, "!\n" );
    fi;
    
    return List( UnderlyingMaps( f ), m -> m( x ) );
    
end );

##
InstallOtherMethod( Eval,
        [ IsMorphismInCategoryOfSmoothMaps ],
  
  f -> Eval( f, DummyInput( "x", RankOfObject( Source( f ) ) ) )
);

##
InstallMethod( EvalJacobianMatrix,
          [ IsMorphismInCategoryOfSmoothMaps, IsDenseList ],
  
  function ( f, x )
    local rank;
    
    rank := RankOfObject( Source( f ) );
    
    if Length( x ) <> rank then
        Error( "the input must be a list of length ", rank, "!\n" );
    fi;
    
    return List( JacobianMatrix( f ), df -> List( df, m -> m( x ) ) );
    
end );

##
InstallOtherMethod( EvalJacobianMatrix,
        [ IsMorphismInCategoryOfSmoothMaps ],
  
  f -> EvalJacobianMatrix( f, DummyInput( "x", RankOfObject( Source( f ) ) ) )
);

##
InstallMethod( SmoothMorphism,
          [ IsCategoryOfSmoothMaps, IsObjectInCategoryOfSmoothMaps, IsDenseList, IsObjectInCategoryOfSmoothMaps ],
  
  MorphismConstructor
);

##
InstallMethod( SmoothMorphism,
          [ IsCategoryOfSmoothMaps, IsObjectInCategoryOfSmoothMaps, IsDenseList, IsObjectInCategoryOfSmoothMaps ],
  
  function ( Smooth, S, maps, T )
    local rank_S, rank_T, vars, all, jacobian_matrix, i;
    
    if not ForAll( maps, IsString ) then
        TryNextMethod( );
    fi;
    
    rank_S := RankOfObject( S );
    rank_T := RankOfObject( T );
    
    vars := List( [ 1 .. RankOfObject( S ) ], i -> Concatenation( "x", String( i ) ) );
    
    all := JoinStringsWithSeparator(
              Concatenation( maps, Concatenation( JacobianMatrixUsingPython( maps, vars ) ) ), "&" );
    
    for i in [ 1 .. rank_S ] do
        all := ReplacedString( all, Concatenation( "x", String( i ) ), Concatenation( "x[", String( i ), "]" ) );
    od;
    
    all := SplitString( all, "&" );
    
    maps := List( all{[ 1 .. rank_T ]}, m -> EvalString( Concatenation( "x -> ", m ) ) );
    
    jacobian_matrix := List( all{[rank_T + 1 .. rank_T + rank_T * rank_S ]}, m -> EvalString( Concatenation( "x -> ", m ) ) );
    
    jacobian_matrix := List( [ 0 .. rank_T - 1 ], i -> jacobian_matrix{[ i * rank_S + 1 .. ( i + 1 ) * rank_S ]} );
    
    return MorphismConstructor( Smooth, S, Pair( maps, jacobian_matrix ), T );
    
end );

##
InstallOtherMethod( \.,
          [ IsCategoryOfSmoothMaps, IsPosInt ],
  
  function ( Smooth, string_as_int )
    local i;
    
    i := Int( NameRNam( string_as_int ) );
    
    if i = fail then
        
        TryNextMethod( );
        
    fi;
    
    return ObjectConstructor( Smooth, i );
    
end );

##
InstallOtherMethod( \.,
          [ IsCategoryOfSmoothMaps, IsPosInt ],
  
  function ( Smooth, string_as_int )
    local f, n, maps, jacobian_matrix;
    
    f := NameRNam( string_as_int );
    
    if Int( f ) <> fail then
        
        TryNextMethod( );
        
    fi;
    
    if Float( f ) <> fail then
        
        return MorphismConstructor( Smooth, Smooth.0, [ [ x -> Float( f ) ], [ [ ] ] ], Smooth.1 );
        
    elif f = "Sqrt" then
        
        return MorphismConstructor( Smooth, Smooth.1, [ [ x -> Sqrt(x[1]) ], [ [ x -> 1. / (2. * Sqrt(x[1])) ] ] ], Smooth.1 );
        
    elif f = "Exp" then
        
        return MorphismConstructor( Smooth, Smooth.1, [ [ x -> Exp(x[1]) ], [ [ x -> Exp(x[1]) ] ] ], Smooth.1 );
        
    elif f = "Log" then
        
        return MorphismConstructor( Smooth, Smooth.1, [ [ x -> Log(x[1]) ], [ [ x -> (1. / x[1]) ] ] ], Smooth.1 );
        
    elif f = "Sin" then
        
        return MorphismConstructor( Smooth, Smooth.1, [ [ x -> Sin(x[1]) ], [ [ x -> Cos(x[1]) ] ] ], Smooth.1 );
        
    elif f = "Cos" then
        
        return MorphismConstructor( Smooth, Smooth.1, [ [ x -> Cos(x[1]) ], [ [ x -> -1. * Sin(x[1]) ] ] ], Smooth.1 );
        
    elif f = "Relu" then
        
        return MorphismConstructor( Smooth,
                  Smooth.1,
                  [ [ x -> Relu( x[1] ) ],
                    [ [ x -> 0.5 * (1. + SignFloat( x[1] + 1.e-50 )) ] ] ],
                  Smooth.1 );
        
    elif f{[ 1 .. Minimum( 3, Length( f ) ) ]} = "Sum" then
        
        n := Int( f{[ 4 .. Length( f ) ]} );
        
        maps := [ x -> Sum( x ) ];
        
        jacobian_matrix := [ ListWithIdenticalEntries( n, x -> 1. ) ];
        
        return MorphismConstructor( Smooth, Smooth.( n ), Pair( maps, jacobian_matrix ), Smooth.1 );
        
    elif f{[ 1 .. Minimum( 3, Length(f) ) ]} = "Mul" then
        
        n := Int( f{[ 4 .. Length( f ) ]} );
        
        maps := [ x -> Product( x ) ];
        
        jacobian_matrix := [ List( [ 1 .. n ], i -> x -> Product( x{Concatenation( [ 1 .. i - 1 ], [ i + 1 .. n ] )} ) ) ];
        
        return MorphismConstructor( Smooth, Smooth.( n ), Pair( maps, jacobian_matrix ), Smooth.1 );

    elif f{[ 1 .. Minimum( 3, Length(f) ) ]} = "x1^" then
        
        n := Float( f{[ 4 .. Length( f ) ]} );
        
        maps := [ x -> x[1] ^ n ];
        
        jacobian_matrix := [ [ x -> n * x[1] ^ ( n - 1 ) ] ];
        
        return MorphismConstructor( Smooth, Smooth.1, Pair( maps, jacobian_matrix ), Smooth.1 );
        
    elif f{[ Maximum( 1, Length(f) - 2 ) .. Length( f ) ]} = "^x1" then
        
        n := Float( f{[ 1 .. Length( f ) - 3 ]} );
        
        maps := [ x -> n ^ x[1] ];
        
        jacobian_matrix := [ [ x -> Log( n ) * ( n^x[1] ) ] ];
        
        return MorphismConstructor( Smooth, Smooth.1, Pair( maps, jacobian_matrix ), Smooth.1 );
        
    else
        
        Error( "unrecognized-string!\n" );
        
    fi;
    
end );

##
InstallMethod( LaTeXOutput,
        [ IsObjectInCategoryOfSmoothMaps ],
  
  function ( U )
    
    return Concatenation( "\\mathbb{R}^{", String( RankOfObject( U ) ), "}" );
    
end );

##
InstallMethod( LaTeXOutput,
        [ IsMorphismInCategoryOfSmoothMaps ],
  
  function ( f )
    local vars, maps, s, t;
    
    vars := ValueOption( "vars" );
    
    if vars = fail then
      vars := List( DummyInput( "x", RankOfObject( Source( f ) ) ), String );
    fi;
    
    maps := LaTeXOutputUsingPython( List( Eval( f ), String ), vars );
    
    maps := JoinStringsWithSeparator( maps, " \\\\ \n " );
    
    return Concatenation(
              "\\begin{array}{c}\n",
              LaTeXOutput( Source( f ) ),
              "\\rightarrow",
              LaTeXOutput( Target( f ) ),
              "\\\\ \n \\hline \\\\ \n \\left( \\begin{array}{ll}\n",
              maps,
              "\n\\end{array} \\right) \n\\end{array}"
            );
    
end );

##
InstallMethod( ViewString,
          [ IsObjectInCategoryOfSmoothMaps ],
  
  function ( U )
    
    return Concatenation( "ℝ^", String( RankOfObject( U ) ) );
    
end );

##
InstallMethod( ViewString,
          [ IsMorphismInCategoryOfSmoothMaps ],
  
  function ( f )
    
    return Concatenation(
              ViewString( Source( f ) ),
              " -> ",
              ViewString( Target( f ) ) );
    
end );

##
InstallMethod( DisplayString,
          [ IsMorphismInCategoryOfSmoothMaps ],
  
  function ( f )
    local maps;
    
    maps := List( Eval( f ), ViewString );
    
    return Concatenation(
              ViewString( Source( f ) ),
              " -> ",
              ViewString( Target( f ) ),
              "\n\n",
              JoinStringsWithSeparator( maps, "\n" ),
              "\n" );
    
end );
