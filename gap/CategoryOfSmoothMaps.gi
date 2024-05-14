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
    AddIdentityMorphism( Smooth,
      
      function ( Smooth, V )
        local rank_V, maps, jacobian_matrix;
        
        rank_V := RankOfObject( V );
        
        maps := List( [ 1 .. rank_V ], i -> x -> x[i] );
        
        jacobian_matrix := List( [ 1 .. rank_V ], i -> List( [ 1 .. rank_V ], j -> x -> Float( KroneckerDelta( i, j ) ) ) );
        
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
    
    ##
    AddAdditiveInverseForMorphisms( Smooth,
      
      function ( Smooth, f )
        local maps, jacobian_matrix;
        
        maps := List( UnderlyingMaps( f ), m -> x -> -m( x ) );
        
        jacobian_matrix := List( JacobianMatrix( f ), l -> List( l, m -> x -> -m( x ) ) );
        
        return MorphismConstructor( Smooth, Source( f ), Pair( maps, jacobian_matrix ), Target( f ) );
        
    end );
    
    ##
    AddSubtractionForMorphisms( Smooth,
      
      function ( Smooth, f, g )
        local maps, jacobian_matrix;
        
        maps := ListN( UnderlyingMaps( f ), UnderlyingMaps( g ), { m, n } -> x -> m( x ) - n( x ) );
        
        jacobian_matrix :=
          ListN( JacobianMatrix( f ), JacobianMatrix( g ), { f_l, g_l } -> ListN( f_l, g_l, { m, n } -> x -> m( x ) - n( x ) ) );
        
        return MorphismConstructor( Smooth, Source( f ), Pair( maps, jacobian_matrix ), Target( f ) );
        
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
        local S, T, rank_S, rank_T, vars, all, maps, jacobian_matrix;
        
        S := Source( f );
        T := Target( f );
        
        rank_S := RankOfObject( S );
        rank_T := RankOfObject( T );
        
        vars := List( [ 1 .. rank_S ], i -> Concatenation( "x", String( i ) ) );
        
        all := List( Concatenation( Eval( f ), Concatenation( EvalJacobianMatrix( f ) ) ), String );
        
        all := List( SimplifyExpressionUsingPython( all, vars ), m -> AsFunction( Expression( vars, m ) ) );
        
        maps := all{[ 1 .. rank_T ]};
        
        jacobian_matrix := List( [ 0 .. rank_T - 1 ], i -> all{[ rank_T + i * rank_S + 1 .. rank_T + ( i + 1 ) * rank_S ]} );
        
        return MorphismConstructor( Smooth, S, Pair( maps, jacobian_matrix ), T );
        
    end );
    
    ##
    AddZeroMorphism( Smooth,
      
      function ( Smooth, S, T )
        local rank_S, rank_T, maps, jacobian_matrix;
        
        rank_S := RankOfObject( S );
        rank_T := RankOfObject( T );
        
        maps := ListWithIdenticalEntries( rank_T, x -> 0. );
        
        jacobian_matrix := ListWithIdenticalEntries( rank_T, ListWithIdenticalEntries( rank_S, x -> 0. ) );
        
        return MorphismConstructor( Smooth, S, Pair( maps, jacobian_matrix ), T );
        
    end );
    
    AddMultiplyWithElementOfCommutativeRingForMorphisms( Smooth,
      
      function ( Smooth, a, f )
        local maps, jacobian_matrix;
        
        maps := List( UnderlyingMaps( f ), m -> x -> a * m( x ) );
        
        jacobian_matrix := List( JacobianMatrix( f ), l -> List( l, m -> x -> a * m( x ) ) );
        
        return MorphismConstructor( Smooth, Source( f ), Pair( maps, jacobian_matrix ), Target( f ) );
        
    end );
    
    ## Products
    AddDirectProduct( Smooth,
      
      function ( Smooth, L )
        
        return ObjectConstructor( Smooth, Sum( L, V -> RankOfObject( V ) ) );
        
    end );
    
    ##
    AddProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth,
      
      function ( Smooth, L, i, S )
        local rank_S, T, rank_T, k, maps, jacobian_matrix;
        
        rank_S := RankOfObject( S );
        
        T := L[i];
        
        rank_T := RankOfObject( T );
        
        k := Sum( [ 1 .. i - 1 ], j -> RankOfObject( L[j] ) );
        
        maps := List( [ 1 .. rank_T ], i -> x -> x[k + i] );
        
        jacobian_matrix :=
          List( [ 1 .. rank_T ],
            i -> List( [ 1 .. rank_S ],
              j -> x -> Float( KroneckerDelta( j, k + i ) ) ) );
        
        return MorphismConstructor( Smooth, S, Pair( maps, jacobian_matrix ), T );
        
    end );
    
    ##
    AddUniversalMorphismIntoDirectProductWithGivenDirectProduct( Smooth,
      
      function ( Smooth, D, test_object, tau, T )
        local maps, jacobian_matrix;
        
        maps := Concatenation( List( tau, f -> UnderlyingMaps( f ) ) );
        
        jacobian_matrix := Concatenation( List( tau, f -> JacobianMatrix( f ) ) );
        
        return MorphismConstructor( Smooth, test_object, Pair( maps, jacobian_matrix ), T );
        
    end );
    
    ##
    AddDirectProductOnMorphismsWithGivenDirectProducts( Smooth,

      function ( Smooth, S, f, g, T )
        
        return DirectProductFunctorialWithGivenDirectProducts( Smooth,
                    S,
                    Pair( Source( f ), Source( g ) ),
                    Pair( f, g ),
                    Pair( Target( f ), Target( g ) ),
                    T );
        
    end );
    
    ##
    AddDirectProductFunctorialWithGivenDirectProducts( Smooth,
      
      function ( Smooth, S, source_diagram, L, target_diagram, T )
        local source_diagram_ranks, target_diagram_ranks, indices, maps, jacobian_matrix;
        
        source_diagram_ranks := List( source_diagram, A -> RankOfObject( A ) );
        target_diagram_ranks := List( target_diagram, A -> RankOfObject( A ) );
        
        indices := List( [ 1 .. Length( L ) ], i -> 1 + Sum( source_diagram_ranks{[ 1 .. i - 1 ]} ) );
        
        maps := Concatenation( List( [ 1 .. Length( L ) ], i -> List( UnderlyingMaps( L[i] ), m -> x -> m( x{[ indices[i] .. indices[i] + source_diagram_ranks[i] - 1 ]} ) ) ) );
        
        jacobian_matrix :=
          Concatenation(
            List( [ 1 .. Length( L ) ], i ->
              List( JacobianMatrix( L[i] ), l ->
                Concatenation(
                      ListWithIdenticalEntries( indices[i] - 1, x -> 0. ),
                      List( l, m -> x -> m( x{[ indices[i] .. indices[i] + source_diagram_ranks[i] - 1 ]} ) ),
                      ListWithIdenticalEntries( RankOfObject( S ) - ( indices[i] + source_diagram_ranks[i] - 1 ), x -> 0. ) ) ) ) );
        
        return MorphismConstructor( Smooth, S, Pair( maps, jacobian_matrix ), T );
        
    end );
    
    ##
    AddCartesianAssociatorRightToLeftWithGivenDirectProducts( Smooth,
      
      function ( Smooth, S_x_UxT, S, U, T, SxU_x_T )
        
        return IdentityMorphism( Smooth, S_x_UxT );
        
    end );
    
    ##
    AddCartesianAssociatorLeftToRightWithGivenDirectProducts( Smooth,
      
      function ( Smooth, SxU_x_T, S, U, T, S_x_UxT )
        
        return IdentityMorphism( Smooth, SxU_x_T );
        
    end );
    
    ##
    AddCartesianLeftUnitorWithGivenDirectProduct( Smooth,
      
      function ( Smooth, S, IxS )
        
        return IdentityMorphism( Smooth, S );
        
    end );
    
    AddCartesianLeftUnitorInverseWithGivenDirectProduct( Smooth,
      
      function ( Smooth, S, IxS )
        
        return IdentityMorphism( Smooth, S );
        
    end );
    
    AddCartesianRightUnitorWithGivenDirectProduct( Smooth,
      
      function ( Smooth, S, SxI )
        
        return IdentityMorphism( Smooth, S );
        
    end );
    
    AddCartesianRightUnitorInverseWithGivenDirectProduct( Smooth,
      
      function ( Smooth, S, SxI )
        
        return IdentityMorphism( Smooth, S );
        
    end );
    
    ##
    AddIsTerminal( Smooth,
      
      function ( Smooth, S )
        
        return RankOfObject( S ) = 0;
        
    end );
    
    ##
    AddTerminalObject( Smooth,
      
      function ( Smooth )
        
        return ObjectConstructor( Smooth, 0 );
        
    end );
    
    ##
    AddUniversalMorphismIntoTerminalObjectWithGivenTerminalObject( Smooth,
      
      function ( Smooth, S, T )
        
        return MorphismConstructor( Smooth, S, Pair( [ ], [ ] ), T );
        
    end );
    
    ##
    AddCartesianBraidingWithGivenDirectProducts( Smooth,
      
      function ( Smooth, SxT, S, T, TxS )
        local rank_A, rank_B, maps_to_B, maps_to_A, maps, jacobian_matrix;
        
        rank_A := RankOfObject( S );
        rank_B := RankOfObject( T );
        
        maps_to_B := List( [ 1 .. rank_B ], i -> x -> x[rank_A + i] );
        maps_to_A := List( [ 1 .. rank_A ], i -> x -> x[i] );
        
        maps := Concatenation( maps_to_B, maps_to_A );
        
        jacobian_matrix :=
          List( [ 1 .. rank_A + rank_B ], i -> List( [ 1 .. rank_A + rank_B ], j -> x -> Float( KroneckerDelta( i, j ) ) ) );
        
        jacobian_matrix :=
          Concatenation( jacobian_matrix{[ rank_A + 1 .. rank_A + rank_B ]}, jacobian_matrix{[ 1 .. rank_A ]} );
        
        return MorphismConstructor( Smooth, SxT, Pair( maps, jacobian_matrix ), TxS );
        
    end );
    
    ##
    AddCartesianBraidingInverseWithGivenDirectProducts( Smooth,
      
      function ( Smooth, SxT, S, T, TxS )
        
        return CartesianBraidingWithGivenDirectProducts( Smooth, TxS, T, S, SxT );
        
    end );
    
    ## Monoidal Structure
    ##
    AddTensorUnit( Smooth,
      
      function ( Smooth )
        
        return TerminalObject( Smooth );
        
    end );
    
    ##
    AddTensorProductOnObjects( Smooth,
      
      function ( Smooth, S, T )
        
        return DirectProduct( Smooth, [ S, T ] );
        
    end );
    
    ##
    AddTensorProductOnMorphismsWithGivenTensorProducts( Smooth,
      
      function ( Smooth, SxA, f, g, TxB )
        
        return DirectProductOnMorphismsWithGivenDirectProducts( Smooth, SxA, f, g, TxB );
        
    end );
    
    ##
    AddAssociatorRightToLeftWithGivenTensorProducts( Smooth,
      
      function ( Smooth, S_x_UxT, S, U, T, SxU_x_T )
        
        return IdentityMorphism( Smooth, S_x_UxT );
        
    end );
    
    ##
    AddAssociatorLeftToRightWithGivenTensorProducts( Smooth,
      
      function ( Smooth, SxU_x_T, S, U, T, S_x_UxT )
        
        return IdentityMorphism( Smooth, SxU_x_T );
        
    end );
    
    ##
    AddLeftUnitorWithGivenTensorProduct( Smooth,
      
      function ( Smooth, A, IxA )
        
        return IdentityMorphism( Smooth, A );
        
    end );
    
    ##
    AddLeftUnitorInverseWithGivenTensorProduct( Smooth,
      
      function ( Smooth, A, IxA )
        
        return IdentityMorphism( Smooth, A );
        
    end );
    
    ##
    AddRightUnitorWithGivenTensorProduct( Smooth,
      
      function ( Smooth, A, AxI )
        
        return IdentityMorphism( Smooth, A );
        
    end );
    
    ##
    AddRightUnitorInverseWithGivenTensorProduct( Smooth,
      
      function ( Smooth, A, AxI )
        
        return IdentityMorphism( Smooth, A );
        
    end );
    
    ##
    AddBraidingWithGivenTensorProducts( Smooth,
      
      function ( Smooth, SxT, S, T, TxS )
        
        return CartesianBraidingWithGivenDirectProducts( Smooth, SxT, S, T, TxS );
        
    end );
    
    ##
    AddBraidingInverseWithGivenTensorProducts( Smooth,
      
      function ( Smooth, SxT, S, T, TxS )
        
        return CartesianBraidingInverseWithGivenDirectProducts( Smooth, SxT, S, T, TxS );
        
      end );
    
    ##
    ## f: R^m -> R^n, J_f in R^{nxm}
    ##
    ## Df: R^m x R^n -> R^m,  (x,    y) ----> y    *  J_f(x)
    ##
    ##                         R^1xm R^1xn    R^1xn   R^{n x m}
    AddReverseDifferentialWithGivenObjects( Smooth,
      
      function ( Smooth, source, f, target )
        local rank_S, rank_T, J, maps;
        
        rank_S := RankOfObject( Source( f ) );
        rank_T := RankOfObject( Target( f ) );
        
        J := TransposedMatWithGivenDimensions( rank_T, rank_S, JacobianMatrix( f ) );
        
        maps := List( J, l -> x -> Sum( [ 1 .. rank_T ], i -> l[i]( x{[ 1 .. rank_S ]} ) * x[rank_S + i] ) );
        
        return SmoothMorphism( Smooth, source, maps, target );
        
    end );
    
    ##
    AddReverseDifferential( Smooth,
      
      function ( Smooth, f )
        local source, target;
        
        source := DirectProduct( Smooth, Pair( Source( f ), Target( f ) ) );
        target := Source( f );
        
        return ReverseDifferentialWithGivenObjects( Smooth, source, f, target );
        
    end );
    
    Finalize( Smooth );
    
    return Smooth;
    
end );

##
InstallOtherMethod( IsCongruentForMorphisms,
        [ IsCategoryOfSmoothMaps, IsMorphismInCategoryOfSmoothMaps, IsMorphismInCategoryOfSmoothMaps ],
  
  function ( Smooth, f, g )
    local rank_S, 100_random_inputs, compare_maps, compare_jacobian_matrices;
    
    rank_S := RankOfObject( Source( f ) );
    
    100_random_inputs := List( [ 1 .. 100 ], i -> List( [ 1 .. rank_S ], j -> 0.001 * Random( [ 1 .. 100 ] ) ) );
    
    compare_maps :=
      ForAll( 100_random_inputs, x -> ForAll( ListN( Eval( f, x ), Eval( g, x ), { a, b } -> a - b < 1.e-10 ), IdFunc ) );
    
    compare_jacobian_matrices :=
      ForAll( 100_random_inputs, x -> ForAll( ListN( EvalJacobianMatrix( f, x ), EvalJacobianMatrix( g, x ), { a, b } -> Sum( a - b ) < 1.e-10 ), IdFunc ) );
    
    return compare_maps and compare_jacobian_matrices;
    
end );

##
InstallOtherMethod( IsEqualForMorphisms,
        [ IsCategoryOfSmoothMaps, IsMorphismInCategoryOfSmoothMaps, IsMorphismInCategoryOfSmoothMaps ],
  
  function ( Smooth, f, g )
    
    return UnderlyingMaps( f ) = UnderlyingMaps( g ) and JacobianMatrix( f ) = JacobianMatrix( g );
    
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
