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
        
        return CreateCapCategoryMorphismWithAttributes( Smooth,
                    source, target,
                    Map, datum[1],
                    JacobianMatrix, datum[2] );
        
    end );
    
    ##
    AddMorphismDatum( Smooth,
      
      function ( Smooth, f )
        
        return Pair( Map( f ), JacobianMatrix( f ) );
        
    end );
    
    ##
    AddIsWellDefinedForMorphisms( Smooth,
      
      function ( Smooth, f )
        
        return IsFunction( Map( f ) ) and IsFunction( JacobianMatrix( f ) );
        
    end );
    
    ##
    AddIdentityMorphism( Smooth,
      
      function ( Smooth, A )
        local rank_A, map, jacobian_matrix;
        
        rank_A := RankOfObject( A );
        
        map := x -> x;
        
        jacobian_matrix := x -> IdentityMat( rank_A );
        
        return MorphismConstructor( Smooth, A, Pair( map, jacobian_matrix ), A );
        
    end );
    
    ##       f        g
    ## R^s ----> R^m ----> R^t
    ##
    ## J(gof)_{x} = J(g)_{f(x)} * J(f)_{x} where * is the usual matrix multiplication
    ##
    AddPreCompose( Smooth,
      
      function ( Smooth, f, g )
        local rank_Sf, rank_Tf, rank_Sg, rank_Tg, map, jacobian_matrix;
        
        rank_Sf := RankOfObject( Source( f ) );
        rank_Tf := RankOfObject( Target( f ) );
        
        rank_Sg := RankOfObject( Source( g ) );
        rank_Tg := RankOfObject( Target( g ) );
        
        map := x -> Map( g )( Map( f )( x ) );
        
        # chain rule for jacobian matrices
        jacobian_matrix := x -> MultiplyMatrices( rank_Tg, JacobianMatrix( g )( Map( f )( x ) ), rank_Sg, rank_Tf, JacobianMatrix( f )( x ), rank_Sf );
        
        return MorphismConstructor( Smooth, Source( f ), Pair( map, jacobian_matrix ), Target( g ) );
        
    end );
    
    ##
    AddSumOfMorphisms( Smooth,
      
      function ( Smooth, S, morphisms, T )
        local map, jacobian_matrix;
        
        map := x -> Sum( morphisms, f -> Map( f )( x ) );
        
        jacobian_matrix := x -> Sum( morphisms, f -> JacobianMatrix( f )( x ) );
        
        return MorphismConstructor( Smooth, S, Pair( map, jacobian_matrix ), T );
        
    end );
    
    ##
    AddAdditiveInverseForMorphisms( Smooth,
      
      function ( Smooth, f )
        local map, jacobian_matrix;
        
        map := x -> -Map( f )( x );
        
        jacobian_matrix := x -> -JacobianMatrix( f )( x );
        
        return MorphismConstructor( Smooth, Source( f ), Pair( map, jacobian_matrix ), Target( f ) );
        
    end );
    
    ##
    AddSubtractionForMorphisms( Smooth,
      
      function ( Smooth, f, g )
        local map, jacobian_matrix;
        
        map := x -> Map( f )( x ) - Map( g )( x );
        
        jacobian_matrix := x -> JacobianMatrix( f )( x ) - JacobianMatrix( g )( x );
        
        return MorphismConstructor( Smooth, Source( f ), Pair( map, jacobian_matrix ), Target( f ) );
        
    end );
    
    ## f, g: R^m -> R^n
    ##
    ## J_fg(x) = Diag(g_1(x), g_2(x), ..., g_n(x)) * J_f(x) + Diag(f_1(x), f_2(x), ..., f_n(x)) * J_g(x)
    ##
    AddMultiplicationForMorphisms( Smooth,
      
      function ( Smooth, f, g )
        local rank_S, rank_T, map, jacobian_matrix;
        
        rank_S := RankOfObject( Source( f ) );
        rank_T := RankOfObject( Target( f ) );
        
        map := x -> ListN( Map( f )( x ), Map( g )( x ), \* );
        
        jacobian_matrix :=
          x -> MultiplyMatrices( rank_T, DiagonalMat( Map( g )( x ) ), rank_T, rank_T, JacobianMatrix( f )( x ), rank_S )
                + MultiplyMatrices( rank_T, DiagonalMat( Map( f )( x ) ), rank_T, rank_T, JacobianMatrix( g )( x ), rank_S );
        
        return MorphismConstructor( Smooth, Source( f ), Pair( map, jacobian_matrix ), Target( f ) );
        
    end );
     
    ##
    AddSimplifyMorphism( Smooth,
      
      function ( Smooth, f, i )
        local S, T, rank_S, rank_T, vars, all, map, jacobian_matrix;
        
        S := Source( f );
        T := Target( f );
        
        rank_S := RankOfObject( S );
        rank_T := RankOfObject( T );
        
        vars := List( [ 1 .. rank_S ], i -> Concatenation( "x", String( i ) ) );
        
        all := List( Concatenation( Eval( f ), Concatenation( EvalJacobianMatrix( f ) ) ), String );
        
        all := SimplifyExpressionUsingPython( vars, all );
        
        map := AsFunction( Expression( vars, Concatenation( "[", JoinStringsWithSeparator( all{[ 1 .. rank_T]}, ", " ), "]" ) ) );
        
        jacobian_matrix :=
          AsFunction( Expression( vars,
            Concatenation(
            "[",
            JoinStringsWithSeparator(
              List( [ 0 .. rank_T - 1 ], i -> Concatenation( "[", JoinStringsWithSeparator( all{[ rank_T + i * rank_S + 1 .. rank_T + ( i + 1 ) * rank_S ]}, ", " ), "]" ) ), ", " ),
            "]" ) ) );
        
        return MorphismConstructor( Smooth, S, Pair( map, jacobian_matrix ), T );
        
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
        local map, jacobian_matrix;
        
        map := x -> a * Map( f )( x );
        
        jacobian_matrix := x -> a * JacobianMatrix( f )( x );
        
        return MorphismConstructor( Smooth, Source( f ), Pair( map, jacobian_matrix ), Target( f ) );
        
    end );
    
    ## Products
    AddDirectProduct( Smooth,
      
      function ( Smooth, L )
        
        return ObjectConstructor( Smooth, Sum( L, V -> RankOfObject( V ) ) );
        
    end );
    
    ##
    AddProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth,
      
      function ( Smooth, L, i, S )
        local T, rank_S, rank_T, index, map, jacobian_matrix;
        
        T := L[i];
        
        rank_S := RankOfObject( S );
        rank_T := RankOfObject( T );
        
        index := Sum( [ 1 .. i - 1 ], j -> RankOfObject( L[j] ) );
        
        map := x -> List( [ 1 .. rank_T ], i -> x[index + i] );
        
        jacobian_matrix :=
          x -> List( [ 1 .. rank_T ],
            i -> List( [ 1 .. rank_S ],
              j -> Float( KroneckerDelta( j, index + i ) ) ) );
        
        return MorphismConstructor( Smooth, S, Pair( map, jacobian_matrix ), T );
        
    end );
    
    ##
    AddUniversalMorphismIntoDirectProductWithGivenDirectProduct( Smooth,
      
      function ( Smooth, D, test_object, tau, T )
        local map, jacobian_matrix;
        
        map := x -> Concatenation( List( tau, f -> Map( f )( x ) ) );
        
        jacobian_matrix := x -> Concatenation( List( tau, f -> JacobianMatrix( f )( x ) ) );
        
        return MorphismConstructor( Smooth, test_object, Pair( map, jacobian_matrix ), T );
        
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
        local ranks_S, ranks_T, n, indices, map, jacobian_matrix;
        
        ranks_S := List( source_diagram, A -> RankOfObject( A ) );
        ranks_T := List( target_diagram, A -> RankOfObject( A ) );
        
        n := Length( L );
        
        indices := List( [ 0 .. n ], i -> Sum( ranks_S{[ 1 .. i ]} ) );
        
        map :=
          x -> Concatenation( List( [ 1 .. n ], i ->
                  Map( L[i] )( List( [ 1 .. ranks_S[i] ], j -> x[indices[i] + j] ) ) ) );
        
        jacobian_matrix :=
          x -> Concatenation(
                  List( [ 1 .. n ], i ->
                    ListN(
                      ListWithIdenticalEntries( ranks_T[i], ListWithIdenticalEntries( indices[i], 0. ) ),
                      JacobianMatrix( L[i] )( List( [ 1 .. ranks_S[i] ], j -> x[indices[i] + j] ) ),
                      ListWithIdenticalEntries( ranks_T[i], ListWithIdenticalEntries( indices[n + 1] - indices[i] - ranks_S[i], 0. ) ),
                          { l, m, r } -> Concatenation( l, m, r ) ) ) );
        
        return MorphismConstructor( Smooth, S, Pair( map, jacobian_matrix ), T );
        
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
        local rank_A, rank_B, map, id_mat, jacobian_matrix;
        
        rank_A := RankOfObject( S );
        
        rank_B := RankOfObject( T );
        
        map := x -> Concatenation( x{[ rank_A + 1 .. rank_A + rank_B ]}, x{[ 1 .. rank_A ]} );
        
        id_mat := DiagonalMat( ListWithIdenticalEntries( rank_A + rank_B, 1. ) );
        
        jacobian_matrix := x -> Concatenation( id_mat{[ rank_A + 1 .. rank_A + rank_B ]}, id_mat{[ 1 .. rank_A ]} );
        
        return MorphismConstructor( Smooth, SxT, Pair( map, jacobian_matrix ), TxS );
        
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
        local rank_S, rank_T, map;
        
        rank_S := RankOfObject( Source( f ) );
        rank_T := RankOfObject( Target( f ) );
        
        map := x -> MultiplyMatrices(
                      1, [ x{[ rank_S + 1 .. rank_S + rank_T ]} ], rank_T,
                      rank_T, JacobianMatrix( f )( x{[ 1 .. rank_S ]} ), rank_S )[1];
        
        return SmoothMorphism( Smooth, source, map, target );
        
    end );
    
    ##
    AddReverseDifferential( Smooth,
      
      function ( Smooth, f )
        local source, target;
        
        source := DirectProduct( Smooth, Pair( Source( f ), Target( f ) ) );
        target := Source( f );
        
        return ReverseDifferentialWithGivenObjects( Smooth, source, f, target );
        
    end );
    
    ##
    AddRandomMorphismWithFixedSourceAndRangeByInteger( Smooth,
      
      function ( Smooth, S, T, n )
        local rank_S, rank_T, constants, l, l1, l2;
        
        rank_S := RankOfObject( S );
        rank_T := RankOfObject( T );
        
        if n <= 0 then
          
          constants := List( [ 1 .. rank_T ], i -> 0.001 * Random( [ 1 .. 1000 ] ) );
          
          return SmoothMorphism( Smooth, Smooth.( rank_S ), constants, Smooth.( rank_T ) );
          
        else
          
          l := Smooth.LinearLayer( rank_S, rank_T );
          
          constants := List( [ 1 .. ( rank_S + 1 ) * rank_T ], i -> 0.001 * Random( [ 1 .. 1000 ] ) );
          
          l1 := [ SmoothMorphism( Smooth, Smooth.( 0 ), constants, Smooth.( ( rank_S + 1 ) * rank_T ) ) ];
          
          l2 := List( [ 1 .. rank_S ], i -> Smooth.Power( Random( [ 1 .. n ] ) ) );
          
          return PreCompose( Smooth, DirectProductFunctorial( Smooth, Concatenation( l1, l2 ) ), l );
          
        fi;
        
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
    
    return Map( f ) = Map( g ) and JacobianMatrix( f ) = JacobianMatrix( g );
    
end );

##
InstallMethod( Eval,
          [ IsMorphismInCategoryOfSmoothMaps, IsDenseList ],
  
  function ( f, x )
    
    Assert( 0, RankOfObject( Source( f ) ) = Length( x ) );
    
    return Map( f )( x );
    
end );

##
InstallOtherMethod( CallFuncList,
          [ IsMorphismInCategoryOfSmoothMaps, IsDenseList ],
  
  function ( f, L )
    
    return Eval( f, L[1] );
    
end );

##
InstallOtherMethod( Eval,
        [ IsMorphismInCategoryOfSmoothMaps ],
  
  f -> Eval( f, DummyInput( f ) )
);

##
InstallMethod( EvalJacobianMatrix,
          [ IsMorphismInCategoryOfSmoothMaps, IsDenseList ],
  
  function ( f, x )
    
    Assert( 0, RankOfObject( Source( f ) ) = Length( x ) );
    
    return JacobianMatrix( f )( x );
    
end );

##
InstallOtherMethod( EvalJacobianMatrix,
        [ IsMorphismInCategoryOfSmoothMaps ],
  
  f -> EvalJacobianMatrix( f, DummyInput( f ) )
);

##
InstallMethod( SmoothMorphism,
          [ IsCategoryOfSmoothMaps, IsObjectInCategoryOfSmoothMaps, IsDenseList, IsObjectInCategoryOfSmoothMaps ],
  
  function ( Smooth, S, datum, T )
    local map, jacobian_matrix;
    
    if not ForAll( datum, IsDenseList ) then
        TryNextMethod ( );
    fi;
    
    if Length( datum[1] ) <> RankOfObject( T ) or not ForAll( datum[1], IsFunction ) then
        Error( "wrong input: 'datum[1]' must contain exactly ", RankOfObject( T ), " functions!\n" );
    fi;
    
    if Length( datum[2] ) <> RankOfObject( T ) then
        Error( "wrong input: 'datum[2]' must contain exactly ", RankOfObject( T ), " lists!\n" );
    fi;
    
    if not ForAll( datum[2], d -> Length( d ) = RankOfObject( S ) or ForAll( d, IsFunction ) ) then
        Error( "wrong input: each list in 'datum[2]' must contain exactly ", RankOfObject( S ), " functions!\n" );
    fi;
    
    map := x -> List( datum[1], m -> m( x ) );
    
    jacobian_matrix := x -> List( datum[2], row -> List( row, m -> m( x ) ) );
    
    return MorphismConstructor( Smooth, S, Pair( map, jacobian_matrix ), T );

end );

##
InstallOtherMethod( SmoothMorphism,
          [ IsCategoryOfSmoothMaps, IsObjectInCategoryOfSmoothMaps, IsDenseList, IsObjectInCategoryOfSmoothMaps, IsBool ],
  
  function ( Smooth, S, maps, T, use_python )
    local rank_S, rank_T, vars, jacobian_matrix, map;
    
    if not ForAll( maps, IsString ) then
        TryNextMethod( );
    fi;
    
    rank_S := RankOfObject( S );
    rank_T := RankOfObject( T );
    
    Assert( 0, Length( maps ) = rank_T );
    
    vars := List( [ 1 .. rank_S ], i -> Concatenation( "x", String( i ) ) );
    
    if use_python then
      
      jacobian_matrix := JacobianMatrix( vars, maps, [ 1 .. rank_S ] );
    
    else
      
      jacobian_matrix := LazyJacobianMatrix( vars, maps, [ 1 .. rank_S ] );
      
    fi;
    
    map := AsFunction( vars, Concatenation( "[", JoinStringsWithSeparator( maps, ", " ), "]" ) );
    
    return MorphismConstructor( Smooth, S, Pair( map, jacobian_matrix ), T );
    
end );

##
InstallOtherMethod( SmoothMorphism,
          [ IsCategoryOfSmoothMaps, IsObjectInCategoryOfSmoothMaps, IsDenseList, IsObjectInCategoryOfSmoothMaps ],
  
  function ( Smooth, S, maps, T )
    
    if not ForAll( maps, IsString ) then
        TryNextMethod( );
    fi;
    
    return SmoothMorphism( Smooth, S, maps, T, false );
    
end );

##
InstallOtherMethod( SmoothMorphism,
          [ IsCategoryOfSmoothMaps, IsObjectInCategoryOfSmoothMaps, IsFunction, IsObjectInCategoryOfSmoothMaps, IsBool ],
  
  function ( Smooth, S, map, T, use_python )
    local dummy_input, maps;
    
    dummy_input := DummyInput( "x", RankOfObject( S ) );
    
    maps := List( map( dummy_input ), String );
    
    return SmoothMorphism( Smooth, S, maps, T, use_python );
    
end );

##
InstallOtherMethod( SmoothMorphism,
          [ IsCategoryOfSmoothMaps, IsObjectInCategoryOfSmoothMaps, IsFunction, IsObjectInCategoryOfSmoothMaps ],
  
  function ( Smooth, S, map, T )
    
    return SmoothMorphism( Smooth, S, map, T, false );
    
end );

##
InstallOtherMethod( SmoothMorphism,
          [ IsCategoryOfSmoothMaps, IsObjectInCategoryOfSmoothMaps, IsDenseList, IsObjectInCategoryOfSmoothMaps ],
  
  function ( Smooth, S, maps, T )
    
    if not ForAll( maps, IsFunction ) then
        TryNextMethod( );
    fi;
    
    return SmoothMorphism( Smooth, S, maps, T, false );
    
end );

##
InstallOtherMethod( SmoothMorphism,
          [ IsCategoryOfSmoothMaps, IsObjectInCategoryOfSmoothMaps, IsDenseList, IsObjectInCategoryOfSmoothMaps ],
  
  function ( Smooth, S, constants, T )
    local rank_S, rank_T, map, jacobian_matrix;
    
    if not ForAll( constants, c -> IsFloat( c ) or IsRat( c ) ) then
        TryNextMethod( );
    fi;
    
    rank_S := RankOfObject( S );
    rank_T := RankOfObject( T );
    
    map := x -> constants;
    
    jacobian_matrix := x -> ListWithIdenticalEntries( rank_T, ListWithIdenticalEntries( rank_S, 0. ) );
    
    return MorphismConstructor( Smooth, S, Pair( map, jacobian_matrix ), T );
    
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
    local f;
    
    f := NameRNam( string_as_int );
    
    if Int( f ) <> fail then
        
        TryNextMethod( );
        
    fi;
    
    if Float( f ) <> fail then
        
        return MorphismConstructor( Smooth, Smooth.0, [ x -> Float( f ), x -> [ [ ] ] ], Smooth.( 1 ) );
        
    elif f = "Sqrt" then
        
        return MorphismConstructor( Smooth, Smooth.( 1 ), [ x -> [ Sqrt( x[1] ) ], x -> [ [ 1 / (2. * Sqrt( x[1] )) ] ] ], Smooth.( 1 ) );
        
    elif f = "Exp" then
        
        return MorphismConstructor( Smooth, Smooth.( 1 ), [ x -> [ Exp( x[1] ) ], x -> [ [ Exp( x[1] ) ] ] ], Smooth.( 1 ) );
        
    elif f = "Log" then
        
        return MorphismConstructor( Smooth, Smooth.( 1 ), [ x -> [ Log( x[1] ) ], x -> [ [ (1 /  x[1] ) ] ] ], Smooth.( 1 ) );
        
    elif f = "Sin" then
        
        return MorphismConstructor( Smooth, Smooth.( 1 ), [ x -> [ Sin( x[1] ) ], x -> [ [ Cos( x[1] ) ] ] ], Smooth.( 1 ) );
        
    elif f = "Cos" then
        
        return MorphismConstructor( Smooth, Smooth.( 1 ), [ x -> [ Cos( x[1] ) ], x -> [ [ -1 * Sin( x[1] ) ] ] ], Smooth.( 1 ) );
        
    elif f = "Relu" then
        
        return
          function ( n )
            local relu;
            
            relu := MorphismConstructor( Smooth,
                          Smooth.( 1 ),
                          Pair( x -> [ Relu( x[1] ) ], x -> [ [ 0.5 * (1 + SignFloat( x[1] + 1.e-50 )) ] ] ),
                          Smooth.( 1 ) );
            
            return DirectProductFunctorial( Smooth, ListWithIdenticalEntries( n, relu ) );
            
          end;
          
    elif f = "Sum" then
        
        return
          function ( n )
            local map, jacobian_matrix;
            
            map := x -> [ Sum( x ) ];
            
            jacobian_matrix := x -> [ ListWithIdenticalEntries( n, 1. ) ];
            
            return MorphismConstructor( Smooth, Smooth.( n ), Pair( map, jacobian_matrix ), Smooth.( 1 ) );
            
          end;
          
    elif f = "Mul" then
        
        return
          function ( n )
            local map, jacobian_matrix;
            
            map := x -> [ Product( x ) ];
            
            jacobian_matrix := x -> [ List( [ 1 .. n ], i -> Product( Concatenation( [ 1 .. i - 1 ], [ i + 1 .. n ] ), j -> x[j] ) ) ];
            
            return MorphismConstructor( Smooth, Smooth.( n ), Pair( map, jacobian_matrix ), Smooth.( 1 ) );
            
          end;
          
    elif f = "Power" then
        
        return
          function ( n )
            local map, jacobian_matrix;
            
            map := x -> [ x[1] ^ n ];
            
            jacobian_matrix := x -> [ [ n * x[1] ^ ( n - 1 ) ] ];
            
            return MorphismConstructor( Smooth, Smooth.( 1 ), Pair( map, jacobian_matrix ), Smooth.( 1 ) );
            
          end;
          
    elif f = "PowerBase" then
        
        return
          function ( n )
            local map, jacobian_matrix;
            
            map := x -> [ n ^ x[1] ];
            
            jacobian_matrix := x -> [ [ Log( n ) * ( n ^ x[1] ) ] ];
            
            return MorphismConstructor( Smooth, Smooth.( 1 ), Pair( map, jacobian_matrix ), Smooth.( 1 ) );
            
          end;
          
    # categorical construction
    elif f = "Sigmoid_" then
        
        return
          function ( n )
            local h;
            
            h := PreCompose( Smooth,
                    AdditionForMorphisms( Smooth,
                        SmoothMorphism( Smooth, Smooth.( 1 ), [ 1 ], Smooth.( 1 ) ),
                        PreCompose( Smooth, Smooth.Exp, Smooth.Power( -1 ) ) ),
                    Smooth.Power( -1 ) );
            
            return DirectProductFunctorial( Smooth, ListWithIdenticalEntries( n, h ) );
            
          end;
          
    # direct construction
    elif f = "Sigmoid" then
        
        return
          function ( n )
            local map, jacobian_matrix;
            
            map := x -> List( [ 1 .. n ], i -> 1 / ( 1 + Exp( -x[i] ) ) );
            
            jacobian_matrix :=
              function ( x )
                
                return DiagonalMat( List( List( [ 1 .. n ], i -> Exp( -x[i] ) ), exp -> exp / ( 1 - exp ) ^ 2 ) );
                
              end;
              
            return MorphismConstructor( Smooth, Smooth.( n ), Pair( map, jacobian_matrix ), Smooth.( n ) );
            
          end;
          
    # categorical construction
    elif f = "Softmax_" then
        
        return
          function ( n )
            local p, q;
            
            # compute Exp on all entries
            p := DirectProductFunctorial( Smooth, ListWithIdenticalEntries( n, Smooth.Exp ) );
            
            # divide by the sum
            q := PreComposeList( Smooth, [ p, Smooth.Sum( n ), Smooth.Power( -1 ) ] );
            
            # make n copies
            q := UniversalMorphismIntoDirectProduct( Smooth, ListWithIdenticalEntries( n, q ) );
            
            # compute the softmax
            return MultiplicationForMorphisms( Smooth, p, q );
            
          end;
          
    # direct construction
    elif f = "Softmax" then
        
        return
          function ( n )
            local map, jacobian_matrix;
            
            map :=
              function ( x )
                local exp_x, s;
                
                exp_x := List( [ 1 .. n ], i -> Exp( x[i] ) );
                
                s := Sum( exp_x );
                
                return List( [ 1 .. n ], i ->  exp_x[i] / s );
                
              end;
            
            jacobian_matrix :=
              function ( x )
                local exp_x, s, d;
                
                exp_x := List( [ 1 .. n ], i -> Exp( x[i] ) );
                
                s := Sum( exp_x );
                
                d := List( [ 1 .. n ], i -> exp_x[i] / s ^ 2 );
                
                return
                  List( [ 1 .. n ],
                    i -> List( [ 1 .. n ],
                      function ( j )
                        
                        if i = j then
                            return  ( s - exp_x[j] ) * d[i];
                        else
                            return    ( - exp_x[j] ) * d[i];
                        fi;
                        
                      end ) );
                
              end;
            
            return MorphismConstructor( Smooth, Smooth.( n ), Pair( map, jacobian_matrix ), Smooth.( n ) );
            
          end;
          
    # categorical construction
    elif f = "QuadraticLoss_" then
        
        return
          function ( n )
            local p1, p2, diff, square, sum, total_sum;
            
            # predicted values
            p1 := ProjectionInFactorOfDirectProduct( Smooth, [ Smooth.( n ), Smooth.( n ) ], 1 );
            
            # ground truth values
            p2 := ProjectionInFactorOfDirectProduct( Smooth, [ Smooth.( n ), Smooth.( n ) ], 2 );
            
            # compute the difference
            diff := SubtractionForMorphisms( Smooth, p1, p2 );
            
            # square entries
            square := DirectProductFunctorial( Smooth, ListWithIdenticalEntries( n, Smooth.Power( 2 ) ) );
            
            # take sum
            sum := Smooth.Sum( n );
            
            # compute the total sum of squars of differences
            total_sum := PreComposeList( Smooth, [ diff, square, sum ] );
            
            # return the average
            return MultiplyWithElementOfCommutativeRingForMorphisms( Smooth, 1 / n, total_sum );
            
          end;
          
    # direct construction
    elif f = "QuadraticLoss" then
        
        return
          function ( n )
            local map, jacobian_matrix;
            
            map := x -> [ Sum( [ 1 .. n ], i -> ( x[i] - x[n + i] ) ^ 2 ) / n ];
            
            jacobian_matrix :=
              x -> [ Concatenation(
                        List( [ 1 .. n ], i -> 2 * ( x[i] - x[n + i] ) / n ),
                        List( [ 1 .. n ], i -> 2 * ( x[n + i] - x[i] ) / n ) ) ];
            
            return MorphismConstructor( Smooth, Smooth.( 2 * n ), Pair( map, jacobian_matrix ), Smooth.( 1 ) );
            
          end;
          
    # categorical construction
    elif f = "CrossEntropyLoss_" then
        
        return
          function ( n )
            local p1, p2, log, log_p1, mul, total_sum;
            
            # predicted values
            p1 := ProjectionInFactorOfDirectProduct( Smooth, [ Smooth.( n ), Smooth.( n ) ], 1 );
            
            # ground truth values
            p2 := ProjectionInFactorOfDirectProduct( Smooth, [ Smooth.( n ), Smooth.( n ) ], 2 );
            
            # compute Log (n copies)
            log := DirectProductFunctorial( Smooth, ListWithIdenticalEntries( n, Smooth.Log ) );
            
            # apply log on the predicted values
            log_p1 := PreCompose( Smooth, p1, log );
            
            # multiply log_p1 with p2
            mul := MultiplicationForMorphisms( Smooth, log_p1, p2 );
            
            # compute the sum
            total_sum := PreCompose( Smooth, mul, Smooth.Sum( n ) );
            
            # return the average
            return MultiplyWithElementOfCommutativeRingForMorphisms( Smooth, -1 / n, total_sum );
            
          end;
          
    # direct construction
    elif f = "CrossEntropyLoss" then
        
        return
          function ( n )
            local map, jacobian_matrix;
            
            map := x -> [ -Sum( [ 1 .. n ], i -> Log( x[i] ) * x[n + i] ) / n ];
            
            jacobian_matrix :=
              x -> [ Concatenation(
                        List( [ 1 .. n ], i -> -x[n + i] / ( n * x[i] ) ),
                        List( [ 1 .. n ], i -> -Log( x[i] ) / n ) ) ];
            
            return MorphismConstructor( Smooth, Smooth.( 2 * n ), Pair( map, jacobian_matrix ), Smooth.( 1 ) );
            
          end;
          
    # categorical construction
    elif f = "SoftmaxCrossEntropyLoss_" then
        
        return
          function ( n )
            local p1, softmax_p1, p2, p;
            
            # predicted values
            p1 := ProjectionInFactorOfDirectProduct( Smooth, [ Smooth.( n ), Smooth.( n ) ], 1 );
            
            # convert these values to probabilities
            softmax_p1 := PreCompose( Smooth, p1, Smooth.Softmax_( n ) );
            
            # ground truth values
            p2 := ProjectionInFactorOfDirectProduct( Smooth, [ Smooth.( n ), Smooth.( n ) ], 2 );
            
            # combine values again
            p := UniversalMorphismIntoDirectProduct( Smooth, [ softmax_p1, p2 ] );
            
            # compose with the cross entropy loss
            return PreCompose( Smooth, p, Smooth.CrossEntropyLoss_( n ) );
            
          end;
          
    elif f = "SoftmaxCrossEntropyLoss" then
        
        return
          function ( n )
            local map, jacobian_matrix;
            
            map :=
              function ( x )
                local l;
                
                l := Log( Sum( [ 1 .. n ], i -> Exp( x[i] ) ) );
                
                return [ Sum( [ 1 .. n ], i -> ( l - x[i] ) * x[n + i] ) / n ];
                
              end;
            
            jacobian_matrix :=
              function ( x )
                local exp_x, s, l, c;
                
                exp_x := List( [ 1 .. n ], i -> Exp( x[i] ) );
                
                s := Sum( exp_x );
                
                l := Log( s );
                
                c := Sum( [ 1 .. n ], i -> x[n + i] ) / s;
                
                return
                  [ Concatenation(
                        List( [ 1 .. n ], i -> ( c * exp_x[i] - x[n + i] ) / n ),
                        List( [ 1 .. n ], i -> ( l - x[i] ) / n ) ) ];
                
              end;
              
            return MorphismConstructor( Smooth, Smooth.( 2 * n ), Pair( map, jacobian_matrix ), Smooth.( 1 ) );
            
          end;
          
    # to create input:
    # e.g.
    #
    # m := 2; n := 4;
    #
    # vec :=
    #   ConvertToExpressions(
    #     Concatenation(
    #       Concatenation(
    #         TransposedMat(
    #           Concatenation(
    #               List( [ 1 .. m ], i -> List( [ 1 .. n ], j -> Concatenation( "w", String( i ), "_", String( j ) ) ) ),
    #               [ List( [ 1 .. n ], j -> Concatenation( "b_", String( j ) ) ) ] ) ) ),
    #           List( [ 1 .. m ], i -> Concatenation( "x", String( i ) ) ) ) );
    #
    # categorical construction
    elif f = "LinearLayer_" then
        
        return
          function ( m, n )
            local S, T, diagram, weights, vec, components;
            
            # the input vector consists of (m + 1) * n + m entries from which (m + 1) * n are parameters
            S := Smooth.( ( m + 1 ) * n + m );
            
            # the output vector consists of n entries
            T := Smooth.( n );
            
            diagram := Concatenation( ListWithIdenticalEntries( n, Smooth.( m + 1 ) ), [ Smooth.( m ) ] );
            
            # the columns of the weight matrix
            weights := List( [ 1 .. n ], i -> ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, diagram, i, S ) );
            
            # the input vector
            vec := ProjectionInFactorOfDirectProductWithGivenDirectProduct( diagram, n + 1, S );
            
            # append 1 to the end of the input vector
            vec := DirectProductFunctorial( Smooth, [ vec, SmoothMorphism( Smooth, Smooth.( 0 ), [ 1 ], Smooth.( 1 ) ) ] );
            
            # compute the dot products
            components := List( [ 1 .. n ], i -> PreCompose( Smooth, MultiplicationForMorphisms( Smooth, vec, weights[i] ), Smooth.Sum( m + 1 ) ) );
            
            # i.e., the output vector is the linear combination of the rows of the weight matrix using the coeffs: [vec 1]
            return UniversalMorphismIntoDirectProductWithGivenDirectProduct( Smooth,
                          ListWithIdenticalEntries( n, Smooth.( 1 ) ), S, components, T );
            
          end;
          
    elif f = "LinearLayer" then
        
        return
          function ( m, n )
            local map, jacobian_matrix;
            
            map :=
              x -> List( [ 1 .. n ], i -> Sum( [ 1 .. m ], j -> x[( m + 1 ) * n + j] * x[( i - 1 ) * ( m + 1 ) + j] ) + x[i * ( m + 1 )] );
            
            jacobian_matrix :=
              x ->  List( [ 1 .. n ], i ->
                      Concatenation(
                        ListWithIdenticalEntries( ( i - 1 ) * ( m + 1 ), 0. ),
                        List( [ 1 .. m ], i -> x[( m + 1) * n + i] ),
                        [ 1. ],
                        ListWithIdenticalEntries( ( n - i ) * ( m + 1 ), 0. ),
                        List( [ 1 .. m ], j -> x[( i - 1 ) * ( m + 1 ) + j] ) ) );
            
            return MorphismConstructor( Smooth, Smooth.( m * ( n + 1 ) + n ), Pair( map, jacobian_matrix ), Smooth.( n ) );
            
          end;
          
    else
        
        Error( "unrecognized-string!\n" );
        
    fi;
    
end );

## e.g., DummyInputStringsForLinearLayer( 2, 4, "w", "b" [, "x"] )
##
InstallGlobalFunction( DummyInputStringsForLinearLayer,
  
  function ( arg... )
    local m, n, weight_str, bias_str, input_str, input_length, parameters;
    
    m := arg[1];
    n := arg[2];
    
    weight_str := arg[3];
    bias_str := arg[4];
    
    if Length( arg ) > 4 then
        input_str := arg[5];
        input_length := m;
    else
        input_length := 0;
    fi;
    
    parameters :=
          Concatenation(
            TransposedMat(
              Concatenation(
                  List( [ 1 .. m ], i -> List( [ 1 .. n ], j -> Concatenation( weight_str, String( i ), "_", String( j ) ) ) ),
                  [ List( [ 1 .. n ], j -> Concatenation( bias_str, "_", String( j ) ) ) ] ) ) );
    
    return Concatenation( parameters, List( [ 1 .. input_length ], i -> Concatenation( input_str, String( i ) ) ) );

end );

##
InstallGlobalFunction( DummyInputForLinearLayer,
  
  function ( arg... )
    
    return ConvertToExpressions( CallFuncList( DummyInputStringsForLinearLayer, arg ) );
    
end );

##
InstallOtherMethod( \^,
        [ IsMorphismInCategoryOfSmoothMaps, IsAdditiveElement ],
  
  function ( f, n )
    local Smooth, p;
    
    Smooth := CapCategory( f );
    
    p := DirectProductFunctorial( Smooth, ListWithIdenticalEntries( RankOfObject( Target( f ) ), Smooth.Power( n ) ) );
    
    return PreCompose( Smooth, f, p );
    
end );

##
InstallOtherMethod( \*,
        [ IsMorphismInCategoryOfSmoothMaps, IsMorphismInCategoryOfSmoothMaps ],
  
  function ( f, g )
    
    return MultiplicationForMorphisms( f, g );
    
end );

##
InstallOtherMethod( \/,
        [ IsMorphismInCategoryOfSmoothMaps, IsMorphismInCategoryOfSmoothMaps ],
  
  function ( f, g )
    
    return MultiplicationForMorphisms( f, g ^ -1 );
    
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
    local rank_S, rank_T, vars, all, maps, jacobian_matrix;
    
    rank_S := RankOfObject( Source( f ) );
    rank_T := RankOfObject( Target( f ) );
    
    vars := List( DummyInput( f ), String );
    
    all := LaTeXOutputUsingPython( vars, List( Concatenation( Eval( f ), Concatenation( EvalJacobianMatrix( f ) ) ), String ) );
    
    maps := all{[ 1 .. rank_T ]};
    
    jacobian_matrix := List( [ 0 .. rank_T - 1 ], i -> all{[ rank_T + i * rank_S + 1 .. rank_T + ( i + 1 ) * rank_S ]} );
    
    return Concatenation(
              "\\begin{array}{c}\n",
              LaTeXOutput( Source( f ) ),
              "\\rightarrow",
              LaTeXOutput( Target( f ) ),
              "\\\\ \n \\hline \\\\ \n \\left( \\begin{array}{l}\n",
              JoinStringsWithSeparator( maps, " \\\\ \n " ),
              "\n\\end{array} \\right)",
              
              "\\\\ \n \\\\ \n \\hline \\\\ \n \\left( \\begin{array}{",
              Concatenation( ListWithIdenticalEntries( rank_S, "l" ) ),
              "}\n",
              JoinStringsWithSeparator( List( jacobian_matrix, l -> JoinStringsWithSeparator( l, " & " ) ), " \\\\ \n " ),
              "\n\\end{array} \\right)",
              
              "\n\\end{array}"
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

##
InstallMethod( Display,
          [ IsMorphismInCategoryOfSmoothMaps ],
  
  function ( f )
    local m;
    
    Print( ViewString( Source( f ) ), " -> ", ViewString( Target( f ) ), "\n\n" );
    
    for m in Map( f )( DummyInput( f ) ) do
        Display( ViewString( m ) );
    od;
    
end );
