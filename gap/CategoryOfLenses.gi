






##
InstallMethod( CategoryOfLenses,
          [ IsCapCategory ],
  
  function ( C )
    local name, Lenses;
    
    name := Concatenation( "CategoryOfLenses( ", Name( C ), " )" );
    
    Lenses := CreateCapCategory( name,
                  IsCategoryOfLenses,
                  IsObjectInCategoryOfLenses,
                  IsMorphismInCategoryOfLenses,
                  IsCapCategoryTwoCell
                  : overhead := false );
    
    Lenses!.is_computable := false;
    
    SetIsSymmetricMonoidalCategory( Lenses, true );
    
    SetIsStrictMonoidalCategory( Lenses, true );
    
    SetUnderlyingCategory( Lenses, C );
    
    ##
    AddObjectConstructor( Lenses,
      
      function ( Lenses, datum )
        
        return CreateCapCategoryObjectWithAttributes( Lenses,
                  UnderlyingPairOfObjects, datum );
        
    end );
    
    ##
    AddObjectDatum( Lenses,
      
      function ( Lenses, A )
        
        return UnderlyingPairOfObjects( A );
        
    end );
    
    ##
    AddIsWellDefinedForObjects( Lenses,
      
      function ( Lenses, A )
        local C;
        
        C := UnderlyingCategory( Lenses );
        
        return ForAll( UnderlyingPairOfObjects( A ), o -> IsWellDefinedForObjects( C, o ) );
        
    end );
    
    ##
    AddIsEqualForObjects( Lenses,
      
      function ( Lenses, A, B )
        local C, pair_A, pair_B;
        
        C := UnderlyingCategory( Lenses );
        
        pair_A := UnderlyingPairOfObjects( A );
        pair_B := UnderlyingPairOfObjects( B );
        
        return IsEqualForObjects( C, pair_A[1], pair_B[1] ) and IsEqualForObjects( C, pair_A[2], pair_B[2] );
        
    end );
    
    ## f:(S1,S2) -> (T1,T2) is a pair of morphisms: f1:S1 -> T1 (forward) & f2:S1xT2 -> S2 (backward)
    ##
    AddMorphismConstructor( Lenses,
      
      function ( Lenses, S, datum, T )
        
        return CreateCapCategoryMorphismWithAttributes( Lenses,
                      S, T,
                      GetMorphism, datum[1],
                      PutMorphism, datum[2],
                      UnderlyingPairOfMorphisms, datum );
        
    end );
    
    ##
    AddMorphismDatum( Lenses,
      
      function ( Lenses, f )
        
        return Pair( GetMorphism( f ), PutMorphism( f ) );
        
    end );
     
    ##
    AddIsWellDefinedForMorphisms( Lenses,
      
      function ( Lenses, f )
        local C, pair_S, pair_T, pair_f;
        
        C := UnderlyingCategory( Lenses );
        
        pair_S := UnderlyingPairOfObjects( Source( f ) );
        pair_T := UnderlyingPairOfObjects( Target( f ) );
        
        pair_f := UnderlyingPairOfMorphisms( f );
        
        return ForAll( pair_f, m -> IsWellDefinedForMorphisms( C, m ) ) and
                IsEqualForObjects( C, pair_S[1], Source( pair_f[1] ) ) and
                IsEqualForObjects( C, pair_T[1], Target( pair_f[1] ) ) and
                IsEqualForObjects( C, DirectProduct( C, [ pair_S[1], pair_T[2] ] ), Source( pair_f[2] ) ) and
                IsEqualForObjects( C, pair_S[2], Target( pair_f[2] ) );
        
    end );

#            f         g
#     S1          U1        T1
#          ----->    ----->
#     S2          U2        T2
#
#     i.e.,
#
#           f1                  g1
#     S1 ------>  U1       U1 -----> T1
#
#     S2 <----- S1 x U2    U2 <---- U1 x T2
#           f2                  g2
#
#
#              f1•g1
#     S1 ----------------> T1
#
#     S2 <--------------- S1xT2
#                ?
#
#
#                ↗ S1
#              /
#         p1 /
#          /
#        /
#     S1    ∃u     S1    f2
#     x  --------> x  --------> S2; i.e., the composition is u•f2
#     T2           U2
#        \
#       f1 ↘ U1
#       x    x
#       T2   T2
#              \
#            g2 ↘
#                 U2
#
    ##
    AddPreCompose( Lenses,
      
      function ( Lenses, f, g )
        local C, S, U, T, h1, h2;
        
        C := UnderlyingCategory( Lenses );
        
        S := UnderlyingPairOfObjects( Source( f ) );
        U := UnderlyingPairOfObjects( Target( f ) );
        T := UnderlyingPairOfObjects( Target( g ) );
        
        h1 := PreCompose( GetMorphism( f ), GetMorphism( g ) );
        
        h2 :=
          PreCompose( C,
            UniversalMorphismIntoDirectProduct( C, [ S[1], U[2] ], DirectProduct( C, [ S[1], T[2] ] ),
              [ ProjectionInFactorOfDirectProduct( C, [ S[1], T[2] ], 1 ),
                PreCompose( C, DirectProductOnMorphisms( C, GetMorphism( f ), IdentityMorphism( C, T[2] ) ), PutMorphism( g ) ) ] ),
            PutMorphism( f ) );
        
        return MorphismConstructor( Lenses, Source( f ), [ h1, h2 ], Target( g ) );
        
    end );
    
    ##
    AddIdentityMorphism( Lenses,
      
      function ( Lenses, A )
        local pair_A, id_1, id_2;
        
        pair_A := UnderlyingPairOfObjects( A );
        
        id_1 := IdentityMorphism( C, pair_A[1] );
        
        id_2 := ProjectionInFactorOfDirectProduct( C, pair_A, 2 );
        
        return MorphismConstructor( Lenses, A, [ id_1, id_2 ], A );
        
    end );
    
    ## Monoidal Structure
    ##
    AddTensorUnit( Lenses,
      
      function ( Lenses )
        local C, T;
        
        C := UnderlyingCategory( Lenses );
        
        T := TerminalObject( C );
        
        return ObjectConstructor( Lenses, Pair( T, T ) );
        
    end );
    
    ##
    AddTensorProductOnObjects( Lenses,
      
      function ( Lenses, A, B )
        local C;
        
        C := UnderlyingCategory( Lenses );
        
        A := UnderlyingPairOfObjects( A );
        B := UnderlyingPairOfObjects( B );
        
        return ObjectConstructor( Lenses,
                    [ DirectProduct( C, [ A[1], B[1] ] ), DirectProduct( C, [ A[2], B[2] ] ) ] );
        
    end );
    
    ##
    AddTensorProductOnMorphismsWithGivenTensorProducts( Lenses,
      
      function ( Lenses, source, f, g, target )
        local C, A, B, U, V, get, put;
        
        C := UnderlyingCategory( Lenses );
        
        f := UnderlyingPairOfMorphisms( f );
        g := UnderlyingPairOfMorphisms( g );
        
        A := UnderlyingPairOfObjects( Source( f ) );
        B := UnderlyingPairOfObjects( Target( f ) );
        
        U := UnderlyingPairOfObjects( Source( g ) );
        V := UnderlyingPairOfObjects( Target( g ) );
        
        get := DirectProductFunctorial( C, [ f[1], g[1] ] );
        
        put := PreCompose( C,
                  DirectProductFunctorial( C,
                      [ IdentityMorphism( C, A[1] ), Braiding( C, U[1], B[2] ), IdentityMorphism( C, V[2] ) ] ),
                  DirectProductFunctorial( C, [ f[2], g[2] ] ) );
        
        return MorphismConstructor( Lenses, source, [ get, put ], target );
        
    end );
    
    ##
    AddAssociatorRightToLeftWithGivenTensorProducts( Lenses,
      
      function ( Lenses, A_x_BxC, A, B, C, AxB_x_C )
        
        return IdentityMorphism( Lenses, A_x_BxC );
        
    end );
    
    ##
    AddAssociatorLeftToRightWithGivenTensorProducts( Lenses,
      
      function ( Lenses, AxB_x_C, A, B, C, A_x_BxC )
        
        return IdentityMorphism( Lenses, AxB_x_C );
        
    end );
    
    ##
    AddLeftUnitorWithGivenTensorProduct( Lenses,
      
      function ( Lenses, A, IxA )
        
        return IdentityMorphism( Lenses, A );
        
    end );
    
    ##
    AddLeftUnitorInverseWithGivenTensorProduct( Lenses,
      
      function ( Lenses, A, IxA )
        
        return IdentityMorphism( Lenses, A );
        
    end );
    
    ##
    AddRightUnitorWithGivenTensorProduct( Lenses,
      
      function ( Lenses, A, AxI )
        
        return IdentityMorphism( Lenses, A );
        
    end );
    
    ##
    AddRightUnitorInverseWithGivenTensorProduct( Lenses,
      
      function ( Lenses, A, AxI )
        
        return IdentityMorphism( Lenses, A );
        
    end );
    
    ##
    ##
    AddBraidingWithGivenTensorProducts( Lenses,
      
      function ( Lenses, AxB, A, B, BxA )
        local C, get, put;
        
        C := UnderlyingCategory( Lenses );
        
        A := UnderlyingPairOfObjects( A );
        B := UnderlyingPairOfObjects( B );
        
        get := Braiding( C, A[1], B[1] );
        
        put := PreCompose( C,
                  ProjectionInFactorOfDirectProduct( C,
                    [ DirectProduct( C, [ A[1], B[1] ] ), DirectProduct( C, [ B[2], A[2] ] ) ], 2 ),
                  Braiding( C, B[2], A[2] ) );
        
        return MorphismConstructor( Lenses, AxB, Pair( get, put ), BxA );
        
    end );
    
    ##
    AddBraidingInverseWithGivenTensorProducts( Lenses,
      
      function ( Lenses, BxA, A, B, AxB )
        
        return BraidingWithGivenTensorProducts( Lenses, BxA, B, A, AxB );
        
    end );
    
    ##
    AddRandomMorphismWithFixedSourceAndRangeByInteger( Lenses,
      
      function ( Lenses, S, T, i )
        local C, pair_S, pair_T, get, put;
        
        C := UnderlyingCategory( Lenses );
        
        pair_S := UnderlyingPairOfObjects( S );
        pair_T := UnderlyingPairOfObjects( T );
        
        get := RandomMorphism( pair_S[1], pair_T[1], i );
        put := RandomMorphism( DirectProduct( C, [ pair_S[1], pair_T[2] ] ), pair_S[2], i );
        
        return MorphismConstructor( Lenses, S, Pair( get, put ), T );
        
    end );
    
    ##
    AddSimplifyMorphism( Lenses,
      
      function ( Lenses, f, i )
        local C, datum;
        
        C := UnderlyingCategory( Lenses );
        
        datum := List( UnderlyingPairOfMorphisms( f ), p -> SimplifyMorphism( C, p, i ) );
        
        return MorphismConstructor(
                  Lenses,
                  Source( f ),
                  datum,
                  Target( f ) );
        
    end );
    
    Finalize( Lenses );
    
    return Lenses;
    
end );

##
InstallOtherMethod( IsEqualForMorphisms,
          [ IsCategoryOfLenses, IsMorphismInCategoryOfLenses, IsMorphismInCategoryOfLenses ],
  
  function ( Lenses, f, g )
    local C;
    
    C := UnderlyingCategory( Lenses );
    
    return IsEqualForMorphisms( C, GetMorphism( f ), GetMorphism( g ) )
            and IsEqualForMorphisms( C, PutMorphism( f ), PutMorphism( g ) );
    
end );

##
InstallOtherMethod( IsCongruentForMorphisms,
          [ IsCategoryOfLenses, IsMorphismInCategoryOfLenses, IsMorphismInCategoryOfLenses ],
  
  function ( Lenses, f, g )
    local C;
    
    C := UnderlyingCategory( Lenses );
    
    return IsCongruentForMorphisms( C, GetMorphism( f ), GetMorphism( g ) )
            and IsCongruentForMorphisms( C, PutMorphism( f ), PutMorphism( g ) );
    
end );

##
InstallMethod( EmbeddingIntoCategoryOfLenses,
          [ IsCategoryOfSmoothMaps, IsCategoryOfLenses ],
  
  function ( C, Lenses )
    local F;
    
    Assert( 0, IsIdenticalObj( C, UnderlyingCategory( Lenses ) ) );
    
    F := CapFunctor( "Embedding functor into category of lenses", C, Lenses );
    
    AddObjectFunction( F,
      function ( A )
        
        return ObjectConstructor( Lenses, Pair( A, A ) );
        
    end );
    
    AddMorphismFunction( F,
      
      function ( source, f, target )
        
        return MorphismConstructor( Lenses, source, Pair( f, ReverseDifferential( C, f ) ), target );
        
    end );
    
    return F;
    
end );


##    TxP3               P
##        ------------>
##    TxP3               P
##
##
##              get
##    TxP3 ------------> P
##
##    TxP3 <------------ TxP4
##              put
##
##  beta_1:=0.8; beta_2:=0.999; epsilon:=0.02; delta:=1.e-8;;
##

##
InstallOtherMethod( \.,
          [ IsCategoryOfLenses, IsPosInt ],
  
  function ( Lenses, string_as_int )
    local C, f;
    
    C := UnderlyingCategory( Lenses );
    
    if not IsCategoryOfSmoothMaps( C ) then
        TryNextMethod( );
    fi;
    
    f := NameRNam( string_as_int );
    
    # categorical construction
    if f = "GradientDescentWithMomentumOptimizer_" then
        
        return
          
          function ( n )
            return
              # both are non-negative
              function ( alpha, mu )
                local P, Smooth, P3, P2, get, p1, p2, p3, s, put;
                
                Smooth := UnderlyingCategory( Lenses );
                
                P := Smooth.( n );
                
                # (PxP, PxP) -> (P, P)
                Smooth := UnderlyingCategory( Lenses );
                
                P3 := Smooth.( 3 * n );
                P2 := Smooth.( 2 * n );
                
                get := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, [ P, P ], 2, P2 );
                
                p1 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, [ P, P, P ], 1, P3 );
                p2 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, [ P, P, P ], 2, P3 );
                p3 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, [ P, P, P ], 3, P3 );
                
                s := AdditionForMorphisms( Smooth,
                        MultiplyWithElementOfCommutativeRingForMorphisms( Smooth, alpha, p3 ),
                        MultiplyWithElementOfCommutativeRingForMorphisms( Smooth, mu, p1 ) );
                
                put := UniversalMorphismIntoDirectProductWithGivenDirectProduct( Smooth,
                            [ P, P ], P3, [ s, AdditionForMorphisms( Smooth, p2, s ) ], P2 );
                
                return MorphismConstructor( Lenses,
                          ObjectConstructor( Lenses, [ P2, P2 ] ),
                          Pair( get, put ),
                          ObjectConstructor( Lenses, [ P, P ] ) );
                
              end;
          
          end;
          
    # direct construction
    elif f = "GradientDescentWithMomentumOptimizer" then
        
        return
          function ( n )
            return
              function ( alpha, mu )
                local Smooth, P, P2, P3, get, map, jacobian_matrix, put, S, T;
                
                Smooth := UnderlyingCategory( Lenses );
                
                P := Smooth.( n );
                P2 := Smooth.( 2 * n );
                P3 := Smooth.( 3 * n );
                
                get := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, [ P, P ], 2, P2 );
                
                map :=
                  function ( vec )
                    local s;
                    
                    s := List( [ 1 .. n ], i -> mu * vec[i] + alpha * vec[2 * n + i] );
                    
                    return Concatenation( s,  List( [ 1 .. n ], i -> vec[n + i] + s[i] ) );
                    
                  end;
                
                jacobian_matrix :=
                  function ( vec )
                    local mu_mat, alpha_mat, zero_mat, id_mat;
                    
                    mu_mat := DiagonalMat( ListWithIdenticalEntries( n, mu ) );
                    alpha_mat := DiagonalMat( ListWithIdenticalEntries( n, alpha ) );
                    zero_mat := DiagonalMat( ListWithIdenticalEntries( n, 0 ) );
                    id_mat := DiagonalMat( ListWithIdenticalEntries( n, 1 ) );
                    
                    return
                      Concatenation(
                          ListN( mu_mat, zero_mat, alpha_mat, Concatenation ),
                          ListN( mu_mat, id_mat, alpha_mat, Concatenation ) );
                    
                  end;
                
                put := MorphismConstructor( Smooth, P3, Pair( map, jacobian_matrix ), P2 );
                
                S := ObjectConstructor( Lenses, [ P2, P2 ] );
                T := ObjectConstructor( Lenses, [ P, P ] );
                
                return MorphismConstructor( Lenses, S, Pair( get, put ), T );
                
              end;
              
          end;
          
    elif f = "AdagradOptimizer_" then
        
        return
          function ( n )
            return
              # the standard value of delta is 1.e-8
              function ( alpha, delta )
                local Smooth, P, P2, P3, get, p1, p2, p3, s, t, x, put, S, T;
                
                Smooth := UnderlyingCategory( Lenses );
                
                P := Smooth.( n );
                P2 := Smooth.( 2 * n );
                P3 := Smooth.( 3 * n );
                
                get := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, [ P, P ], 2, P2 );
                
                p1 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, [ P, P, P ], 1, P3 );
                
                p2 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, [ P, P, P ], 2, P3 );
                
                p3 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, [ P, P, P ], 3, P3 );
                
                s := AdditionForMorphisms( Smooth,
                        p1,
                        PreCompose( Smooth, p3, DirectProductFunctorial( Smooth, ListWithIdenticalEntries( n, Smooth.Power( 2 ) ) ) ) );
                
                t := MultiplicationForMorphisms( Smooth,
                        SmoothMorphism( Smooth, P3, ListWithIdenticalEntries( n, alpha ), Smooth.( n ) ),
                        PreCompose( Smooth,
                            AdditionForMorphisms( Smooth,
                                  SmoothMorphism( Smooth, P3, ListWithIdenticalEntries( n, delta ), Smooth.( n ) ),
                                  PreCompose( Smooth,
                                      s,
                                      DirectProductFunctorial( Smooth, ListWithIdenticalEntries( n, Smooth.Sqrt ) ) ) ),
                            DirectProductFunctorial( Smooth, ListWithIdenticalEntries( n, Smooth.Power( -1 ) ) ) ) );
                
                x := AdditionForMorphisms( Smooth, p2, MultiplicationForMorphisms( Smooth, t, p3 ) );
                
                put := UniversalMorphismIntoDirectProduct( Smooth, [ s, x ] );
                
                S := ObjectConstructor( Lenses, Pair( P2, P2 ) );
                T := ObjectConstructor( Lenses, Pair( P, P ) );
                
                return MorphismConstructor( Lenses, S, Pair( get, put ), T );
                
              end;
            
          end;
          
    # direct construction
    elif f = "AdagradOptimizer" then
        
        return
          function ( n )
            return
              function ( alpha, delta )
                local Smooth, P, P2, P3, get, map, jacobian_matrix, put, S, T;
                
                Smooth := UnderlyingCategory( Lenses );
                
                P := Smooth.( n );
                P2 := Smooth.( 2 * n );
                P3 := Smooth.( 3 * n );
                
                get := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, [ P, P ], 2, P2 );
                
                map :=
                  function ( vec )
                    local l;
                    
                    l := List( [ 1 .. n ], i -> vec[2 * n + i] ^ 2 );
                    
                    return
                      Concatenation(
                        List( [ 1 .. n ], i -> vec[i] + l[i] ),
                        List( [ 1 .. n ], i -> vec[n + i] + alpha * vec[2 * n + i] / ( delta + Sqrt( vec[i] + l[i] ) ) ) );
                  end;
                
                jacobian_matrix :=
                  function ( vec )
                    local l, m;
                    
                    l := List( [ 1 .. n ], i -> Sqrt( vec[i] + vec[2 * n + i] ^ 2 ) );
                    
                    m := List( [ 1 .. n ], i -> - alpha * vec[2 * n + i] / ( l[i] * ( delta + l[i] ) ^ 2 ) );
                    
                    return
                      Concatenation(
                          ListN(
                            IdentityMat( n ),
                            ListWithIdenticalEntries( n, ListWithIdenticalEntries( n, 0 ) ),
                            DiagonalMat( List( [ 1 .. n ], i -> 2 * vec[2 * n + i] ) ),
                              Concatenation ),
                          ListN(
                            DiagonalMat( List( [ 1 .. n ], i -> m[i] / 2 ) ),
                            IdentityMat( n ),
                            DiagonalMat( List( [ 1 .. n ], i -> m[i] * vec[2 * n + i] + alpha / ( delta + l[i] ) ) ),
                              Concatenation ) );
                    
                  end;
                
                put := MorphismConstructor( Smooth, P3, Pair( map, jacobian_matrix ), P2 );
                
                S := ObjectConstructor( Lenses, Pair( P2, P2 ) );
                T := ObjectConstructor( Lenses, Pair( P, P ) );
                
                return MorphismConstructor( Lenses, S, Pair( get, put ), T );
                
              end;
              
          end;
          
    # categorical construction
    elif f = "GradientDescentOptimizer_" then
        
        return
          function ( n )
            return
              function ( alpha )
                local Smooth, P, P2, get, p1, p2, put, S;
                
                # (P, P) -> (P, P)
                Smooth := UnderlyingCategory( Lenses );
                
                P := Smooth.( n );
                P2 := Smooth.( 2 * n );
                
                get := IdentityMorphism( Smooth, P );
                
                p1 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, [ P, P ], 1, P2 );
                p2 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, [ P, P ], 2, P2 );
                
                put := AdditionForMorphisms( Smooth,
                          p1, MultiplyWithElementOfCommutativeRingForMorphisms( Smooth, alpha, p2 ) );
                
                S := ObjectConstructor( Lenses, Pair( P, P ) );
                
                return MorphismConstructor( Lenses, S, Pair( get, put ), S );
                
              end;
              
          end;
          
    # direct construction
    elif f = "GradientDescentOptimizer" then
        
        return
          function ( n )
            return
              function ( alpha )
                local Smooth, P, get, map, jacobian_matrix, put, S;
                
                Smooth := UnderlyingCategory( Lenses );
                
                P := Smooth.( n );
                
                get := IdentityMorphism( Smooth, P );
                
                map :=
                  function ( vec )
                    
                    return List( [ 1 .. n ], i -> vec[i] + alpha * vec[n + i] );
                    
                  end;
                
                jacobian_matrix :=
                  function ( vec )
                    
                    return ListN( IdentityMat( n ), DiagonalMat( ListWithIdenticalEntries( n, alpha ) ), Concatenation );
                    
                  end;
                
                put := MorphismConstructor( Smooth, Smooth.( 2 * n ), Pair( map, jacobian_matrix ), P );
                
                S := ObjectConstructor( Lenses, Pair( P, P ) );
                
                return MorphismConstructor( Lenses, S, Pair( get, put ), S );
                
              end;
              
            end;
            
    elif f = "DefaultGradientDescentOptimizer_" then
        
        return
          function ( n )
            
            return Lenses.GradientDescentOptimizer_( n )( 0.1 );
            
          end;
          
    elif f = "DefaultGradientDescentOptimizer" then
        
        return
          function ( n )
            
            return Lenses.GradientDescentOptimizer( n )( 0.1 );
            
          end;
          
    # categorical construction
    elif f = "AdamOptimizer_" then
        
        return
          function ( n )
            return
              function ( beta_1, beta_2, epsilon, delta )
                local Smooth, P, T, diagram, TxP3, get, TxP4, p1, p2, p3, p4, p5,
                  put_1, m, put_2, b, m_hat, v, put_3, v_hat, delta_n, s, put_4, put;
                
                Smooth := UnderlyingCategory( Lenses );
                
                P := Smooth.( n );
                
                T := Smooth.1;
                
                diagram := [ T, P, P, P ];
                
                # the status object is TxPxP
                TxP3 := DirectProduct( Smooth, diagram );
                
                get := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, diagram, 4, TxP3 );
                
                diagram := Concatenation( diagram, [ P ] );
                
                TxP4 := DirectProduct( Smooth, diagram );
                
                p1 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, diagram, 1, TxP4 );
                p2 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, diagram, 2, TxP4 );
                p3 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, diagram, 3, TxP4 );
                p4 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, diagram, 4, TxP4 );
                p5 := ProjectionInFactorOfDirectProductWithGivenDirectProduct( Smooth, diagram, 5, TxP4 );
                
                put_1 := AdditionForMorphisms( Smooth, p1, SmoothMorphism( Smooth, TxP4, [ 1 ], Smooth.( 1 ) ) );
                
                m := AdditionForMorphisms( Smooth,
                          MultiplyWithElementOfCommutativeRingForMorphisms( Smooth, beta_1, p2 ),
                          MultiplyWithElementOfCommutativeRingForMorphisms( Smooth, 1 - beta_1, p5 ) );
                
                put_2 := m;
                
                b := PreComposeList( Smooth,
                          [ p1,
                            SubtractionForMorphisms( Smooth,
                              SmoothMorphism( Smooth, Smooth.( 1 ), [ 1 ], Smooth.( 1 ) ),
                              Smooth.PowerBase( beta_2 ) ),
                            Smooth.Power( -1 ) ] );
                
                b := UniversalMorphismIntoDirectProduct( Smooth,
                            ListWithIdenticalEntries( n, b ) );
                
                m_hat := MultiplicationForMorphisms( Smooth, m, b );
                
                v := AdditionForMorphisms( Smooth,
                          MultiplyWithElementOfCommutativeRingForMorphisms( Smooth,
                              beta_2, p3 ),
                          MultiplyWithElementOfCommutativeRingForMorphisms( Smooth,
                              1 - beta_2, MultiplicationForMorphisms( Smooth, p5, p5 ) ) );
                
                put_3 := v;
                
                b := PreComposeList( Smooth,
                          [ p1,
                            SubtractionForMorphisms( Smooth,
                                SmoothMorphism( Smooth, Smooth.( 1 ), [ 1 ], Smooth.( 1 ) ),
                                Smooth.PowerBase( beta_2 ) ),
                            Smooth.Power( -1 ) ] );
                
                b := UniversalMorphismIntoDirectProduct( Smooth,
                            ListWithIdenticalEntries( n, b ) );
                
                v_hat := MultiplicationForMorphisms( Smooth, v, b );
                
                delta_n := SmoothMorphism( Smooth,
                              TxP4,
                              ListWithIdenticalEntries( n, delta ),
                              Smooth.( n ) );
                
                s := PreCompose( Smooth,
                        AdditionForMorphisms( Smooth,
                          delta_n,
                          PreCompose( Smooth,
                              v_hat,
                              DirectProductFunctorial( Smooth, ListWithIdenticalEntries( n, Smooth.Sqrt ) ) ) ),
                        DirectProductFunctorial( Smooth, ListWithIdenticalEntries( n, Smooth.Power( -1 ) ) ) );
                
                s := MultiplyWithElementOfCommutativeRingForMorphisms( Smooth, epsilon, MultiplicationForMorphisms( Smooth, s, m_hat ) );
                
                put_4 := AdditionForMorphisms( Smooth, p4, s );
                
                put := UniversalMorphismIntoDirectProduct( Smooth, [ put_1, put_2, put_3, put_4 ] );
                
                return MorphismConstructor( Lenses,
                          ObjectConstructor( Lenses, Pair( TxP3, TxP3 ) ),
                          Pair( get, put ),
                          ObjectConstructor( Lenses, Pair( P, P ) ) );
                
                end;
          
          end;
          
    elif f = "AdamOptimizer" then
        
        return
          function ( n )
            return
              #
              # the default values: beta_1 := 0.8, beta_2 := 0.999, epsilon := 0.02, delta := 1.e-8
              #
              function ( beta_1, beta_2, epsilon, delta )
                local Smooth, map_1, jacobian_matrix_1, get, map_2, jacobian_matrix_2, put;
                
                Smooth := UnderlyingCategory( Lenses );
                
                map_1 :=
                  function ( vec )
                    
                    return List( [ 2 * n + 2 .. 3 * n + 1 ], i -> vec[i] );
                    
                  end;
                
                jacobian_matrix_1 :=
                  function ( vec )
                    local id_mat, zero_mat;
                    
                    id_mat := IdentityMat( n );
                    
                    zero_mat := ListWithIdenticalEntries( n, ListWithIdenticalEntries( 2 * n + 1, 0 ) );
                    
                    return List( [ 1 .. n ], i -> Concatenation( zero_mat[i], id_mat[i] ) );
                    
                  end;
                 
                get := MorphismConstructor( Smooth, Smooth.( 3 * n + 1 ), Pair( map_1, jacobian_matrix_1 ), Smooth.( n ) );
                
                map_2 :=
                  # vec: t, m, v, x, d
                  function ( vec )
                    local c_1, c_2, c_3, c_4, t, m, v, x;
                    
                    if IsZero( vec[1] ) then
                        Error( "the first entry of the vector (time) must be an integer greater or equal to 1!\n" );
                    fi;
                    
                    c_1 := 1 - beta_1;
                    c_2 := 1 - beta_2;
                    c_3 := 1 - beta_2 ^ vec[1];
                    c_4 := epsilon / c_3;
                    
                    t := [ vec[1] + 1 ];
                    m := List( [ 1 .. n ], i -> beta_1 * vec[1 + i] + c_1 * vec[1 + 3 * n + i] );
                    v := List( [ 1 .. n ], i -> beta_2 * vec[1 + n + i] + c_2 * vec[1 + 3 * n + i] ^ 2 );
                    x := List( [ 1 .. n ], i -> vec[1 + 2 * n + i] + c_4 * ( m[i] / ( delta + Sqrt( v[i] / c_3 ) ) ) );
                    
                    return Concatenation( t, m, v, x );
                    
                  end;
                
                jacobian_matrix_2 :=
                  function ( vec )
                    local c_1, c_2, c_3, c_4, c_5, c_6, c_7, c_8, m, v, sqrt, tau, j_t, j_m, j_v, j_x;
                    
                    c_1 := 1 - beta_1;
                    c_2 := 1 - beta_2;
                    c_3 := beta_2 ^ vec[1];
                    c_4 := 1 - c_3;
                    c_5 := Log( beta_2 );
                    c_6 := c_3 * c_5;
                    c_7 := epsilon * beta_1 / c_4;
                    c_8 := epsilon / c_4 ^ 2;
                    
                    m := List( [ 1 .. n ], i -> beta_1 * vec[1 + i] + c_1 * vec[1 + 3 * n + i] );
                    v := List( [ 1 .. n ], i -> beta_2 * vec[1 + n + i] + c_2 * vec[1 + 3 * n + i] ^ 2 );
                    sqrt := List( [ 1 .. n ], i -> Sqrt( v[i] / c_4 ) );
                    tau := List( [ 1 .. n ], i -> c_8 * m[i] / ( delta + sqrt[i] ) );
                    
                    j_t := [ Concatenation( [ 1 ], ListWithIdenticalEntries( 4 * n, 0 ) ) ];
                    
                    j_m :=
                      ListN(
                        ListWithIdenticalEntries( n, [ 0 ] ),
                        DiagonalMat( ListWithIdenticalEntries( n, beta_1 ) ),
                        ListWithIdenticalEntries( n, ListWithIdenticalEntries( 2 * n, 0 ) ),
                        DiagonalMat( ListWithIdenticalEntries( n, c_1 ) ),
                        Concatenation );
                    
                    j_v :=
                      ListN(
                        ListWithIdenticalEntries( n, [ 0 ] ),
                        ListWithIdenticalEntries( n, ListWithIdenticalEntries( n, 0 ) ),
                        DiagonalMat( ListWithIdenticalEntries( n, beta_2 ) ),
                        ListWithIdenticalEntries( n, ListWithIdenticalEntries( n, 0 ) ),
                        DiagonalMat( List( [ 1 .. n ], i -> 2 * vec[1 + 3 * n + i] * c_2 ) ),
                        Concatenation );
                    
                    j_x :=
                      ListN(
                        List( [ 1 .. n ], i -> [ c_6 * tau[i] * ( 1 - v[i] / ( 2 * c_4 * sqrt[i] * ( delta + sqrt[i] ) ) ) ] ),
                        DiagonalMat( List( [ 1 .. n ], i -> c_7 / ( delta + sqrt[i] ) ) ),
                        DiagonalMat( List( [ 1 .. n ], i -> -beta_2 * epsilon * m[i] / ( 2 * sqrt[i] * c_4 ^ 2 * ( delta + sqrt[i] ) ^ 2 ) ) ),
                        IdentityMat( n ),
                        DiagonalMat( List( [ 1 .. n ], i ->
                          -epsilon * ( vec[1 + 3 * n + i] * c_2 * m[i] / ( sqrt[i] * c_4 ^ 2 * ( delta + sqrt[i] ) ^ 2 ) - c_1 / ( c_4 * ( delta + sqrt[i] ) ) ) ) ),
                        Concatenation );
                    
                    return Concatenation( j_t, j_m, j_v, j_x );
                    
                  end;
                
                put := MorphismConstructor( Smooth, Smooth.( 4 * n + 1 ), Pair( map_2, jacobian_matrix_2 ), Smooth.( 3 * n + 1 ) );
                
                return
                  MorphismConstructor( Lenses,
                        ObjectConstructor( Lenses, Pair( Smooth.( 3 * n + 1 ), Smooth.( 3 * n + 1 ) ) ),
                        Pair( get, put ),
                        ObjectConstructor( Lenses, Pair( Smooth.( n ), Smooth.( n ) ) ) );
              end;
              
          end;
          
    elif f = "DefaultAdamOptimizer_" then
        
        return n -> Lenses.AdamOptimizer_( n )( 0.8, 0.999, 0.02, 1.e-8 );
        
    elif f = "DefaultAdamOptimizer" then
        
        return n -> Lenses.AdamOptimizer( n )( 0.8, 0.999, 0.02, 1.e-8 );
        
    elif f in [ "IdFunc", "Sum", "Mean", "Mul", "Power", "PowerBase", "Relu", "Sigmoid_", "Sigmoid", "Softmax_", "Softmax", "QuadraticLoss_",
                "QuadraticLoss", "CrossEntropyLoss_", "CrossEntropyLoss", "SoftmaxCrossEntropyLoss_", "SoftmaxCrossEntropyLoss" ] then
        
        return
          function ( arg... )
            
            return ApplyFunctor( EmbeddingIntoCategoryOfLenses( C, Lenses ), CallFuncList( C.( f ), arg ) );
            
          end;
          
    elif f in [ "Sqrt", "Exp", "Log", "Sin", "Cos", "BinaryCrossEntropyLoss_", "BinaryCrossEntropyLoss" ] then
        
        return ApplyFunctor( EmbeddingIntoCategoryOfLenses( C, Lenses ), C.( f ) );
        
    fi;
end );

##
InstallMethod( ViewString,
          [ IsObjectInCategoryOfLenses ],
  
  function ( U )
    
    U := UnderlyingPairOfObjects( U );
    
    return Concatenation( "(", ViewString( U[1] ), ", ", ViewString( U[2] ), ")" );
    
end );

##
InstallMethod( ViewString,
          [ IsMorphismInCategoryOfLenses ],
  
  function ( f )
    
    return Concatenation(
              ViewString( Source( f ) ),
              " -> ",
              ViewString( Target( f ) ),
              " defined by:",
              "\n\nGet Morphism:\n----------\n",
              ViewString( GetMorphism( f ) ),
              "\n\nPut Morphism:\n----------\n",
              ViewString( PutMorphism( f ) )
            );
    
end );

##
InstallMethod( DisplayString,
          [ IsMorphismInCategoryOfLenses ],
  
  function ( f )
    
    return Concatenation(
              ViewString( Source( f ) ),
              " -> ",
              ViewString( Target( f ) ),
              " defined by:\n",
              "\nGet Morphism:\n----------\n",
              DisplayString( GetMorphism( f ) ),
              "\nPut Morphism:\n----------\n",
              DisplayString( PutMorphism( f ) ) );
    
end );

##
InstallMethod( Display,
          [ IsMorphismInCategoryOfLenses ],
  
  function ( f )
    
    Print( Concatenation(
              ViewString( Source( f ) ),
              " -> ",
              ViewString( Target( f ) ),
              " defined by:\n",
              "\nGet Morphism:\n------------\n" ) );
    
    Display( GetMorphism( f ) );
    
    Print( "\nPut Morphism:\n------------\n" );
    
    Display( PutMorphism( f ) );
    
end );
