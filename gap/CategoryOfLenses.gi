






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
    
    if f = "AdamOptimizerWithHyperparameters_" then
        
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
          
    elif f = "AdamOptimizerWithHyperparameters" then
        
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
                    
                    return vec{[ 2 * n + 2 .. 3 * n + 1 ]};
                    
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
                  function ( vec )
                    local t, m, v, x, d, c_1, c_2, c_3, c_4, new_t, new_m, new_v, new_x;
                    
                    t := vec[1];
                    m := vec{[ 2 .. n + 1 ]};
                    v := vec{[ n + 2 .. 2 * n + 1 ]};
                    x := vec{[ 2 * n + 2 .. 3 * n + 1 ]};
                    d := vec{[ 3 * n + 2 .. 4 * n + 1 ]};
                    
                    c_1 := 1 - beta_1;
                    c_2 := 1 - beta_2;
                    c_3 := 1 - beta_2 ^ t;
                    c_4 := epsilon / c_3;
                    
                    new_t := [ t + 1 ];
                    new_m := List( [ 1 .. n ], i -> beta_1 * m[i] + c_1 * d[i] );
                    new_v := List( [ 1 .. n ], i -> beta_2 * v[i] + c_2 * d[i] ^ 2 );
                    new_x := List( [ 1 .. n ], i -> x[i] + c_4 * ( new_m[i] / ( delta + Sqrt( new_v[i] / c_3 ) ) ) );
                    
                    return Concatenation( new_t, new_m, new_v, new_x );
                    
                  end;
                
                jacobian_matrix_2 :=
                  function ( vec )
                    local t, m, v, x, d, c_1, c_2, c_3, c_4, c_5, c_6, c_7, c_8, new_m, new_v, sqrt, tau, j_t, j_m, j_v, j_x;
                    
                    t := vec[1];
                    m := vec{[ 2 .. n + 1 ]};
                    v := vec{[ n + 2 .. 2 * n + 1 ]};
                    x := vec{[ 2 * n + 2 .. 3 * n + 1 ]};
                    d := vec{[ 3 * n + 2 .. 4 * n + 1 ]};
                    
                    c_1 := 1 - beta_1;
                    c_2 := 1 - beta_2;
                    c_3 := beta_2 ^ t;
                    c_4 := 1 - c_3;
                    c_5 := Log( beta_2 );
                    c_6 := c_3 * c_5;
                    c_7 := epsilon * beta_1 / c_4;
                    c_8 := epsilon / c_4 ^ 2;
                    
                    new_m := List( [ 1 .. n ], i -> beta_1 * m[i] + c_1 * d[i] );
                    new_v := List( [ 1 .. n ], i -> beta_2 * v[i] + c_2 * d[i] ^ 2 );
                    sqrt := List( [ 1 .. n ], i -> Sqrt( new_v[i] / c_4 ) );
                    tau := List( [ 1 .. n ], i -> c_8 * new_m[i] / ( delta + sqrt[i] ) );
                    
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
                        DiagonalMat( List( [ 1 .. n ], i -> 2 * d[i] * c_2 ) ),
                        Concatenation );
                    
                    j_x :=
                      ListN(
                        List( [ 1 .. n ], i -> [ c_6 * tau[i] * ( 1 - new_v[i] / ( 2 * c_4 * sqrt[i] * ( delta + sqrt[i] ) ) ) ] ),
                        DiagonalMat( List( [ 1 .. n ], i -> c_7 / ( delta + sqrt[i] ) ) ),
                        DiagonalMat( List( [ 1 .. n ], i -> -beta_2 * epsilon * new_m[i] / ( 2 * sqrt[i] * c_4 ^ 2 * ( delta + sqrt[i] ) ^ 2 ) ) ),
                        IdentityMat( n ),
                        DiagonalMat( List( [ 1 .. n ], i -> -epsilon * ( d[i] * c_2 * new_m[i] / ( sqrt[i] * c_4 ^ 2 * ( delta + sqrt[i] ) ^ 2 ) - c_1 / ( c_4 * ( delta + sqrt[i] ) ) ) ) ),
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
          
    elif f = "AdamOptimizer_" then
        
        return n -> Lenses.AdamOptimizerWithHyperparameters_( n )( 0.8, 0.999, 0.02, 1.e-8 );
        
    elif f = "AdamOptimizer" then
        
        return n -> Lenses.AdamOptimizerWithHyperparameters( n )( 0.8, 0.999, 0.02, 1.e-8 );
        
    elif f in [ "IdFunc", "Sum", "Mul", "Power", "PowerBase", "Relu", "Sigmoid_", "Sigmoid", "Softmax_", "Softmax", "QuadraticLoss_",
                "QuadraticLoss", "CrossEntropyLoss_", "CrossEntropyLoss", "SoftmaxCrossEntropyLoss_", "SoftmaxCrossEntropyLoss" ] then
        
        return
          function ( arg... )
            
            return ApplyFunctor( EmbeddingIntoCategoryOfLenses( C, Lenses ), CallFuncList( C.( f ), arg ) );
            
          end;
          
    elif f in [ "Sqrt", "Exp", "Log", "Sin", "Cos" ] then
        
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
