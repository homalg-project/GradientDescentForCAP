# SPDX-License-Identifier: GPL-2.0-or-later
# GradientDescentForCAP: Exploring categorical machine learning in CAP
#
# Implementations
#


##
InstallMethod( OneEpochUpdateLens,
          [ IsMorphismInCategoryOfParametrisedMorphisms, IsFunction, IsDenseList, IsPosInt ],
  
  function ( parametrised_morphism, optimizer, training_examples, batch_size )
    local Para, Smooth, nr_parameters, nr_training_examples, r, random_indices, cost, costs, Lenses, L, negative_learning_rate, get_per_epoch, put_per_epoch, epoch_lens;
    
    Para := CapCategory( parametrised_morphism );
    Smooth := UnderlyingCategory( Para );
    
    if IsEmpty( training_examples ) then
        Error( "the passed list 'training_examples' must contain at least one element!\n" );
    fi;
    
    nr_parameters := RankOfObject( UnderlyingObject( parametrised_morphism ) );
    
    if RankOfObject( UnderlyingObject( Target( parametrised_morphism ) ) ) <> 1 then
        Error( "the target of the parametrised_morphism morphism must be equal to 1!\n" );
    fi;
    
    nr_training_examples := Length( training_examples );
    
    if batch_size > nr_training_examples then
        Error( "the batch size must be smaller than the number of training examples!\n" );
    fi;
    
    r := nr_training_examples mod batch_size;
    
    if r <> 0 then
        
        random_indices := Shuffle( [ 1 .. nr_training_examples ] ){[1 .. batch_size - r]};
        
        Append( training_examples, training_examples{random_indices} );
        
        nr_training_examples := nr_training_examples + batch_size - r;
        
    fi;
    
    parametrised_morphism := AdjustToBatchSize( parametrised_morphism, batch_size );
    
    cost := SwitchSourceAndUnderlyingObject( parametrised_morphism );
    
    costs :=
      List( SplitDenseList( training_examples, batch_size ),
        batch -> UnderlyingMorphism( ReparametriseMorphism( cost, Smooth.Constant( Concatenation( batch ) ) ) ) );
    
    optimizer := optimizer( nr_parameters );
    
    Lenses := CapCategory( optimizer );
    
    L := EmbeddingIntoCategoryOfLenses( Smooth, Lenses );
    
    costs := List( costs, cost -> ApplyFunctor( L, cost ) );
    
    # multiply gradients with -1 to let them direct toward the local minimum
    negative_learning_rate :=
        MorphismConstructor( Lenses,
          ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 1 ) ] ),
          [ Smooth.IdFunc( 1 ), Smooth.Constant( 1, [ -1 ] ) ],
          ObjectConstructor( Lenses, [ Smooth.( 1 ), Smooth.( 0 ) ] ) );
    
    costs := List( costs, cost -> PreCompose( Lenses, optimizer, PreCompose( Lenses, cost, negative_learning_rate ) ) );
    
    get_per_epoch := PreCompose( UniversalMorphismIntoDirectProduct( Smooth, List( costs, cost -> GetMorphism( cost ) ) ), Smooth.Mean( nr_training_examples / batch_size ) );
    
    put_per_epoch := PreComposeList( Smooth, List( costs, cost -> PutMorphism( cost ) ) );
    
    epoch_lens :=
      MorphismConstructor( Lenses,
        Source( optimizer ),
        Pair( get_per_epoch, put_per_epoch ),
        Target( negative_learning_rate ) );
    
    return epoch_lens;
    
end );

##
InstallOtherMethod( OneEpochUpdateLens,
          [ IsMorphismInCategoryOfParametrisedMorphisms, IsFunction, IsString, IsPosInt ],
  
  function ( parametrised_morphism, optimizer, training_examples_path, batch_size )
    local Para, Smooth, training_examples;
    
    Para := CapCategory( parametrised_morphism );
    Smooth := UnderlyingCategory( Para );
    
    if not IsCategoryOfParametrisedMorphisms( Para ) then
        Error( "the passed morphism 'parametrised_morphism' must be a morphism in a category of parametrised morphisms!\n" );
    fi;
    
    if RankOfObject( UnderlyingObject( Target( parametrised_morphism ) ) ) <> 1 then
        Error( "the target of the parametrised_morphism morphism must be equal to 1!\n" );
    fi;
    
    if not IsExistingFile( training_examples_path ) then
        Error( "no file is found at ", training_examples_path, "!\n" );
    fi;
    
    training_examples := EvalString( IO_ReadUntilEOF( IO_File( training_examples_path ) ) );
    
    return OneEpochUpdateLens( parametrised_morphism, optimizer, training_examples, batch_size );
    
end );


##
InstallMethod( Fit,
          [ IsMorphismInCategoryOfLenses, IsPosInt, IsDenseList ],
  
  function( epoch_lens, n, w )
    local MOD, get, put, get_source, get_target, put_source, put_target, l, l_n, str_i, l_i, spaces, loss, i;
    
    MOD := GradientDescentForCAP.MOD;
    
    GradientDescentForCAP.MOD := "train";
    
    get := GetMorphism( epoch_lens );
    put := PutMorphism( epoch_lens );
    
    get_source := RankOfObject( Source( get ) );
    get_target := RankOfObject( Target( get ) );
    
    put_source := RankOfObject( Source( put ) );
    put_target := RankOfObject( Target( put ) );
    
    l := Length( w );
    
    if not ( get_source = l and get_target = 1 and put_source = l and put_target = l ) then
        Error( "the passed arguments are inconsistent!\n" );
    fi;
    
    l_n := Length( String( n ) );
    
    Print( "Epoch ", JoinStringsWithSeparator( ListWithIdenticalEntries( l_n - 1, " " ), "" ), "0/", String( n ), " - loss = ", String( get( w )[1] ), "\n" );
    
    for i in [ 1 .. n ] do
        
        str_i := String( i );
        
        l_i := Length( str_i );
        
        spaces := JoinStringsWithSeparator( ListWithIdenticalEntries( l_n - l_i, " " ), "" );
        
        w := put( w );
        
        loss := get( w );
        
        Print( "Epoch ", spaces, String( i ), "/", String( n ), " - loss = ", String( loss[1] ), "\n" );
        
        #Display( w );
        
    od;
    
    GradientDescentForCAP.MOD := MOD;
    
    return w;
    
end );
