# SPDX-License-Identifier: GPL-2.0-or-later
# GradientDescentForCAP: Exploring categorical machine learning in CAP
#
# Implementations
#
InstallMethod( LogitsMorphismOfNeuralNetwork,
          [ IsCategoryOfParametrisedMorphisms, IsPosInt, IsDenseList, IsPosInt ],
  
  function ( Para, input_layer_dim, hidden_layers_dims, output_layer_dim )
    local Smooth, dims, N, L, l_i, P_i, i;
    
    Smooth := UnderlyingCategory( Para );
    
    dims := Concatenation( [ input_layer_dim ], hidden_layers_dims, [ output_layer_dim ] );
    
    N := Length( dims );
    
    L := [ ];
    
    for i in [ 1 .. N - 1 ] do
        
        l_i := Para.AffineTransformation( dims[i], dims[i + 1] );
        
        P_i := UnderlyingObject( l_i );
        
        Add( L, l_i );
        
        if i <> N - 1 then
          
          Add( L, Para.Relu( dims[i + 1] ) );
          
        fi;
        
    od;
    
    return PreComposeList( Para, L );
    
end );

##
InstallMethod( PredictionMorphismOfNeuralNetwork,
          [ IsCategoryOfParametrisedMorphisms, IsPosInt, IsDenseList, IsPosInt, IsString ],
  
  function ( Para, input_layer_dim, hidden_layers_dims, output_layer_dim, activation )
    local logits;
    
    logits := LogitsMorphismOfNeuralNetwork( Para, input_layer_dim, hidden_layers_dims, output_layer_dim );
    
    if not activation in [ "Softmax", "Sigmoid", "IdFunc" ] then
        Error( "unrecognized activation functions!\n" );
    fi;
    
    return PreCompose( Para, logits, Para.( activation )( output_layer_dim ) );
    
end );

##
InstallMethod( LossMorphismOfNeuralNetwork,
          [ IsCategoryOfParametrisedMorphisms, IsPosInt, IsDenseList, IsPosInt, IsString ],
  
  function ( Para, input_layer_dim, hidden_layers_dims, output_layer_dim, activation )
    local Smooth, logits, paramter_obj, nr_parameters, id_output, loss;
    
    Smooth := UnderlyingCategory( Para );
    
    logits := LogitsMorphismOfNeuralNetwork( Para, input_layer_dim, hidden_layers_dims, output_layer_dim );
    
    paramter_obj := UnderlyingObject( logits );
    
    nr_parameters := RankOfObject( paramter_obj );
    
    logits := UnderlyingMorphism( logits );
    
    id_output := Smooth.IdFunc( output_layer_dim );
    
    logits := DirectProductFunctorial( Smooth, [ logits, id_output ] );
    
    if not activation in [ "Softmax", "Sigmoid", "IdFunc" ] then
        Error( "unkown activation function: ", activation, "!\n" );
    fi;
    
    if activation = "Softmax" then
        
        loss := "SoftmaxCrossEntropyLoss";
        
    elif activation = "Sigmoid" then
        
        Assert( 0, output_layer_dim = 1 );
        
        loss := "SigmoidBinaryCrossEntropyLoss";
        
    elif activation = "IdFunc" then
        
        loss := "QuadraticLoss";
        
    fi;
    
    loss := Smooth.( loss )( output_layer_dim );
    
    return
      MorphismConstructor( Para,
          ObjectConstructor( Para, Smooth.( input_layer_dim + output_layer_dim ) ),
          Pair( paramter_obj, PreCompose( Smooth, logits, loss ) ),
          ObjectConstructor( Para, Smooth.( 1 ) ) );
    
end );
