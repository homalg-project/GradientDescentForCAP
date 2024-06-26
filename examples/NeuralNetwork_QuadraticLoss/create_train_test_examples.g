

# locate the current dir
current_dir := DirectoryCurrent( );

# create a file for the training dataset

## we want to approximate these secret perfect weights!
perfect_weights := [ 2, -3, 1 ];

## The input dimension is 2 and the output dimension is 1 --> an example dimension is 2 + 1 = 3.
## Each training example is of the form [x1, x2, y ] where y := 2x1 - 3x2 + 1 + some_error.

files := [ "training_examples.txt", "test_examples.txt" ];
nr_examples := [ 100, 20 ];

noise := 0.5;

for i in [ 1, 2 ] do
    
    file := Filename( current_dir, files[i] );
    
    PrintTo( file, "[\n" );
    
    for j in [ 1 .. nr_examples[i] ] do
        
        x1 := Random( [ -0.01, 0.01 ] ) * Random( [ 1 .. 100 ] );
        x2 := Random( [ -0.01, 0.01 ] ) * Random( [ 1 .. 100 ] );
        
        error := Random( [ -0.001, 0.001 ] ) * Random( [ 1 .. 100 ] );
        
        AppendTo( file, [ x1, x2, [ x1, x2, 1 ] * perfect_weights + noise * error ], ",\n" );
        
    od;
    
    AppendTo( file, "]" );
    
od;

Display( "Done!" );
QUIT;
