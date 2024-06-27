

# locate the current dir
current_dir := DirectoryCurrent( );

# create a file for the training dataset

# we have 4 classes in the plan

# class 1: the union of the following sets:
#
#        : everything inside the circle: (x1-0.5)^2 + (x2-0.5)^2 - 0.20 = 0
#    i.e., the solutions of: (x1-0.5)^2 + (x2-0.5)^2 - 0.20 <= 0

#        : everything inside the circle: (x1+0.5)^2 + (x2-0.5)^2 - 0.20 = 0
#    i.e., the solutions of: (x1+0.5)^2 + (x2-0.5)^2 - 0.20 <= 0

#        : the polytop defined by the points (0.5, 0), (-0.5, 0), (0, -1)
#    i.e., the common solutions of the inequalities: x2 <= 0, 1 - 2x1 + x2 >= 0, 1 + 2x1 + x2 >= 0

# class 2: everything else

files := [ "training_examples.txt", "test_examples.txt" ];
nr_examples := [ 3000, 100 ];

for i in [ 1, 2 ] do
    
    file := Filename( current_dir, files[i] );
    
    PrintTo( file, "[\n" );
    
    for j in [ 1 .. nr_examples[i] ] do
        
        # we want more centered training examples
        x1 := Random( [ -0.01, 0.01 ] ) * Random( Concatenation ( [ 1 .. 30 ], [ 1 .. 30 ], [ 30 .. 100 ] ) );
        x2 := Random( [ -0.01, 0.01 ] ) * Random( Concatenation ( [ 1 .. 30 ], [ 1 .. 30 ], [ 30 .. 100 ] ) );
        
        if (x1 - 0.5)^2 + (x2 - 0.5)^2 - 0.20 <= 0. or (x1 + 0.5)^2 + (x2 - 0.5)^2 - 0.20 <= 0. or (x2 <= 0. and 1 - 2 * x1 + x2 >= 0. and 1 + 2 * x1 + x2 >= 0.) then
            
            label := 1;
            
        else
            
            label := 0;
            
        fi;
        
        AppendTo( file, [ x1, x2 , label ], ",\n" );
        
    od;
    
    AppendTo( file, "]" );
    
od;

Display( "Done!" );
QUIT;
