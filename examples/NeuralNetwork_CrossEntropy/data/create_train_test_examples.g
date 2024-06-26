

# locate the current dir
current_dir := DirectoryCurrent( );

# create a file for the training dataset

# we have 4 classes in the plan

# class 0: everything outside of classes 1, 2 or 4

# class 1: everything inside the circle: (x1-0.5)^2 + (x2-0.5)^2 - 0.20 = 0
#    i.e., the solutions of: (x1-0.5)^2 + (x2-0.5)^2 - 0.20 <= 0

# class 2: everything inside the circle: (x1+0.5)^2 + (x2-0.5)^2 - 0.20 = 0
#    i.e., the solutions of: (x1+0.5)^2 + (x2-0.5)^2 - 0.20 <= 0

# class 3: the polytop defined by the points (0.5, 0), (-0.5, 0), (0, -1)
#    i.e., the common solutions of the inequalities: x2 <= 0, 1 - 2x1 + x2 >= 0, 1 + 2x1 + x2 >= 0

files := [ "training_examples.txt", "test_examples.txt" ];
nr_examples := [ 3000, 100 ];

noise := 0.5;

for i in [ 1, 2 ] do
    
    file := Filename( current_dir, files[i] );
    
    PrintTo( file, "[\n" );
    
    for j in [ 1 .. nr_examples[i] ] do
        
        x1 := Random( [ -0.01, 0.01 ] ) * Random( [ 1 .. 100 ] );
        x2 := Random( [ -0.01, 0.01 ] ) * Random( [ 1 .. 100 ] );
        
        if (x1 - 0.5)^2 + (x2 - 0.5)^2 - 0.20 <= 0. then
            
            label := [ 0, 1, 0, 0 ];
            
        elif (x1 + 0.5)^2 + (x2 - 0.5)^2 - 0.20 <= 0. then
            
            label := [ 0, 0, 1, 0 ];
            
        elif x2 <= 0. and 1 - 2 * x1 + x2 >= 0. and 1 + 2 * x1 + x2 >= 0. then
            
            label := [ 0, 0, 0, 1 ];
            
        else
            
            label := [ 1, 0, 0, 0 ];
            
        fi;
        
        AppendTo( file, Concatenation( [ x1, x2 ], label ), ",\n" );
        
    od;
    
    AppendTo( file, "]" );
    
od;

Display( "Done!" );
QUIT;
