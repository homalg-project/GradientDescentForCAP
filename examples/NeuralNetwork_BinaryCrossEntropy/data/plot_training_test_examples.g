LoadPackage( "GradientDescentForCAP" );

file := IO_File( "training_examples.txt" );
#file := IO_File( "test_examples.txt" );

examples := EvalString( IO_ReadUntilEOF( file ) );

points := List( examples, example -> example{[1, 2]} );
labels := List( examples, example -> example[3] );
ScatterPlotUsingPython( points, labels : size := "100" );
