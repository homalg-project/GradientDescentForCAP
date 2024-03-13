# SPDX-License-Identifier: GPL-2.0-or-later
# MachineLearningForCAP: Exploring categorical machine learning in CAP
#
# Implementations
#

##
InstallMethod( Relu,
      [ IsFloat ],
  
  function ( a )
    
    return Maximum( a, 0. );
    
end );

##
InstallOtherMethod( Relu,
    [ IsObject ],
  
  function ( a )
    
    return Relu( Float( a ) );
    
end );
