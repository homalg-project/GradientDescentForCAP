





DeclareCategory( "IsCategoryOfParametrisedMorphisms",
        IsCapCategory );

DeclareCategory( "IsObjectInCategoryOfParametrisedMorphisms",
        IsCapCategoryObject );

DeclareCategory( "IsMorphismInCategoryOfParametrisedMorphisms",
        IsCapCategoryMorphism );


DeclareOperation( "CategoryOfParametrisedMorphisms", [ IsCapCategory ] );

DeclareAttribute( "UnderlyingCategory", IsCategoryOfParametrisedMorphisms );

DeclareOperation( "AsMorphismInCategoryOfParametrisedMorphisms", [ IsCategoryOfParametrisedMorphisms, IsCapCategoryMorphism ] );

DeclareAttribute( "UnderlyingObject", IsObjectInCategoryOfParametrisedMorphisms );

DeclareAttribute( "ParameterObject", IsMorphismInCategoryOfParametrisedMorphisms );
DeclareAttribute( "ParametrisedMorphism", IsMorphismInCategoryOfParametrisedMorphisms );

DeclareOperation( "ReparametriseMorphism", [ IsMorphismInCategoryOfParametrisedMorphisms, IsCapCategoryMorphism ] );
DeclareOperation( "SwitchSourceAndParameterObject", [ IsMorphismInCategoryOfParametrisedMorphisms ] );

