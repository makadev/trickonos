{?

/*
   Typecheck, instances
   Instance does not have a fix typename, its declaration defined:
     "Class_"+Classname of instance
    
   this assumes that instancing works

 */

class SomeClass
end;

someinstance := SomeClass();

ASSERT someinstance is class_someclass;

?}