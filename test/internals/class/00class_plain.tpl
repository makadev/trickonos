{?

class plain end;

plain_obj := plain(); // instance by Call Method

plain_obj_2 := plain.Create(); // instance by Create Method

ASSERT plain is ClassType_plain;
ASSERT plain_obj is Class_Plain;
ASSERT plain_obj_2 is CLASS_PLAIN;

/* start testing setmember/getmember defaults */

plain_obj.x := 1;

ASSERT plain_obj.x = 1;
ASSERT plain_obj_2.x = nil;

plain_obj.x := nil;

ASSERT plain_obj.x = nil;

?}