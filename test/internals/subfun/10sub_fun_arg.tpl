{?

  function fun_creator(somearg);
    function somearg;           // replaces something
      ASSERT somearg is Func;
      Result := 1;
    end;
    ASSERT somearg is Func; // top function
    ASSERT somearg() = 1;   // top function result = 1
    Result := somearg;
  end;
 
  ASSERT fun_creator('ignored') is func; // returns top function
  ASSERT fun_creator('ignored')() = 1;   // calls top function
 
  ASSERT somearg = nil; // top fun not in global env declared
 
?}