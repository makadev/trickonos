{?

  function fun_creator;
    var x;
    function x;           // replaces x
      ASSERT x is Func;
      Result := 1;
    end;
    ASSERT x is Func; // top function
    ASSERT x() = 1;   // top function result = 1
    Result := x;
  end;
 
  ASSERT fun_creator() is func; // returns top function
  ASSERT fun_creator()() = 1;   // calls top function
 
  ASSERT x = nil; // top fun not in global env declared
 
?}