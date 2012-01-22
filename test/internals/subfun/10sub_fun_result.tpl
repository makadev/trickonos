{?

  function fun_creator; // var in lower function/name in global env
    function Result;    // var in top function/var in lower function
      ASSERT Result is Func;
      Result := 1;
    end;
    ASSERT Result is Func; // top function
    ASSERT Result() = 1;   // top function result = 1
  end;
 
  ASSERT fun_creator() is func; // returns top function
  ASSERT fun_creator()() = 1;   // calls top function
 
  ASSERT Result = nil; // top fun not in global env declared
?}