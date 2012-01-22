{?
  x := 0;
  
  function fun_creator; // <- var in lower fun
    x := x+1;
    ASSERT x < 10; // check for recursion 
    function fun_creator;
      ASSERT fun_creator is Func; // <- var in top fun
      Result := 1;
    end;
    ASSERT fun_creator is Func; // fun object in lower fun var
    ASSERT fun_creator() = 1;   // calls top fun
    Result := fun_creator;
  end;
 
  ASSERT fun_creator() is func; // calls lower fun, since it is set in globalenv
  ASSERT fun_creator()() = 1;   // call on top func object which is passed
 
?}