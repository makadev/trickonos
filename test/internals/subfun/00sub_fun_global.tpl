{?

  function fun_creator;      // lower fun
    function some_fun;       // top fun, only declared when lower fun is executed
      Result := 1;
    end;
  end;

  ASSERT some_fun = nil; // some_fun not declared, since codepath with decl not taken

  fun_creator(); // declare some_fun
 
  ASSERT some_fun is func;
  ASSERT some_fun <> fun_creator;
  
  some_fun2 := some_fun;
  some_fun := nil;
  
  /* and again */
  ASSERT some_fun = nil;
  fun_creator();
  ASSERT some_fun is func;
  ASSERT some_fun <> fun_creator;
  ASSERT some_fun = some_fun2;  // 2 different objects, but compare should be ok
  
?}