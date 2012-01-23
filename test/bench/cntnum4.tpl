{?

  class counter
    function cntnum;
      self.x := 1;
      repeat
        self.x := self.x + 1;
      until self.x > 1000000;
    end;
  end;
  
  counter().cntnum();
  
?}
