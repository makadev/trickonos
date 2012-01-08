
  function calc_nbit_primes( nbits );
  var i,j,k,l,f;
  begin
    if nbits > 1 then
      begin
        i := 1;
        j := (1 shl nbits)-1;
        Result := [2];
        repeat
          i := i+2;
          k := Result.Iterator();
          while k.Next() and
                (f := (((i div (l := k.Current))*l) <> i)) do
          end;
          k := nil;
          if f then
            Result.Append(i);
          end;
        until i >= j;
      end
    elseif nbits > 0 then
      begin
        Result := [2];
      end
    else
      begin
        Result := [];
      end;
  end;

