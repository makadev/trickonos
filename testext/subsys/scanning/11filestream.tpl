{?

  scan := System.Scanning.FileScanStream('scanme1');
 
  ASSERT scan <> nil;

  s := '';
  while scan.Next() do
    s := s + scan.current;
  end;

  ASSERT s = 'abcdefg';

?}