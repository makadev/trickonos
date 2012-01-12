{?

  strscan := System.Scanning.StringScanStream('abcdefg');
 
  ASSERT strscan <> nil;

  s := '';
  while strscan.Next() do
    s := s + strscan.current;
  end;

  ASSERT s = 'abcdefg';

?}