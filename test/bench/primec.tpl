{?
  System.marker('<?--','-->');

  USE "primesnb.tpl";

  nrbits := 12;

  writeln "calc "+nrbits.tostr()+"bit primes";
  primes := calc_nbit_primes(nrbits);

  writeln "done, calculated " + primes.length().tostr() + " primes";
?}
