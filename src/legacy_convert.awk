{if (length ($1) < 5) print $0; else
  {if ($1<0) $1 += 2*3.14159265;
  print sprintf("%10f%10f", $1, $2); };}
