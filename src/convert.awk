## simple awk script to add 2*pi to negative longitudes
## thus creating a map with lon in [0,2pi]
{if (length ($1) < 5) print $0; else
  {if ($1<0) $1 += 2*3.14159265;
  print sprintf("%11.8f %11.8f", $1, $2); };}
