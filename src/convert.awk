#pi2 = 6.28318530718
{if (length ($1) < 5) {print $0;sg=0;} else
  {
  if ($1<0) {$1 += 6.28318530718 ; sg=-1 ;} else
    {
    if ($1>0) {sg=1;} else
      {
      if (sg<0) {$1 = 6.28318530718;} else 
        {
        if (sg==0) 
          {
          tt2=$2;
          getline;
          if ($1<0) {sg = -1; $1 += 6.28318530718; tt1 = 6.28318530718;} else {sg = 1; tt1 = 0.0}
          print sprintf(" %11.8f %11.8f", tt1, tt2); 
          };
        };
      };
    };
  print sprintf(" %11.8f %11.8f", $1, $2);
  };
}
