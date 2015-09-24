{if (length ($1) < 5) {print $0;sg=0;} else
  {
  if ($1<0) {$1 += 2*3.14159265;sg=-1;} else
    {
    if ($1>0) {sg=1;} else
      {
      if (sg<0) {$1=2*3.14159265;} else 
        {
        if (sg==0) 
          {
          tt2=$2;
          getline;
          if ($1>0) {sg=1;} else {$1 += 2*3.14159265;sg=-1;}
          if(sg<0) {tt1=2*3.14159265;}
          else {tt1=0.0;}
          print sprintf(" %11.8f %11.8f", tt1, tt2); 
          };
        };
      };
    };
  print sprintf(" %11.8f %11.8f", $1, $2);
  };
}
