// call 4 times. 
// xmin or xmax should (-)infty : only consider one boundary at a time
// Then switch co-ordinates x <-> y.
void map_restrict_poly () {


  while (i < *nin) {
    while (ISNA(xin[i])) i++;
    line_start = i;
    while (i < *nin && (xin[i] < *xmin || xin[i] > *xmax)) i++;    
