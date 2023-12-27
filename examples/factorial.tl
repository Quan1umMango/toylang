let fact = 1;
let n = 5;
if(n == 0 or n == 1) {
    fact = 1;
}else {
    while (n!=0) {
        fact *= n;
        n-=1;
      }
  }
  exit(fact);
