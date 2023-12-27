
let fib = 1;
let prev1 = 1;
let prev2 = 0;
let n = 5; 
while(n!=0) {
    fib = prev1+prev2;
    prev2 = prev1;
    prev1 = fib;
    n -= 1;
}
exit(fib);

