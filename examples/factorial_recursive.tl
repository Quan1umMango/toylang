// Factorial of numbers using recursion
fn factorial(int32 x):int32{
	if(x==0) { return 1; }
	return x * factorial(x-1);
}

exit(factorial(8));
