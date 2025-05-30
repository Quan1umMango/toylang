# Pluto 
Toy language created in  Rust for educational purposes.

## Example
Simple program to calculate the factorial of a number (n) and exit with the factorial.
```tl
let fact = 1;
let n = 5;
if(n == 0 or n == 1) {
    fact = 1;
}
while (n>1) {
        fact *= n;
        n-=1;
      }
}
  exit(fact);

```

## Description
Pluto is a lightweight and simple programming language. It is very early in development. Pluto runs on the (Basm)[https://github.com/Quan1umMango/bytecode] (name yet to be changed) Virtual machine.

## Usage
### Compiling
```
cargo run <project-name>.tl
```

### Viewing output
Currently, there is no printing to the screen directly. 
You have to use the ```exit()``` function with some integer exit code and it prints the code and exits.
## Variables
Variables are declared with the ``let`` keyword.
```tl
// Comments begin with two /. They are ignored by the compiler
let x = 34; 
let y = x + 1;
let z = true;
let arr = [1,2,3];
let arr2 = [1;10]; // Creates a array of size 10, all elements as 1;
```

## Available Datatypes:
- Bool
- Int32
- Infer 
- Array<Type> where type is any other datatype 
- Slice<Type>
- Pointer<Type>
- Void

```tl
let b = true;
let int = 420;
```
``Infer`` type is for the compiler when it doesn't know what value the variable has. All variables are set to Infer then converted to whatever type there is on the other side of the assignment. You would almost never have to use this. 
Variables cannot be assigned different types.
``Void`` is still in development.

Variable once created, cannot change their type.
```tl
let int = 1;
int = false;
```
Gives an error saying you can't assign int which has the type int32 to bool.

If and else statements
```tl
if (some_condition) {

}else if (some_other_condition) {

}else {

}
```
While loop:
```tl
while(condition) {

}
```

Comparisions and other assignment also included.

## Functions 
They start with the ``fn`` keyword. 
General Structure:
```
fn <ident>(<type> <argIdent1>, <type> <argIdent2>,...,<type> <argIdentN>) : <optional-return-type>  {
  function body;
}
```
Example: Program to get the nth number in the fibbonacci series.
```
fn fib(int32 n): int32 {
  let ans = 1;
  let prev1 = 1;
  let prev2 = 0;
  while(n!=0) {
    ans = prev1+prev2;
    prev2 = prev1;
    prev1 = ans;
  
  n -= 1;
  }
  return ans;
}

exit(fib(40));
```

