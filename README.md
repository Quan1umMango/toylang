# Toylang
Toy language created in  Rust for educational purposes.

=======

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
Now theres two 

## Usage
### Compiling
```
cargo run <project-name>.tl <compilation-mode>
```

### Compiltion modes 
#### Bytecode (b):
Creates a `.basm` file which can be executed with the [Basm](https://github.com/Quan1umMango/bytecode) Virtual Machine 
#### Native (c):
This creates a binary which can then be executed. Not recommended as it can only be execute on one type of machine and lacks language features. The development for this compilation mode is paused.
Development might continue in the future.
### Viewing output
Currently, there is no printing to the screen directly. 
You have to use the ```exit()``` function with some integer exit code. 
If you're using the bytecode version, it outputs to the screen then exits.
If youre using Native compiler view it using ```echo %ERRORLEVEL%``` on windows. 

## Variables
Variables are declared with the ``let`` keyword.
```tl
let x = 34;
let y = x + 1;
let z = true;
```

## There are currently 4 types:
- Bool
- Int32
- Infer 
- Void
```tl
let bool = true;
let int = 420;
```
``Infer`` type is for the compiler when it doesn't know what value the variable has. All variables are set to Infer then converted to whatever type there is on the other side of the assignment. You would almost never have to use this. 
Variables cannot be assigned different types.
``Void`` is still in development.
```tl
let int = 1;
int = false;
```
Gives an error saying you can't assign int which has the type Int32 to Bool.

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

