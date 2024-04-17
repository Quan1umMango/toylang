# Toylang
Toy language created in  Rust for educational purposes.

<<<<<<< HEAD
Has if statements, scopes, variables and exit.
Nothing more so no uncessesary bloat.


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
This implementation lacks an intermediate representation, so it is only able to compile to x86-64 assembly code. I'd like to implement an IR someday but as it is, the language won't compile correctly if you are not using windows x86-64 and have nasm and ld linked installed on your machine.

## Usage
### Compiling
```
cargo run <project-name>.tl
```
This creates a binary which can then be executed.
### Viewing output
Currently, there is no printing to the screen directly. 
You have to use the ```exit()``` function with some integer exit code. 
Then view it using ```echo %ERRORLEVEL%``` on windows. 

## Variables
Variables are declared with the ``let`` keyword.
```tl
let x = 34;
let y = x + 1;
let z = true;
```

## There are currently 3 types:
- Bool
- Int32
- Infer 
```tl
let bool = true;
let int = 420;
```
``Infer`` type is for the compiler when it doesn't know what value the variable has. All variables are set to Infer then converted to whatever type there is on the other side of the assignment. You would almost never have to use this. 
Variables cannot be assigned different types.
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
>>>>>>> 801daf79f380512be0f05f30af6b4e2535d685f3
