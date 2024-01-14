
# Functions

Functions work like in most other languages.
They can take some values as input and return a value as output.
They can also have side effects, unless specified otherwise, which won't be covered here.

## Declaring a function

The syntax for declaring a function is as follows:

```mylang
def function_name(param1: Type1, param2: Type2, ...) -> ReturnType {
    // Function body
}
```

All of the types can in some cases be omitted, if the compiler can infer the types.

```mylang
def function_name(param1, param2, ...) -> {
    // Function body
}
```

Note that the `->` is still required when omitting the return type.

If the function body consists of only a single expression, the brackets can be replaces by a `return` statement:

```mylang
def sum(a: Int, b: Int) -> Int return a + b;
```

## Main function

The main function is the entry point of the program.
Its name must be `main` and it must take no parameters and return `Void`:

```mylang
def main() -> Void {
    // Start of the program
}
```

## Calling a function

Functions are run similarly to other languages:

```mylang
var value = sum(1, 2); // value = 3
```

## Anonymous functions

Functions can also be declared anonymously:

```mylang
def sum = (a: Int, b: Int) -> Int return a + b;
```

Directly assigning an anonymous function to a variable is equivalent to declaring the function like shown above.
Meaning that the following two functions are the same:

```mylang
def function_a() -> Void { println("Hello, World!") }
def function_b = () -> Void { println("Hello, World!") }
```

## Function types

Functions also have types.
Meaning that you can for example pass a function as a parameter to another function:

```mylang
def call_twice(int: Int, function: (param: Int) -> Int) -> Int
    return function(function(int));
```

Calling `call_twice` would then require a function that takes in and returns an `Int`:

```mylang
def add_one(int: Int) -> Int
    return int + 1;

def main() -> Void {
    let int = call_twice(10, add_one); // int = 12
}
```

You can even use lambda expressions to pass a function as a parameter:

```mylang
def main() -> Void {
    let int = call_twice(10, (int: Int) -> Int return int + 1); // int = 12
}
```

In reality, function types are actually traits and the `call_twice` function is generic over the function type.
When declaring a function, the compiler will create a new anonymous type for the function.
That anonymous type will implement the function's trait and some other traits, like `Copy` and `Sized(..)`
This allows the type system to know which concrete function is used when using functions as parameters.
This, in turn, allows the compiler to make some optimizations, like inlining the function.

# Methods

Functions can also be declared as methods. Methods exist in many languages.
They are functions that belong to a type and can be called on instances of that type.

## Declaration

You can use the `for` keyword followed by the parameter name and type to declare a method.
There are multiple ways you might want to format the method declaration:

```mylang
// Concise
for int: Int def is_even() -> Int
    return int % 2 == 0;

// Two lines
for int: Int
def is_odd() -> Int
    return int % 2 == 1;

// Indented
for int: Int
    def double() -> Int
        return int * 2;

// In brackets
for int: Int {
    def half() -> Int
        return int / 2;
}
```

When using the bracket syntax, all functions declared in the block will be methods of the specified type.

The previous examples shows methods that take in an `Int` as a value, but they can of course also take other types:

```mylang
for ints: *[3]Int {
    def sum() -> Int
        return ints.get(0) + ints.get(1) + ints.get(2);
}
```

## Calling a method

Methods are called like functions, but they are called on an instance of a type:

```mylang
let int = 10;
let is_even = int.is_even(); // is_even = true
```

## Method types

Please note that the first parameter of a method is also part of the function type.
Which means that: 

```mylang
for int: Int {
    def add(other: Int) -> Int
        return int + other;
}
```

... has the type `(int: Int, other: Int) -> Int`.

