
# Generics

A generic function or type is a function or type that is generic over a compile-time parameter.
Such compile-time parameters are usually types, but can also be values.
When a function or a type is generic over a type, then that type is called a type parameter.

## Terminology

* **Generic function**: A function that is generic over a compile-time parameter.
* **Generic type**: A type that is generic over a compile-time parameter.
* **Type parameter**: A compile-time parameter that is a type.
* **Constraint**: A trait that a type parameter must implement.
* **Concrete type**: The type that a type parameter is replaced with when using a generic function or type.

## Generic functions

You could for example have a forget function that takes in a value of *any* type and returns nothing:

```mylang
def forget(_: Any) -> Void // Any is a trait that all types implement
    return;
```

Unlike many other languages, generic functions and types does not need `<>` or `[]` to specify the type parameter.
Since the compiler knows that `Any` is a trait, it knows that the function is generic over the input type.
If you want to refer to the concrete type of the input, you can use the following *double parameter* syntax:

```mylang
def identity(value: T: Any) -> T
    return value;

def main() -> Void {
    let five: Int = identity(5);
}
```

The above function is generic over the type `T`, which can be any type, and returns a value of the same type.
If you want the caller to explicitly write out the type, you can add a type parameter to the function:

```mylang
def identity(T: Any, value: T) -> T
    return value;

def main() -> Void {
    let five = identity(Int, 5);
}
```

Note that the type parameter must come before any other parameters that use the type.
This is always the case for generic functions and types.

## Type constraints

So far we have shown functioned that are constrained by the `Any` trait, meaning that *any* type can be used.
(This is actually not completely accurate, see the section **Implicit type constraints**.)
If you replace the `Any` trait with, for example, the `Add` trait as a constraint, you can use the `+` operator.

```mylang
def add(a: T: Add, b: Add) -> T
    return a + b;
```

A type parameter can have multiple constraints, which are separated by `+`:

```mylang
def print_default(T: Default + ToStr) -> Void {
    println(T.default().to_str());
}
```

### Generic return types

You can also declare a type parameter as the return type:

```mylang
def uninitialize() -> Any
    return uninit;
```

This function can return a value of any type, where the value is simply uninitialized memory.
Though, the concrete return type must be known at compile time and can be specified by the caller:

```mylang
def main() -> Void {
    let int: Int = uninitialize();
}
```

Here the compiler can infer that the return type of `uninitialize` should be `Int`.

### Existential return types

The above mentioned generic return types might not be what you want.
Sometimes you want to return a value of some type that implements a trait.
In such case it might be useful to hide the concrete type of the return value and only expose the trait.
This can be done with an existential type, which are denoted with the `some` keyword:

```mylang
def get_to_str_value() -> some ToStr
    return true; // Bool implements ToStr
```

This is similar to how object-oriented languages tend to work when returning an object of some interface.
Existential types won't be covered further in this document.

## Generic types

As mentioned, types can also be generic.
Here is an example of a generic struct type:

```mylang
struct Point(T: Num) {
    x: T;
    y: T;
}
```

The `Point` type is generic over the type `T`, which must implement the `Num` trait.
The `Num` trait requires that the type must implement the basic math operations such as `+` and `-`.
The type `T` can then be used in the struct definition.

You can also have generic enum and union types:

```mylang
union Maybe(T: Any) {
    def Just = struct {value: T};
    def Nothing = struct {};
}
```

## Nested generics

The type system is quite expressive and allows for nested type parameters, even when using double parameter syntax:

```mylang
/*  Takes in some outer list that contains some inner lists that contains
 *      some values.
 *  Returns a new list of the type of the inner lists that contains all
 *      the values of the inner lists.
 */
def flatten(list: Outer: List(Inner: List(T: Any))) -> Inner {
    var result = Inner.new_empty();
    for (inner in list) {
        for (value in inner)
            result.push_back(value);
    }
    return result;
}
```

The `flatten` example shows how you can use type parameters inside the type of another type parameter.
This is also the case for the primitive reference types, pointer types, and optional types types.
But you don't have to use parenthesis around the inner typer parameter, when using primitive types with symbols:

```mylang
def ref_with_identifier(ref: Ref(T: Any)) -> Ref(T)
    return &ref;

def ref_with_symbol(ref: *T: Any) -> *T
    return &ref;
```

Those two functions are equivalent.

## Non type compile-time parameters

As mentioned in the first paragraph, compile-time parameters can also be values.
Which can be useful when you want to specify the size of a type, for example:

```mylang
// A fixed length mathematical vector
struct Vec(n: USize, T: Num) {
    data: [n]T;
}
```

## Default compile-time parameters

Just like other other parameters, compile-time parameters can have default values,
but only when you aren't using the double parameter syntax, as it would be ambiguous.
This is typically not used with functions, but it can be useful with types:

```mylang
struct Vec(n: USize = 2, T: Num = Int) {
    data: [n]T;
}
```

This allows you to use the `Vec` type without specifying the compile-time parameters:

```mylang
def main() -> Void {
    let vec = Vec.zero();
    println(vec);
}
```

Normally, the compiler wouldn't be able to fully infer the type of vec.
The default parameters allows the compiler to assume that `n` is `2` and `T` is `Int`.

## Implicit type constriants

**TODO**
