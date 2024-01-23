
# Structs

Structs are types that can contain multiple runtime values.

## Terminology

* **Struct (type)**: A type that can contain multiple values.
* **Struct field**: One of the values in a struct value.
* **Struct variable**: A variable that holds a struct value.

## Declaring a struct

A struct can be declared with the `struct` keyword:

```mylang
struct Point {
    var x: Float;
    var y: Float;
}
```

Some traits are automatically implemented for types that pass certain requirements.
You can use the `of` keyword, to make the compiler enforce that a certain trait must be implemented.
For example, the `Sized` trait is automatically implemented for structs, if they have a fixed size.
Using `of Sized(8)` will make the compiler enforce that the struct has a size of 8 bytes:

```mylang
// Allowed. Each float is 4 bytes with 0 padding.
struct Point of Sized(8) {
    var x: Float;
    var y: Float;
}

// Error: Point can't implement Sized(8)
struct Point of Sized(8) {
    var x: Float;
    var y: Float;
    var z: Float;
}
```

The fields of a struct can also be declared to me immutable with the `let` keyword:

```mylang
struct Point {
    let x: Float;
    let y: Float;
}
```

If you want a struct to also contain all the fields of another struct, you can use `..`:

```mylang
struct Point {
    var x: Float;
    var y: Float;
}

struct Point3D {
    ..Point;
    var z: Float;
}
```

## Struct variables



