# pl-compiler

A *to be implemented* compiler for a toy programming language.

The language is a statically typed, imperative language with a focus on performance and with
an expressive type system. It has no runtime and has seamless interop using the C/platform
ABI. Memory management is manual and allocators are passed around as arguments to functions
to allow for fine grained control over memory allocation.

Overall it takes inspiration from the following languages (ordered alphebetically):
- C
- C++
- Carbon
- Haskell
- Jai
- Rust
- Scala
- Zig

## Example

The following is a simple implementation of a stack using the language.

```mylang
import "std.mylang" as Std;
import Std.Ds.ArrList;
import Std.Mem.Allocator;

pub struct Stack(T: Sized()) {
    data: ArrList(T);
}

for Stack(T: Sized()) as Self {
    pub def new() -> Self
        return Self.{ArrList(T).new()};
}

for stack: &Stack(T: Sized() + Move) {
    pub def push(value: T, allocator: &Allocator) -> Void
        return &stack.data.add_last(value, &allocator).#panic;

    pub def pop() -> T
        return stack.data.remove_last().#panic;
}

for stack: *Stack(T: _) {
    pub def len() -> USize
        return stack.data.len();
}

pub main(a: Int) -> Void {
    var stack = Stack(Int32).new();
    
    &stack.push(42, &Allocator.malloc);
    #assert(stack.len() == 1);

    let value = stack.pop();
    #assert(value == 42);
}
```

## Features

Here are some of the more or less unique features of the language:

### The `Sized(size: USize)` and `Aligned(align: USize)` traits.

These traits are automatically implemented for all types with a known size and alignment.
The `Aligned` trait is a subtrait of `Sized` and is therefor rarely used directly.

```mylang
// The following two functions are defined in the prelude.
def sizeof(T: Sized(size: USize)) -> USize return size;
def alignof(T: Aligned(align: USize)) -> USize return align;
```

The `Sized` trait can also be used to restrict the size of an otherwise unsized type.

```mylang
def main() -> Void {

    // The `dyn` keyword can be used to create an heterogeneous array of types that implement
    // a certain trait. Since we need to know the size of an type to allocate it on the stack,
    // we normally would need to use references to the elements. Though, we can use the `Sized`
    // trait to restrict the size of the elements, which allows us to allocate the array on
    // the stack.
    var to_strs = [3](dyn ToStr + Sized(16)).uninit();
    &to_strs[0] = true;
    &to_strs[1] = 123::UInt32; // The type is ambiguous, so we need to hint at the type.
    &to_strs[2] = 3.14::Float64;

    for (elem in to_strs)
        print(elem.to_str(&Allocator.malloc));

    // We could therefor also use our stack implementation from the example above to store
    // elements of different types.
    var stack = Stack(dyn ToStr + Sized(16)).new();
    &stack.push(true,           &Allocator.malloc);
    &stack.push(123::UInt32,    &Allocator.malloc);
    &stack.push(3.14::Float64,  &Allocator.malloc);
}
```

More information about these traits can be found in the [`Sized` and `Aligned`].

### You can add default implementations of foreign traits for foreign types.

In some cases you want to use an imported type, but that type does not implement a trait that
you need. This would normally require you to wrap the type in a new type and implement the
trait for that type. This is because, if you allowed users to implement foreign traits for
foreign types, you might end up with conflicting implementations of the same trait for the
same type.

Though, in some cases you might not care about conflicting implementations and whether or not 
your implementation is the one that the compiler will choose. In these cases you can simply
add a default implementation of the trait for the foreign type. A type is allowed to have
multiple default implementations of the same trait, and if this is the case, the compiler will
choose one of them, but it is unspecified which one. This also means that if you need a
specific implementation then this solution is not for you.

To add a default implementation for a trait, you simply specify a value using the `virt`
keyword. The `virt` keyword means that a value might get defined later, and it is therefor not
an error to redefine it.

```mylang
module Math {
    trait Add for Self {
        virt add(a: Self, b: Self) -> Self;
    }
}

module Graphics {
    struct Vec3I {
        var x: Int;
        var y: Int;
        var z: Int;
    }
}

import Math.Add;
import Graphics.Vec3I;

for Vec3I impl Add {
    virt add(a: Vec3I, b: Vec3I) -> Vec3I {
        return Vec3I.{a.x + b.x, a.y + b.y, a.z + b.z};
    }
}
```

### Norminal and structural types

**TODO**

### The type system can handle expressive uses of anonymous types.

All types can be declared and used anonymously.
The `some` keyword allows you to declare an existential type.

```mylang
// This function is also defined in the prelude. It gives you a copy of a value even if it is
// not `copy`. It then implements the `Forget` trait for this value specifically, which allows
// you to forget about the returned value without running a destructor.
def temp_copy(value: *T: Sized(size: USize)) -> some T + Sized(size) + Forget {

    // Defines the concrete type of the existential
    def New = newtype T;
    for New impl Forget;

    // Returns a copy of the value
    var copy = uninit;
    Std.Mem.copy(&copy, value, size);
    return copy;
}
```

The `newtype` keyword allows you to declare a new and distinct type, which can be use in the
return type of a function to declare that the function returns a new and distinct type on
every call.

```mylang
// This function returns a value with type that has the same representation as an `Int32` and
// also has the same methods and traits implemented, though the type will be anonymous and it
// will be a completely new and distinct type on every call.
def unique() -> newtype UInt32 {
    return 10;
}

def main() -> Void {
    var a = unique();
    // The next line will not compile, since calling `unique` again will return a new and
    // distinct type.
    // &a = unique();
}
```

More information about this can be found in the [Anonymous types] and [Generative types].

