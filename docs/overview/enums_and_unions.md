
# Enums

Enums are types that represents a set of named values.

## Terminology

* **Enum (type)**: A type that represents a set of named values.
* **Enum variant**: One of the named values of an enum.
* **Enum value**: The value associated with an enum variant.
* **Enum tag (value)**: The tag of an enum variant used to determine which variant it is.
* **Enum tag type**: The type of the tag of an enum.
* **Enum variable**: A variable that holds an enum variant.

## Declaring an enum

An enum type can be declared with the `enum` keyword:

```mylang
enum Fruit {
    def apple  = "Apple";
    def banana = "Banana";
    def orange = "Orange";
}
```

In this case, the enum value type is `Str`. Though, it could be any type you want.
You can specify the value type with the `of` keyword:

```mylang
enum Corner of Vec(2, I32) {
    def top_left     = Vec.of(0, 0);
    def top_right    = Vec.of(1, 0);
    def bottom_left  = Vec.of(0, 1);
    def bottom_right = Vec.of(1, 1);
}
```

In the above examples, the enum variants are defined inside the enum declaration.
You can also use pre-defined variables as enum variants:

```mylang
def apple  = "Apple";
def banana = "Banana";
def orange = "Orange";

enum Fruit {
    apple;
    banana;
    orange;
}
```

This allows multiple enums to overlap in their variants.
If you want to declare an enum that also has all the variants of another enum, you can use `..`:

```mylang
enum Fruit {
    apple;
    banana;
    orange;
}

enum Food {
    ..Fruit;
    pizza;
    sandwich;
    ice_cream;
}
```

The enum values don't have to be known at compile time.
You could also use mutable variables declared with the `var` keyword:

```mylang
var apple  = "Apple";
var banana = "Banana";
var orange = "Orange";

enum Fruit {
    apple;
    banana;
    orange;
}

def main() -> Void {
    apple = "Pear"; // Odd, but allowed
}
```

## Enum variables

To make an enum variable you simply have to assign a variable with the enum type with one of its variants
If you define the enum variant within the enum declaration,
then the variant identifier will live in the scope of the enum type:

```mylang
enum Fruit {
    def apple  = "Apple";
    def banana = "Banana";
    def orange = "Orange";
}

def favorite_fruit: Fruit = Fruit.apple;
```

You can of course also use `import` statements to import the enum variants into the current scope:

```mylang
import Fruit;

def favorite_fruit: Fruit = apple;
```

Please note that in this case, it is important to remember to give the variable the `Fruit` type.
Otherwise the compiler won't know if it should assign the enum variant or the enum value to the variable.
This also ensures that there is no ambiguity when using variables that are variants of multiple enums:

```mylang
def fruit:  Fruit = apple;  // fruit is assigned the enum variant, apple, from the Fruit enum
def food:   Food  = apple;  // food is assigned the enum variant, apple, from the Food enum
def string: Str   = apple;  // string is assigned the enum value, "Apple".
```

When you have an enum variable, you can access its value using the `value` method:

```mylang
def main() -> Void {
    let favorite_fruit = Fruit.apple;
    #assert(favorite_fruit.value() == "Apple");
}
```

## Enum tag

Enum variables are represented with a tag value.
The type of the tag is `UInt8` by default and can be changed with the `tag` keyword like below.
When using the default `UInt8` tag type, the tag values are automatically assigned sequentially starting from 0.
You can also specify the tag value explicitly:

```mylang
enum Answer of [4]Str tag Bool {
    def yes tag true  = .{"y", "Y", "yes", "Yes"};
    def no  tag false = .{"n", "N", "no",  "No"};
}
```

... or like this:

```mylang
def yes = .{"y", "Y", "yes", "Yes"};
def no  = .{"n", "N", "no",  "No"};

enum Answer of [4]Str tag Bool {
    yes tag true;
    no  tag false;
}
```

Keep in mind that tag values must be unique and known at compile time.

If you want to check the tag value of an enum variable, you can use the `tag` method:

```mylang
def main() -> Void {
    let answer = Answer.yes;
    #assert(answer.tag() == true);
}
```

## Checking enum variants

**TODO**: For enums, maybe we should use the `==` operator instead of `is`?

To check which variant an enum variable holds, you can use the `is` operator:

```mylang
def is_my_favorite_fruit(fruit: Fruit) -> Bool
    return fruit is Fruit.apple;
```

You can also use the block form of the `is` operator:

```mylang
def eat_fruit(fruit: Fruit) -> Void {
    import Fruit;
    fruit is {
        (apple)  println("Eating an apple!");
        (banana) println("Eating a banana!");
        (orange) println("Eating an orange!");
    };
}
```

The each branch of the block can also contain multiple variants separated by commas.
They can also be combined with the `and` or `or` keywords to create more complex conditions.
Additionally, you can also add an `else` branch to the block:

```mylang
for person: *Person
def likes_fruit(fruit: Fruit) -> Bool {
    import Fruit;
    return fruit is {
        (apple, banana) true;
        (orange and person.likes_oranges) true;
        else false;
    };
}
```

## Anonymous enums

All of the previous declarations indicate norminal enum types.
You can also declare an enum as a [structural type](./anonymous_types.md#Structural) by creating an alias to an anonymous enum type.
Anonymous enums are declared the same way, but without a name:

```mylang
def Fruit = enum {
    apple;
    banana;
    orange;
}
```

The type checker will consider all anonymous enums to be the same type if they have the same variants and tags.

Anonymous enums can be used everywhere norminal types can, including in function signatures:

```mylang
def get_a_fruit() -> enum {apple; banana; orange}
    return apple;
```

# Unions

Unions are types that represents values from a set of types.

## Terminology

* **Union (type)**: A type that represents values from a set of types.
* **Union variant**: One of the types in the set of types of a union.
* **Union value**: The value that is held by a union variable.
* **Union tag (value)**: The tag of a union variant used to determine which variant it is.
* **Union tag type**: The type of the tag of a union.
* **Union variable**: A variable that holds a value from a union.

## Declaring a union

A union type can be declared with the `union` keyword:

```mylang
union Animal {
    struct Bird {name: Str}
    struct Cat  {name: Str}
    struct Dog  {name: Str}
}
```

You can specify a trait constraint that all variants must implement with the `of` keyword:

```mylang
union Animal of Sized(sizeof(Str)) {
    struct Bird {name: Str}
    struct Cat  {name: Str}
    struct Dog  {name: Str}
}
```

This is especially useful with the `Sized` trait, as it allows you to specify the maximum size of the union.
The actual size of the union will be the size of the largest variant plus the size of the tag.

The union variants can also be used as types by themselves:

```mylang
def get_tweety() -> Animal.Bird
    return Animal.Bird.{"Tweety"};
```

Like enums, union variants can also be pre-defined types:

```mylang
union Color {
    Str;
    [3]UInt8;
}
```

This allows multiple unions to overlap in their variants.
Unions also allow you to use `..` to declare a union that has all the variants of another union:

```mylang
union Animal {
    struct Bird {name: Str}
    struct Cat  {name: Str}
    struct Dog  {name: Str}
}

union Organism {
    ..Animal;
    struct Bacteria {dna: Dna}
}
```

## Union variables

To create a union variable, you simply have to assign a variable with the union type with one of its variants:

```mylang
union Animal {
    struct Bird {name: Str}
    struct Cat  {name: Str}
    struct Dog  {name: Str}
}

def main() -> Void {
    let animal: Animal = Animal.Cat.{"Garfield"};
}
```

Though, just like enums, you might need to add type annotations.
Otherwise the compiler won't know if it should assign the value as a union variant or not:

```mylang
def animal:   Animal   = Animal.Cat.{"Garfield"};
def organism: Organism = Animal.Cat.{"Garfield"};
```

You can't access the value of a union variable directly.
This is because we don't necessarily know which type a union variable holds.

## Union tag

In addition to the value, a union variable also holds a tag value.
As with enums, the type of the tag is `UInt8` by default and can be changed with the `tag` keyword.

```mylang
union UnionWithLargeTag of Sized(8) tag UInt64 {
    Int64   tag 0;
    UInt64  tag 1;
    Float64 tag 2;
}
```

But unlike enums, unions can use `Void` as the tag type, which makes the union tagless:

```mylang
union UnionWithoutTag of Sized(8) tag Void {
    Int64;
    UInt64;
    Float64;
}
```

To get the tag value of a union variable, you can use the `tag` method:

```mylang
def main() -> Void {
    let variable: UnionWithLargeTag = 123::Int64;
    #assert(variable.tag() == 0);
}
```

The `tag` method will simply return `void` if the union is tagless.

## Checking union variants

To check which variant a union variable holds, you can use the `is` operator:

```mylang
def is_it_a_cat(animal: Animal) -> Bool
    return animal is Animal.Cat;
```

You can also use the block form of the `is` operator:

```mylang
def pet_animal(animal: Animal) -> Void {
    import Animal;
    animal is {
        (Bird) println("Petting a bird!");
        (Cat)  println("Petting a cat!");
        (Dog)  println("Petting a dog!");
    };
}
```

With unions, the `is` operator can also give you access to the value of the union variable:

```mylang
def get_name_if_cat(animal: Animal) -> Str {
    if (animal is Animal.Cat cat)
        return cat.name;
    else
        return "Not a cat";
}

def get_name(animal: Animal) -> Str {
    import Animal;
    return animal is {
        (Bird bird) bird.name;
        (Cat  cat)  cat.name;
        (Dog  dog)  dog.name;
    };
}
```

Like enums, the block `is` operator can also contain multiple variants, other conditions and an `else` branch.

## Anonymous unions

Unions can also be declared as anonymous types, which can be used to create [structural types](./anonymous_types.md#Structural):

```mylang
def Animal = union {
    struct Bird {name: Str}
    struct Cat  {name: Str}
    struct Dog  {name: Str}
}
```

Even the variants can be structural:

```mylang
def Animal = union {
    def Bird = struct {name: Str}
    def Cat  = struct {name: Str}
    def Dog  = struct {name: Str}
}
```

The type checker will consider all anonymous unions to be the same type if they have the same variants and tags.

