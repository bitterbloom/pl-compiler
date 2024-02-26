# IR

This IR is currently only used to generate QBE
code, but it should be designed in a way that it
can be used to with other backends as well.

It get generated from the AST after some analysis
has been done. Multiple passes will have to be
done on the IR before it can be used to generate
code.

The IR consists of a set of modules, which contain
type, function, and variable definitions. The
functions contain a list of instructions, which
might be grouped into blocks.

Unlike most other IRs, this IR does not follow the
SSA form. This is because the QBE backend does not
require it, and it makes the IR easier to work
with, especially when working with loops.

## Grammar

* `<___>`  = placeholder

* `[___]?` = optional

* `[___],` = repeatable (comma separated)

## Overview

The ir consists of modules.
Modules *cannot* be nested.

```
module <Module> {
    <definitions...>
}
```

Modules can contain functions.

```
def [export]? <function>([%<arg>: <Type>], ) -> <Type> {
    <instructions...>
}
```

Functions consists of instructions and blocks.
Blocks contain instructions and *can* be nested.

```
<block>: {
    <instructions...>
}
```

Variables can either be temporary or local.
Both temporaries and locals are mutable.

Temporaries are used to store intermediate values.
They cannot be taken the address of and might only
live in registers.

```
%<temp> = <expr>;
```

**TODO...**

## Example

The following source code:

```mylang
extern printf(format: *[]UInt8, ...) -> Int32;

def counter() -> {
    let value: &Int32 = static -1;
    &value++;
    return .{"counter was called: %d\n\0", value};
}

pub def main() -> {
    for (in #range(10)) {
        let {format, value} = counter();
        printf(format.data(), .{value});
    }
}
```

could initially be turned into this IR:

```
module _ {
    extern $printf(%format: ^[]UInt8, ...): Int32

    var %counter.0: Int32 = -1
    let %counter.1: Arr = { 24, "counter was called: %d\n\0" }

    def :counter.2 = struct {
        %0: ^[24]UInt8
        %1: Int32
    }

    def $counter(): counter.2 {
        let %value: ^Int32
        %value = %counter.0
        let %0: Int32
        %0 = load %value
        %0 = add_ub %0, 1
        store %value = %0
        var %1: counter.2
        %1 = { %counter.1, %0 }
        return %1
    }

    export $main(): Void {
        @0 {
            var %0: UInt32
            %0 = 0
            jump @1
            @1 {
                let %1: counter.2
                %1 = call counter()
                tmp %2: ^[]UInt8
                %2 = call Arr.$data(%1.%0)
                call $printf(%2, ..., %1.%1)
                %0 = add_ub %0, 1
                tmp %1: UInt32
                %1 = equals %0, 10
                jump @2 if %1 else @1
            }
        }
        @2 {
            return void
        }
    }
}
```

until finally turned into this analysed IR:

```
module _ {
    extern $printf(%format: ^[]UInt8, ...): Int32

    let %counter.1.%data: ^[]UInt8 = "counter was called: %d\n\0"

    export $main(): Void {
        tmp %0: UInt32
        %0 = 0
        tmp %counter.0: Int32 = -1
        jump @1
        @1 {
            %counter.0 = add_ub %counter.0, 1
            %0 = add_ub %0, 1
            tmp %1: UInt32
            %1 = equals %0, 10
            call $printf(%counter.1.%data, ..., %counter.0)
            jump @2 if %1 else @1
        }
        @2 {
            return void
        }
    }
}
```

which could be turned into this QBE code:

```qbe
data $counter.1.data = { b "counter was called: %d\n", b 0 }

function export w $main() {
@start
    %0 =w copy 0
    %counter.0 =w copy -1
    jmp @1
@1
    %counter.0 =w add %counter.0, 1
    %0 =w add %0, 1
    %1 =w ceqw %0, 10
    %2 =w call $printf(l $counter.1.data, ..., w %counter.0)
    jnz %1, @2, @1
@2
    ret 0
}
```

