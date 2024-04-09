# IR

This IR is currently only used to generate QBE
code, but it should be designed in a way that it
can be used to with other backends as well.

It gets generated from the AST after some analysis
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
def [export]? $<function>([%<arg>: <Type>], ) -> <Type> {
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
    &value += 1;
    return .{"counter was called: %d\n\0", value^};
}

pub def main() -> {
    for (in range(10)) {
        let {format, value} = counter();
        printf(format.data, ..., value);
    }
}
```

which would first be transformed into this:

```mylang
extern printf(format: ^[]UInt8, ...) -> Int32;

var static_int: Int32 = -1;
let const_str: Str = Str.{len = 24, data = "counter was called: %d\n\0"};

struct RetType {
    str: Str;
    int: Int32;
}

def counter() -> RetType {
    let value: ^Int32 = ^static_int;
    var deref0: Int32 = value^;
    deref0 += 1;
    value^ = deref0;
    let deref1: Int32 = value^;
    let ret_val: RetType = RetType.{str = const_str, int = deref1};
    return ret_val;
}

pub def main() -> Void {
    do {
        var i = 0;
        while (i < 10) {
            let counter_ret = counter();
            printf(counter_ret.str.data, ..., counter_ret.int.value);
            i += 1;
        }
    }
    return void;
}
```

could initially be turned into this IR:

```
module _ {
    extern $printf(%format: ^[]UInt8, ...): Int32

    var $static_int: Int32 = -1
    let $const_str: Str = {len = 24, data = "counter was called: %d\n\0"}

    def :RetType = struct {
        %str: Str
        %int: Int32
    }

    def $counter(): RetType {
        let %value: ^Int32
        %value = addr $static_int
        var %deref0: Int32
        %deref0 = load %value
        %deref0 = add_ub %deref0, 1
        store %static_int = %deref
        let %ret_val: RetType
        %ret_val.%str = $const_str
        %ret_val.%int = %deref
        return %ret_val
    }

    export $main(): Void {
        @macro_expansion {
            var %i: UInt32
            %i = 0
            jump @while_cond
            @while_cond {
                tmp %cond: Bool
                %cond = less %i, 10
                jump @while_body if %cond else @return
            }
            @while_body {
                let %counter_ret: RetType
                %counter_ret = call $counter()
                let %data: ^[]UInt8
                %data = %counter_ret.%str.%data
                let %int: Int32
                %int = %counter_ret.%int
                let %ignore: Int32
                %ignore = call $printf(%data_ptr, ..., %counter_ret.%int)
                %i = add_ub %i, 1
                jump @while_cond
            }
        }
        @return {
            return void
        }
    }
}
```

until finally turned into this analysed IR:

```
module _ {
    extern $printf(%format: ^[]UInt8, ...): Int32

    let $const_str.data: []UInt8 = "counter was called: %d\n\0"

    export $main(): Void {
        var %i: UInt32
        %i = 0
        var $static_int: Int32
        $static_int = -1
        jump @while_body
        @while_body {
            %static_int = add_ub %static_int, 1
            %ignore = call $printf(addr $const_str.data, ..., %static_int)
            tmp %cond = less %i, 10
            %i = add_ub %i, 1
            jump @while_body if %cond else @return
        }
        @return {
            return void
        }
    }
}
```

which could be turned into this QBE code:

```qbe
data $counter_str.data = { b "counter was called: %d\n", b 0 }

function export $main() {
@entry
    %i =w copy 0
    %static_int =w copy -1
    jmp @while_body
@while_body
    %static_int =w add %static_int, 1
    %ignore =w call $printf(l $const_str.data, ..., w %static_int)
    %cond =w cultw %i, 10
    %i =w add %i, 1
    jnz %cond, @while_body, @return
@return
    ret
}
```

