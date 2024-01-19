module qbe::test;
import std::io;
import std::os;
import std::math::random;
import std::time;

fault TestFault {
    FAILED_TO_COMPILE,
    WRONG_RESULT,
}

fn void! test_hello_world() @test {
    ShellProcess compiler = spawn_process()!;
    defer compiler.destroy();
    uint test_file_num = get_rand_file_num();
    compiler.prepare_compile_to(test_file_num)!;

    File* in = &&compiler.get_stdin();
    qbe::emit_hello_world(in)!;

    char[100] buffer;
    expect_result(&compiler, test_file_num, &buffer, "hello world")!;
}

fn void! test_simple() @test {
    ShellProcess compiler = spawn_process()!;
    defer compiler.destroy();
    uint test_file_num = get_rand_file_num();
    compiler.prepare_compile_to(test_file_num)!;

    File* in = &&compiler.get_stdin();
    Data data = Data{
        .name = "simple",
        .literals = Literal[]{
            Literal{`"simple"`, {qbe::TypeKind.B}},
            Literal{`0`, {qbe::TypeKind.B}},
        },
    };
    qbe::emit_data(in, data)!;
    Func func = Func{
        .name = "main",
        .type = {qbe::TypeKind.W},
        .args = Temp[]{},
        .blocks = Block[]{
            Block{
                .name = "entry",
                .insts = Inst[]{
                    Inst{
                        .kind = InstKind.TEMP,
                        .name = "a",
                        .temp_type = {qbe::TypeKind.W},
                        .temp_expr = Expr{
                            .kind = ExprKind.CALL,
                            .call_name = "puts",
                            .args = {
                                Value{
                                    .kind = ValueKind.DATA,
                                    .data = data,
                                },
                            },
                        },
                    },
                    Inst{
                        .kind = InstKind.RET,
                        .ret_value = Value{
                            .kind = ValueKind.LITERAL,
                            .literal = Literal{
                                .literal = "0",
                                .type = {qbe::TypeKind.N},
                            },
                        },
                    },
                },
            },
        },
    };
    qbe::emit_func(in, func)!;

    char[100] buffer;
    expect_result(&compiler, test_file_num, &buffer, data.name)!;
}

fn void! test_fib() @test {
    emit_fib("0", "0")!;
    emit_fib("1", "1")!;
    emit_fib("2", "1")!;
    emit_fib("3", "2")!;
    emit_fib("20", "6765")!;
}

fn void! emit_fib(String input, String output) @private {
    ShellProcess compiler = spawn_process()!;
    defer compiler.destroy();
    uint test_file_num = get_rand_file_num();
    compiler.prepare_compile_to(test_file_num)!;

    File* in = &&compiler.get_stdin();
    Data fmt = Data{
        .name = "fmt",
        .literals = Literal[]{
            Literal{`"%d\n"`, {qbe::TypeKind.B}},
            Literal{`0`, {qbe::TypeKind.B}},
        },
    };
    qbe::emit_data(in, fmt)!;

    Temp n   = Temp{"n",   {qbe::TypeKind.W}};
    Temp one = Temp{"one", {qbe::TypeKind.W}};
    Temp two = Temp{"two", {qbe::TypeKind.W}};
    Temp a   = Temp{"a",   {qbe::TypeKind.W}};
    Temp b   = Temp{"b",   {qbe::TypeKind.W}};
    Temp c   = Temp{"c",   {qbe::TypeKind.W}};
    Block recurse = Block{
        .name = "recurse",
        .insts = Inst[]{
            Inst{
                .kind = InstKind.TEMP,
                .name = two.name,
                .temp_type = {qbe::TypeKind.W},
                .temp_expr = Expr{
                    .kind = ExprKind.SUB,
                    .left = Value{
                        .kind = ValueKind.TEMP,
                        .temp = n,
                    },
                    .right = Value{
                        .kind = ValueKind.LITERAL,
                        .literal = Literal{
                            .literal = "2",
                            .type = {qbe::TypeKind.N},
                        },
                    },
                },
            },
            Inst{
                .kind = InstKind.TEMP,
                .name = a.name,
                .temp_type = {qbe::TypeKind.W},
                .temp_expr = Expr{
                    .kind = ExprKind.CALL,
                    .call_name = "fib",
                    .args = Value[]{
                        Value{
                            .kind = ValueKind.TEMP,
                            .temp = one,
                        },
                    },
                },
            },
            Inst{
                .kind = InstKind.TEMP,
                .name = b.name,
                .temp_type = {qbe::TypeKind.W},
                .temp_expr = Expr{
                    .kind = ExprKind.CALL,
                    .call_name = "fib",
                    .args = Value[]{
                        Value{
                            .kind = ValueKind.TEMP,
                            .temp = two,
                        },
                    },
                },
            },
            Inst{
                .kind = InstKind.TEMP,
                .name = c.name,
                .temp_type = {qbe::TypeKind.W},
                .temp_expr = Expr{
                    .kind = ExprKind.ADD,
                    .left = Value{
                        .kind = ValueKind.TEMP,
                        .temp = a,
                    },
                    .right = Value{
                        .kind = ValueKind.TEMP,
                        .temp = b,
                    },
                },
            },
            Inst{
                .kind = InstKind.RET,
                .ret_value = Value{
                    .kind = ValueKind.TEMP,
                    .temp = c,
                },
            },
        },
    };
    Block is_zero = Block{
        .name = "is_zero",
        .insts = Inst[]{
            Inst{
                .kind = InstKind.RET,
                .ret_value = Value{
                    .kind = ValueKind.LITERAL,
                    .literal = Literal{
                        .literal = "0",
                        .type = {qbe::TypeKind.N},
                    },
                },
            },
        },
    };
    Block is_one = Block{
        .name = "is_one",
        .insts = Inst[]{
            Inst{
                .kind = InstKind.RET,
                .ret_value = Value{
                    .kind = ValueKind.LITERAL,
                    .literal = Literal{
                        .literal = "1",
                        .type = {qbe::TypeKind.N},
                    },
                },
            },
        },
    };
    Block not_zero = Block{
        .name = "not_zero",
        .insts = Inst[]{
            Inst{
                .kind = InstKind.TEMP,
                .name = one.name,
                .temp_type = {qbe::TypeKind.W},
                .temp_expr = Expr{
                    .kind = ExprKind.SUB,
                    .left = Value{
                        .kind = ValueKind.TEMP,
                        .temp = n,
                    },
                    .right = Value{
                        .kind = ValueKind.LITERAL,
                        .literal = Literal{
                            .literal = "1",
                            .type = {qbe::TypeKind.N},
                        },
                    },
                },
            },
            Inst{
                .kind = InstKind.JNZ,
                .jnz_value = Value{
                    .kind = ValueKind.TEMP,
                    .temp = one,
                },
                .not_zero_block = recurse.name,
                .if_zero_block = is_one.name,
            },
        },
    };
    Func fib = Func{
        .name = "fib",
        .type = {qbe::TypeKind.W},
        .args = Temp[]{n},
        .blocks = Block[]{
            Block{
                .name = "entry",
                .insts = Inst[]{
                    Inst{
                        .kind = InstKind.JNZ,
                        .jnz_value = Value{
                            .kind = ValueKind.TEMP,
                            .temp = n,
                        },
                        .not_zero_block = not_zero.name,
                        .if_zero_block = is_zero.name,
                    },
                },
            },
            is_zero,
            not_zero,
            is_one,
            recurse,
        },
    };
    qbe::emit_func(in, fib)!;

    Temp f   = Temp{"f",   {qbe::TypeKind.W}};
    Temp r   = Temp{"r",   {qbe::TypeKind.W}};
    Func main = Func{
        .name = "main",
        .type = {qbe::TypeKind.W},
        .args = Temp[]{},
        .blocks = Block[]{
            Block{
                .name = "entry",
                .insts = Inst[]{
                    Inst{
                        .kind = InstKind.TEMP,
                        .name = f.name,
                        .temp_type = {qbe::TypeKind.W},
                        .temp_expr = Expr{
                            .kind = ExprKind.CALL,
                            .call_name = "fib",
                            .args = Value[]{
                                Value{
                                    .kind = ValueKind.LITERAL,
                                    .literal = Literal{
                                        .literal = input,
                                        .type = {qbe::TypeKind.W},
                                    },
                                },
                            },
                        },
                    },
                    Inst{
                        .kind = InstKind.TEMP,
                        .name = r.name,
                        .temp_type = {qbe::TypeKind.W},
                        .temp_expr = Expr{
                            .kind = ExprKind.CALL,
                            .call_name = "printf",
                            .args = Value[]{
                                Value{
                                    .kind = ValueKind.DATA,
                                    .data = fmt,
                                },
                                Value{
                                    .kind = ValueKind.TEMP,
                                    .temp = f,
                                },
                            },
                        },
                    },
                    Inst{
                        .kind = InstKind.RET,
                        .ret_value = Value{
                            .kind = ValueKind.TEMP,
                            .temp = r,
                        },
                    },
                },
            },
        },
    };
    qbe::emit_func(in, main)!;

    char[100] buffer;
    expect_result(&compiler, test_file_num, &buffer, output)!;
}

distinct ShellProcess = SubProcess;

fn ShellProcess! spawn_process() @private {
    SubProcess! process = process::create({"sh"}, { .inherit_environment = true });
    if (catch process) {
        io::print("Could not create shell process due to: \"");
        if (catch process) {
            case SubProcessResult.FAILED_TO_CREATE_PIPE:        io::print("Failed to create pipe");
            case SubProcessResult.FAILED_TO_OPEN_STDIN:         io::print("Failed to open stdout");
            case SubProcessResult.FAILED_TO_OPEN_STDOUT:        io::print("Failed to open stdout");
            case SubProcessResult.FAILED_TO_OPEN_STDERR:        io::print("Failed to open stderr");
            case SubProcessResult.FAILED_TO_START_PROCESS:      io::print("Failed to start process");
            case SubProcessResult.FAILED_TO_INITIALIZE_ACTIONS: io::print("Failed to initialize action");
            default:                                            io::print("Unknown reason");
        }
        io::printn("\".");
        process!;
    }
    return (ShellProcess) process;
}

fn bool ShellProcess.destroy(ShellProcess* process) @private {
    return ((SubProcess*) process).destroy();
}

fn CInt! ShellProcess.join(ShellProcess* process) @private {
    return ((SubProcess*) process).join();
}

fn usz! ShellProcess.read_stdout(ShellProcess* process, char[] buffer) @private {
    return ((SubProcess*) process).read_stdout(buffer, buffer.len);
}

fn usz! ShellProcess.read_stderr(ShellProcess* process, char[] buffer) @private {
    return ((SubProcess*) process).read_stderr(buffer, buffer.len);
}

fn void! ShellProcess.prepare_compile_to(ShellProcess* process, uint file_num) @private {
    File process_in = process.get_stdin();
    io::fprintf(&process_in, "mkdir -p ./temp/\n")!;
    io::fprintf(&process_in, "qbe - | gcc -x assembler -o ./temp/test%d -\n ", file_num)!;
}

fn File ShellProcess.get_stdin(ShellProcess* process) @private {
    return file::from_handle(process.stdin_file);
}

fn uint get_rand_file_num() @private {
    SimpleRandom rand;
    Time seed = time::now();
    rand.set_seed(&&bitcast(seed, char[8]));
    return rand.next_int();
}

fn void! expect_result(ShellProcess* compiler, uint file_num, char[] buffer, char[] expected) @private {
    compiler.join()!;

    usz read;
    read = compiler.read_stderr(buffer)!;
    if (read > 0) {
        io::printn("\nstderr:");
        io::print(buffer[:read]);
        io::printn("...");
        return TestFault.FAILED_TO_COMPILE?;
    }

    ShellProcess runner = spawn_process()!;
    defer runner.destroy();
    File runner_in = file::from_handle(runner.stdin_file);
    io::fprintf(&runner_in, "./temp/test%d\n", file_num)!;
    runner.join()!;

    read = runner.read_stdout(buffer)! - 1; // The last character is always a newline.
    if CHECK: (read == expected.len) {
        foreach (i, c : buffer[:read]) {
            if (c != expected[i]) break CHECK;
        }
        return;
    }

    io::printfn("\nstdout[%s]:", read);
    io::printn(buffer[:read]);
    io::printfn("expected[%s]:", expected.len);
    io::printn(expected);
    return TestFault.WRONG_RESULT?;
}

