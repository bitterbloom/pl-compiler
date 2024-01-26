module main;

import std::io;
import ir::qbe_gen;
import qbe;

fn void! main(String[] argv) {
    io::printn("Hello World!");

    ShellProcess compiler = qbe::test::spawn_process()!;
    defer compiler.destroy();
    compiler.prepare_compile_to(0)!;
    File* out = io::stdout();
    qbe::emit_program(out, qbe_gen::gen_program(/*...*/))!;
}
