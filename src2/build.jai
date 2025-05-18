// Metaprogram that builds our main application.
#import "Compiler";
#import "Basic";

build :: () {

    w := compiler_create_workspace();
    if !w {
        print("Workspace creation failed.\n");
        return;
    }

    build_options := get_build_options(w);
    build_options.output_type            = .EXECUTABLE;      
    build_options.output_executable_name = "main";
    build_options.output_path            = "../bin/debug";
    set_build_options(build_options, w);

    // Build 'algebra module'
    add_build_file("./algebra/basis-element.jai",  w);
    add_build_file("./algebra/basis-term.jai",     w);
    add_build_file("./algebra/coefficient.jai",    w);
    add_build_file("./algebra/exterior-space.jai", w);
    add_build_file("./algebra/factor.jai",         w);
    add_build_file("./algebra/term.jai",           w);

    // Build 'ast module'
    add_build_file("./ast/passes/expand.jai",           w);
    add_build_file("./ast/passes/flatten.jai",          w);
    add_build_file("./ast/passes/resolve-symbols.jai",  w);
    add_build_file("./ast/passes/semantic-check.jai",   w);

    add_build_file("./ast/ast.jai",       w);
    add_build_file("./ast/operators.jai", w);

    // Build 'memory module'
    add_build_file("./memory/bump-heap.jai",        w);
    add_build_file("./memory/relative-pointer.jai", w);
    add_build_file("./memory/stack.jai",            w);

    // Build 'parser module'
    add_build_file("./parser/input.jai",  w);
    add_build_file("./parser/lexer.jai",  w);
    add_build_file("./parser/parser.jai", w);
    add_build_file("./parser/token.jai",  w);

    // Build 'main'
    add_build_file("./main.jai", w);

    // The dc (during compile) option tells the compiler not to emit output for this metaprogram file while building our application.
    set_build_options_dc(.{do_output=false});
}

#run build();