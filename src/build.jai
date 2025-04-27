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

    add_build_file("./algebra-types.jai",       w);
    add_build_file("./ast.jai",                 w);
    add_build_file("./eval.jai",                w);
    add_build_file("./exterior-space.jai",      w);
    add_build_file("./bump-heap.jai",           w);
    add_build_file("./input.jai",               w);
    add_build_file("./lexer.jai",               w);
    add_build_file("./main.jai",                w);
    add_build_file("./operators.jai",           w);
    add_build_file("./parser.jai",              w);
    add_build_file("./relative-pointer.jai",    w);
    add_build_file("./stack.jai",               w);
    add_build_file("./token.jai",               w);
    add_build_file("./working-set.jai",         w);

    // Ast-transform files
    add_build_file("./ast-transforms/expand.jai",              w);
    add_build_file("./ast-transforms/flatten.jai",             w);
    add_build_file("./ast-transforms/resolve-symbols.jai",     w);
    add_build_file("./ast-transforms/semantic-check.jai",      w);

  //    add_build_file("./coefficient.jai",         w); // TODO: 04.13.2025 should be replaced with algebra_types.jai

 
    

    // The dc (during compile) option tells the compiler not to emit output for this metaprogram file while building our application.
    set_build_options_dc(.{do_output=false});
}

#run build();