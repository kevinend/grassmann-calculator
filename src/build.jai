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

    add_build_file("./main.jai",  w);
    add_build_file("./input.jai", w);

    // The dc (during compile) option tells the compiler not to emit output for this metaprogram file while building our application.
    set_build_options_dc(.{do_output=false});
}

#run build();