// -*- C++ -*- (c) 2010 Petr Rockai <me@mornfall.net>

#include <wibble/commandline/parser.h>
#include <wibble/string.h>
#include "dvecompile.h"

namespace divine {

extern const char *generator_custom_api_h_str;
extern const char *pool_h_str;
extern const char *circular_h_str;
extern const char *blob_h_str;
extern const char *compile_defines_str;

using namespace wibble;

struct Compile {
    commandline::BoolOption *o_ltsmin;
    commandline::BoolOption *o_ltsmin_ltl;
    commandline::BoolOption *o_textbook;
    commandline::Engine *cmd_compile;
    commandline::StandardParserWithMandatoryCommand &opts;

    void die_help( std::string bla )
    {
        opts.outputHelp( std::cerr );
        die( bla );
    }


    void die( std::string bla ) __attribute__((noreturn))
    {
        std::cerr << bla << std::endl;
        exit( 1 );
    }

    void run( std::string command ) {
        int status = system( command.c_str() );
#ifdef POSIX
        if ( status != -1 && WEXITSTATUS( status ) != 0 )
            die( "Error running external command: " + command );
#endif
    }

    void gplusplus( std::string in, std::string out, std::string flags = "" ) {
        std::stringstream cmd;
        std::string multiarch =
#if defined(USE_GCC_M32)
            "-m32 "
#elif defined(USE_GCC_M64)
            "-m64 "
#else
            ""
#endif
            ;
        cmd << "g++ -O2 -shared -fPIC " << multiarch << flags << " -o " << out << " " << in;
        run( cmd.str() );
    }

    void compileDve( std::string in, bool ltsmin, bool ltsmin_ltl,
    											  bool textbook ) {
    	if (textbook)
    		die( "Textbook LTL semantics not yet implemented." );
    	if (ltsmin && ltsmin_ltl)
            die( "Use -l OR -L." );
        dve_compiler compiler(ltsmin || ltsmin_ltl, ltsmin_ltl);
        compiler.read( in.c_str() );
        compiler.analyse();

        std::string outfile = str::basename( in ) + ".cpp";
        std::ofstream out( outfile.c_str() );
        compiler.setOutput( out );
        compiler.print_generator();

        if (ltsmin) {
            gplusplus( outfile, str::basename( in ) + "2C" );
        } else {
            gplusplus( outfile, str::basename( in ) + ".so" );
        }
    }

    void compileMurphi( std::string in );

    void main() {
        if ( !opts.hasNext() )
            die_help( "FATAL: No input file specified." );
        std::string input = opts.next();

        if ( access( input.c_str(), R_OK ) )
            die( "FATAL: cannot open input file " + input + " for reading" );
        if ( str::endsWith( input, ".dve" ) ) {
            compileDve( input, o_ltsmin->boolValue(), o_ltsmin_ltl->boolValue(),
            										  o_textbook->boolValue() );
#ifdef HAVE_MURPHI
        } else if ( str::endsWith( input, ".m" ) ) {
            compileMurphi( input );
#endif
        } else {
            std::cerr << "Do not know how to compile this file type." << std::endl;
        }
    }

    Compile( commandline::StandardParserWithMandatoryCommand &_opts )
        : opts( _opts)
    {
        cmd_compile = _opts.addEngine( "compile",
                                       "<input>",
                                       "model compiler");
        o_ltsmin = cmd_compile->add< commandline::BoolOption >(
            "ltsmin", 'l', "ltsmin", "",
            "ltsmin interface" );
        o_ltsmin_ltl = cmd_compile->add< commandline::BoolOption >(
            "ltsmin", 'L', "LTSmin", "",
            "ltsmin interface with LTSmin LTL semantics" );
        o_textbook = cmd_compile->add< commandline::BoolOption >(
            "textbook", 't', "textbook", "",
            "ltsmin interface with textbook LTL semantics" );

    }

};

}
