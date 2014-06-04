#include <tools/compile.h>
#include <tools/dvecompile.h>
#include <divine/generator/common.h>

#include <string>
#include <iostream>
#include <iomanip>

using namespace wibble::str;

namespace divine {

static const size_t BUFLEN = 1024 * 16;

const char *compile_defines_str = "\
#define assert_eq(a,b) assert(a == b)\n\
#define assert_neq(a,b) assert(a != b);\n\
#define assert_leq(a,b) assert(a <= b);\n\
#define assert_die() assert(false);\n\
#define BLOB_NO_HASH\n";
}

void dve_compiler::write_C(dve_expression_t & expr, std::ostream & ostr, std::string state_name)
{
    std::map< int, const char * > op;

    op[ T_LT ] = "<"; op[ T_LEQ ] = "<=";
    op[ T_EQ ] = "=="; op[ T_NEQ ] = "!=";
    op[ T_GT ] = ">"; op[ T_GEQ ] = ">=";

    op[ T_PLUS ] = "+"; op[ T_MINUS ] = "-";
    op[ T_MULT ] = "*"; op[ T_DIV ] = "/"; op[ T_MOD ] = "%";

    op[ T_AND ] = "&"; op[ T_OR ] = "|"; op[ T_XOR ] = "^";
    op[ T_LSHIFT ] = "<<"; op[ T_RSHIFT ] = ">>";

    op[ T_BOOL_AND ] = "&&"; op[ T_BOOL_OR ] = "||";

    op[ T_ASSIGNMENT ] = "=";

    dve_symbol_table_t * parent_table = expr.get_symbol_table();
    if (!parent_table) gerr << "Writing expression: Symbol table not set" << thr();
    switch (expr.get_operator())
    {
        case T_ID:
            if (!(parent_table->get_variable(expr.get_ident_gid())->is_const()))
                ostr<<state_name<<".";
            if(parent_table->get_variable(expr.get_ident_gid())->get_process_gid() != NO_ID)
            {
                ostr << parent_table->get_process(parent_table->get_variable(expr.get_ident_gid())->
                                                  get_process_gid())->get_name(); //name of process
                ostr<<".";
            }
            ostr << parent_table->get_variable(expr.get_ident_gid())->get_name();
            if (ltsmin) ostr << ".var";
            break;
        case T_FOREIGN_ID:
            ostr <<state_name<<"."<< parent_table->get_process(parent_table->get_variable(expr.get_ident_gid())->
                                              get_process_gid())->get_name(); //name of process
            ostr<<".";
            ostr << parent_table->get_variable(expr.get_ident_gid())->get_name();
            if (ltsmin) ostr << ".var";
            break;
        case T_NAT:
            ostr << expr.get_value();
            break;
        case T_PARENTHESIS:
            ostr << "(";
            write_C(*expr.left(), ostr, state_name);
            ostr << ")";
            break;
        case T_SQUARE_BRACKETS:
            if (!(parent_table->get_variable(expr.get_ident_gid())->is_const()))
                ostr<<state_name<<".";
            if(parent_table->get_variable(expr.get_ident_gid())->get_process_gid() != NO_ID)
            {
                ostr << parent_table->get_process(parent_table->get_variable(expr.get_ident_gid())->
                                                  get_process_gid())->get_name(); //name of process
                ostr<<".";
            }
            ostr << parent_table->get_variable(expr.get_ident_gid())->
                get_name(); ostr<<"["; write_C(*expr.left(), ostr, state_name); ostr<<"]" ;
            if (ltsmin) ostr << ".var";
            break;
        case T_FOREIGN_SQUARE_BRACKETS:
            ostr << parent_table->get_process(parent_table->get_variable(expr.get_ident_gid())->
                                              get_process_gid())->get_name(); //name of preocess
            ostr<<"->";
            ostr << parent_table->get_variable(expr.get_ident_gid())->get_name();
            ostr<<"["; write_C(*expr.left(), ostr, state_name); ostr<<"]";
            if (ltsmin) ostr << ".var";
            break;

        case T_LT: case T_LEQ: case T_EQ: case T_NEQ: case T_GT: case T_GEQ:
        case T_PLUS: case T_MINUS: case T_MULT: case T_DIV: case T_MOD:
        case T_AND: case T_OR: case T_XOR: case T_LSHIFT: case T_RSHIFT:
        case T_BOOL_AND: case T_BOOL_OR: case T_ASSIGNMENT:
            write_C( *expr.left(), ostr, state_name );
            ostr << " " << op[ expr.get_operator() ] << " ";
            write_C( *expr.right(), ostr, state_name );
            break;

        case T_DOT:
            ostr << in_state(
                parent_table->get_state(expr.get_ident_gid())->get_process_gid(), 
                parent_table->get_state(expr.get_ident_gid())->get_lid(), state_name );
            break;

        case T_IMPLY:
            write_C(*expr.left(), ostr, state_name);
            ostr<<" -> "; // FIXME this looks wrong, -> in C is dereference
            write_C(*expr.right(), ostr, state_name);
            break;
        case T_UNARY_MINUS:
            ostr<<"-";
            write_C(*expr.right(), ostr, state_name);
            break;
        case T_TILDE:
            ostr<<"~";
            write_C(*expr.right(), ostr, state_name);
            break;
        case T_BOOL_NOT:
            ostr<<" ! (";
            write_C(*expr.right(), ostr, state_name);
            ostr<< " )";
            break;
        default:
            gerr << "Problem in expression - unknown operator"
                 << " number " << expr.get_operator() << psh();
    }
}

std::string dve_compiler::cexpr( dve_expression_t & expr, std::string state )
{
    std::stringstream str;
    str << "(";
    write_C( expr, str, state.c_str() );
    str << ")";
    return str.str();
}

void dve_compiler::gen_header()
{
	line( "#include <stdlib.h>" );
    line( "#include <stdio.h>" );
    line( "#include <string.h>" );
    line( "#include <stdint.h>" );
    line();

    if (ltsmin) {
        // note: everything is 32 bit, this introduces a bug
        // for example when byte value should wrap, now it doesn't
        // thus it should be a 32 bit aligned byte instead of a 32 bit int
        line( "typedef uint64_t ulong_long_int_t;" );
        line( "typedef int64_t slong_long_int_t;" );
        line( "typedef uint32_t ulong_int_t;" );
        line( "typedef int32_t slong_int_t;" );
        line( "typedef union" );
        line( "{" );
        line( "    uint16_t var;" );
        line( "    uint32_t __padding__;" );
        line( "} ushort_int_t;" );
        line( "typedef union" );
        line( "{" );
        line( "    int16_t var;" );
        line( "    uint32_t __padding__;" );
        line( "} sshort_int_t;" );
        line( "typedef union" );
        line( "{" );
        line( "    uint8_t var;" );
        line( "    uint32_t __padding__;" );
        line( "} ubyte_t;" );
        line( "typedef ubyte_t byte_t;" );
        line( "typedef union" );
        line( "{" );
        line( "    int8_t var;" );
        line( "    uint32_t __padding__;" );
        line( "} sbyte_t;" );
        line( "typedef size_t size_int_t;" );
        line();
        line( "typedef struct transition_info" );
        line( "{" );
        line( "    int* label;" );
        line( "    int  group;" );
        line( "    int  por_proviso;" );
        line( "} transition_info_t;" );
        line();
    } else {
        line( "typedef uint64_t ulong_long_int_t;" );
        line( "typedef int64_t slong_long_int_t;" );
        line( "typedef uint32_t ulong_int_t;" );
        line( "typedef int32_t slong_int_t;" );
        line( "typedef uint16_t ushort_int_t;" );
        line( "typedef int16_t sshort_int_t;" );
        line( "typedef uint8_t byte_t;" );
        line( "typedef uint8_t ubyte_t;" );
        line( "typedef int8_t sbyte_t;" );
        line( "typedef size_t size_int_t;" );
        line();
        line( compile_defines_str );
        line();
        line( divine::pool_h_str );
        line();
        line( divine::circular_h_str );
        line();
        line( divine::blob_h_str );
        line();
        line( "using namespace divine;" );
        line( divine::generator_custom_api_h_str );
        line();
    }
}

void dve_compiler::gen_state_struct()
{
    for (size_int_t i=0; i!=glob_var_count; i++)
    {
        dve_symbol_t * var = get_symbol_table()->get_variable(get_global_variable_gid(i));
        if (var->is_const())
        {
            append( "const " );
            if ( var->is_byte() )
                append( "byte_t " );
            else
                append( "sshort_int_t " );

            append( var->get_name() );

            if (var->is_vector())
            {
                append( "[" + fmt( var->get_vector_size() ) + "]" );

                if ( var->get_init_expr_count() ) append( " = {" );
                for (size_int_t j=0; j!=var->get_init_expr_count(); j++)
                {
                    if (ltsmin) append("{");
                    append( cexpr( *((dve_expression_t*)var->get_init_expr(j)), "") );
                    if (ltsmin) append("}");
                    if (j!=(var->get_init_expr_count()-1))
                        append( ", " );
                    else
                        append( "}" );
                }
            } else if ( var->get_init_expr() ) {
                append( string( " = " ) );
                if (ltsmin) append(" {");
                append( cexpr( *((dve_expression_t*) var->get_init_expr()), "") );
                if (ltsmin) append("}");
            }
            line( ";" );
        }
    }
    line();

    bool global = true;
    string name;
    string process_name = "UNINITIALIZED";
    line( "struct state_struct_t" );
    block_begin();
    for (size_int_t i=0; i!=state_creators_count; ++i)
    {
        switch (state_creators[i].type)
        {
            case state_creator_t::VARIABLE:
            {
                name=get_symbol_table()->get_variable(state_creators[i].gid)->get_name();
                if (state_creators[i].array_size)
                {
                    if (state_creators[i].var_type==VAR_BYTE)
                        append( "byte_t " );
                    else if (state_creators[i].var_type==VAR_INT)
                        append( "sshort_int_t " );
                    else gerr << "Unexpected error generating state struct array" << thr();
                    line( name + "[" + fmt( state_creators[i].array_size ) + "];" );
                }
                else
                {
                    if (state_creators[i].var_type==VAR_BYTE)
                        line( "byte_t " + name + ";" );
                    else if (state_creators[i].var_type==VAR_INT)
                        line( "sshort_int_t " + name + ";" );
                    else gerr << "Unexpected error" << thr();
                }
            }
            break;
            case state_creator_t::PROCESS_STATE:
            {
                if (global)
                {
                    global = false;
                }
                else
                {
                    block_end();
                    line( "__attribute__((__packed__)) " + process_name + ";" );
                }
                line( "struct" );
                block_begin();

                process_name=
                    get_symbol_table()->get_process(state_creators[i].gid)->get_name();
                line( "ushort_int_t state;" );
            }
            break;
            case state_creator_t::CHANNEL_BUFFER:
            {
                name=get_symbol_table()->get_channel(state_creators[i].gid)->get_name();
                line( "struct" );
                block_begin();
                line( "ushort_int_t number_of_items;" );
                line( "struct" );
                block_begin();
                dve_symbol_t * symbol =
                    get_symbol_table()->get_channel(state_creators[i].gid);
                size_int_t item_count = symbol->get_channel_type_list_size();

                for (size_int_t j=0; j<item_count; ++j)
                    if (symbol->get_channel_type_list_item(j)==VAR_BYTE)
                        line( "byte_t x" + fmt( j ) + ";" );
                    else if (symbol->get_channel_type_list_item(j)==VAR_INT)
                        line( "sshort_int_t x" + fmt( j ) + ";" );
                    else gerr << "Unexpected error generating state struc channel" << thr();
                block_end();
                line( "content[" + fmt( symbol->get_channel_buffer_size() ) + "];" );
                block_end();
                line( "__attribute__((__packed__)) " + name + ";" );
            }
            break;
            default: gerr << "Unexpected error generating state struct" << thr();
                break;
        };
    }
    if (!global)
    {
        block_end();
        line( "__attribute__((__packed__)) " + process_name + ";" );
    }
    block_end();
    line( "__attribute__((__packed__));" );

    line( "int state_size = sizeof(state_struct_t);" );
    line();
}


void dve_compiler::gen_initial_state()
{
    if (!ltsmin) {
        setAllocator( new generator::Allocator );
        state_t initial_state =  dve_explicit_system_t::get_initial_state();
        append( "char initial_state[] = {" );
        for(int i = 0; i < initial_state.size; i++)
        {
            append( fmt( (unsigned int)(unsigned char)initial_state.ptr[i] ) );
            if(i != initial_state.size - 1)
                append( ", " );
        }
        line( "};" );
        line();
    } else {

        char sep[2] = "";
        char buf[BUFLEN];
        append( "state_struct_t initial_state = { " );
        for (size_int_t i=0; i!=state_creators_count; ++i)
        {
            switch (state_creators[i].type)
            {
                case state_creator_t::VARIABLE:
                {
                    if (state_creators[i].array_size)
                    {
                         for(size_int_t j=0; j<state_creators[i].array_size; j++)
                         {
                            append(sep); sprintf(sep,",");
                            snprintf(buf, BUFLEN, "%d", (initial_values_counts[state_creators[i].gid]?
                                                initial_values[state_creators[i].gid].all_values[j]:0));
                            append(buf);
                         }
                    }
                    else
                    {
                            append(sep); sprintf(sep,",");
                            snprintf(buf, BUFLEN, "%d", (initial_values_counts[state_creators[i].gid]?
                                                initial_values[state_creators[i].gid].all_value:0));
                            append(buf);
                    }
                }
                break;
                case state_creator_t::PROCESS_STATE:
                {
                    append(sep); sprintf(sep,",");
                    snprintf(buf, BUFLEN, "%zu", initial_states[state_creators[i].gid]);
                    append(buf);
                }
                break;
                case state_creator_t::CHANNEL_BUFFER:
                {
                    // initialize channel to 0
                    append(sep); sprintf(sep,",");
                    append("0"); // number_of_items 

                    dve_symbol_t * symbol =
                        get_symbol_table()->get_channel(state_creators[i].gid);
                    size_int_t item_count = symbol->get_channel_type_list_size();
                    size_int_t chan_size = symbol->get_channel_buffer_size();
                    for(size_int_t i=0; i < chan_size; ++i) {
                        for (size_int_t j=0; j<item_count; ++j) {
                            append(sep);
                            append("0");
                        }
                    }
                }
                break;
                default: gerr << "Unexpected error generating initial state" << thr();
                    break;
            };
        }
        line( " };" );
        line();
    }
}

void dve_compiler::output_dependency_comment( ext_transition_t &ext_transition )
{
    // only for ltsmin
    if (!ltsmin)
        return;

    int count = count_state_variables();
    char buf[BUFLEN];

    append("// read:         " );
    for(size_int_t i = 0; i < count; i++)
    {
        snprintf(buf, BUFLEN, "%s%d", ((i==0)?"":","), ext_transition.sv_read[i]);
        append(buf);
    }
    line();

    append("// actions_read: " );
    for(size_int_t i = 0; i < count; i++)
    {
        snprintf(buf, BUFLEN, "%s%d", ((i==0)?"":","), ext_transition.sv_actions_read[i]);
        append(buf);
    }
    line();

    append("// may-write:    " );
    for(size_int_t i = 0; i < count; i++)
    {
        snprintf(buf, BUFLEN, "%s%d", ((i==0)?"":","), ext_transition.sv_may_write[i]);
        append(buf);
    }
    line();

    append("// must-write:   " );
    for(size_int_t i = 0; i < count; i++)
    {
        snprintf(buf, BUFLEN, "%s%d", ((i==0)?"":","), ext_transition.sv_must_write[i]);
        append(buf);
    }
    line();
}

void dve_compiler::analyse_expression( dve_expression_t & expr, ext_transition_t &ext_transition, std::vector<int> &dep)
{
    dve_symbol_table_t * parent_table = expr.get_symbol_table();
    if (!parent_table) gerr << "Writing expression: Symbol table not set" << thr();
    switch (expr.get_operator())
    {
        case T_ID:
            //if (!(parent_table->get_variable(expr.get_ident_gid())->is_const())) // should this be here?
            mark_dependency(expr.get_ident_gid(), state_creator_t::VARIABLE, -1, dep);
            break;
        case T_FOREIGN_ID:
            mark_dependency(expr.get_ident_gid(), state_creator_t::VARIABLE, -1, dep);
            break;
        case T_NAT:
            break;
        case T_PARENTHESIS:
            analyse_expression(*expr.left(), ext_transition, dep);
            break;
        case T_FOREIGN_SQUARE_BRACKETS:
        case T_SQUARE_BRACKETS:
            if ((*expr.left()).get_operator() == T_NAT)
            {
                mark_dependency(expr.get_ident_gid(), state_creator_t::VARIABLE, (*expr.left()).get_value(), dep);
            } else {
                // some expression, mark all & continue analysis
            	if (dep == ext_transition.sv_may_write || dep == ext_transition.sv_read) {
            		mark_dependency(expr.get_ident_gid(), state_creator_t::VARIABLE, -1, dep);
            	}
				if (dep == ext_transition.sv_may_write && may_write_add_read) {
					mark_dependency(expr.get_ident_gid(), state_creator_t::VARIABLE, -1, ext_transition.sv_read);
					mark_dependency(expr.get_ident_gid(), state_creator_t::VARIABLE, -1, ext_transition.sv_actions_read);
            	}
                if ((*expr.left()).get_operator() == T_ASSIGNMENT) {
                    analyse_expression(*expr.left(), ext_transition, ext_transition.sv_may_write);
                    analyse_expression(*expr.left(), ext_transition, ext_transition.sv_must_write);
                } else if (dep == ext_transition.sv_may_write) {
                    analyse_expression(*expr.left(), ext_transition, ext_transition.sv_read);
                } else if (dep != ext_transition.sv_must_write) {
                    analyse_expression(*expr.left(), ext_transition, dep);
                }
            }
            break;
        case T_LT: case T_LEQ: case T_EQ: case T_NEQ: case T_GT: case T_GEQ:
        case T_PLUS: case T_MINUS: case T_MULT: case T_DIV: case T_MOD:
        case T_AND: case T_OR: case T_XOR: case T_LSHIFT: case T_RSHIFT:
        case T_BOOL_AND: case T_BOOL_OR:
            analyse_expression( *expr.left(), ext_transition, ext_transition.sv_read );
            analyse_expression( *expr.right(), ext_transition, ext_transition.sv_read );
            // this probably is not very nice to do...
            if (dep == ext_transition.sv_actions_read) analyse_expression( *expr.left(), ext_transition, ext_transition.sv_actions_read );
            if (dep == ext_transition.sv_actions_read) analyse_expression( *expr.right(), ext_transition, ext_transition.sv_actions_read );
            break;
        case T_ASSIGNMENT:
            analyse_expression( *expr.left(), ext_transition, ext_transition.sv_may_write );
            analyse_expression( *expr.left(), ext_transition, ext_transition.sv_must_write );
            analyse_expression( *expr.right(), ext_transition, ext_transition.sv_read );
            if (dep == ext_transition.sv_actions_read) analyse_expression( *expr.right(), ext_transition, ext_transition.sv_actions_read );
            break;
        case T_DOT:
            // dot addes an explicit == (see code), thus must be read
            mark_dependency(parent_table->get_state(expr.get_ident_gid())->get_process_gid(),
                            state_creator_t::PROCESS_STATE, -1, ext_transition.sv_read);
            mark_dependency(parent_table->get_state(expr.get_ident_gid())->get_process_gid(),
                            state_creator_t::PROCESS_STATE, -1, ext_transition.sv_actions_read);
            break;
        case T_IMPLY:
            analyse_expression( *expr.left(), ext_transition, dep );
            analyse_expression( *expr.right(), ext_transition, dep );
            break;
        case T_UNARY_MINUS:
            analyse_expression( *expr.right(), ext_transition, dep);
            break;
        case T_TILDE:
            analyse_expression( *expr.right(), ext_transition, dep );
            break;
        case T_BOOL_NOT:
            analyse_expression( *expr.right(), ext_transition, dep );
            break;
        default:
            gerr << "Problem in expression - unknown operator"
                 << " number " << expr.get_operator() << psh();
    }
}

void dve_compiler::analyse_transition_dependencies( ext_transition_t &ext_transition )
{
    // only for ltsmin
    if (!ltsmin)
        return;

    // initialize read/write dependency vector
    int count = count_state_variables();
    ext_transition.sv_read.resize(count);
    ext_transition.sv_may_write.resize(count);
    ext_transition.sv_must_write.resize(count);
    ext_transition.sv_actions_read.resize(count);

    // guard

    // mark process as read
    mark_dependency(ext_transition.first->get_process_gid(),
                    state_creator_t::PROCESS_STATE, -1, ext_transition.sv_read);
    mark_dependency(ext_transition.first->get_process_gid(),
                    state_creator_t::PROCESS_STATE, -1, ext_transition.sv_actions_read);

    if (ext_transition.first->get_guard())
    analyse_expression( *(ext_transition.first->get_guard()), ext_transition,
                        ext_transition.sv_read);

    if (ext_transition.synchronized)
    {
        // mark process as read
        mark_dependency(ext_transition.second->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, ext_transition.sv_read);
        mark_dependency(ext_transition.second->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, ext_transition.sv_actions_read);

        // analyse ext_transition->second->get_guard
        if (ext_transition.second->get_guard())
            analyse_expression( *(ext_transition.second->get_guard()), ext_transition,
            ext_transition.sv_read);
    } else {
        int sm = ext_transition.first->get_sync_mode();
        if (sm == SYNC_EXCLAIM_BUFFER || sm == SYNC_ASK_BUFFER)
        {
            mark_dependency(ext_transition.first->get_channel_gid(),
                            state_creator_t::CHANNEL_BUFFER, -1, ext_transition.sv_read);
            mark_dependency(ext_transition.first->get_channel_gid(),
                            state_creator_t::CHANNEL_BUFFER, -1, ext_transition.sv_actions_read);
        }
    }

    if (have_property)
    {
        // mark process as read/write?
        mark_dependency(ext_transition.property->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, ext_transition.sv_read);
        mark_dependency(ext_transition.property->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, ext_transition.sv_actions_read);

        // analyse ext_transition->property->get_guard
        if (ext_transition.property->get_guard())
            analyse_expression( *(ext_transition.property->get_guard()), ext_transition,
            ext_transition.sv_read);
    }

    // effect
    // todo: synchronized & channel effects...
    if (ext_transition.synchronized)
    {
        for(size_int_t s = 0;s < ext_transition.first->get_sync_expr_list_size();s++)
        {
            // todo: test  :)
            analyse_expression( *(ext_transition.first->get_sync_expr_list_item(s)), ext_transition,
                                ext_transition.sv_may_write);
            analyse_expression( *(ext_transition.first->get_sync_expr_list_item(s)), ext_transition,
                                ext_transition.sv_must_write);
            analyse_expression( *(ext_transition.second->get_sync_expr_list_item(s)), ext_transition,
                                ext_transition.sv_read);
            analyse_expression( *(ext_transition.second->get_sync_expr_list_item(s)), ext_transition,
                                ext_transition.sv_actions_read);
        }
    } else {
        int sm = ext_transition.first->get_sync_mode();
        if (sm == SYNC_EXCLAIM_BUFFER)
        {
            // mark entire channel
            mark_dependency(ext_transition.first->get_channel_gid(),
                            state_creator_t::CHANNEL_BUFFER, -1, ext_transition.sv_may_write);
            mark_dependency(ext_transition.first->get_channel_gid(),
                            state_creator_t::CHANNEL_BUFFER, -1, ext_transition.sv_must_write);
            // mark sync expressions
            for(size_int_t s = 0;s < ext_transition.first->get_sync_expr_list_size();s++)
            {
                analyse_expression( *(ext_transition.first->get_sync_expr_list_item(s)), ext_transition,
                                    ext_transition.sv_read);
                analyse_expression( *(ext_transition.first->get_sync_expr_list_item(s)), ext_transition,
                                    ext_transition.sv_actions_read);
            }
        }
        if (sm == SYNC_ASK_BUFFER)
        {
            // mark entire channel
            mark_dependency(ext_transition.first->get_channel_gid(),
                            state_creator_t::CHANNEL_BUFFER, -1, ext_transition.sv_read);
            mark_dependency(ext_transition.first->get_channel_gid(),
                            state_creator_t::CHANNEL_BUFFER, -1, ext_transition.sv_actions_read);
            mark_dependency(ext_transition.first->get_channel_gid(),
                            state_creator_t::CHANNEL_BUFFER, -1, ext_transition.sv_may_write);
            mark_dependency(ext_transition.first->get_channel_gid(),
                            state_creator_t::CHANNEL_BUFFER, -1, ext_transition.sv_must_write);
            // mark sync expressions
            for(size_int_t s = 0;s < ext_transition.first->get_sync_expr_list_size();s++)
            {
                analyse_expression( *(ext_transition.first->get_sync_expr_list_item(s)), ext_transition,
                                    ext_transition.sv_may_write);
                analyse_expression( *(ext_transition.first->get_sync_expr_list_item(s)), ext_transition,
                                    ext_transition.sv_must_write);
            }

        }

    }

    // mark process as read (write is probably in transition effect)
    mark_dependency(ext_transition.first->get_process_gid(),
                    state_creator_t::PROCESS_STATE, -1, ext_transition.sv_may_write);
    mark_dependency(ext_transition.first->get_process_gid(),
                    state_creator_t::PROCESS_STATE, -1, ext_transition.sv_must_write);

    // analyse ext_transition->first
    for(size_int_t e = 0;e < ext_transition.first->get_effect_count();e++) {
        analyse_expression( *(ext_transition.first->get_effect(e)), ext_transition,
        ext_transition.sv_read);
        analyse_expression( *(ext_transition.first->get_effect(e)), ext_transition,
        ext_transition.sv_actions_read);
    }

    // analyse ext_transition->second?
    if (ext_transition.synchronized)
    {
        // mark process as read (write is probably in transition effect)
        mark_dependency(ext_transition.second->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, ext_transition.sv_may_write);
        mark_dependency(ext_transition.second->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, ext_transition.sv_must_write);

        // analyse ext_transition->second
        for(size_int_t e = 0;e < ext_transition.second->get_effect_count();e++) {
            analyse_expression( *(ext_transition.second->get_effect(e)), ext_transition,
            ext_transition.sv_read);
            analyse_expression( *(ext_transition.second->get_effect(e)), ext_transition,
            ext_transition.sv_actions_read);
        }
    }

    if (have_property)
    {
        // mark process as read/write?
        mark_dependency(ext_transition.property->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, ext_transition.sv_may_write);
        mark_dependency(ext_transition.property->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, ext_transition.sv_must_write);
    }
}

void dve_compiler::analyse_transition(
    dve_transition_t * transition,
    vector<ext_transition_t> &ext_transition_vector )
{
    if(!transition->is_sync_ask())
    {
        // transition not of type SYNC_ASK
        if(!have_property)
        {
            // no properties, just add to ext_transition vector
            ext_transition_t ext_transition;
            ext_transition.synchronized = false;
            ext_transition.first = transition;
            analyse_transition_dependencies(ext_transition);
            ext_transition_vector.push_back(ext_transition);
        }
        else
        {
            // this transition is not a property, but there are properties
            // forall properties, add this transition to ext_transition_vector
            for(iter_property_transitions = property_transitions.begin();
                iter_property_transitions != property_transitions.end();
                iter_property_transitions++)
            {
                ext_transition_t ext_transition;
                ext_transition.synchronized = false;
                ext_transition.first = transition;
                ext_transition.property = (*iter_property_transitions);
                analyse_transition_dependencies(ext_transition);
                ext_transition_vector.push_back(ext_transition);
            }
        }
    }
    else
    {
        // transition of type SYNC_ASK
        iter_channel_map = channel_map.find(transition->get_channel_gid());
        if(iter_channel_map != channel_map.end())
        {
            // channel of this transition is found
            // (strange test, no else part for if statement)
            // assume: channel should always be present
            // forall transitions that also use this channel, add to ext_transitions
            for(iter_transition_vector  = iter_channel_map->second.begin();
                iter_transition_vector != iter_channel_map->second.end();
                iter_transition_vector++)
            {
                if (transition->get_process_gid() != (*iter_transition_vector)->get_process_gid() ) //not synchronize with yourself
                {
                    if(!have_property)
                    {
                        // system has no properties, so add only once without property
                        ext_transition_t ext_transition;
                        ext_transition.synchronized = true;
                        ext_transition.first = transition;
                        ext_transition.second = (*iter_transition_vector);
                        analyse_transition_dependencies(ext_transition);
                        ext_transition_vector.push_back(ext_transition);
                    }
                    else
                    {
                        // system has properties, so forall properties, add the combination if this transition,
                        // the transition that also uses this channel and the property
                        for(iter_property_transitions = property_transitions.begin();
                            iter_property_transitions != property_transitions.end();
                            iter_property_transitions++)
                        {
                            ext_transition_t ext_transition;
                            ext_transition.synchronized = true;
                            ext_transition.first = transition;
                            ext_transition.second = (*iter_transition_vector);
                            ext_transition.property = (*iter_property_transitions);
                            analyse_transition_dependencies(ext_transition);
                            ext_transition_vector.push_back(ext_transition);
                        }
                    }
                }
            }
        }
    }
}

void dve_compiler::analyse()
{
    dve_transition_t * transition;
    have_property = get_with_property();

    // obtain transition with synchronization of the type SYNC_EXCLAIM and property transitions
    for(size_int_t i = 0; i < get_trans_count(); i++)
    {
        transition = dynamic_cast<dve_transition_t*>(get_transition(i));
        if(transition->is_sync_exclaim())
        {
            iter_channel_map = channel_map.find(transition->get_channel_gid());
            if(iter_channel_map == channel_map.end()) //new channel
            {
                vector<dve_transition_t*> transition_vector;
                transition_vector.push_back(transition);
                channel_map.insert(pair<size_int_t,vector<dve_transition_t*> >(
                                       transition->get_channel_gid(),transition_vector));
            }
            else{
                iter_channel_map->second.push_back(transition);
            }
        }

        if( is_property( transition->get_process_gid() ) )
            property_transitions.push_back(transition);
    }

    // obtain map of transitions
    for(size_int_t i = 0; i < get_trans_count(); i++)
    {
        transition = dynamic_cast<dve_transition_t*>(get_transition(i));
        if(!transition->is_sync_exclaim() && !is_property( transition->get_process_gid() ) )
        {
            // not syncronized sender without buffer and not a property transition
            iter_transition_map = transition_map.find(transition->get_process_gid());

            //new process it means that new state in process is also new
            if( iter_transition_map == transition_map.end())
            {
                // new process, add to transition map
                map<size_int_t,vector<ext_transition_t> >  process_transition_map;
                vector<ext_transition_t> ext_transition_vector;

                analyse_transition( transition, ext_transition_vector );

                // for this process state, add the ext transitions
                process_transition_map.insert(pair<size_int_t,vector<ext_transition_t> >(
                                                  transition->get_state1_lid(),ext_transition_vector));
                // then add this vector to the transition map for this process
                transition_map.insert(pair<size_int_t,map<size_int_t,vector<ext_transition_t> > >(
                                          transition->get_process_gid(),process_transition_map));
            } else {
                // existing process, find process_transition_map
                iter_process_transition_map =
                    iter_transition_map->second.find(transition->get_state1_lid());

                //new state in current process
                if( iter_process_transition_map == iter_transition_map->second.end())
                {
                    vector<ext_transition_t> ext_transition_vector;
                    analyse_transition( transition, ext_transition_vector );

                    // and reinsert result
                    iter_transition_map->second.insert(
                        pair<size_int_t,vector<ext_transition_t> >(
                            transition->get_state1_lid(),ext_transition_vector) );
                } else analyse_transition( transition, iter_process_transition_map->second );
            }
        }
    }
}

void dve_compiler::transition_guard( ext_transition_t *et, std::string in )
{
    if_begin( false );

    // ltsmin guard extension
    if ( ltsmin && !many) {
        if_clause( in_state( et->first->get_process_gid(), et->first->get_state1_lid(), in) );
    }

    if_cexpr_clause( et->first->get_guard(), in );

    if( et->synchronized )
    {
        if_clause( in_state( et->second->get_process_gid(),
                             et->second->get_state1_lid(), in ) );
        if_cexpr_clause( et->second->get_guard(), in );
    }
    else
    {
        int chan = et->first->get_channel_gid();
        if(et->first->get_sync_mode() == SYNC_EXCLAIM_BUFFER)
            if_clause( relate( channel_items( chan, in ), "!=",
                               fmt( channel_capacity( chan ) ) ) );

        if(et->first->get_sync_mode() == SYNC_ASK_BUFFER)
            if_clause( relate( channel_items( chan, in ), "!=", "0" ) );
    }
    if(have_property)
    {
        if_clause( in_state( et->property->get_process_gid(),
                             et->property->get_state1_lid(), in ) );
        if_cexpr_clause( et->property->get_guard(), in );
    }

    if_end();
}

void dve_compiler::transition_effect( ext_transition_t *et, std::string in, std::string out )
{
    if(et->synchronized)
    {
        for(size_int_t s = 0;s < et->first->get_sync_expr_list_size();s++) {
            assign( cexpr( *et->first->get_sync_expr_list_item(s), out ),
                    cexpr( *et->second->get_sync_expr_list_item(s), in ) );
            line("cpy[((int*)&"+cexpr( *et->first->get_sync_expr_list_item(s), out )+" - (int*)&"+out+")] = 0;");
        }
    }
    else
    {
        int chan = et->first->get_channel_gid();
        if(et->first->get_sync_mode() == SYNC_EXCLAIM_BUFFER)
        {
            for(size_int_t s = 0;s < et->first->get_sync_expr_list_size();s++)
            {
                assign( channel_item_at( chan, channel_items( chan, in ), s, out ),
                        cexpr( *et->first->get_sync_expr_list_item( s ), in ) );
                line("cpy[((int*)&"+channel_item_at( chan, channel_items( chan, in ), s, out )+" - (int*)&"+out+")] = 0;");
            }
            line( channel_items( chan, out ) + "++;" );
        }
        if(et->first->get_sync_mode() == SYNC_ASK_BUFFER)
        {
            for(size_int_t s = 0;s < et->first->get_sync_expr_list_size();s++) {
                assign( cexpr( *et->first->get_sync_expr_list_item(s), out ),
                        channel_item_at( chan, "0", s, in ) );
            	line("cpy[((int*)&"+cexpr( *et->first->get_sync_expr_list_item(s), out )+" - (int*)&"+out+")] = 0;");
            }
            line( channel_items( chan, out ) + "--;" );

            line( "for(size_int_t i = 1 ; i <= " + channel_items( chan, out ) + "; i++)" );
            block_begin();
            for(size_int_t s = 0;s < et->first->get_sync_expr_list_size();s++)
            {
                assign( channel_item_at( chan, "i-1", s, out ), channel_item_at( chan, "i", s, in ) );
            	line("cpy[((int*)&"+channel_item_at( chan, "i-1", s, out )+" - (int*)&"+out+")] = 0;");
                assign( channel_item_at( chan, "i", s, out ), "0" );
            	line("cpy[((int*)&"+channel_item_at( chan, "i", s, out )+" - (int*)&"+out+")] = 0;");
            }
            block_end();
        }
    }

    //first transition effect
    assign( process_state( et->first->get_process_gid(), out ),
            fmt( et->first->get_state2_lid() ) );
	line("cpy[((int*)&"+process_state( et->first->get_process_gid(), out )+" - (int*)&"+out+")] = 0;");

    for(size_int_t e = 0;e < et->first->get_effect_count();e++) {
        print_cexpr( *et->first->get_effect(e), out );
        line("cpy[((int*)&"+cexpr(*et->first->get_effect(e)->left(), out)+" - (int*)&"+out+")] = 0;");
    }

    if(et->synchronized) //second transiton effect
    {
        assign( process_state( et->second->get_process_gid(), out ),
                fmt( et->second->get_state2_lid() ) );
    	line("cpy[((int*)&"+process_state( et->second->get_process_gid(), out )+" - (int*)&"+out+")] = 0;");
        for(size_int_t e = 0;e < et->second->get_effect_count();e++) {
            print_cexpr( *et->second->get_effect(e), out );
            line("cpy[((int*)&"+cexpr(*et->second->get_effect(e)->left(), out)+" - (int*)&"+out+")] = 0;");
        }
    }

    if(have_property) //change of the property process state
    {
        assign( process_state( et->property->get_process_gid(), out ),
                fmt( et->property->get_state2_lid() ) );
		line("cpy[((int*)&"+process_state( et->property->get_process_gid(), out )+" - (int*)&"+out+")] = 0;");
    }

    // show dependency information in the source
    output_dependency_comment(*et);
}

void dve_compiler::new_output_state() {
    if (ltsmin) {
            line( "*out = *in;" );
    } else {
        line( "divine::Blob blob_out( *(setup->pool), setup->slack + state_size );" );
        line( "state_struct_t *out = &blob_out.get< state_struct_t >( setup->slack );" );
        line( "blob_out.clear( 0, setup->slack );" );
        line( "*out = *in;" );
    }
}

void dve_compiler::yield_state() {
    if (ltsmin) {
        if (many) {
            line ("transition_info.group = " + fmt( current_label++) + ";");
        }
        line( "callback(arg, &transition_info, out, cpy);" );
        line( "++states_emitted;" );
    } else {
        if ( many ) {
            line( "if (buf_out->space() < 2) {" );
            line( "    buf_out->unadd( states_emitted );" );
            line( "    return;" );
            line( "}");
            line( "buf_out->add( (*buf_in)[ 0 ] );" );
            line( "buf_out->add( blob_out );" );
            line( "++states_emitted;" );
        } else {
            line( "*to = blob_out;" );
            line( "return " + fmt( current_label ) + ";" );
        }
    }
}

void dve_compiler::gen_ltsmin_successors(bool condition)
{
    string in = "(*in)", out = "(*out)", space = "";
    bool some_commited_state = false;

    // find some commited state
    for(size_int_t i = 0; i < get_process_count(); i++)
        for(size_int_t j = 0; j < dynamic_cast<dve_process_t*>(get_process(i))->get_state_count(); j++)
            if(dynamic_cast<dve_process_t*>(get_process(i))->get_commited(j))
                some_commited_state = true;

    if (some_commited_state)
    {
        for(size_int_t i = 0; i < this->get_process_count(); i++)
        {
            if( transition_map.find(i) != transition_map.end() && !is_property( i ) )
                for(iter_process_transition_map = transition_map.find(i)->second.begin();
                    iter_process_transition_map != transition_map.find(i)->second.end();
                    iter_process_transition_map++)
                {
                    if(dynamic_cast<dve_process_t*>(get_process(i))->get_commited(
                           iter_process_transition_map->first))
                    {
                        for(iter_ext_transition_vector = iter_process_transition_map->second.begin();
                            iter_ext_transition_vector != iter_process_transition_map->second.end();
                            iter_ext_transition_vector++)
                        {
                            // !! jak je to s property synchronizaci v comitted stavech !!
                            if( !iter_ext_transition_vector->synchronized ||
                                dynamic_cast<dve_process_t*>(
                                    get_process(iter_ext_transition_vector->second->get_process_gid()))->
                                get_commited(iter_ext_transition_vector->second->get_state1_lid()) )
                            {
                                // only generate if not synchonized or synchonized with a committed transition
                                new_label();

                                transition_guard( &*iter_ext_transition_vector, in );                                
                                block_begin();
                                new_output_state();
                                transition_effect( &*iter_ext_transition_vector, in, out );
                                yield_state();
                                block_end();

                                line("return states_emitted;");
                            }
                        }
                    }
                }
        }
    }

    for(size_int_t i = 0; i < get_process_count(); i++)
    {
        if(transition_map.find(i) != transition_map.end() && !is_property( i ))
            for(iter_process_transition_map = transition_map.find(i)->second.begin();
                iter_process_transition_map != transition_map.find(i)->second.end();
                iter_process_transition_map++)
            {
                for(iter_ext_transition_vector = iter_process_transition_map->second.begin();
                    iter_ext_transition_vector != iter_process_transition_map->second.end();
                    iter_ext_transition_vector++)
                {
                    // make sure this transition is not a committed one
                    if (!
                        dynamic_cast<dve_process_t*>(
                            get_process(iter_ext_transition_vector->first->get_process_gid()))->
                        get_commited(iter_ext_transition_vector->first->get_state1_lid()) )
                    {

                        new_label();
                        if (condition)
                            transition_guard( &*iter_ext_transition_vector, in );
                        
                        block_begin();
                        if (some_commited_state)
                        {
                            // committed state
                            if_begin( true );

                            for(size_int_t p = 0; p < get_process_count(); p++)
                                for(size_int_t c = 0; c < dynamic_cast<dve_process_t*>(get_process(p))->get_state_count(); c++)
                                    if(dynamic_cast<dve_process_t*>(get_process(p))->get_commited(c))
                                        if_clause( in_state( p, c, in ) );

                            if_end();
                            line("    return 0;"); // bail out early
                        }


                        new_output_state();
                        transition_effect( &*iter_ext_transition_vector, in, out );
                        yield_state();
                        block_end();
                        line("return states_emitted;");
                    }
                }
            }
    }
}


void dve_compiler::gen_successors()
{
    string in = "(*in)", out = "(*out)", space = "";
    bool some_commited_state = false;

    new_label();
    if_begin( true );

    for(size_int_t i = 0; i < get_process_count(); i++)
        for(size_int_t j = 0; j < dynamic_cast<dve_process_t*>(get_process(i))->get_state_count(); j++)
            if(dynamic_cast<dve_process_t*>(get_process(i))->get_commited(j))
                if_clause( in_state( i, j, in ) );

    if_end(); block_begin(); // committed states

    for(size_int_t i = 0; i < this->get_process_count(); i++)
    {
        if( transition_map.find(i) != transition_map.end() && !is_property( i ) )
            for(iter_process_transition_map = transition_map.find(i)->second.begin();
                iter_process_transition_map != transition_map.find(i)->second.end();
                iter_process_transition_map++)
            {
                if(dynamic_cast<dve_process_t*>(get_process(i))->get_commited(
                       iter_process_transition_map->first))
                {
                    new_label();

                    if_begin( true );
                    if_clause( in_state( i, iter_process_transition_map->first, in ) );
                    if_end(); block_begin();


                    for(iter_ext_transition_vector = iter_process_transition_map->second.begin();
                        iter_ext_transition_vector != iter_process_transition_map->second.end();
                        iter_ext_transition_vector++)
                    {
                        // !! jak je to s property synchronizaci v comitted stavech !!
                        if( !iter_ext_transition_vector->synchronized ||
                            dynamic_cast<dve_process_t*>(
                                get_process(iter_ext_transition_vector->second->get_process_gid()))->
                            get_commited(iter_ext_transition_vector->second->get_state1_lid()) )
                        {
                            new_label();

                            transition_guard( &*iter_ext_transition_vector, in );
                            block_begin();
                            new_output_state();
                            transition_effect( &*iter_ext_transition_vector, in, out );
                            yield_state();
                            block_end();
                        }
                    }

                    block_end();
                }
            }
    }

    new_label(); // Trick. : - )
    line( ";" );

    block_end();
    line( "else" );
    block_begin();

    for(size_int_t i = 0; i < get_process_count(); i++)
    {
        if(transition_map.find(i) != transition_map.end() && !is_property( i ))
            for(iter_process_transition_map = transition_map.find(i)->second.begin();
                iter_process_transition_map != transition_map.find(i)->second.end();
                iter_process_transition_map++)
            {
                // make sure this transition is not a committed one
                if (!
                    dynamic_cast<dve_process_t*>(
                        get_process(i))->get_commited(iter_process_transition_map->first) )
                {

                    new_label();
                    if_begin( true );
                    if_clause( in_state( i, iter_process_transition_map->first, in ) );

                    if_end(); block_begin();

                    for(iter_ext_transition_vector = iter_process_transition_map->second.begin();
                        iter_ext_transition_vector != iter_process_transition_map->second.end();
                        iter_ext_transition_vector++)
                    {
                        new_label();

                        transition_guard( &*iter_ext_transition_vector, in );
                        block_begin();
                        new_output_state();
                        transition_effect( &*iter_ext_transition_vector, in, out );
                        line( "system_in_deadlock = false;" );
                        yield_state();
                        block_end();
                    }
                    block_end();
                }
            }
    }
    block_end();

    if (ltsmin_ltl) return;

    new_label();

    if_begin( true );
    if_clause( "system_in_deadlock" );
    if_end(); block_begin();

    for(iter_property_transitions = property_transitions.begin();
        iter_property_transitions != property_transitions.end();
        iter_property_transitions++)
    {
        new_label();
        if_begin( false );

        if_clause( in_state( (*iter_property_transitions)->get_process_gid(),
                             (*iter_property_transitions)->get_state1_lid(), in ) );
        if_cexpr_clause( (*iter_property_transitions)->get_guard(), in );

        if_end(); block_begin();
        new_output_state();

        assign( process_state( (*iter_property_transitions)->get_process_gid(), out ),
                fmt( (*iter_property_transitions)->get_state2_lid() ) );
        line("cpy[((int*)&"+process_state( (*iter_property_transitions)->get_process_gid(), out )+" - (int*)&"+out+")] = 0;");

        yield_state();
        block_end();
    }
    block_end();
}

void dve_compiler::gen_is_accepting()
{
    // not compulsory, so don't bother if not needed
    if(!have_property)
        return;

    if (ltsmin) {
        line( "extern \"C\" int buchi_is_accepting( void* model, void *_state )" );
        block_begin();
        line( "(void)model;" );
        line( "state_struct_t &state = * (state_struct_t*) _state;" );
    } else {
        line( "extern \"C\" bool is_accepting( CustomSetup *setup, Blob b, int size )" );
        block_begin();
        line( "state_struct_t &state = b.get< state_struct_t >( setup->slack );" );
    }

    for(size_int_t i = 0; i < dynamic_cast<dve_process_t*>(get_process((get_property_gid())))->get_state_count(); i++)
    {
        if (dynamic_cast<dve_process_t*>(get_process((get_property_gid())))->get_acceptance(i, 0, 1) )
        {
            if_begin( true );
            if_clause( in_state( get_property_gid(), i, "state" ) );
            if_end();
            line( "    return true;" );
        }
    }
    line( "return false;" );
    block_end();

    line();
}

void dve_compiler::print_generator()
{
    gen_header();
    gen_state_struct();
    gen_initial_state();

    line( "extern \"C\" int get_state_size() {" );
    line( "    return state_size;" );
    line( "}" );
    line();

    if (ltsmin) {
        line( "extern \"C\" void get_initial_state( void *to )" );
        block_begin();
        line( "memcpy(to, &initial_state, state_size);" );
        block_end();
        line();

        line( "extern \"C\" int have_property()" );
        block_begin();
        if (have_property) {
            line("return true;");
        } else {
            line("return false;");
        }
        block_end();
        line();

        gen_is_accepting();



        many = false;
        current_label = 0;
        line( "extern \"C\" int get_successor( void* model, int t, const state_struct_t *in, void (*callback)(void* arg, transition_info_t *transition_info, state_struct_t *out, int *cpy), void *arg ) " );
        block_begin();
        line( "transition_info_t transition_info = { NULL, t, 0 };" );
        line( "(void)model; // ignore model" );
        line( "int states_emitted = 0;" );
        line( "state_struct_t tmp;" );
        line( "state_struct_t *out = &tmp;" );
        append( "int cpy[" + fmt(count_state_variables())+ "] = { " );
        for (int i = 0; i < count_state_variables(); i++) append ("1, ");
		line("};");
        line( "goto switch_state;" );
        gen_ltsmin_successors(true);
        // switch block
        line( "switch_state: switch( t )" );
        block_begin();
        for(int i=0; i < current_label; i++)
                line( "case " + fmt( i ) + ": goto l" + fmt( i ) + ";" );
        line( "default: printf (\"Wrong group! Using greybox/long call + -l (DiVinE LTL semantics)? This combo is not implemented.\"); exit (-1);" );
        block_end();
        line("return 0;");
        // end switch block
        block_end();
        line();
        
        many = false;
        current_label = 0;
        line( "extern \"C\" int get_action( void* model, int t, const state_struct_t *in, void (*callback)(void* arg, transition_info_t *transition_info, state_struct_t *out, int *cpy), void *arg ) " );
        block_begin();
        line( "transition_info_t transition_info = { NULL, t, 0 };" );
        line( "(void)model; // ignore model" );
        line( "int states_emitted = 0;" );
        line( "state_struct_t tmp;" );
        line( "state_struct_t *out = &tmp;" );
        append( "int cpy[" + fmt(count_state_variables())+ "] = { " );
        for (int i = 0; i < count_state_variables(); i++) append ("1, ");
		line("};");
        line( "goto switch_state;" );
        gen_ltsmin_successors(false);
        // switch block
        line( "switch_state: switch( t )" );
        block_begin();
        for(int i=0; i < current_label; i++)
                line( "case " + fmt( i ) + ": goto l" + fmt( i ) + ";" );
        line( "default: printf (\"Wrong group! Using greybox/long call + -l (DiVinE LTL semantics)? This combo is not implemented.\"); exit (-1);" );
        block_end();
        line("return 0;");
        // end switch block
        block_end();
        line();        
        
        many = true;
        current_label = 0;

        line( "extern \"C\" int get_successors( void *model, const state_struct_t *in, void (*callback)(void *arg, transition_info_t *transition_info, state_struct_t *out, int *cpy), void *arg ) " );
        block_begin();
        line( "(void)model; // ignore model" );
        line( "bool system_in_deadlock = true;" );
        line( "transition_info_t transition_info = { NULL, -1, 0 };" );
        line( "int states_emitted = 0;" );
        line( "state_struct_t tmp;" );
        line( "state_struct_t *out = &tmp;" );
        append( "int cpy[" + fmt(count_state_variables())+ "] = { " );
        for (int i = 0; i < count_state_variables(); i++) append ("1, ");
		line("};");
        gen_successors();
        line( "return states_emitted;" );
        block_end();
        line();

        // state descriptors
        gen_state_info();
        gen_transition_info();
    } else {
        line( "extern \"C\" int setup( CustomSetup *setup ) {" );
        line( "    setup->state_size = state_size;" );
        line( "    setup->has_property = " + fmt( get_with_property() ) + ";" );
        line( "}" );

        line( "extern \"C\" void get_initial( CustomSetup *setup, Blob *out ) {" );
        line( "    Blob b( *(setup->pool), state_size + setup->slack );" );
        line( "    memcpy(b.data() + setup->slack, initial_state, state_size);" );
        line( "    *out = b;" );
        line( "}" );
        line();

        many = false;
        current_label = 1;

        gen_is_accepting();

        line( "extern \"C\" int get_successor( CustomSetup *setup, int next_state, Blob from, Blob *to ) " );
        block_begin();
        line( "const state_struct_t *in = &from.get< state_struct_t >( setup->slack );" );
        line( "bool system_in_deadlock = false;" );
        line( "goto switch_state;" );

        gen_successors();

        new_label();
        line( "return 0;" );

        line( "switch_state: switch( next_state )" );
        block_begin();
        for(int i=1; i < current_label; i++)
            if (i==1)
                line( "case " + fmt( i ) + ": system_in_deadlock = true; goto l" + fmt( i ) + ";" );
            else
                line( "case " + fmt( i ) + ": goto l" + fmt( i ) + ";" );
        block_end();

        block_end();

        // many = true;
        // current_label = 0;
#if 0
        line( "extern \"C\" void get_many_successors( int slack, char *_pool, char *," );
        line( "                                       char *_buf_in, char *_buf_outf, char *_buf_outs ) " );
        block_begin();
        line( "divine::Pool *pool = (divine::Pool *) _pool;" );
        line( "typedef divine::Circular< divine::Blob, 0 > Buffer;" );
        line( "Buffer *buf_in = (Buffer *) _buf_in;" );
        line( "Buffer *buf_out = (Buffer *) _buf_out;" );
        line( "int states_emitted;" );
        line( "bool system_in_deadlock;" );
        line( "state_struct_t *in;" );

        line( "next:" );
        line( "system_in_deadlock = true;" );
        line( "states_emitted = 0;" );
        line( "in = (state_struct_t*) ((*buf_in)[ 0 ].data() + slack);" );
        gen_successors();
        line( "buf_in->drop( 1 );" );
        line( "if ( buf_in->empty() ) return;" );
        line( "goto next;" );
        block_end();
#endif
    }
}

void dve_compiler::gen_state_info()
{
    char buf[BUFLEN];
    // number of variables in the state
    line( "extern \"C\" int get_state_variable_count() " );
    block_begin();
    snprintf(buf, BUFLEN, "return %d;", count_state_variables());
    line(buf);
    block_end();
    line();

    // name of state variables
    line( "extern \"C\" const char* get_state_variable_name(int var)" );
    block_begin();

    line("switch (var)");
    block_begin();

    // iterate over state variables, output name per variable
    bool global = true;
    string name = "UNINITIALIZED";
    string process_name = "UNINITIALIZED";
    int k=0;
    for (size_int_t i=0; i<state_creators_count; ++i, ++k)
    {
    	snprintf(buf, BUFLEN, "case %d:", k);
        line(buf);

        switch (state_creators[i].type)
        {
            case state_creator_t::VARIABLE:
                name = (global?"":process_name + ".") + 
                        get_symbol_table()->get_variable(state_creators[i].gid)->get_name();

                if (state_creators[i].array_size)
                {
                    for(size_int_t j=0; j < state_creators[i].array_size; ++j)
                    {
                        snprintf(buf, BUFLEN, "    return \"%s[%zu]\";", name.c_str(), j);
                        line(buf);
                        if (j < state_creators[i].array_size - 1) {
                            snprintf(buf, BUFLEN, "case %d:", ++k);
                            line(buf);
                        }
                    }
                    continue;
                }
                break;
            case state_creator_t::PROCESS_STATE:
                global = false;
                name = get_symbol_table()->get_process(state_creators[i].gid)->get_name();
                process_name = name;
                break;
            case state_creator_t::CHANNEL_BUFFER:
            {
                name = get_symbol_table()->get_channel(state_creators[i].gid)->get_name();
                snprintf(buf, BUFLEN, "    return \"%s.number_of_items\";", name.c_str());
                line(buf);

                dve_symbol_t * symbol =
                  get_symbol_table()->get_channel(state_creators[i].gid);
                size_int_t item_count = symbol->get_channel_type_list_size();
                size_int_t chan_size = symbol->get_channel_buffer_size();
                for(size_int_t i=0; i < chan_size; ++i)
                {
                    for (size_int_t j=0; j<item_count; ++j)
                    {
                        snprintf(buf, BUFLEN, "case %d:", ++k);
                        line(buf);
                        snprintf(buf, BUFLEN, "    return \"%s[%zu].x%zu\";", name.c_str(), i,j);
                        line(buf);
                    }
                }
                continue;
            }
            default: gerr << "Unexpected error generating variable names" << thr();
                break;
        };

        snprintf(buf, BUFLEN, "    return \"%s\";", name.c_str());
        line(buf);
    }

    line("default:");
    line("    return NULL;");
    block_end();

    block_end();
    line();

    // gather type information
    std::map<string, int> type_no;
    std::map<string, std::vector<string> > type_value;
    int type_count = 0;
    for (size_int_t i=0; i<state_creators_count; ++i, ++k)
    {
        vector<string> values;
        values.clear();
        string type_name = "UNINITIALIZED";

        switch (state_creators[i].type)
        {
            case state_creator_t::VARIABLE:
            {
                dve_symbol_t * var = get_symbol_table()->get_variable(state_creators[i].gid);
                if (var->is_byte()) { type_name = "byte"; } else { type_name = "int"; };
                break;
            }
            case state_creator_t::PROCESS_STATE:
            {
                type_name = get_symbol_table()->get_process(state_creators[i].gid)->get_name();
                for(size_int_t j = 0;
                    j < dynamic_cast<dve_process_t*>(this->get_process(state_creators[i].gid))->get_state_count();
                    j++)
                {
                    // add to possible process values
                    values.push_back(
                        get_symbol_table()->get_state(
                        dynamic_cast<dve_process_t*>(this->get_process(state_creators[i].gid))->get_state_gid(j))->get_name()
                    );
                }
                break;
            }
            case state_creator_t::CHANNEL_BUFFER:
            {
                // the int type is added for the channel by default (number of items is int)
                // iterate over the channel, if type byte is used, add it also
                type_name = "byte";

                dve_symbol_t * symbol =
                    get_symbol_table()->get_channel(state_creators[i].gid);
                size_int_t item_count = symbol->get_channel_type_list_size();
                for (size_int_t j=0; j<item_count; ++j)
                {
                    if (symbol->get_channel_type_list_item(j)==VAR_BYTE)
                    {
                        // check existence of byte type
                        if (type_no.find(type_name) == type_no.end()) {
                            type_no[type_name] = type_count++;
                            type_value[type_name] = values;
                            // if byte exists, the only other possibility is int, which is added
                            // by default
                            break;
                        }
                    }
                }
                // byte migth be added ab
                type_name = "int";
                break;
            }
            default: gerr << "Unexpected error while gathering type information" << thr();
                break;
        }
        if (type_no.find(type_name) == type_no.end())
        {
            type_no[type_name] = type_count++;
            type_value[type_name] = values;
        }
    }

    // name of state variables
    line( "extern \"C\" int get_state_variable_type(int var)" );
    block_begin();

    line("switch (var)");
    block_begin();

    // iterate over state variables, output name per variable
    k = 0;
    for (size_int_t i=0; i<state_creators_count; ++i, ++k)
    {
        string type_name = "UNINITIALIZED";

        snprintf(buf, BUFLEN, "case %d:", k);
        line(buf);

        switch (state_creators[i].type)
        {
            case state_creator_t::VARIABLE:
            {
                dve_symbol_t * var = get_symbol_table()->get_variable(state_creators[i].gid);
                if (var->is_byte()) { type_name = "byte"; } else { type_name = "int"; };

                if (state_creators[i].array_size)
                {
                    for(size_int_t j=0; j < state_creators[i].array_size - 1; ++j)
                    {
                        snprintf(buf, BUFLEN, "    return %d;", type_no[type_name]);
                        line(buf);
                        snprintf(buf, BUFLEN, "case %d:", ++k);
                        line(buf);
                    }
                }
                break;
            }
            case state_creator_t::PROCESS_STATE:
                type_name = get_symbol_table()->get_process(state_creators[i].gid)->get_name();
                break;
            case state_creator_t::CHANNEL_BUFFER:
            {
                snprintf(buf, BUFLEN, "    return %d;", type_no["int"]);
                line(buf);

                dve_symbol_t * symbol =
                  get_symbol_table()->get_channel(state_creators[i].gid);
                size_int_t item_count = symbol->get_channel_type_list_size();
                size_int_t chan_size = symbol->get_channel_buffer_size();
                for(size_int_t i=0; i < chan_size; ++i)
                {
                    for (size_int_t j=0; j<item_count; ++j)
                    {
                        snprintf(buf, BUFLEN, "case %d:", ++k);
                        line(buf);
                        if (symbol->get_channel_type_list_item(j) == VAR_BYTE)
                        {
                            snprintf(buf, BUFLEN, "    return %d;", type_no["byte"]);
                            line(buf);
                        } else {
                            snprintf(buf, BUFLEN, "    return %d;", type_no["int"]);
                            line(buf);
                        }
                    }
                }
                continue;
            }
            default: gerr << "Unexpected error while writing name per variable" << thr();
                break;
        };

        snprintf(buf, BUFLEN, "    return %d;", type_no[type_name]);
        line(buf);
    }

    line("default:");
    line("    return -1;");
    block_end();

    block_end();
    line();


    // number of different types in the state
    line( "extern \"C\" int get_state_variable_type_count() " );
    block_begin();
    snprintf(buf, BUFLEN, "return %d;", type_count);
    line(buf);
    block_end();
    line();

    // names of types
    line( "extern \"C\" const char* get_state_variable_type_name(int type) " );
    block_begin();
        line("switch (type)");
        block_begin();
        for(std::map<string, int>::iterator ix = type_no.begin(); ix != type_no.end(); ++ix)
        {
            snprintf(buf, BUFLEN, "case %d:", ix->second);
            line(buf);
            snprintf(buf, BUFLEN, "    return \"%s\";", ix->first.c_str());
            line(buf);
        }
        line("default:");
        line("    return NULL;");
        block_end();
    block_end();
    line();

    // number of different values of a type
    line( "extern \"C\" int get_state_variable_type_value_count(int type)" );
    block_begin();
        line("switch (type)");
        block_begin();
        for(std::map<string, int>::iterator ix = type_no.begin(); ix != type_no.end(); ++ix)
        {
            snprintf(buf, BUFLEN, "case %d: // %s", ix->second, ix->first.c_str());
            line(buf);
            snprintf(buf, BUFLEN, "    return %zu;", type_value[ix->first].size());
            line(buf);
        }
        line("default:");
        line("    return -1;");
        block_end();
    block_end();
    line();

    // values of types
    line( "extern \"C\" const char* get_state_variable_type_value(int type, int value) " );
    block_begin();
        line("switch (type)");
        block_begin();
        for(std::map<string, int>::iterator ix = type_no.begin(); ix != type_no.end(); ++ix)
        {
            if (type_value[ix->first].size())
            {
                snprintf(buf, BUFLEN, "case %d:", ix->second);
                line(buf);
                block_begin();
                    line("switch (value)");
                    block_begin();
                    for(int i=0; i < type_value[ix->first].size(); ++i)
                    {
                        snprintf(buf, BUFLEN, "case %d:", i);
                        line(buf);
                        snprintf(buf, BUFLEN, "    return \"%s\";", type_value[ix->first][i].c_str());
                        line(buf);
                    }
                    block_end();
                block_end();
            }
        }
        block_end();
        line("return NULL;");
    block_end();
    line();


}

bool dve_compiler::eq_expr(dve_expression_t* e1, dve_expression_t* e2) {
    bool result = false;
    const char* s1 = strdup(cexpr(*e1, "(..)").c_str());
    const char* s2 = strdup(cexpr(*e2, "(..)").c_str());
    result = strcmp(s1, s2)==0;
    free((void*)s1);
    free((void*)s2);
    return result;
}

int dve_compiler::add_guard_pc(std::vector<guard> &guard, divine::size_int_t gid, divine::size_int_t lid) {
    // look for program counter guard
    for(int i=0; i < guard.size(); i++) {
        if (guard[i].type == GUARD_PC) {
            if (guard[i].pc.gid == gid && guard[i].pc.lid == lid) {
                return i;
            }
        }
    }

    // not found, add this one as new
    struct guard g;
    g.type = GUARD_PC;
    g.pc.gid = gid;
    g.pc.lid = lid;

    guard.push_back(g);
    return guard.size() - 1;
}

int dve_compiler::add_guard_expr(std::vector<guard> &guard, dve_expression_t* expr) {
    // look for expression guard
    for(int i=0; i < guard.size(); i++) {
        if (guard[i].type == GUARD_EXPR) {
            if (eq_expr(expr, guard[i].expr.guard)) {
                return i;
            }
        }
    }

    // not found, add this one as new
    struct guard g;
    g.type = GUARD_EXPR;
    g.expr.guard = expr;

    guard.push_back(g);
    return guard.size() - 1;
}

int dve_compiler::add_guard_chan(std::vector<guard> &guard, divine::size_int_t chan, divine::sync_mode_t sync_mode) {
    // look for expression guard
    for(int i=0; i < guard.size(); i++) {
        if (guard[i].type == GUARD_CHAN) {
            if (guard[i].chan.chan == chan && guard[i].chan.sync_mode == sync_mode) {
                return i;
            }
        }
    }

    // not found, add this one as new
    struct guard g;
    g.type = GUARD_CHAN;
    g.chan.chan = chan;
    g.chan.sync_mode = sync_mode;

    guard.push_back(g);
    return guard.size() - 1;
}

void dve_compiler::fill_transition_vector(std::vector<ext_transition_t>& transitions)
{
    // true if committed states are found
    bool some_commited_state = false;

    // committed transitions
    for(size_int_t i = 0; i < this->get_process_count(); i++)
    {
        if( transition_map.find(i) != transition_map.end() && !is_property( i ) )
            for(iter_process_transition_map = transition_map.find(i)->second.begin();
                iter_process_transition_map != transition_map.find(i)->second.end();
                iter_process_transition_map++)
            {
                if(dynamic_cast<dve_process_t*>(get_process(i))->get_commited(
                       iter_process_transition_map->first))
                {
                    for(iter_ext_transition_vector = iter_process_transition_map->second.begin();
                        iter_ext_transition_vector != iter_process_transition_map->second.end();
                        iter_ext_transition_vector++)
                    {
                        // !! jak je to s property synchronizaci v comitted stavech !!
                        if( !iter_ext_transition_vector->synchronized ||
                            dynamic_cast<dve_process_t*>(
                                get_process(iter_ext_transition_vector->second->get_process_gid()))->
                            get_commited(iter_ext_transition_vector->second->get_state1_lid()) )
                        {
                            // store transition in map
                            ext_transition_t tmp = *iter_ext_transition_vector;
                            tmp.commited = 1;
                            tmp.buchi = 0;
                            transitions.push_back(tmp);
                            trans_count++;
                        }
                    }
                }
            }
    }

    // normal transitions
    for(size_int_t i = 0; i < get_process_count(); i++)
    {
        if(transition_map.find(i) != transition_map.end() && !is_property( i ))
            for(iter_process_transition_map = transition_map.find(i)->second.begin();
                iter_process_transition_map != transition_map.find(i)->second.end();
                iter_process_transition_map++)
            {
                for(iter_ext_transition_vector = iter_process_transition_map->second.begin();
                    iter_ext_transition_vector != iter_process_transition_map->second.end();
                    iter_ext_transition_vector++)
                {
                    // make sure this transition is not a committed one
                    if (!
                        dynamic_cast<dve_process_t*>(
                            get_process(iter_ext_transition_vector->first->get_process_gid()))->
                        get_commited(iter_ext_transition_vector->first->get_state1_lid()) )
                    {
                        ext_transition_t tmp = *iter_ext_transition_vector;
                        tmp.commited = 0;
                        tmp.buchi = 0;
                        transitions.push_back(tmp);
                        trans_count++;
                    }
                }
            }
    }

    if (have_property && !ltsmin_ltl) {
        for(iter_property_transitions = property_transitions.begin();
            iter_property_transitions != property_transitions.end();
            iter_property_transitions++)
        {
            ext_transition_t ext_transition;
            ext_transition.synchronized = false;
            ext_transition.first = *iter_property_transitions;
            ext_transition.property = *iter_property_transitions;
            ext_transition.buchi = 1;
            analyse_transition_dependencies(ext_transition);
            transitions.push_back(ext_transition);
            trans_count++;
        }
    }
}

bool dve_compiler::split_conjunctive_expression(std::vector<guard>& guard, dve_expression_t* expr)
{
    dve_symbol_table_t * parent_table = expr->get_symbol_table();
    if (!parent_table) gerr << "Splitting expression: Symbol table not set" << thr();
    switch (expr->get_operator())
    {
        case T_PARENTHESIS:
            return split_conjunctive_expression(guard, expr->left());
        case T_BOOL_AND:
            if (!split_conjunctive_expression(guard, expr->left())) {
                // add left guard
                struct guard g;
                g.type = GUARD_EXPR;
                g.expr.guard = expr->left();
                guard.push_back(g);
            }
            if (!split_conjunctive_expression(guard, expr->right())) {
                // add right guard
                struct guard g;
                g.type = GUARD_EXPR;
                g.expr.guard = expr->right();
                guard.push_back(g);
            }
            return true;
        default:
            return false;
    }
}

void dve_compiler::merge_dependent_expression(std::vector<guard>& guard, int sv_count)
{
    std::vector<struct guard> result;
    // mark state vector dependent indices for each guard
    std::vector< std::vector<int> > guard_matrix(guard.size());
    for(int i=0; i < guard.size(); i++) {
        ext_transition_t et;
        et.sv_read.resize(sv_count);
        et.sv_may_write.resize(sv_count);
        et.sv_must_write.resize(sv_count);
        et.sv_actions_read.resize(sv_count);
        analyse_expression(*guard[i].expr.guard, et, et.sv_read);
        guard_matrix[i] = et.sv_read;
    }

    // merge guards in reverse order
    // this preserves the original order of the guards, thus (i < 5) && a[i] will
    // be merged in the correct order
    for(int i=guard.size()-1; i >=0; i--) {
        // check all guards before this one
        int dep = -1;
        for(int j=i-1; j>=0 && dep == -1; j--) {
            dep = -1;
            for(int k=0; k < sv_count; k++) {
                if (guard_matrix[i][k] == 1 && guard_matrix[j][k] == 1) {
                    dep = j;
                    break;
                }
            }
        }
        // none of the guards are dependent, push out
        if (dep == -1) {
            guard_matrix.pop_back();
            result.push_back(guard[i]);
        // else merge the to
        } else {
            // merge guard
            for(int k=0; k < sv_count; k++) {
                guard_matrix[dep][k] = max(guard_matrix[dep][k], guard_matrix[i][k]);
            }
            guard_matrix.pop_back();
            // merge expression
            struct guard g = guard[dep];
            g.expr.guard =
                new dve_expression_t(T_BOOL_AND, *guard[dep].expr.guard, *guard[i].expr.guard,
                                    dynamic_cast<dve_system_t*>(guard[dep].expr.guard->get_parent_system()) );

            guard[dep] = g;
            guard.pop_back();
        }
    }
    guard.swap(result);
}

class mystreambuf: public std::streambuf {};

static void report (ostream &log, string name, size_int_t count, size_int_t total) {
    double percent = ((double)count) / total * 100;
    log << name << ": " << setw(8) << count << " / " << setw(8) << total <<
            " (" << fixed << setprecision(1) << setw(5) << percent << "%)" <<
            endl;
}

void dve_compiler::gen_transition_info()
{
    mystreambuf nostreambuf;
    std::ostream nocout(&nostreambuf);
    std::ostream &log = m_verbose ? cout : nocout;


    int sv_count = count_state_variables();
    bool has_commited = false;
    char buf[BUFLEN];

    // initialize read/write dependency vector
    std::vector<int> c_base_sv_read(sv_count);
    std::vector<int> c_base_sv_actions_read(sv_count);
    std::vector<ext_transition_t> transitions;
    fill_transition_vector(transitions);

    // mark commited processes as read
    for(size_int_t i = 0; i < get_process_count(); i++)
        for(size_int_t j = 0; j < dynamic_cast<dve_process_t*>(get_process(i))->get_state_count(); j++)
            if(dynamic_cast<dve_process_t*>(get_process(i))->get_commited(j)) {
                has_commited = true;
                mark_dependency(dynamic_cast<dve_process_t*>(get_process(i))->get_gid(),
                                state_creator_t::PROCESS_STATE, -1, c_base_sv_read);
                mark_dependency(dynamic_cast<dve_process_t*>(get_process(i))->get_gid(),
                                state_creator_t::PROCESS_STATE, -1, c_base_sv_actions_read);
            }


    /////////////////////////////////
    // SPLIT THE GUARD EXPRESSIONS //
    /////////////////////////////////

    // transition -> set of guards
    std::map<int, std::set<int> > guards;
    // set of guards
    std::vector<guard> guard;

    // reserve two guards for committed/not committed
    if (has_commited) {
        struct guard g;
        g.type = GUARD_COMMITED_FIRST;
        guard.push_back(g);
    }

    for(size_int_t i = 0; i < transitions.size(); i++) {
        ext_transition_t& current = transitions[i];

        // reference to the set of guards for transition i
        set<int>& gs = guards[i];
        int g;

        // push the local state as guard
        g = add_guard_pc(guard, current.first->get_process_gid(), current.first->get_state1_lid());
        gs.insert(g);

        // push first guard as a whole
        if (current.first->get_guard()) {
            // try splitting the first guard
            std::vector<struct guard> gtmp;
            if (split_conjunctive_expression(gtmp, current.first->get_guard())) {
                // merge dependent guards again
                merge_dependent_expression(gtmp, sv_count);
                // add all split guards
                for(int j=0; j<gtmp.size(); j++) {
                    g = add_guard_expr(guard, gtmp[j].expr.guard);
                    gs.insert(g);
                }
            } else {
                g = add_guard_expr(guard, current.first->get_guard());
                gs.insert(g);
            }
        }

        // check synchronized
        if ( current.synchronized ) {
            g = add_guard_pc(guard, current.second->get_process_gid(), current.second->get_state1_lid());
            gs.insert(g);
            if (current.second->get_guard()) {
                // try splitting the second guard
                std::vector<struct guard> gtmp;
                if (split_conjunctive_expression(gtmp, current.second->get_guard())) {
                    // merge dependent guards again
                    merge_dependent_expression(gtmp, sv_count);
                    // add all split guards
                    for(int j=0; j<gtmp.size(); j++) {
                        g = add_guard_expr(guard, gtmp[j].expr.guard);
                        gs.insert(g);
                    }
                } else {
                    g = add_guard_expr(guard, current.second->get_guard());
                    gs.insert(g);
                }
            }
        } else {
            // synchronized on channel?
            int chan = current.first->get_channel_gid();
            if (current.first->get_sync_mode() == SYNC_EXCLAIM_BUFFER ||
                current.first->get_sync_mode() == SYNC_ASK_BUFFER) {
                g = add_guard_chan(guard, chan, current.first->get_sync_mode());
                gs.insert(g);
            }
        }

        // committed?
        if (has_commited) {
            if (!current.commited) {
                gs.insert(0);
            }
        }
    }

    // extract dependency matrix per guard expression
    std::vector< std::vector<int> > guard_matrix;

    guard_matrix.resize(guard.size());
    for(int i=0; i<guard.size(); i++) {
        std::vector<int> &per_guard_matrix = guard_matrix[i];
    	 // clear per guard matrix
		per_guard_matrix.resize(sv_count);
		per_guard_matrix.clear();
		switch(guard[i].type) {
			case GUARD_PC:
				mark_dependency(guard[i].pc.gid,
								state_creator_t::PROCESS_STATE, -1, per_guard_matrix);
				break;
			case GUARD_EXPR: {
				// use ext_transition dummy to store read/write vector
				ext_transition_t et;
				et.sv_read.resize(sv_count);
				et.sv_may_write.resize(sv_count);
				et.sv_must_write.resize(sv_count);
				analyse_expression(*guard[i].expr.guard, et, et.sv_read);
				per_guard_matrix = et.sv_read;
				} break;
			case GUARD_CHAN:
				mark_dependency(guard[i].chan.chan,
								state_creator_t::CHANNEL_BUFFER, -1, per_guard_matrix);
				break;
			case GUARD_COMMITED_FIRST:
				for(size_int_t p = 0; p < get_process_count(); p++)
					for(size_int_t c = 0; c < dynamic_cast<dve_process_t*>(get_process(p))->get_state_count(); c++)
						if(dynamic_cast<dve_process_t*>(get_process(p))->get_commited(c))
							mark_dependency(p, state_creator_t::PROCESS_STATE, -1, per_guard_matrix);
				break;
			default:
				for(int j=0; j<sv_count; j++) per_guard_matrix[j]=1;
				break;
		}
    }

    // buchi transits on deadlocks, hence also depends on all other guards!
    for(size_int_t i = 0; i < transitions.size(); i++) {
        ext_transition_t& current = transitions[i];
        set<int>& gs = guards[i];
        if (current.buchi)
        for(size_int_t j = 0; j < transitions.size(); j++) {
            ext_transition_t& current2 = transitions[j];
            set<int>& gs2 = guards[j];
            if (!current2.buchi) {
            	gs.insert(gs2.begin(), gs2.end());
            }
        }
    }

    // compute transition coenabledness
   std::vector< std::vector<bool> > transition_coenabled(transitions.size());

    for(size_int_t i=0; i < transitions.size(); i++) {
        set<int> &gs = guards[i];
        std::vector<bool> tmp(transitions.size());
        transition_coenabled[i] = tmp;

        // for each guard of transition i
        for(set<int>::iterator ix = gs.begin(); ix != gs.end(); ix++) {
            // check coenabledness with each guard of transition j
            for(size_int_t j=0; j < transitions.size(); j++) {
                bool coenabled = true;
                set<int> &gs2 = guards[j];
                for(set<int>::iterator iy = gs2.begin(); iy != gs2.end(); iy++) {
                     coenabled &= may_be_coenabled(guard[*ix], guard[*iy]);
                }
                transition_coenabled[i][j] = coenabled;
            }
        }
    }

    // compute nes guard -> set of transitions
    std::vector< std::vector<bool> > guard_nes(guard.size());
    for(size_int_t i=0; i < guard.size(); i++) {
        std::vector<bool> tmp(transitions.size());
        guard_nes[i] = tmp;
        std::vector<bool> &per_guard_nes = guard_nes[i];
        for(size_int_t j=0; j < transitions.size(); j++) {
            per_guard_nes[j] = is_guard_nes(guard[i], transitions[j]);
        }
    }

    // compute nds guard -> set of transitions
    std::vector< std::vector<bool> > guard_nds(guard.size());
    for(size_int_t i=0; i < guard.size(); i++) {
        std::vector<bool> tmp(transitions.size());
        guard_nds[i] = tmp;
        std::vector<bool> &per_guard_nds = guard_nds[i];
        for(size_int_t j=0; j < transitions.size(); j++) {
            per_guard_nds[j] = is_guard_nds(guard[i], transitions[j]);
        }
    }

    // compute nds of transitions x transition
    std::vector< std::vector<bool> > transition_nds(transitions.size());
    for(size_int_t i=0; i < transitions.size(); i++) {
        set<int> &gs = guards[i];
        std::vector<bool> tmp(transitions.size());
        transition_nds[i] = tmp;
        // for each guard of transition i, mark the disabling transitions as disabling..
        for(set<int>::iterator ix = gs.begin(); ix != gs.end(); ix++) {
            for(size_int_t j=0; j < transitions.size(); j++) {
                transition_nds[i][j] = guard_nds[*ix][j];
            }
        }
    }

    // compute variable set of transition
    std::vector< std::vector<bool> > transition_variable_set(transitions.size());
    // compute read set of transition
    std::vector< std::vector<bool> > transition_read_set(transitions.size());
    // compute actions read set of transition
    std::vector< std::vector<bool> > transition_actions_read_set(transitions.size());
    // compute write set of transition
    std::vector< std::vector<bool> > transition_write_set(transitions.size());
    // compute sets
    for(size_int_t i = 0; i < transitions.size(); i++) {
        ext_transition_t& current = transitions[i];


        std::vector<bool> tmp1(sv_count);
        transition_variable_set[i] = tmp1;
        std::vector<bool> tmp2(sv_count);
        transition_read_set[i] = tmp2;
        std::vector<bool> tmp3(sv_count);
        transition_write_set[i] = tmp3;
        std::vector<bool> tmp4(sv_count);
        transition_actions_read_set[i] = tmp4;

        for(size_int_t j = 0; j < sv_count; j++)
        {
            int dep = current.commited?current.sv_read[j]:max(c_base_sv_read[j], current.sv_read[j]);
            if (!dep && current.buchi) { // use guard info
                set<int> &gs = guards[i];
                for(set<int>::iterator ix = gs.begin(); ix != gs.end(); ix++) {
                    std::vector<int> &per_guard_matrix = guard_matrix[*ix];
                    if (per_guard_matrix[j]) {
                        dep = 1;
                        break;
                    }
                }
            }
            transition_variable_set[i][j] =  dep || current.sv_may_write[j];
            transition_read_set[i][j] =  dep;
            transition_write_set[i][j] =  current.sv_may_write[j];
            transition_actions_read_set[i][j] = current.commited?current.sv_actions_read[j]:max(c_base_sv_actions_read[j], current.sv_actions_read[j]);            
            
        }
    }

    // output transition vectors
    snprintf(buf, BUFLEN, "int transition_dependency[][3][%d] = ", sv_count);
    line(buf);
    block_begin();
    line("// { ... read ...}, { ... may-write ...}, { ... must-write ...}");

    for(size_int_t i = 0; i < transitions.size(); i++) {
        ext_transition_t& current = transitions[i];
        if (i!=0) { line(","); }
        append("{{" );
        for(size_int_t j = 0; j < sv_count; j++)
        {
			snprintf(buf, BUFLEN, "%s%d", ((j==0)?"":","), transition_read_set[i][j]?1:0 );
			append(buf);
        }
        append("},{" );
        for(size_int_t j = 0; j < sv_count; j++)
        {
            snprintf(buf, BUFLEN, "%s%d", ((j==0)?"":","), current.sv_may_write[j] );
            append(buf);
        }
        append("},{" );
        for(size_int_t j = 0; j < sv_count; j++)
        {
            snprintf(buf, BUFLEN, "%s%d", ((j==0)?"":","), current.sv_must_write[j] );
            append(buf);
        }
        append("}}");
    }
    line();
    block_end();
    line(";");
    line();

    // output action reads vectors
    snprintf(buf, BUFLEN, "int actions_read[][%d] = ", sv_count);
    line(buf);
    block_begin();

    for(size_int_t i = 0; i < transitions.size(); i++) {
        ext_transition_t& current = transitions[i];
        if (i!=0) { line(","); }
        append("{" );
        for(size_int_t j = 0; j < sv_count; j++)
        {
			snprintf(buf, BUFLEN, "%s%d", ((j==0)?"":","), transition_actions_read_set[i][j]?1:0 );
			append(buf);
        }
        append("}" );
    }
    line();
    block_end();
    line(";");
    line();   
        
    // number of transitions
    line( "extern \"C\" int get_transition_count() " );
    block_begin();
    snprintf(buf, BUFLEN, "return %zu;", transitions.size());
    line(buf);
    block_end();
    line();

    // read dependencies
    line( "extern \"C\" const int* get_transition_read_dependencies(int t) " );
    block_begin();
    snprintf(buf, BUFLEN, "if (t>=0 && t < %zu) return transition_dependency[t][0];", transitions.size());
    line(buf);
    snprintf(buf, BUFLEN, "return NULL;");
    line(buf);
    block_end();
    line();

    // actions read dependencies
    line( "extern \"C\" const int* get_transition_actions_read_dependencies(int t) " );
    block_begin();
    snprintf(buf, BUFLEN, "if (t>=0 && t < %zu) return actions_read[t];", transitions.size());
    line(buf);
    snprintf(buf, BUFLEN, "return NULL;");
    line(buf);
    block_end();
    line();

    // may_write dependencies
    line( "extern \"C\" const int* get_transition_may_write_dependencies(int t) " );
    block_begin();
    snprintf(buf, BUFLEN, "if (t>=0 && t < %zu) return transition_dependency[t][1];", transitions.size());
    line(buf);
    snprintf(buf, BUFLEN, "return NULL;");
    line(buf);
    block_end();
    line();

    // must_write dependencies
    line( "extern \"C\" const int* get_transition_must_write_dependencies(int t) " );
    block_begin();
    snprintf(buf, BUFLEN, "if (t>=0 && t < %zu) return transition_dependency[t][2];", transitions.size());
    line(buf);
    snprintf(buf, BUFLEN, "return NULL;");
    line(buf);
    block_end();
    line();

    /////////////////////////////////////
    /////////////////////////////////////

    // write get_active
    line( "extern \"C\" int get_active( state_struct_t *in, int t ) " );
    block_begin();
    line("switch(t)");
    block_begin();
    for(size_int_t i = 0; i < transitions.size(); i++) {
        ext_transition_t& current = transitions[i];
        if (current.synchronized) {
            snprintf(buf, BUFLEN, "case %zu: return ((%s) && (%s));", i,
                in_state(current.first->get_process_gid(), current.first->get_state1_lid(), "(*in)").c_str(),
                in_state(current.second->get_process_gid(), current.second->get_state1_lid(), "(*in)").c_str());
            line(buf);
        } else {
            snprintf(buf, BUFLEN, "case %zu: return (%s);", i, in_state(current.first->get_process_gid(), current.first->get_state1_lid(), "(*in)").c_str());
            line(buf);
        }
    }
    block_end();
    line("return false;");
    block_end();
    line();

    /////////////////////////////////////
    /////////////////////////////////////

    // write get_group_pid (process id)
    // note: just for testing conflict reduction strategy
    line( "extern \"C\" void get_group_pid_lid( int t, int* pid0, int* lid0, int* pid1, int* lid1 ) " );
    block_begin();
    line("switch(t)");
    block_begin();
    for(size_int_t i = 0; i < transitions.size(); i++) {
        ext_transition_t& current = transitions[i];
        if (current.synchronized) {
            snprintf(buf, BUFLEN, "case %zu: *pid0 = %zu; *lid0=%zu; *pid1 = %zu; *lid1 = %zu; return;", i,
                current.first->get_process_gid(), current.first->get_state1_lid(),
                current.second->get_process_gid(), current.second->get_state1_lid());
            line(buf);
        } else {
            snprintf(buf, BUFLEN, "case %zu: *pid0 = %zu; *lid0 = %zu; *pid1 = -1; *lid1 = -1; return;", i, current.first->get_process_gid(), current.first->get_state1_lid());
            line(buf);
        }
    }
    block_end();
    line("*pid0 = *lid0 = -1;");
    line("*pid1 = *lid0 = -1;");
    line("return;");
    block_end();
    line();

    // export the guard value for this state
    line ("extern \"C\" int get_guard(void* model, int g, state_struct_t* src) " );
    block_begin();
    line ("(void)model;");
    line("switch(g)");
    block_begin();
        for(int i=0; i<guard.size(); i++) {
            switch(guard[i].type) {
                case GUARD_PC:
                    snprintf(buf, BUFLEN, "case %d: return (%s);", i, in_state(guard[i].pc.gid, guard[i].pc.lid, "(*src)").c_str());
                    line(buf);
                    break;
                case GUARD_EXPR:
                    snprintf(buf, BUFLEN, "case %d: return (%s);", i, cexpr(*guard[i].expr.guard,"(*src)").c_str());
                    line(buf);
                    break;
                case GUARD_CHAN:
                    snprintf(buf, BUFLEN, "case %d: return (%s);", i, relate( channel_items(guard[i].chan.chan, "(*src)"), "!=",
                        (guard[i].chan.sync_mode == SYNC_EXCLAIM_BUFFER? fmt( channel_capacity( guard[i].chan.chan ) ) : "0" ) ).c_str());
                    line(buf);
                    break;
                case GUARD_COMMITED_FIRST:
                    snprintf(buf, BUFLEN, "case %d:", i);
                    line(buf);
                    // committed state
                    block_begin();
                    if_begin( true );

                    for(size_int_t p = 0; p < get_process_count(); p++)
                        for(size_int_t c = 0; c < dynamic_cast<dve_process_t*>(get_process(p))->get_state_count(); c++)
                            if(dynamic_cast<dve_process_t*>(get_process(p))->get_commited(c))
                                if_clause( in_state( p, c, "(*src)" ) );

                    if_end();
                    line("    return 0;"); // bail out early
                    line("return 1;");
                    block_end();
                    break;
            }
        }
    block_end();
    snprintf(buf, BUFLEN, "return false;");
    line(buf);
    block_end();
    line();

    // export the guard value for this state
    line ("extern \"C\" void get_guard_all(void* model, state_struct_t* src, int* guard) " );
    block_begin();
    line ("(void)model;");
    for(int i=0; i<guard.size(); i++) {
        switch(guard[i].type) {
            case GUARD_PC:
                snprintf(buf, BUFLEN, "guard[%d] = (%s);", i, in_state(guard[i].pc.gid, guard[i].pc.lid, "(*src)").c_str());
                line(buf);
                break;
            case GUARD_EXPR:
                snprintf(buf, BUFLEN, "guard[%d] = (%s);", i, cexpr(*guard[i].expr.guard,"(*src)").c_str());
                line(buf);
                break;
            case GUARD_CHAN:
                snprintf(buf, BUFLEN, "guard[%d] = (%s);", i, relate( channel_items(guard[i].chan.chan, "(*src)"), "!=",
                    (guard[i].chan.sync_mode == SYNC_EXCLAIM_BUFFER? fmt( channel_capacity( guard[i].chan.chan ) ) : "0" ) ).c_str());
                line(buf);
                break;
            case GUARD_COMMITED_FIRST:
                // committed state
                block_begin();
                if_begin( true );

                for(size_int_t p = 0; p < get_process_count(); p++)
                    for(size_int_t c = 0; c < dynamic_cast<dve_process_t*>(get_process(p))->get_state_count(); c++)
                        if(dynamic_cast<dve_process_t*>(get_process(p))->get_commited(c))
                            if_clause( in_state( p, c, "(*src)" ) );

                if_end();

                snprintf(buf, BUFLEN, "    guard[%d] = 0;", i);
                line(buf);
                snprintf(buf, BUFLEN, "guard[%d] = 1;", i);
                line(buf);
                block_end();
                break;
        }
    }
    block_end();
    line();

    // export the number of guards
    line ("extern \"C\" const int get_guard_count() " );
    block_begin();
    snprintf(buf, BUFLEN, "return %zu;", guard.size());
    line(buf);
    block_end();
    line();

    snprintf(buf, BUFLEN, "int* guards_per_transition[%zu] = ", transitions.size() );
    line(buf);
    block_begin();
        for(int i=0; i < transitions.size(); i++) {
            set<int>& gs = guards[i];

            snprintf(buf, BUFLEN, "((int[]){");
            append(buf);
            snprintf(buf, BUFLEN, "%zu", gs.size());
            append(buf);
            for(set<int>::iterator ix = gs.begin(); ix != gs.end(); ix++) {
                snprintf(buf, BUFLEN, ", %d", *ix);
                append(buf);
            }
            line("}),");
        }
    block_end();
    line(";");
    line();

    // export the guards per transition group
    line ("extern \"C\" const int* get_guards(int t) " );
    block_begin();
    snprintf(buf, BUFLEN, "if (t>=0 && t < %zu) return guards_per_transition[t];", transitions.size());
    line(buf);
    line("return NULL;");
    block_end();
    line();

    line ("extern \"C\" const int** get_all_guards() " );
    block_begin();
    line("return (const int**)&guards_per_transition;");
    block_end();
    line();

    /////////////////////////////////
    // EXPORT THE GUARD MATRIX     //
    /////////////////////////////////

    snprintf(buf, BUFLEN, "int guard[][%d] = ", sv_count);
    line(buf);
    block_begin();
        for(int i=0; i<guard.size(); i++) {
            std::vector<int> &per_guard_matrix = guard_matrix[i];
            if (i != 0) { line(","); }
            append("{" );
            // guard
            for(size_int_t i = 0; i < sv_count; i++)
            {
                snprintf(buf, BUFLEN, "%s%d", ((i==0)?"":","), per_guard_matrix[i]);
                append(buf);
            }
            append("}" );
        }
        line();
    block_end();
    line(";");
    line();

    // export the guard matrix
    line ("extern \"C\" const int* get_guard_matrix(int g) " );
    block_begin();
    snprintf(buf, BUFLEN, "if (g>=0 && g < %zu) return guard[g];", guard.size());
    line(buf);
    snprintf(buf, BUFLEN, "return NULL;");
    line(buf);
    block_end();
    line();

    log << "Groups: " << transitions.size()  << endl;
    log << "Guards: " << guard.size() << endl;
    log << "--------------" << endl;
    size_int_t count = 0;

    //////////////////////////////////////////
    // EXPORT GUARD MAY BE COENABLED MATRIX //
    //////////////////////////////////////////

    // guard may be co-enabled matrix (#guards x #guards)
    snprintf(buf, BUFLEN, "int guardmaybecoenabled[%zu][%zu] = ", guard.size(), guard.size());
    line(buf);
    block_begin();
    for(size_int_t i=0; i < guard.size(); i++) {
        append("{");
        for(size_int_t j=0; j < guard.size(); j++) {
        if (j !=0) append(", ");
            if (may_be_coenabled(guard[i], guard[j])) {
                append("1");
            } else {
                append("0");
                count++;
            }
        }
        if (i == guard.size() - 1)
            line("}");
        else
            line("},");
    }
    report(log, "!MCE", count, guard.size()*guard.size());

    block_end();
    line(";");
    line();


    // may be co-enabled function
    line ("extern \"C\" const int* get_guard_may_be_coenabled_matrix(int g) " );
    block_begin();
    snprintf(buf, BUFLEN, "if (g>=0 && g < %zu) return guardmaybecoenabled[g];", guard.size());
    line(buf);
    snprintf(buf, BUFLEN, "return NULL;");
    line(buf);
    block_end();
    line();

    ///////////////////////////////////////////////
    // EXPORT NECESSARY ENABLING SETS FOR GUARDS //
    ///////////////////////////////////////////////

    // guard nes matrix (#guards x #transitions)
    snprintf(buf, BUFLEN, "int guard_nes[%zu][%zu] = ", guard.size(), transitions.size());
    line(buf);
    block_begin();
    count = 0;
    for(size_int_t i=0; i < guard.size(); i++) {
        append("{");
        for(size_int_t j=0; j < transitions.size(); j++) {
            if (j != 0) append(", ");
            append(guard_nes[i][j]?"1":"0");
            count += guard_nes[i][j] ? 1 : 0;
        }
        if (i == guard.size() - 1)
            line("}");
        else
            line("},");
    }
    report(log, "!NES", count, guard.size()*transitions.size());

    block_end();
    line(";");
    line();

    // guard nes function
    line ("extern \"C\" const int* get_guard_nes_matrix(int g) " );
    block_begin();
    snprintf(buf, BUFLEN, "if (g>=0 && g < %zu) return guard_nes[g];", guard.size());
    line(buf);
    snprintf(buf, BUFLEN, "return NULL;");
    line(buf);
    block_end();
    line();

    ////////////////////////////////////////////////
    // EXPORT NECESSARY DISABLING SETS FOR GUARDS //
    ////////////////////////////////////////////////

    // guard nes matrix (#guards x #transitions)
    snprintf(buf, BUFLEN, "int guard_nds[%zu][%zu] = ", guard.size(), transitions.size());
    line(buf);
    block_begin();
    count = 0;
    for(size_int_t i=0; i < guard.size(); i++) {
        append("{");
        for(size_int_t j=0; j < transitions.size(); j++) {
            if (j != 0) append(", ");
            append( guard_nds[i][j] ?"1":"0");
            count += !guard_nds[i][j] ? 1 : 0;
        }
        if (i == guard.size() - 1)
            line("}");
        else
            line("},");
    }
    report(log, "!NDS", count, guard.size()*transitions.size());

    block_end();
    line(";");
    line();

    // guard nes function
    line ("extern \"C\" const int* get_guard_nds_matrix(int g) " );
    block_begin();
    snprintf(buf, BUFLEN, "if (g>=0 && g < %zu) return guard_nds[g];", guard.size());
    line(buf);
    snprintf(buf, BUFLEN, "return NULL;");
    line(buf);
    block_end();
    line();


    ////////////////////////////////////////////////
    // EXPORT DO NOT ACCORD MATRIX                //
    ////////////////////////////////////////////////

    // guard do not accord matrix (#transitions x #transitions)
    snprintf(buf, BUFLEN, "int dna[%zu][%zu] = ", transitions.size(), transitions.size());
    line(buf);
    block_begin();
    count = 0;
    for(size_int_t i=0; i < transitions.size(); i++) {
        append("{");
        for(size_int_t j=0; j < transitions.size(); j++) {
            if (j != 0) append(", ");
            bool dna = is_dna(transitions, transition_coenabled, transition_nds, transition_write_set, i, j);
            append(dna ? "1" : "0");
            count += !dna ? 1 : 0;
        }
        if (i == transitions.size() - 1)
            line("}");
        else
            line("},");
    }
    report(log, "!DNA", count, transitions.size()*transitions.size());

    block_end();
    line(";");
    line();

    // guard dna function
    line ("extern \"C\" const int* get_dna_matrix(int t) " );
    block_begin();
    snprintf(buf, BUFLEN, "if (t >= 0 && t < %zu) return dna[t];", transitions.size());
    line(buf);
    snprintf(buf, BUFLEN, "return NULL;");
    line(buf);
    block_end();
    line();
}

bool
dve_compiler::is_guard_nes( guard& g, ext_transition_t& t )
{
    switch (g.type) {
        case GUARD_PC:
            if (g.pc.gid == t.first->get_process_gid())
                return (g.pc.lid == t.first->get_state2_lid());
            if (t.synchronized && g.pc.gid == t.second->get_process_gid())
                return (g.pc.lid == t.second->get_state2_lid());
            return false;
        case GUARD_CHAN:
            if (!t.synchronized && g.chan.chan == t.first->get_channel_gid()) {
                if (t.first->get_sync_mode() == SYNC_EXCLAIM_BUFFER ||
                    t.first->get_sync_mode() == SYNC_ASK_BUFFER) {
                    return g.chan.sync_mode != t.first->get_sync_mode();
                }
            }
            return false;
        case GUARD_COMMITED_FIRST:
            return ( dynamic_cast<dve_process_t*>(get_process(t.first->get_process_gid()))->get_commited(t.first->get_state1_lid()) &&
                    !dynamic_cast<dve_process_t*>(get_process(t.first->get_process_gid()))->get_commited(t.first->get_state2_lid()));
        default: return true; // dont know.
    }
    return true;
}

bool
dve_compiler::is_guard_nds( guard& g, ext_transition_t& t )
{
    switch (g.type) {
        case GUARD_PC:
            if (g.pc.gid == t.first->get_process_gid())
                return (g.pc.lid == t.first->get_state1_lid() && g.pc.lid != t.first->get_state2_lid());
            if (t.synchronized && g.pc.gid == t.second->get_process_gid())
                return (g.pc.lid == t.second->get_state1_lid() && g.pc.lid != t.second->get_state2_lid());
            return false;
        case GUARD_CHAN:
            if (!t.synchronized && g.chan.chan == t.first->get_channel_gid()) {
                if (t.first->get_sync_mode() == SYNC_EXCLAIM_BUFFER ||
                    t.first->get_sync_mode() == SYNC_ASK_BUFFER) {
                    return g.chan.sync_mode == t.first->get_sync_mode();
                }
            }
            return false;
        /*
        case GUARD_COMMITED_FIRST:
            return ( dynamic_cast<dve_process_t*>(get_process(t.first->get_process_gid()))->get_commited(t.first->get_state1_lid()) &&
                   !dynamic_cast<dve_process_t*>(get_process(t.first->get_process_gid()))->get_commited(t.first->get_state2_lid()));
        */
        default: return true;// dont know.
    }
    return true;
}

bool
dve_compiler::is_dna(std::vector<ext_transition_t>& transitions,
        std::vector< std::vector<bool> >& transition_coenabled,
        std::vector< std::vector<bool> >& transition_nds,
        std::vector< std::vector<bool> >& deps,
        int t1, int t2 )
{
    ext_transition_t *trans1 = &transitions[t1];
    ext_transition_t *trans2 = &transitions[t2];

    if (have_property)
        return true; // for LTL models we are not interested in POR anyway (use LTSmin LTL layer)

    if (trans1->commited || trans2->commited)
        return true; // TODO

    // two transitions always accord if they are never coenabled:
    if (!transition_coenabled[t1][t2])
        return false;

    // overappoximation:
    // two transitions do not accord if
    // 1) they disable each other
    if (transition_nds[t1][t2] || transition_nds[t2][t1])
        return true;

    // 2) the common variables of t1 and t2 are a subset of the all the variables in the write sets of t1 and t2
    std::vector<simple_predicate> sps1;
    std::vector<simple_predicate> sps2;
    if ( !extract_assigns(trans1, sps1, deps[t2]) ||
         !extract_assigns(trans2, sps2, deps[t1]) )
        return true;

    for(size_int_t i=0; i < sps1.size(); i++) {
        for(size_int_t j=0; j < sps2.size(); j++) {
            if (!commutes(sps1[i], sps2[j])) {
                return true;
            }
        }
    }

    return false;
}

bool
dve_compiler::commutes ( simple_predicate& p1, simple_predicate& p2 )
{
    if (p1.variable_name != p2.variable_name) {
        return true;
    } else if (p1.relation == PRED_EQ && p2.relation == PRED_EQ) {
        return p1.variable_value == p2.variable_value;
    } else if ( p1.relation == PRED_GT && p2.relation == PRED_GT) {
        return true;
    } else if ( (p1.relation == PRED_GEQ && p2.relation == PRED_LEQ) ||
                (p1.relation == PRED_LEQ && p2.relation == PRED_GEQ)) {
        return true;
    }
    return false;
}

bool
dve_compiler::dependent ( std::vector<int>& a, std::vector<bool>& b )
{
    for (size_int_t j = 0; j < a.size(); j++) {
        if (a[j] && b[j]) return true;
    }
    return false;
}

bool
dve_compiler::extract_assigns ( ext_transition_t *et,
                                std::vector<simple_predicate>& p,
                                std::vector<bool>& deps )
{
    ext_transition_t tmp;
    int count = count_state_variables();
    tmp.sv_read.resize(count);
    tmp.sv_actions_read.resize(count);
    tmp.sv_may_write.resize(count);
    tmp.sv_must_write.resize(count);

    sync_mode_t sm = et->first->get_sync_mode();
    if(et->synchronized)
    {
        return false; // TODO: yields incorrect reductions for at least prod_cell2 and firewire_link2

        // synchronizing communication
        for (size_int_t s = 0;s < et->first->get_sync_expr_list_size();s++) {
            dve_expression_t& left = *et->first->get_sync_expr_list_item(s);
            dve_expression_t& right = *et->second->get_sync_expr_list_item(s);
            tmp.sv_read.clear();
            tmp.sv_actions_read.clear();
            analyse_expression (left, tmp, tmp.sv_read);
            analyse_expression (right, tmp, tmp.sv_read);
            analyse_expression (left, tmp, tmp.sv_actions_read);
            analyse_expression (right, tmp, tmp.sv_actions_read);
            if (!dependent(tmp.sv_read, deps))
                continue;

            simple_predicate sp;
            sp.relation = PRED_EQ;
            if ( !get_const_varname(left, sp.variable_name) ||
                 !get_const_expression(right, sp.variable_value) ) {
                return false;
            }
            p.push_back(sp);
        }

    } else if (sm == SYNC_EXCLAIM_BUFFER || sm == SYNC_ASK_BUFFER) {
        int chan = et->first->get_channel_gid();

        tmp.sv_read.clear();
        tmp.sv_actions_read.clear();
        mark_dependency(et->first->get_channel_gid(),
                        state_creator_t::CHANNEL_BUFFER, -1, tmp.sv_read);
        mark_dependency(et->first->get_channel_gid(),
                        state_creator_t::CHANNEL_BUFFER, -1, tmp.sv_actions_read);
        if (dependent(tmp.sv_read, deps)) {
            simple_predicate sp;
            if (sm == SYNC_EXCLAIM_BUFFER) {
                sp.relation = PRED_GEQ;
            } else if (sm == SYNC_ASK_BUFFER) {
                sp.relation = PRED_LEQ;
            }
            sp.variable_name = channel_items( chan, "out" );
            p.push_back(sp);

        }
    }

    // first transition effect (Program counter)
    simple_predicate sp;
    sp.relation = PRED_EQ;
    sp.variable_name = process_state( et->first->get_process_gid(), "out" );
    sp.variable_value = et->first->get_state2_lid();
    tmp.sv_read.clear();
    tmp.sv_actions_read.clear();
    mark_dependency(et->first->get_process_gid(),
                    state_creator_t::PROCESS_STATE, -1, tmp.sv_read);
    mark_dependency(et->first->get_process_gid(),
                    state_creator_t::PROCESS_STATE, -1, tmp.sv_actions_read);
    if (dependent(tmp.sv_read, deps))
        p.push_back(sp);

    for(size_int_t e = 0;e < et->first->get_effect_count();e++) {
        if (!get_assignment(*et->first->get_effect(e), p, deps)) {
            return false;
        }
    }

    // second transition effect
    if (et->synchronized) {
        simple_predicate sp;
        sp.relation = PRED_EQ;
        sp.variable_name = process_state( et->second->get_process_gid(), "out" );
        sp.variable_value = et->second->get_state2_lid();
        tmp.sv_read.clear();
        tmp.sv_actions_read.clear();
        mark_dependency(et->second->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, tmp.sv_read);
        mark_dependency(et->second->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, tmp.sv_actions_read);
        if (dependent(tmp.sv_read, deps))
            p.push_back(sp);

        for (size_int_t e = 0;e < et->second->get_effect_count();e++) {
            if (!get_assignment(*et->second->get_effect(e), p, deps)) {
                return false;
            }
        }
    }

    // have_property: we skip LTL properties anyway!

    return true;
}

bool
dve_compiler::get_assignment ( dve_expression_t & expr,
                               std::vector<simple_predicate>& p,
                               std::vector<bool>& deps) {
    simple_predicate sp;
    ext_transition_t tmp;
    int count = count_state_variables();
    tmp.sv_read.resize(count);
    tmp.sv_actions_read.resize(count);
    tmp.sv_may_write.resize(count);
    tmp.sv_must_write.resize(count);
    if (expr.get_operator() == T_ASSIGNMENT) {
        if ( !get_const_varname(*expr.left(), sp.variable_name) )
            return false;

        analyse_expression (*expr.left(), tmp, tmp.sv_read);
        analyse_expression (*expr.left(), tmp, tmp.sv_actions_read);
        if (!dependent(tmp.sv_read, deps))
            return true; // no need to add

        if ( get_const_expression(*expr.right(), sp.variable_value) )
        {
            sp.relation = PRED_EQ;
            p.push_back(sp);
            return true;
        }
        else
        {
            string other;
            switch (expr.right()->get_operator()) { // x = x + 1  AND  x = x - 1
            case T_PLUS: case T_MINUS:
                if ( !get_const_varname(*expr.right()->left(), other) )
                    return NULL;
                if (other != sp.variable_name)
                    return NULL;
                if ( !get_const_expression(*expr.right()->right(), sp.variable_value) )
                    return NULL;
                if (sp.variable_value != 1)
                    return NULL;
                sp.relation = PRED_GT; // ++ / --
                p.push_back(sp);
                return true;
            }
        }
    }
    return false;
}

bool
dve_compiler::may_be_coenabled( guard& ga, guard& gb)
{
    // if type different, return default
    if (ga.type == gb.type || ga.type == GUARD_COMMITED_FIRST) {
        switch (ga.type) {
            case GUARD_PC:
                // if one committed, and the other is not, they may not be co-enabled
                if (dynamic_cast<dve_process_t*>(get_process(ga.pc.gid))->get_commited(ga.pc.lid) !=
                    dynamic_cast<dve_process_t*>(get_process(gb.pc.gid))->get_commited(gb.pc.lid))
                    return false;
                // may be co enabled if the gid is different (different process)
                // or if the lid is the same (same process, same local state)
                return (ga.pc.gid != gb.pc.gid || ga.pc.lid == gb.pc.lid);
            case GUARD_EXPR: {
                // difficult static analysis. Give it a try for simple expressions
                std::vector<simple_predicate> ga_sp;
                std::vector<simple_predicate> gb_sp;
                extract_predicates(ga_sp, *ga.expr.guard);
                extract_predicates(gb_sp, *gb.expr.guard);
                for(int i=0; i < ga_sp.size(); i++) {
                    for(int j=0; j < gb_sp.size(); j++) {
                        if (is_conflict_predicate(ga_sp[i], gb_sp[j])) return false;
                    }
                }
                } break;
            case GUARD_CHAN:
                return (ga.chan.chan != gb.chan.chan || ga.chan.sync_mode == gb.chan.sync_mode);
            case GUARD_COMMITED_FIRST:
                // this only works with local states
                if (gb.type == GUARD_PC) {
                    // all non-committed local states may not be co-enabled with this guard
                    return (dynamic_cast<dve_process_t*>(get_process(gb.pc.gid))->get_commited(gb.pc.lid));
                }
                break;
        }
    } else {
        // if gb is GUARD COMMITED FIRST, reverse the argument order
        if (gb.type == GUARD_COMMITED_FIRST) return may_be_coenabled(gb, ga);
    }
    // default
    return true;
}

bool
dve_compiler::get_const_expression( dve_expression_t & expr, int & value)
{
    dve_symbol_table_t * parent_table = expr.get_symbol_table();
    if (!parent_table) gerr << "Get const expression: Symbol table not set" << thr();
    switch (expr.get_operator())
    {
        case T_NAT:
            value = (int)expr.get_value();
            return true;
        case T_PARENTHESIS:
            return get_const_expression(*expr.left(), value);
        case T_UNARY_MINUS:
            if ( get_const_expression(*expr.right(), value) ) {
                value = -value;
                return true;
            }
        default:
            value = 0;
    }
    return false;
}

bool dve_compiler::get_const_varname( dve_expression_t & expr, string& var)
{
    string var_square_bracket;
    dve_symbol_table_t * parent_table = expr.get_symbol_table();
    if (!parent_table) gerr << "Get const var: Symbol table not set" << thr();
    switch (expr.get_operator())
    {
        case T_SQUARE_BRACKETS:
            int val;
            if (get_const_expression(*expr.left(), val)) {
                var_square_bracket = "[" + fmt(val) + "]";
            } else {
                return false;
            }
            // fall through
        case T_ID:
            {
                string proc_part("");
                string var_part(parent_table->get_variable(expr.get_ident_gid())->get_name());
                if (parent_table->get_variable(expr.get_ident_gid())->get_process_gid() != NO_ID) {
                    proc_part = parent_table->get_process(parent_table->get_variable(expr.get_ident_gid())->
                                get_process_gid())->get_name();
                    proc_part += ".";
                }
                var = proc_part + var_part + var_square_bracket;
            }
            return true;
        default:
            var = "";
    }
    return false;
}

void dve_compiler::extract_predicates( std::vector<simple_predicate>& p, dve_expression_t& expr)
{
    dve_symbol_table_t * parent_table = expr.get_symbol_table();
    if (!parent_table) gerr << "Extract predicates: Symbol table not set" << thr();
    switch (expr.get_operator())
    {
        case T_PARENTHESIS:
            return extract_predicates(p, *expr.left());
        case T_LT: case T_LEQ: case T_EQ: case T_NEQ: case T_GT: case T_GEQ: {
            simple_predicate sp;
            if (get_const_varname(*expr.left(), sp.variable_name)) {
                if (get_const_expression(*expr.right(), sp.variable_value) ) {
                    switch(expr.get_operator()) {
                        case T_LT:  sp.relation = PRED_LT;  break;
                        case T_LEQ: sp.relation = PRED_LEQ; break;
                        case T_EQ:  sp.relation = PRED_EQ;  break;
                        case T_NEQ: sp.relation = PRED_NEQ; break;
                        case T_GT:  sp.relation = PRED_GT;  break;
                        case T_GEQ: sp.relation = PRED_GEQ; break;
                    }
                    p.push_back(sp);
                }
            }
            } break;
        case T_BOOL_AND:
        {
            extract_predicates( p, *expr.left());
            extract_predicates( p, *expr.right());
        }
    }
    // no disjoint expression found
    p.clear();
    return;
}

bool dve_compiler::is_conflict_predicate(simple_predicate& p1, simple_predicate p2)
{
    // assume no conflict
    bool no_conflict = true;
    // conflict only possible on same variable
    if (p1.variable_name == p2.variable_name) {
        switch(p1.relation) {
            case PRED_LT:
                // no conflict if one of these cases
                no_conflict =
                (p2.variable_value < p1.variable_value - 1) ||
                (p2.variable_value == p1.variable_value - 1 && p2.relation != PRED_GT) ||
                (p2.relation == PRED_LT || p2.relation == PRED_LEQ || p2.relation == PRED_NEQ);
                break;

            case PRED_LEQ:
                // no conflict if one of these cases
                no_conflict =
                (p2.variable_value < p1.variable_value) ||
                (p2.variable_value == p1.variable_value && p2.relation != PRED_GT) ||
                (p2.relation == PRED_LT || p2.relation == PRED_LEQ || p2.relation == PRED_NEQ);
                break;

            case PRED_EQ:
                // no conflict if one of these cases
                no_conflict =
                (p2.variable_value == p1.variable_value && (p2.relation == PRED_EQ || p2.relation == PRED_LEQ || p2.relation == PRED_GEQ)) ||
                (p2.variable_value != p1.variable_value && p2.relation == PRED_NEQ) ||
                ((p2.variable_value < p1.variable_value && p2.relation == PRED_GT) || p2.relation == PRED_GEQ) ||
                (p2.variable_value > p1.variable_value && (p2.relation == PRED_LT || p2.relation == PRED_LEQ));
                break;

            case PRED_NEQ:
                // no conflict if one of these cases
                no_conflict =
                (p2.variable_value != p1.variable_value) ||
                (p2.variable_value == p1.variable_value && p2.relation != PRED_EQ);
                break;

            case PRED_GT:
                // no conflict if one of these cases
                no_conflict =
                (p2.variable_value > p1.variable_value + 1) ||
                (p2.variable_value == p1.variable_value + 1 && p2.relation != PRED_LT) ||
                (p2.relation == PRED_GT || p2.relation == PRED_GEQ || p2.relation == PRED_NEQ);
                break;

            case PRED_GEQ:
                // no conflict if one of these cases
                no_conflict =
                (p2.variable_value > p1.variable_value) ||
                (p2.variable_value == p1.variable_value && p2.relation != PRED_LT) ||
                (p2.relation == PRED_GT || p2.relation == PRED_GEQ || p2.relation == PRED_NEQ);
                break;
        }
    }
    return !no_conflict;
}

void dve_compiler::mark_dependency( size_int_t gid, int type, int idx, std::vector<int> &dep )
{
    size_int_t size = 0;
    bool mark = false;
    for (size_int_t i=0; i!=state_creators_count; ++i)
    {
        mark = state_creators[i].gid == gid && type == state_creators[i].type;
        switch (state_creators[i].type)
        {
            case state_creator_t::VARIABLE:
            {
                if (state_creators[i].array_size)
                {
                     for(size_int_t j=0; j<state_creators[i].array_size; j++)
                     {
                        if (mark && (idx == -1 || idx == j)) dep[size]=1;
                        size++;
                     }
                }
                else
                {
                    if (mark) { dep[size]=1; }
                    size++;
                }
            }
            break;
            case state_creator_t::PROCESS_STATE:
            {
                if (mark) { dep[size]=1; }
                size++;
            }
            break;
            case state_creator_t::CHANNEL_BUFFER:
            {
                // mark number of items
                if (mark) dep[size]=1;
                size++;

                // mark channel
                dve_symbol_t * symbol =
                  get_symbol_table()->get_channel(state_creators[i].gid);
                size_int_t item_count = symbol->get_channel_type_list_size();
                size_int_t chan_size = symbol->get_channel_buffer_size();
                for(size_int_t i=0; i < chan_size; ++i) {
                    for (size_int_t j=0; j<item_count; ++j) {
                        if (mark) dep[size]=1;
                        size++;
                    }
                }
            }
            break;
            default: gerr << "Unexpected error while marking dependency" << thr();
                break;
        };
    }
}

int
dve_compiler::count_state_variables()
{
    size_int_t size = 0;
    for (size_int_t i=0; i!=state_creators_count; ++i)
    {
        switch (state_creators[i].type)
        {
            case state_creator_t::VARIABLE:
                size += (state_creators[i].array_size)?state_creators[i].array_size:1;
                break;
            case state_creator_t::PROCESS_STATE:
                size++;
                break;
            case state_creator_t::CHANNEL_BUFFER:
            {
                dve_symbol_t * symbol =
                  get_symbol_table()->get_channel(state_creators[i].gid);
                size_int_t item_count = symbol->get_channel_type_list_size();
                size_int_t chan_size = symbol->get_channel_buffer_size();
                size += (chan_size * item_count) + 1;
                break;
            }
            default: gerr << "Unexpected error while counting length of state" << thr();
                break;
        };
    }
    return size;
}
