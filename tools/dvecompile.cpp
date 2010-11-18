#include <tools/compile.h>
#include <tools/dvecompile.h>
#include <divine/generator/common.h>

using namespace wibble::str;

namespace divine {
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
            ostr << parent_table->get_process(parent_table->get_variable(expr.get_ident_gid())->
                                              get_process_gid())->get_name(); //name of process
            ostr<<"->";
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
            ostr<<state_name<<".";
            ostr<<parent_table->get_process(parent_table->get_state(expr.get_ident_gid())->
                                            get_process_gid())->get_name(); ostr<<".state"<<(ltsmin?".var":"")<<" == ";
            ostr<<parent_table->get_state(expr.get_ident_gid())->get_lid();
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
        char buf[10];
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
                            sprintf(buf, "%d", (initial_values_counts[state_creators[i].gid]?
                                                initial_values[state_creators[i].gid].all_values[j]:0));
                            append(buf);
                         }
                    }
                    else
                    {
                            append(sep); sprintf(sep,",");
                            sprintf(buf, "%d", (initial_values_counts[state_creators[i].gid]?
                                                initial_values[state_creators[i].gid].all_value:0));
                            append(buf);
                    }
                }
                break;
                case state_creator_t::PROCESS_STATE:
                {
                    append(sep); sprintf(sep,",");
                    sprintf(buf, "%zu", initial_states[state_creators[i].gid]);
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
    char buf[1024];

    append("// read : " );
    for(size_int_t i = 0; i < count; i++)
    {
        sprintf(buf, "%s%d", ((i==0)?"":","), ext_transition.sv_read[i]);
        append(buf);
    }
    line();

    append("// write: " );
    for(size_int_t i = 0; i < count; i++)
    {
        sprintf(buf, "%s%d", ((i==0)?"":","), ext_transition.sv_write[i]);
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
                mark_dependency(expr.get_ident_gid(), state_creator_t::VARIABLE, -1, dep);
                analyse_expression(*expr.left(), ext_transition, dep);
            }
            break;
        case T_LT: case T_LEQ: case T_EQ: case T_NEQ: case T_GT: case T_GEQ:
        case T_PLUS: case T_MINUS: case T_MULT: case T_DIV: case T_MOD:
        case T_AND: case T_OR: case T_XOR: case T_LSHIFT: case T_RSHIFT:
        case T_BOOL_AND: case T_BOOL_OR:
            analyse_expression( *expr.left(), ext_transition, ext_transition.sv_read );
            analyse_expression( *expr.right(), ext_transition, ext_transition.sv_read );
            break;
        case T_ASSIGNMENT:
            analyse_expression( *expr.left(), ext_transition, ext_transition.sv_write );
            analyse_expression( *expr.right(), ext_transition, ext_transition.sv_read );
            break;
        case T_DOT:
            // dot addes an explicit == (see code), thus must be read
            mark_dependency(parent_table->get_state(expr.get_ident_gid())->get_process_gid(),
                            state_creator_t::PROCESS_STATE, -1, ext_transition.sv_read);
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
    ext_transition.sv_write.resize(count);

    // guard

    // mark process as read
    mark_dependency(ext_transition.first->get_process_gid(),
                    state_creator_t::PROCESS_STATE, -1, ext_transition.sv_read);

    if (ext_transition.first->get_guard())
    analyse_expression( *(ext_transition.first->get_guard()), ext_transition,
                        ext_transition.sv_read);

    if (ext_transition.synchronized)
    {
        // mark process as read
        mark_dependency(ext_transition.second->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, ext_transition.sv_read);

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
        }
    }

    if (have_property)
    {
        // mark process as read/write?
        mark_dependency(ext_transition.property->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, ext_transition.sv_read);

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
                                ext_transition.sv_write);
            analyse_expression( *(ext_transition.second->get_sync_expr_list_item(s)), ext_transition,
                                ext_transition.sv_read);
        }
    } else {
        int sm = ext_transition.first->get_sync_mode();
        if (sm == SYNC_EXCLAIM_BUFFER)
        {
            // mark entire channel
            mark_dependency(ext_transition.first->get_channel_gid(),
                            state_creator_t::CHANNEL_BUFFER, -1, ext_transition.sv_write);
            // mark sync expressions
            for(size_int_t s = 0;s < ext_transition.first->get_sync_expr_list_size();s++)
            {
                analyse_expression( *(ext_transition.first->get_sync_expr_list_item(s)), ext_transition,
                                    ext_transition.sv_read);
            }
        }
        if (sm == SYNC_ASK_BUFFER)
        {
            // mark entire channel
            mark_dependency(ext_transition.first->get_channel_gid(),
                            state_creator_t::CHANNEL_BUFFER, -1, ext_transition.sv_read);
            mark_dependency(ext_transition.first->get_channel_gid(),
                            state_creator_t::CHANNEL_BUFFER, -1, ext_transition.sv_write);
            // mark sync expressions
            for(size_int_t s = 0;s < ext_transition.first->get_sync_expr_list_size();s++)
            {
                analyse_expression( *(ext_transition.first->get_sync_expr_list_item(s)), ext_transition,
                                    ext_transition.sv_write);
            }

        }

    }

    // mark process as read (write is probably in transition effect)
    mark_dependency(ext_transition.first->get_process_gid(),
                    state_creator_t::PROCESS_STATE, -1, ext_transition.sv_write);

    // analyse ext_transition->first
    for(size_int_t e = 0;e < ext_transition.first->get_effect_count();e++)
        analyse_expression( *(ext_transition.first->get_effect(e)), ext_transition,
        ext_transition.sv_read);

    // analyse ext_transition->second?
    if (ext_transition.synchronized)
    {
        // mark process as read (write is probably in transition effect)
        mark_dependency(ext_transition.second->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, ext_transition.sv_write);

        // analyse ext_transition->second
        for(size_int_t e = 0;e < ext_transition.second->get_effect_count();e++)
            analyse_expression( *(ext_transition.second->get_effect(e)), ext_transition,
            ext_transition.sv_read);
    }

    if (have_property)
    {
        // mark process as read/write?
        mark_dependency(ext_transition.property->get_process_gid(),
                        state_creator_t::PROCESS_STATE, -1, ext_transition.sv_write);
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
        for(size_int_t s = 0;s < et->first->get_sync_expr_list_size();s++)
            assign( cexpr( *et->first->get_sync_expr_list_item(s), out ),
                    cexpr( *et->second->get_sync_expr_list_item(s), in ) );
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
            }
            line( channel_items( chan, out ) + "++;" );
        }
        if(et->first->get_sync_mode() == SYNC_ASK_BUFFER)
        {
            for(size_int_t s = 0;s < et->first->get_sync_expr_list_size();s++)
                assign( cexpr( *et->first->get_sync_expr_list_item(s), out ),
                        channel_item_at( chan, "0", s, in ) );
            line( channel_items( chan, out ) + "--;" );

            line( "for(size_int_t i = 1 ; i <= " + channel_items( chan, out ) + "; i++)" );
            block_begin();
            for(size_int_t s = 0;s < et->first->get_sync_expr_list_size();s++)
            {
                assign( channel_item_at( chan, "i-1", s, out ), channel_item_at( chan, "i", s, in ) );
                assign( channel_item_at( chan, "i", s, out ), "0" );
            }
            block_end();
        }
    }

    //first transition effect
    assign( process_state( et->first->get_process_gid(), out ),
            fmt( et->first->get_state2_lid() ) );

    for(size_int_t e = 0;e < et->first->get_effect_count();e++)
        print_cexpr( *et->first->get_effect(e), out );

    if(et->synchronized) //second transiton effect
    {
        assign( process_state( et->second->get_process_gid(), out ),
                fmt( et->second->get_state2_lid() ) );
        for(size_int_t e = 0;e < et->second->get_effect_count();e++)
            print_cexpr( *et->second->get_effect(e), out );
    }

    if(have_property) //change of the property process state
        assign( process_state( et->property->get_process_gid(), out ),
                fmt( et->property->get_state2_lid() ) );

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
        line( "callback(arg, &transition_info, out);" );
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

void dve_compiler::gen_ltsmin_successors()
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

                                // committed state
                                if_begin( true );

                                for(size_int_t p = 0; p < get_process_count(); p++)
                                    for(size_int_t c = 0; c < dynamic_cast<dve_process_t*>(get_process(p))->get_state_count(); c++)
                                        if(dynamic_cast<dve_process_t*>(get_process(p))->get_commited(c))
                                            if_clause( in_state( p, c, in ) );

                                if_end(); // otherwise this condition is disjoint with the new condition

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
                        if (!ltsmin) line( "system_in_deadlock = false;" );
                        yield_state();
                        block_end();
                    }
                    block_end();
                }
            }
    }
    block_end();

    if (!ltsmin)
    {
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

            assign( process_state( (*iter_property_transitions)->get_process_gid(), in ),
                    fmt( (*iter_property_transitions)->get_state2_lid() ) );

            yield_state();
            block_end();
        }
        block_end();
    }
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
        line( "extern \"C\" int get_successor( void* model, int t, state_struct_t *in, void (*callback)(void* arg, transition_info_t *transition_info, state_struct_t *out), void *arg ) " );
        block_begin();
        line( "transition_info_t transition_info = { NULL, t };" );
        line( "(void)model; // ignore model" );
        line( "int states_emitted = 0;" );
        line( "state_struct_t tmp;" );
        line( "state_struct_t *out = &tmp;" );
        line( "goto switch_state;" );
        gen_ltsmin_successors();
        // switch block
        line( "switch_state: switch( t )" );
        block_begin();
        for(int i=0; i < current_label; i++)
                line( "case " + fmt( i ) + ": goto l" + fmt( i ) + ";" );
        block_end();
        line("return 0;");
        // end switch block
        block_end();
        line();

        many = true;
        current_label = 0;

        line( "extern \"C\" int get_successors( void *model, state_struct_t *in, void (*callback)(void *arg, transition_info_t *transition_info, state_struct_t *out), void *arg ) " );
        block_begin();
        line( "(void)model; // ignore model" );
        line( "transition_info_t transition_info = { NULL, -1 };" );
        line( "int states_emitted = 0;" );
        line( "state_struct_t tmp;" );
        line( "state_struct_t *out = &tmp;" );
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
        line( "state_struct_t *in = &from.get< state_struct_t >( setup->slack );" );
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
    char buf[1024];
    // number of variables in the state
    line( "extern \"C\" int get_state_variable_count() " );
    block_begin();
    sprintf(buf, "return %d;", count_state_variables());
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
        sprintf(buf, "case %d:", k);
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
                        sprintf(buf, "    return \"%s[%zu]\";", name.c_str(), j);
                        line(buf);
                        if (j < state_creators[i].array_size - 1) {
                            sprintf(buf, "case %d:", ++k);
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
                sprintf(buf, "    return \"%s.number_of_items\";", name.c_str());
                line(buf);

                dve_symbol_t * symbol =
                  get_symbol_table()->get_channel(state_creators[i].gid);
                size_int_t item_count = symbol->get_channel_type_list_size();
                size_int_t chan_size = symbol->get_channel_buffer_size();
                for(size_int_t i=0; i < chan_size; ++i)
                {
                    for (size_int_t j=0; j<item_count; ++j)
                    {
                        sprintf(buf, "case %d:", ++k);
                        line(buf);
                        sprintf(buf, "    return \"%s[%zu].x%zu\";", name.c_str(), i,j);
                        line(buf);
                    }
                }
                continue;
            }
            default: gerr << "Unexpected error generating variable names" << thr();
                break;
        };

        sprintf(buf, "    return \"%s\";", name.c_str());
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

        sprintf(buf, "case %d:", k);
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
                        sprintf(buf, "    return %d;", type_no[type_name], j);
                        line(buf);
                        sprintf(buf, "case %d:", ++k);
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
                sprintf(buf, "    return %d;", type_no["int"]);
                line(buf);

                dve_symbol_t * symbol =
                  get_symbol_table()->get_channel(state_creators[i].gid);
                size_int_t item_count = symbol->get_channel_type_list_size();
                size_int_t chan_size = symbol->get_channel_buffer_size();
                for(size_int_t i=0; i < chan_size; ++i)
                {
                    for (size_int_t j=0; j<item_count; ++j)
                    {
                        sprintf(buf, "case %d:", ++k);
                        line(buf);
                        if (symbol->get_channel_type_list_item(j) == VAR_BYTE)
                        {
                            sprintf(buf, "    return %d;", type_no["byte"]);
                            line(buf);
                        } else {
                            sprintf(buf, "    return %d;", type_no["int"]);
                            line(buf);
                        }
                    }
                }
                continue;
            }
            default: gerr << "Unexpected error while writing name per variable" << thr();
                break;
        };

        sprintf(buf, "    return %d;", type_no[type_name]);
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
    sprintf(buf, "return %d;", type_count);
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
            sprintf(buf, "case %d:", ix->second);
            line(buf);
            sprintf(buf, "    return \"%s\";", ix->first.c_str());
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
            sprintf(buf, "case %d: // %s", ix->second, ix->first.c_str());
            line(buf);
            sprintf(buf, "    return %zu;", type_value[ix->first].size());
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
                sprintf(buf, "case %d:", ix->second);
                line(buf);
                block_begin();
                    line("switch (value)");
                    block_begin();
                    for(int i=0; i < type_value[ix->first].size(); ++i)
                    {
                        sprintf(buf, "case %d:", i);
                        line(buf);
                        sprintf(buf, "    return \"%s\";", type_value[ix->first][i].c_str());
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
                        transitions.push_back(tmp);
                        trans_count++;
                    }
                }
            }
    }
}

void dve_compiler::gen_transition_info()
{
    int sv_count = count_state_variables();
    char buf[1024];

    // initialize read/write dependency vector
    std::vector<int> c_base_sv_read(sv_count);
    std::vector<ext_transition_t> transitions;
    fill_transition_vector(transitions);

    // mark commited processes as read
    for(size_int_t i = 0; i < get_process_count(); i++)
        for(size_int_t j = 0; j < dynamic_cast<dve_process_t*>(get_process(i))->get_state_count(); j++)
            if(dynamic_cast<dve_process_t*>(get_process(i))->get_commited(j))
                mark_dependency(dynamic_cast<dve_process_t*>(get_process(i))->get_gid(),
                                state_creator_t::PROCESS_STATE, -1, c_base_sv_read);

    // output transition vectors
    sprintf(buf, "int transition_dependency[][2][%d] = ", sv_count);
    line(buf);
    block_begin();
    line("// { ... read ...}, { ... write ...}");

    for(size_int_t i = 0; i < transitions.size(); i++) {
        ext_transition_t& current = transitions[i];
        if (i!=0) { line(","); }
        append("{{" );
        for(size_int_t j = 0; j < sv_count; j++)
        {
            sprintf(buf, "%s%d", ((j==0)?"":","), max(c_base_sv_read[j], current.sv_read[j]) );
            append(buf);
        }
        append("},{" );
        for(size_int_t j = 0; j < sv_count; j++)
        {
            sprintf(buf, "%s%d", ((j==0)?"":","), current.sv_write[j] );
            append(buf);
        }
        append("}}");
    }
    line();
    block_end();
    line(";");
    line();

    // number of transitions
    line( "extern \"C\" int get_transition_count() " );
    block_begin();
    sprintf(buf, "return %zu;", transitions.size());
    line(buf);
    block_end();
    line();

    // read dependencies
    line( "extern \"C\" const int* get_transition_read_dependencies(int t) " );
    block_begin();
    sprintf(buf, "if (t>=0 && t < %zu) return transition_dependency[t][0];", transitions.size());
    line(buf);
    sprintf(buf, "return NULL;");
    line(buf);
    block_end();
    line();

    // write dependencies
    line( "extern \"C\" const int* get_transition_write_dependencies(int t) " );
    block_begin();
    sprintf(buf, "if (t>=0 && t < %zu) return transition_dependency[t][1];", transitions.size());
    line(buf);
    sprintf(buf, "return NULL;");
    line(buf);
    block_end();
    line();

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

int dve_compiler::count_state_variables()
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
