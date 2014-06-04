// -*- C++ -*- (c) 2009, 2010 Milan Ceska & Petr Rockai
#include <divine/legacy/system/dve/dve_explicit_system.hh>
#include <string>
#include <math.h>
#include <map>
#include <vector>
#include <wibble/string.h>

#ifndef TOOLS_DVECOMPILE_H
#define TOOLS_DVECOMPILE_H

using namespace divine;
using namespace std;

struct ext_transition_t
{
    int synchronized;
    int commited;               // used for ltsmin
    int buchi;		            // used for ltsmin
    dve_transition_t *first;
    dve_transition_t *second;   // only when first transition is synchronized;
    dve_transition_t *property; // transition of property automaton
    std::vector<int> sv_read;
    std::vector<int> sv_may_write;
    std::vector<int> sv_must_write;
    std::vector<int> sv_actions_read;
};

typedef enum {GUARD_EXPR, GUARD_PC, GUARD_CHAN, GUARD_COMMITED_FIRST} guard_type;
struct guard
{
    guard_type type;
    union
    {
        struct {
            dve_expression_t * guard;
        } expr; // expression

        struct {
            divine::size_int_t gid;
            divine::size_int_t lid;
        } pc; // programm counter

        struct {
            divine::size_int_t chan;
            divine::sync_mode_t sync_mode;
        } chan;
    };
};

typedef enum {PRED_LT, PRED_LEQ, PRED_EQ, PRED_NEQ, PRED_GT, PRED_GEQ} predicate_relation_t;
struct simple_predicate
{
    std::string             variable_name;
    predicate_relation_t    relation;
    int                     variable_value;
};

struct dve_compiler: public dve_explicit_system_t
{
    bool ltsmin; // LTSmin successor generator
    bool ltsmin_ltl; // divine LTL semantics in LTsmin successor generator
    bool many;
    int current_label;
    bool may_write_add_read;

    bool have_property;
    map<size_int_t,map<size_int_t,vector<ext_transition_t> > > transition_map;
    map<size_int_t,vector<dve_transition_t*> > channel_map;

    vector<dve_transition_t*> property_transitions;
    vector<dve_transition_t*>::iterator iter_property_transitions;

    map<size_int_t,vector<dve_transition_t*> >::iterator iter_channel_map;
    vector<dve_transition_t*>::iterator iter_transition_vector;
    map<size_int_t,vector<ext_transition_t> >::iterator iter_process_transition_map;
    map<size_int_t,map<size_int_t,vector<ext_transition_t> > >::iterator iter_transition_map;
    vector<ext_transition_t>::iterator iter_ext_transition_vector;

    string m_line;
    ostream *m_output;
    bool m_verbose;
    int m_indent;

    void indent() { ++m_indent; }
    void deindent() { --m_indent; }

    void append( std::string l ) { m_line += l; }

    void outline() {
        ostream &out = *m_output;
        for ( int i = 0; i < m_indent; ++i )
            out << "    ";
        out << m_line << std::endl;
        m_line = "";
    }

    void line( std::string l = "" ) {
        append( l );
        outline();
    }

    dve_compiler(bool ltsmin, bool ltsmin_ltl, error_vector_t & evect=gerr, bool may_write_add_read=false)
        : explicit_system_t(evect), dve_explicit_system_t(evect), current_label(0), m_indent( 0 ), 
            ltsmin(ltsmin), ltsmin_ltl(ltsmin_ltl), may_write_add_read(may_write_add_read)
    {}
    virtual ~dve_compiler() {}

    int  count_state_variables();
    void analyse_expression( dve_expression_t & expr, ext_transition_t &ext_transition, std::vector<int> &dep );
    void output_dependency_comment( ext_transition_t &ext_transition );
    void mark_dependency ( size_int_t gid, int type, int idx, std::vector<int> &dep);
    void analyse_transition_dependencies( ext_transition_t &ext_transition );
    void analyse_transition( dve_transition_t * transition,
                             vector<ext_transition_t> &ext_transition_vector );
    void analyse();

    void write_C(dve_expression_t & expr, std::ostream & ostr, std::string state_name);

    bool m_if_disjoint;
    bool m_if_empty;

    void if_begin( bool disjoint ) {
        m_if_empty = true;
        m_if_disjoint = disjoint;
        append( "if ( " );
    }

    void if_clause( std::string c ) {
        if ( !m_if_empty ) {
            if ( m_if_disjoint )
                append( " || " );
            else
                append( " && " );
        }
        m_if_empty = false;
        append( " ( " );
        append( c );
        append( " ) " );
    }

    void if_cexpr_clause( dve_expression_t *expr, std::string state ) {
        if (!expr)
            return;
        if_clause( cexpr( *expr, state ) );
    }

    void if_end() {
        if ( m_if_empty ) {
            if ( m_if_disjoint )
                append( "false " );
            else
                append( "true " );
        }
        append( ")" );
        outline();
    }

    void assign( std::string left, std::string right ) {
        line( left + " = " + right + ";" );
    }

    std::string relate( std::string left, std::string op, std::string right ) {
        return left + " " + op + " " + right;
    }

    std::string process_name( int i ) {
        return get_symbol_table()->get_process( i )->get_name();
    }

    std::string channel_name( int i ) {
        return get_symbol_table()->get_channel( i )->get_name();
    }

    std::string process_state( int i, std::string state ) {
        return state + "." + process_name( i ) + ".state" + (ltsmin?".var":"");
    }

    std::string channel_items( int i, std::string state ) {
        return state + "." + channel_name( i ) + ".number_of_items" + (ltsmin?".var":"");
    }

    std::string channel_item_at( int i, std::string pos, int x, std::string state ) {
        return state + "." + channel_name( i ) + ".content[" + pos + "].x" + wibble::str::fmt( x ) + (ltsmin?".var":"");
    }

    int channel_capacity( int i ) {
        return get_symbol_table()->get_channel( i )->get_channel_buffer_size();
    }

    void transition_guard( ext_transition_t *, std::string );
    void transition_effect( ext_transition_t *, std::string, std::string );

    bool is_property( int i ) {
        return get_with_property() && i == get_property_gid();
    }

    std::string cexpr( dve_expression_t &expr, std::string state );
    void print_cexpr( dve_expression_t &expr, std::string state )
    {
        line( cexpr( expr, state ) + ";" );
    }

    void new_label() {
        if (many)
            return;
        append( std::string( "l" ) + wibble::str::fmt( current_label ) + ": " );
        current_label ++;
    }

    void block_begin() { line( "{" ); indent(); }
    void block_end() { deindent(); line( "}" ); }

    std::string in_state( int process, int state, std::string from_state ) {
        return "(" + process_state( process, from_state ) + " == " + wibble::str::fmt( state ) + ")";
    }

    void setOutput( std::ostream &o ) {
        m_output = &o;
    }
    void setVerbose( bool v ) {
        m_verbose = v;
    }
    
    void setMayReadAddWrite(bool w) {
        may_write_add_read = w;        
    }

    void yield_state();
    void new_output_state();

    void gen_successors();
    void gen_ltsmin_successors(bool condition);
    void gen_is_accepting();
    void gen_header();
    void gen_state_struct();
    void gen_initial_state();
    void gen_state_info();
    bool eq_expr(dve_expression_t*, dve_expression_t*);
    int  add_guard_expr(std::vector<guard> &guard, dve_expression_t* expr);
    int  add_guard_pc  (std::vector<guard> &guard, divine::size_int_t gid, divine::size_int_t lid);
    int  add_guard_chan(std::vector<guard> &guard, divine::size_int_t chan, divine::sync_mode_t sync_mode);
    void fill_transition_vector(std::vector<ext_transition_t>&);
    bool split_conjunctive_expression(std::vector<guard>& guard, dve_expression_t* expr);
    void merge_dependent_expression(std::vector<guard>& guard, int sv_count);
    void gen_transition_info();
    bool get_const_varname( dve_expression_t & expr, string & var);
    bool get_const_expression( dve_expression_t & expr, int & value);
    bool is_guard_nes( guard& g, ext_transition_t& t );
    bool is_guard_nds( guard& g, ext_transition_t& t );
    bool is_dna(std::vector<ext_transition_t>& transitions,
                std::vector< std::vector<bool> >& transition_coenabled,
                std::vector< std::vector<bool> >& transition_nds,
                std::vector< std::vector<bool> >& deps,
                int t1, int t2);
    bool may_be_coenabled( guard& ga, guard& gb);
    void extract_predicates( std::vector<simple_predicate>& p, dve_expression_t& e);
    bool is_conflict_predicate(simple_predicate& p1, simple_predicate p2);
    bool get_assignment ( dve_expression_t & expr,
                          std::vector<simple_predicate>& p,
                          std::vector<bool>& deps);
    bool extract_assigns ( ext_transition_t *et, std::vector<simple_predicate>& p,
                           std::vector<bool>& deps );
    bool dependent ( std::vector<int>& a, std::vector<bool>& b );
    bool commutes ( simple_predicate& a, simple_predicate& b );
    void print_generator();
};

#endif
