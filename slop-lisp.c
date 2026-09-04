#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdbool.h>

typedef enum
{
    ATOM_NIL,
    ATOM_INT,
    ATOM_SYM,
    ATOM_STR,
    ATOM_PAIR,
    ATOM_PRIM,
    ATOM_CLOS,
} AtomKind;

typedef struct atom Atom;
typedef Atom *AtomRef;
typedef struct interpreter Interpreter;

typedef AtomRef (*PrimitiveFn)(Interpreter *I, AtomRef args);

struct atom
{
    AtomKind kind;
    
    union
    {
        int integer;
        char *symbol;
        char *string;
        struct { AtomRef head, tail; } pair;
        PrimitiveFn primitive;
        struct { AtomRef params, body, env; } closure;
    } as;

    bool marked;
    struct atom *next;
};

#define car(atom) ((atom)->as.pair.head)
#define cdr(atom) ((atom)->as.pair.tail)

Atom NIL = (Atom) { .kind = ATOM_NIL, .marked = true, .next = NULL };
AtomRef nil = &NIL;

#define MAX_ALLOCATED 2
#define ROOTSSZ 64

struct interpreter
{
    AtomRef allocated;
    size_t n_allocated;
    
    AtomRef env;
    
    AtomRef roots[ROOTSSZ];
    size_t root_top;
};

/* interpreter lifetime */

void interpreter_init (Interpreter *I);
void interpreter_deinit (Interpreter *I);

/* allocation */

AtomRef new_atom (Interpreter *I, AtomKind kind);
AtomRef atom_nil (Interpreter *I);
AtomRef atom_int (Interpreter *I, int i);
AtomRef atom_sym (Interpreter *I, const char *sym);
AtomRef atom_sym_n (Interpreter *I, const char *ptr, size_t n); /* sized */
AtomRef atom_str (Interpreter *I, const char *str);
AtomRef atom_str_n (Interpreter *I, const char *ptr, size_t n); /* sized */
AtomRef atom_pair (Interpreter *I, AtomRef head, AtomRef tail);
AtomRef atom_prim (Interpreter *I, PrimitiveFn fn);
AtomRef atom_clos (Interpreter *I, AtomRef args, AtomRef body, AtomRef env);

/* garbage collector */

void collect (Interpreter *I);
void mark (Interpreter *I, AtomRef root);
void sweep (Interpreter *I);

/* environment */

void env_prim_set (Interpreter *I, const char *symbol, PrimitiveFn fn);
void env_c_set (Interpreter *I, const char *symbol, AtomRef value);
void env_set (Interpreter *I, AtomRef symbol, AtomRef value);
AtomRef env_get (Interpreter *I, AtomRef symbol);
void env_frame_push (Interpreter *I, AtomRef parent);
AtomRef env_frame_pop (Interpreter *I);

/* root stack */

void root_push (Interpreter *I, AtomRef ref);
AtomRef root_pop (Interpreter *I);
void root_clear (Interpreter *I);

/* evaluation */

AtomRef eval (Interpreter *I, AtomRef expr);

AtomRef prim_lt (Interpreter *I, AtomRef args)
{
    AtomRef lhs = car(args);
    AtomRef rhs = car(cdr(args));
    
    AtomRef ev_lhs = eval (I, lhs);
    AtomRef ev_rhs = eval (I, rhs);
    
    assert (ev_lhs->kind == ATOM_INT);
    assert (ev_rhs->kind == ATOM_INT);
    
    if (ev_lhs->as.integer < ev_rhs->as.integer)
    {
        return atom_int (I, 1);
    }
    else
    {
        return nil;
    }
}

AtomRef prim_print (Interpreter *I, AtomRef args)
{
    // FIXME: print everything in the list, and handle all atom types
    AtomRef arg = car(args);
    assert (arg->kind == ATOM_STR);
    printf ("%s\n", arg->as.string);
    return nil;
}

int main (void)
{
    Interpreter I = { 0 };
    interpreter_init (&I);
    
    env_prim_set (&I, "lt", prim_lt);
    env_prim_set (&I, "print", prim_print);
    

    /*
        (define x 69)
        (if (lt x 10)
            (print "I love Hatsune Miku~")
            (print "I'm the top goy!"))
    */
    AtomRef x_sym = atom_sym (&I, "x");
    root_push (&I, x_sym);
    AtomRef x_val = atom_int (&I, 69);
    root_push (&I, x_val);
    AtomRef def_arg_1 = atom_pair (&I, x_val, nil);
    root_pop (&I); // x_val
    root_push (&I, def_arg_1);
    AtomRef def_arg_2 = atom_pair (&I, x_sym, def_arg_1);
    root_pop (&I); // def_arg_1
    root_pop (&I); // x_sym
    root_push (&I, def_arg_2);
    AtomRef def_sym = atom_sym (&I, "define");
    root_push (&I, def_sym);
    AtomRef def_expr = atom_pair (&I, def_sym, def_arg_2);
    root_pop (&I); // def_sym
    root_pop (&I); // def_arg_2
    root_push (&I, def_expr);
    (void) eval (&I, def_expr);
    root_pop (&I); // def_expr
    

    AtomRef expr_else_print = atom_sym (&I, "print");
    root_push (&I, expr_else_print);
    
    AtomRef expr_else_print_what = atom_str (&I, "I'm the top goy!");
    root_push (&I, expr_else_print_what);
    
    AtomRef expr_else_print_args = atom_pair (&I, expr_else_print_what, nil);
    root_push (&I, expr_else_print_args);
    
    AtomRef expr_else = atom_pair (&I, expr_else_print, expr_else_print_args);
    root_push (&I, expr_else);
    
    AtomRef expr_then_print = atom_sym (&I, "print");
    root_push (&I, expr_then_print);
    
    AtomRef expr_then_print_what = atom_str (&I, "I love Hatsune Miku~");
    root_push (&I, expr_then_print_what);
    
    AtomRef expr_then_print_args = atom_pair (&I, expr_then_print_what, nil);
    root_push (&I, expr_then_print_args);
    
    AtomRef expr_then = atom_pair (&I, expr_then_print, expr_then_print_args);
    root_push (&I, expr_then);
    
    AtomRef x = atom_sym (&I, "x");
    root_push (&I, x);
    
    AtomRef ten = atom_int (&I, 10);
    root_push (&I, ten);
    
    AtomRef lt = atom_sym (&I, "lt");
    root_push (&I, lt);
    
    AtomRef lt_args_1 = atom_pair (&I, ten, nil);
    root_push (&I, lt_args_1);
    
    AtomRef lt_args_2 = atom_pair (&I, x, lt_args_1);
    root_push (&I, lt_args_2);
    
    AtomRef cond = atom_pair (&I, lt, lt_args_2);
    root_push (&I, cond);
    
    AtomRef if_args_1 = atom_pair (&I, expr_else, nil);
    root_push (&I, if_args_1);
    
    AtomRef if_args_2 = atom_pair (&I, expr_then, if_args_1);
    root_push (&I, if_args_2);
    
    AtomRef if_args_3 = atom_pair (&I, cond, if_args_2);
    root_push (&I, if_args_3);
    
    AtomRef if_atom = atom_sym (&I, "if");
    root_push (&I, if_atom);
    
    AtomRef expr = atom_pair (&I, if_atom, if_args_3); // final expression
    root_push (&I, expr);
    
    (void) eval (&I, expr);
    
    root_clear (&I);

    interpreter_deinit (&I);
}

void env_prim_set (Interpreter *I, const char *symbol, PrimitiveFn fn)
{
    AtomRef fn_atom = atom_prim (I, fn);
    root_push (I, fn_atom);
    AtomRef fn_sym = atom_sym (I, symbol);
    root_push (I, fn_sym);
    env_set (I, fn_sym, fn_atom);
    root_pop (I);
    root_pop (I);
}

void env_c_set (Interpreter *I, const char *symbol, AtomRef value)
{
    AtomRef sym = atom_sym (I, symbol);
    root_push (I, sym);
    env_set (I, sym, value);
    root_pop (I);
}

static AtomRef eval_if (Interpreter *I, AtomRef if_expr)
{
    assert (if_expr->kind == ATOM_PAIR);
    AtomRef cond = car(cdr(if_expr));
    AtomRef then_br = car(cdr(cdr(if_expr)));
    AtomRef else_br = car(cdr(cdr(cdr(if_expr))));
    
    AtomRef evaluated_cond = eval (I, cond);
    if (evaluated_cond != nil)
        return eval (I, then_br);
    else
        return eval (I, else_br);
}

static AtomRef eval_list (Interpreter *I, AtomRef list)
{
    if (list == nil) return nil;
    
    AtomRef head = eval (I, car(list));
    root_push (I, head);
    
    AtomRef tail = eval_list (I, cdr(list));
    root_push (I, tail);
    
    AtomRef result = atom_pair (I, head, tail);
    
    root_pop(I);
    root_pop(I);
    
    return result;
}

AtomRef bind (Interpreter *I, AtomRef symbols, AtomRef vals)
{
    AtomRef symbol = symbols;
    AtomRef val = vals;
    while (symbol != nil && val != nil)
    {
        env_set (I, car(symbol), car(val));
        val = cdr(val);
        symbol = cdr(symbol);
    }
}

AtomRef apply (Interpreter *I, AtomRef fn, AtomRef args)
{
    switch (fn->kind)
    {
        case ATOM_PRIM: return fn->as.primitive (I, args);
        case ATOM_CLOS:
        {
            AtomRef old_env = I->env;
            root_push (I, old_env);
            
            env_frame_push (I, fn->as.closure.env);
            bind (I, fn->as.closure.params, args);
            
            AtomRef result = eval (I, fn->as.closure.body);
            
            I->env = old_env;
            root_pop (I);
            return result;
        }
        break;
        default: assert (false);
    }
}

AtomRef eval_lambda (Interpreter *I, AtomRef lambda)
{
    AtomRef args = car(cdr(lambda));
    AtomRef body = car(cdr(cdr(lambda)));
    AtomRef env = I->env;
    return atom_clos (I, args, body, env);
}

AtomRef eval (Interpreter *I, AtomRef expr)
{
    switch (expr->kind)
    {
        case ATOM_INT:
        case ATOM_NIL:
        case ATOM_STR:
        case ATOM_PRIM:
            return expr;
        case ATOM_SYM:
            return env_get (I, expr);
        break;
        case ATOM_PAIR:
        {
            AtomRef what = car (expr);
            AtomRef to = cdr (expr);
            
            if (what->kind == ATOM_SYM && strcmp (what->as.symbol, "if") == 0)
                return eval_if (I, expr);
            else if (what->kind == ATOM_SYM && strcmp (what->as.symbol, "quote") == 0)
                return to;
            else if (what->kind == ATOM_SYM && strcmp (what->as.symbol, "define") == 0)
            {
                env_set (I, car(to), car(cdr(to)));
                return nil;
            }
            else if (what->kind == ATOM_SYM && strcmp (what->as.symbol, "lambda") == 0)
                return eval_lambda (I, expr);
            else
            {
                root_push (I, to);
                root_push (I, what);
                
                AtomRef fn = eval (I, what);
                root_pop (I); // what
                root_push (I, fn);
                
                AtomRef args = eval_list (I, to);
                root_push (I, args);

                AtomRef result = apply (I, fn, args);
                root_pop (I); // args
                root_pop (I); // fn
                root_pop (I); // to
                return result;
            }
        }
        break;
    }
}

void interpreter_init (Interpreter *I)
{
    I->allocated = NULL;
    I->env = nil;
    I->root_top = 0;
    I->n_allocated = 0;
}

void interpreter_deinit (Interpreter *I)
{
    sweep (I);
}

AtomRef new_atom (Interpreter *I, AtomKind kind)
{
    Atom *a = malloc (sizeof *a);
    assert (a);
    
    if (I->n_allocated >= MAX_ALLOCATED)
    {
        collect (I);
    }
    
    Atom *old_head = I->allocated;
    I->allocated = a;
    I->n_allocated += 1;
    
    a->kind = kind;
    a->marked = false;
    a->next = old_head;
    
    return a;
}

AtomRef atom_nil (Interpreter *I)
{
    return nil;
}

AtomRef atom_int (Interpreter *I, int i)
{
    Atom *a = new_atom (I, ATOM_INT);
    a->as.integer = i;
    return a;
}

AtomRef atom_sym (Interpreter *I, const char *sym)
{
    Atom *a = new_atom (I, ATOM_SYM);
    a->as.symbol = strdup (sym);
    return a;
}

AtomRef atom_sym_n (Interpreter *I, const char *ptr, size_t n); /* sized */

AtomRef atom_str (Interpreter *I, const char *str)
{
    Atom *a = new_atom (I, ATOM_STR);
    a->as.string = strdup (str);
    return a;
}

AtomRef atom_str_n (Interpreter *I, const char *ptr, size_t n); /* sized */

AtomRef atom_pair (Interpreter *I, AtomRef head, AtomRef tail)
{
    Atom *a = new_atom (I, ATOM_PAIR);
    a->as.pair.head = head;
    a->as.pair.tail = tail;
    return a;
}

AtomRef atom_prim (Interpreter *I, PrimitiveFn fn)
{
    Atom *a = new_atom (I, ATOM_PRIM);
    a->as.primitive = fn;
    return a;
}

AtomRef atom_clos (Interpreter *I, AtomRef args, AtomRef body, AtomRef env)
{
    Atom *a = new_atom (I, ATOM_CLOS);
    a->as.closure.params = args;
    a->as.closure.body = body;
    a->as.closure.env = env;
}

void root_push (Interpreter *I, AtomRef ref)
{
    assert (I->root_top < ROOTSSZ);
    I->roots[I->root_top++] = ref;
}

AtomRef root_pop (Interpreter *I)
{
    assert (I->root_top > 0);
    return I->roots[--I->root_top];
}

void root_clear (Interpreter *I)
{
    I->root_top = 0;
}

void collect (Interpreter *I)
{
    mark (I, I->env);
    mark (I, nil);
    for (size_t i = 0; i < I->root_top; ++i)
    {
        mark (I, I->roots[i]);
    }
    sweep (I);
}

void mark (Interpreter *I, AtomRef root)
{
    if (root->marked) return;
    root->marked = true;
    
    if (root->kind == ATOM_PAIR)
    {
        mark (I, root->as.pair.head);
        mark (I, root->as.pair.tail);
    }
}

static void delete_atom (AtomRef atom)
{
    switch (atom->kind)
    {
        case ATOM_STR: free (atom->as.string); break;
        case ATOM_SYM: free (atom->as.symbol); break;
        default: break;
    }
    free (atom);
}

void sweep (Interpreter *I)
{
    if (I->n_allocated == 0) return;
    
    AtomRef a = I->allocated;
    while (a && !a->marked)
    {
        AtomRef next = a->next;
        delete_atom (a);
        I->n_allocated -= 1;
        a = next;
    }
    
    I->allocated = a;
    
    if (!a) return;
    if (a != nil) a->marked = false;
    
    while (a->next)
    {
        if (!a->next->marked)
        {
            AtomRef next = a->next->next;
            delete_atom (a->next);
            I->n_allocated -= 1;
            a->next = next;
        }
        else
        {
            a = a->next;
        }
    }
    
    for (AtomRef p = I->allocated; p; p = p->next)
    {
        if (p == nil) continue;
        p->marked = false;
    }
}

void env_frame_push (Interpreter *I, AtomRef parent_env)
{
    AtomRef new = atom_pair (I, nil, parent_env); // new frame B, tie the string to frame A
    root_push (I, new);
    parent_env = new;   // set I->env to frame B
    root_pop (I);
}

AtomRef env_frame_pop (Interpreter *I)
{
    AtomRef top = car(I->env);
    I->env = cdr (I->env); // set I->env back to frame A
    return top;
}

void env_set(Interpreter *I, AtomRef symbol, AtomRef value)
{
    AtomRef kv = atom_pair(I, symbol, value);
    root_push (I, kv);
    car(I->env) = atom_pair(I, kv, car(I->env)); // push to the newest frame
    root_pop (I);
}

AtomRef env_get (Interpreter *I, AtomRef symbol)
{
    if (I->env == nil) return nil;
    AtomRef cur_frame = I->env;
    while (cur_frame != nil)
    {
        AtomRef cur = car(cur_frame);
        while (cur != nil)
        {
            AtomRef kv = car(cur);
            AtomRef key = car(kv);
            if (strcmp (key->as.symbol, symbol->as.symbol) == 0)
            {
                return cdr(kv);
            }
            cur = cdr(cur);
        }
        cur_frame = cdr(cur_frame);
    }
    return nil;
}
