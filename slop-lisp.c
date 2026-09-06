#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdbool.h>

typedef enum
{
    ATOM_NIL,
    ATOM_NUM,
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
        float number;
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
AtomRef atom_num (Interpreter *I, float n);
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

/* parsing */

typedef enum
{
    PARSE_OK = 0,
    PARSE_ERR_UNRECOGNISED_CHAR,
    PARSE_ERR_UNEXPECTED_EOF,
    PARSE_ERR_UNEXPECTED_TOKEN,
    PARSE_ERR_INVALID_NUMBER_SYNTAX,
    PARSE_ERR_INVALID_CHAR_SYNTAX,
    PARSE_ERR_UNCLOSED_QUOTE,
    PARSE_ERR_INVALID_ESCAPE_CODE,
    PARSE_ERR_UNBALANCED_PAREN,
} ParseErrorCode;

typedef struct
{
    ParseErrorCode code;
    size_t offset;
} ParseError;

AtomRef parse_atom (Interpreter *I, const char *expr_str, ParseError *out_err);
char *serialize_atom (AtomRef expr, size_t *out_sz);

/* primitives */

AtomRef prim_lt (Interpreter *I, AtomRef args)
{
    AtomRef lhs = car(args);
    AtomRef rhs = car(cdr(args));
    
    AtomRef ev_lhs = eval (I, lhs);
    AtomRef ev_rhs = eval (I, rhs);
    
    assert (ev_lhs->kind == ATOM_NUM);
    assert (ev_rhs->kind == ATOM_NUM);
    
    if (ev_lhs->as.number < ev_rhs->as.number)
    {
        return atom_num (I, 1);
    }
    else
    {
        return nil;
    }
}

AtomRef prim_print (Interpreter *I, AtomRef args)
{
    // FIXME: print everything in the list, and handle all atom types
    (void) I;
    AtomRef arg = car(args);
    assert (arg->kind == ATOM_STR);
    printf ("%s\n", arg->as.string);
    return nil;
}

/* input / runtime */

char *read_whole (FILE *in, size_t *out_sz)
{
    size_t cap = 128;
    size_t sz = 0;
    char *buf;
    
    if (!in) return NULL;
    
    buf = malloc (cap * sizeof *buf);
    
    while (1)
    {
        size_t n = fread (buf + sz, sizeof *buf, cap - sz, in);
        if (n == 0)
        {
            if (feof (in)) break;
            else if (ferror (in))
            {
                perror ("fread");
                free (buf);
                return NULL;
            }
        }
        
        sz += n;
        
        if (sz >= (cap - 1))
        {
            cap *= 2;
            buf = realloc (buf, cap * sizeof *buf);
        }
    }
    
    buf = realloc (buf, (sz + 1) * sizeof *buf);
    buf[sz] = 0;
    
    if (out_sz) *out_sz = sz;
    
    return buf;
}
 
int main (int argc, char *argv[])
{
    FILE *in = stdin;
    if (argc > 1)
    {
        in = fopen (argv[1], "rb");
    }
    
    size_t src_len;
    char *src = read_whole (in, &src_len);
    if (!src) goto cleanup;
    
    Interpreter I = { 0 };
    interpreter_init (&I);
    
    env_prim_set (&I, "lt", prim_lt);
    env_prim_set (&I, "print", prim_print);
    env_c_set (&I, "nil", nil);
    
    ParseError err;
    AtomRef expr = parse_atom (&I, src, &err);
    if (!expr)
    {
        printf ("PARSING ERR %d at offset %lu\n", err.code, err.offset);
        goto another_cleanup_ig;
    }
    root_push (&I, expr);
    
    free (src);
    
    AtomRef result = eval (&I, expr);
    root_pop (&I);
    root_push (&I, result);
    
    if (result)
    {
        printf ("there is SOME result btw.\n");
    }

another_cleanup_ig:
        interpreter_deinit (&I);
    
cleanup:
        if (argc > 1)
        {
            fclose (in);
        }
    
        return 0;
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

void bind (Interpreter *I, AtomRef symbols, AtomRef vals)
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

static AtomRef eval_do (Interpreter *I, AtomRef body)
{
    AtomRef result = nil;
    while (body != nil && body->kind == ATOM_PAIR)
    {
        result = eval (I, car(body));
        body = cdr(body);
    }
    return result;
}

AtomRef eval (Interpreter *I, AtomRef expr)
{
    switch (expr->kind)
    {
        case ATOM_NUM:
        case ATOM_NIL:
        case ATOM_STR:
        case ATOM_PRIM:
        case ATOM_CLOS:
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
                AtomRef sym = car(to);
                AtomRef val_expr = car(cdr(to));

                root_push(I, sym);
                AtomRef val = eval(I, val_expr);
                root_push(I, val);

                env_set(I, sym, val);

                root_pop(I); // val
                root_pop(I); // sym
                return nil;
            }
            else if (what->kind == ATOM_SYM && strcmp (what->as.symbol, "do") == 0)
            {
                return eval_do (I, to);
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
        default:
            assert (false);
    }
}

void interpreter_init (Interpreter *I)
{
    I->allocated = NULL;
    I->root_top = 0;
    I->n_allocated = 0;
    I->env = nil;
    I->env = atom_pair(I, nil, nil);
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
    (void) I;
    return nil;
}

AtomRef atom_num (Interpreter *I, float n)
{
    Atom *a = new_atom (I, ATOM_NUM);
    a->as.number = n;
    return a;
}

AtomRef atom_sym (Interpreter *I, const char *sym)
{
    Atom *a = new_atom (I, ATOM_SYM);
    a->as.symbol = strdup (sym);
    return a;
}

AtomRef atom_sym_n (Interpreter *I, const char *ptr, size_t n)
{
    Atom *a = new_atom (I, ATOM_SYM);
    a->as.symbol = strndup (ptr, n);
    return a;
}

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
    return a;
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

enum token_kind
{
    TOKEN_EOF = 0,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_SYMBOL,
    TOKEN_NUMBER,
    TOKEN_STRING,
};

struct token
{
    enum token_kind kind;
    
    const char *ptr;
    size_t length;
    size_t offset;
    
    struct token *next;
};

static struct token *
        token_new (enum token_kind kind, const char *ptr, size_t offset, size_t length)
{
    struct token *t = malloc (sizeof *t);
    if (!t) return NULL;
    
    t->kind = kind;
    t->ptr = ptr;
    t->offset = offset;
    t->length = length;
    
    t->next = NULL;
    
    return t;
}

static void token_free (struct token *t)
{
    while (t)
    {
        struct token *next = t->next;
        free (t);
        t = next;
    }
}

static bool is_space (char c)
{
    return c && strchr ("\n\r\t\v ", (unsigned char) c) != NULL;
}

static bool is_alpha (char c)
{
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static bool is_digit (char c)
{
    return (c >= '0' && c <= '9');
}

static bool is_symbol_char (char c)
{
    return c && (is_alpha (c) || is_digit (c) || strchr ("+-!@$%~^&*<>=.,/`", (unsigned char) c) != NULL);
}

static char to_lower (char c)
{
    if (c >= 'A' && c <= 'Z') return c - 32;
    return c;
}

struct lexer
{
    const char *src;
    size_t idx;
    size_t len;
    ParseError *err;
};

static bool reached_end (struct lexer *L)
{
    return L->idx >= L->len;
}

static char current_char (struct lexer *L)
{
    return L->src[L->idx];
}

static const char *
        current_ptr (struct lexer *L)
{
    return L->src + L->idx;
}

static void bump_n (struct lexer *L, size_t N)
{
    if (L->idx + N <= L->len) L->idx += N;
}

static void bump (struct lexer *L)
{
    bump_n (L, 1);
}

static void skip_whitespace (struct lexer *L)
{
    while (!reached_end (L) && is_space (current_char (L))) bump (L);
}

static struct token *
        lex_number (struct lexer *L)
{
    size_t start = L->idx;
    
    if (current_char (L) == '-' || current_char (L) == '+')
    {
        bump (L);
    }
    
    while (!reached_end (L) && is_digit (current_char (L)))
    {
        bump (L);
    }
    
    if (!reached_end (L) && current_char (L) == '.')
    {
        bump (L);
         
        while (!reached_end (L) && is_digit (current_char (L)))
        {
            bump (L);
        }
    }
    
    if (!reached_end (L) && to_lower (current_char (L)) == 'e')
    {
        bump (L);
        if (!reached_end (L) && (current_char (L) == '-' || current_char (L) == '+'))
        {
            bump (L);
        }
        while (!reached_end (L) && is_digit (current_char (L)))
        {
            bump (L);
        }
    }
    
    return token_new (TOKEN_NUMBER, L->src + start, start, L->idx - start);
}

static struct token *
        lex_next (struct lexer *L)
{
    struct token *t;
    char current;
    
    skip_whitespace (L);
    if (reached_end (L))
    {
        return token_new (TOKEN_EOF, current_ptr (L), L->idx, 0);
    }
    
    current = current_char (L);
    
    switch (current)
    {
        case '(': 
            t = token_new (TOKEN_LPAREN, current_ptr (L), L->idx, 1);
            bump (L);
            return t;
        case ')':
            t = token_new (TOKEN_RPAREN, current_ptr (L), L->idx, 1);
            bump (L);
            return t;
    }
    
    if (current == '-' || current == '+')
    {
        if (L->idx + 1 >= L->len || !is_digit (L->src[L->idx+1]))
        {
            /* fall through to `is_symbol_char` lmao */
        }
        else
        {
            return lex_number (L);
        }
    }
    
    if (is_digit (current) || current == '.')
    {
        return lex_number (L);
    }
    
    if (is_symbol_char (current))
    {
        size_t offset = L->idx + 1;
        
        while (offset < L->len && is_symbol_char (L->src[offset]))
        {
            offset += 1;
        }
        
        t = token_new (TOKEN_SYMBOL, current_ptr (L), L->idx, offset - L->idx);
        L->idx = offset;
        return t;
    }
    
    if (current == '"')
    {
        size_t offset = L->idx + 1;
        
        while (offset < L->len && L->src[offset] != '"')
        {
            if (L->len - offset > 1 && L->src[offset] == '\\')
                offset += 1;
            offset += 1;
        }
        
        if (offset >= L->len || L->src[offset] != '"')
        {
            if (L->err)
            {
                L->err->code = PARSE_ERR_UNCLOSED_QUOTE;
                L->err->offset = L->idx;
            }
            return NULL;
        }
        
        offset += 1;
        
        t = token_new (TOKEN_STRING, current_ptr (L), L->idx, offset - L->idx);
        L->idx = offset;
        return t;
    }
    
    if (L->err)
    {
        L->err->code = PARSE_ERR_UNRECOGNISED_CHAR;
        L->err->offset = L->idx;
    }
    
    return NULL;
}

static struct token *
        lex (const char *src, size_t n, ParseError *out_err)
{
    struct lexer L;
    struct token *head = NULL;
    struct token *tail = NULL;
    
    L.src = src;
    L.idx = 0;
    L.len = n ? n : strlen (src);
    L.err = out_err;
    
    while (1)
    {
        struct token *next = lex_next (&L);
        if (!next)
        {
            token_free (head);
            return NULL;
        }
        if (head == NULL)
        {
            head = next;
            tail = head;
        }
        else
        {
            tail->next = next;
            tail = tail->next;
        }
        if (next->kind == TOKEN_EOF)
            break;
    }
    
    return head;
}

static struct token *
        next (struct token **head, ParseError *out_err)
{
    struct token *next_tok;
    if (!head || !*head)
    {
        if (out_err)
        {
            out_err->code = PARSE_ERR_UNEXPECTED_EOF;
            out_err->offset = 0;
        }
        return NULL;
    }
    
    next_tok = *head;
    *head = (*head)->next;
    
    return next_tok;
}

static struct token *
peek (struct token **head, ParseError *out_err)
{
    if (!head || !*head)
    {
        if (out_err)
        {
            out_err->code = PARSE_ERR_UNEXPECTED_EOF;
            out_err->offset = 0;
        }
        return NULL;
    }
    
    return *head;
}

static struct token *
expect (struct token **head, enum token_kind kind, ParseError *out_err)
{
    struct token *next_tok = next (head, out_err);
    if (!head || !*head)
    {
        if (out_err)
        {
            out_err->code = PARSE_ERR_UNEXPECTED_EOF;
            out_err->offset = 0;
        }
        return NULL;
    }
    
    if (next_tok->kind != kind)
    {
        if (out_err)
        {
            out_err->code = PARSE_ERR_UNEXPECTED_TOKEN;
            out_err->offset = next_tok->offset;
        }
        return NULL;
    }
    
    return next_tok;
}

static AtomRef parse_list_tail (Interpreter *I, struct token **head, ParseError *out_err);
static AtomRef parse_atom_tokens (Interpreter *I, struct token **head, ParseError *out_err);

static AtomRef parse_list (Interpreter *I, struct token **head, ParseError *out_err)
{
    if (!expect (head, TOKEN_LPAREN, out_err))
        return NULL;
    return parse_list_tail (I, head, out_err);
}

static AtomRef parse_list_tail (Interpreter *I, struct token **head, ParseError *out_err)
{
    AtomRef head_expr, tail_expr;
    struct token *tok = peek (head, out_err);
    if (!tok) return NULL;
    
    if (tok->kind == TOKEN_RPAREN)
    {
        next (head, out_err);
        return atom_pair (I, nil, nil);
    }
    
    head_expr = parse_atom_tokens (I, head, out_err);
    if (!head_expr) return NULL;
    
    tok = peek (head, out_err);
    if (!tok) return NULL;
    
    root_push (I, head_expr);
    
    if (tok->kind == TOKEN_RPAREN)
    {
        next (head, out_err);
        AtomRef result = atom_pair (I, head_expr, nil);
        root_pop (I);
        return result;
    }
    
    tail_expr = parse_list_tail (I, head, out_err);
    if (!tail_expr)
    {
        root_pop (I);
        return NULL;
    }
    root_push (I, tail_expr);
    
    AtomRef result = atom_pair (I, head_expr, tail_expr);
    root_pop (I);
    root_pop (I);
    return result;
}

static AtomRef parse_number (Interpreter *I, struct token **head, ParseError *out_err)
{
    struct token *num_tok = expect (head, TOKEN_NUMBER, out_err);
    char *num_buf;
    char *end_ptr;
    float i;
    
    if (!num_tok) return NULL;
    
    num_buf = calloc (num_tok->length + 1, sizeof *num_buf);
    memcpy (num_buf, num_tok->ptr, num_tok->length);
    
    i = (float) strtod (num_buf, &end_ptr);
    if (*end_ptr != 0)
    {
        free (num_buf);
        if (out_err)
        {
            out_err->code = PARSE_ERR_INVALID_NUMBER_SYNTAX;
            out_err->offset = num_tok->offset;
        }
        return NULL;
    }
    
    free (num_buf);
    return atom_num (I, i);
}

static AtomRef parse_symbol (Interpreter *I, struct token **head, ParseError *out_err)
{
    struct token *sym_tok = expect (head, TOKEN_SYMBOL, out_err);
    if (!sym_tok) return NULL;
    return atom_sym_n (I, sym_tok->ptr, sym_tok->length);
}

static char *str_escape_n (const char *str, size_t n)
{
    char *result = malloc (n + 1); // +1 for null terminator
    if (!result) return NULL;

    char *r = result;
    const char *s = str;

    for (size_t i = 0; i < n; ++i, ++s, ++r) // Increment i
    {
        if (*s == '\\' && i + 1 < n)
        {
            switch (*(s+1))
            {
                case 'r': *r = '\r'; break;
                case 'n': *r = '\n'; break;
                case 't': *r = '\t'; break;
                case '0': *r = '\0'; break;
                case 'b': *r = '\b'; break;
                case '\\': *r = '\\'; break;
                case '\'': *r = '\''; break;
                case '\"': *r = '\"'; break;
                default: free (result); return NULL;
            }
            s += 1;
            i += 1;
        }
        else
        {
            *r = *s;
        }
    }
    *r = '\0'; // Null-terminate for strdup
    return result;
}

static AtomRef
        parse_string (Interpreter *I, struct token **head, ParseError *out_err)
{
    struct token *str_tok = expect (head, TOKEN_STRING, out_err);
    char *str;
    AtomRef result;
    
    str = str_escape_n (str_tok->ptr+1, str_tok->length - 2);
    if (!str)
    {
        if (out_err)
        {
            out_err->code = PARSE_ERR_INVALID_ESCAPE_CODE;
            out_err->offset = str_tok->offset;
        }
        return NULL;
    }
    
    result = atom_str (I, str);
    free (str);
    return result;
}

static AtomRef
        parse_atom_tokens (Interpreter *I, struct token **head, ParseError *out_err)
{
    struct token *next_tok = peek (head, out_err);
    if (!next_tok) return NULL;
    
    switch (next_tok->kind)
    {
        case TOKEN_STRING: return parse_string (I, head, out_err);
        case TOKEN_NUMBER: return parse_number (I, head, out_err);
        case TOKEN_SYMBOL: return parse_symbol (I, head, out_err);
        case TOKEN_LPAREN: return parse_list (I, head, out_err);
        case TOKEN_EOF:
            if (out_err)
            {
                out_err->code = PARSE_ERR_UNEXPECTED_EOF;
                out_err->offset = next_tok->offset;
            }
            return NULL;
        case TOKEN_RPAREN:
            if (out_err)
            {
                out_err->code = PARSE_ERR_UNBALANCED_PAREN;
                out_err->offset = next_tok->offset;
            }
            return NULL;
            default: assert (false);
    }
}

AtomRef parse_atom (Interpreter *I, const char *s, ParseError *out_err)
{
    struct token *tokens = lex (s, strlen (s), out_err);
    struct token *cursor = tokens;
    struct token **head = &cursor;
    AtomRef root;
    
    if (!tokens) return NULL;
    
    root = parse_atom_tokens (I, head, out_err);
    
    token_free (tokens);
    
    return root;
}
