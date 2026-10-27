module slisp;
import std::io;
import std::collections::map;
import std::collections::list;
import std::collections::set;

macro @errf(fault f, String fmt, ...)
{
	io::eprintf ("FAULT [%s]: ", f.short_name());
	io::eprintf (fmt, ...$vaarg);
	io::eprintn ();
	return f~;
}

enum Tag
{
	NIL,
	NUM,
	STR,
	SYM,
	PAIR,
	PRIM,
}

alias Atom = AtomData*;
alias PrimitiveCallback = fn Atom? (State*, Env, Atom args);

struct AtomData
{
	Tag tag;

	union as
	{
		double number;
		String string;
		String symbol;
		struct pair { Atom head; Atom tail; }
		PrimitiveCallback primitive;
	}
}

<* @require self.tag == Tag.PAIR *>
fn Atom AtomData.car (&self)
{
	return self.as.pair.head;
}

<* @require self.tag == Tag.PAIR *>
fn Atom AtomData.cdr (&self)
{
	return self.as.pair.tail;
}

fn void AtomData.free (&self)
{
    switch (self.tag)
    {
    case STR: 
        self.as.string.free(mem); 
        break;
    case SYM: 
        self.as.symbol.free(mem); 
        break;
    default: 
        break;
    }
    
    mem::free(self);
}

const AtomData NIL = { .tag = Tag.NIL };
Atom nil = &NIL;

alias Env = Atom; // (parent ((symbol value) ((symbol value) (... nil))))

<* @require sym.tag == Tag.SYM *>
fn Atom? env_get (Env env, Atom sym)
{
	for (Env e = env; e != nil; e = e.car())
	{
		Atom list = e.cdr();

		while (list != nil)
		{
			Atom head = list.car();
			Atom rest = list.cdr();

			if (head.car() == sym) return head.cdr();

			list = rest;
		}
	}

	return @errf(UNBOUND_SYMBOL, "symbol `%s` is unbound", sym.as.symbol);
}

<* @require sym.tag == Tag.SYM *>
fn void? env_set (Env env, Atom sym, Atom val)
{
	for (Env e = env; e != nil; e = e.car())
	{
		Atom list = e.cdr();

		while (list != nil)
		{
			Atom head = list.car();
			Atom rest = list.cdr();

			if (head.car() == sym)
			{
				head.as.pair.tail = val;
				return;
			}

			list = rest;
		}
	}

	return @errf(UNBOUND_SYMBOL, "symbol `%s` is undefined", sym.as.symbol);
}

<* @require sym.tag == Tag.SYM *>
fn void? env_define (Env env, State *s, Atom sym, Atom val)
{
	Atom binding = s.pair (sym, val);
	s.rpush (binding);
	env.as.pair.tail = s.pair(binding, env.cdr());
	s.rpop ()!;
}

const sz GC_SWEEP_FLOOR = 2;

struct State
{
	OrderedMap{String, Atom} intern_table; // makes sure, that only one identical symbol exists at once
	List{Atom} roots; // FIXME: replace with something, that doesn't allocate all the fucking time
	GarbageCollector gc;
	Env global;
}

fn void State.init (&self)
{
	self.roots.init (mem);
	self.intern_table.init (mem);
	self.gc.init ();

	self.global = self.pair (nil, nil);
	self.gc.env = self.global;
}

struct GarbageCollector
{
	List{Atom} allocated;
	HashSet{uptr} marked;
	Env env;
}

fn void GarbageCollector.init (&self)
{
	self.marked.init (mem);
	self.allocated.init (mem);
	self.env = nil;
}

fn void GarbageCollector.free (&self)
{
	self.allocated.free ();
	self.marked.free ();
}

fn void GarbageCollector.clear (&self)
{
	self.marked.clear();
}

fn void GarbageCollector.mark (&self, Atom atom)
{
    Atom cur = atom;
    while (cur != nil && !self.marked.contains ((uptr) cur))
    {
        self.marked.add ((uptr) cur);

        if (cur.tag == Tag.PAIR)
        {
            self.mark (cur.car());
            cur = cur.cdr ();
        }
        else
        {
            break;
        }
    }
}

fn void GarbageCollector.sweep (&self)
{
    sz write_index = 0;
    sz count = self.allocated.len();

    for (sz i = 0; i < count; i++)
    {
        Atom atom = self.allocated.get(i);

        if (self.marked.contains ((uptr) atom))
        {
			self.allocated.set_at (write_index, atom);
            write_index++;
        }
        else
        {
            atom.free();
        }
    }

    self.allocated.size = write_index;
	self.clear ();
}

fn void State.garbage_collect (&self)
{
	self.gc.mark (self.global);

	foreach (atom : self.roots)
	{
		self.gc.mark (atom);
	}

	self.intern_table.@each(; key, value)
    {
        self.gc.mark (value);
    };

	self.gc.sweep ();
}

fn void State.destroy (&self)
{
    self.roots.clear ();
    self.intern_table.clear ();

	self.gc.clear ();
	self.gc.sweep ();
	self.gc.free ();

    self.roots.free ();
    self.intern_table.free ();
}

faultdef UNBOUND_SYMBOL;
faultdef REDEFINED_SYMBOL;
faultdef TYPE_FUCKUP; // idk..
faultdef NOT_APPLICABLE;

fn Atom State.atom (&self, Tag tag)
{
	if (self.gc.allocated.size > GC_SWEEP_FLOOR) self.garbage_collect ();

	Atom new = mem::alloc (AtomData); // FIXME: dedicated atom area
	new.tag = tag;

	self.gc.allocated.push_front (new);

	return new;
}

fn Atom State.number (&self, double num)
{
	Atom new = self.atom (Tag.NUM);
	new.as.number = num;
	return new;
}

fn Atom State.string (&self, String str)
{
	Atom new = self.atom (Tag.STR);
	new.as.string = str.copy (mem);
	return new;
}

fn Atom State.symbol (&self, String sym)
{
	if (try Atom *existing = self.intern_table.get_ref (sym)) return *existing;
	Atom new = self.atom (Tag.SYM);
	new.as.symbol = sym.copy (mem);
	self.intern_table.set(new.as.symbol, new);
	return new;
}

fn Atom State.pair (&self, Atom head, Atom tail)
{
	Atom new = self.atom (Tag.PAIR);
	new.as.pair.head = head;
	new.as.pair.tail = tail;
	return new;
}

fn Atom State.primitive (&self, PrimitiveCallback cb)
{
	Atom new = self.atom (Tag.PRIM);
	new.as.primitive = cb;
	return new;
}

fn void State.rpush (&self, Atom atom)
{
	self.roots.push_front (atom);
}

fn Atom? State.rpop (&self)
{
	return self.roots.pop_first ();
}

fn void? State.define_symbol (&self, String sym, Atom value, Env env)
{
	Atom symbol_atom = self.symbol (sym);
	self.rpush (symbol_atom);
	fault result = @catch(env_define(env, self, symbol_atom, value));
	self.rpop ()!;
	return result~;
}

fn void? State.define_primitive (&self, String sym, PrimitiveCallback prim, Env env)
{
	Atom symbol_atom = self.symbol (sym);
	self.rpush (symbol_atom);
	Atom prim_atom = self.primitive (prim);
	self.rpush (prim_atom);
	fault result = @catch (env_define (env, self, symbol_atom, prim_atom));
	self.rpop()!;
	self.rpop()!;
	return result~;
}

<* @require args.tag == Tag.PAIR
   @require args.cdr().tag == Tag.PAIR
   @require args.cdr().cdr().tag == Tag.PAIR *>
fn Atom? State.evaluate_if (&self, Atom args, Env env)
{
	self.rpush (args);
	defer self.rpop()!!;

	Atom cond = args.car ();
	Atom then_br = args.cdr().car();
	Atom else_br = args.cdr().cdr().car();

	Atom evaluated_cond = self.evaluate (cond, env)!;
	if (evaluated_cond != nil)
	{
		return self.evaluate (then_br, env);
	}
	else
	{
		return self.evaluate (else_br, env);
	}
}

fn Atom? State.evaluate_do (&self, Atom args, Env env)
{
	Atom result = nil;
	while (args.tag == Tag.PAIR)
	{
		result = self.evaluate (args.car(), env)!;
		args = args.cdr();
	}
	return result;
}

fn Atom? State.evaluate_define (&self, Atom args, Env env)
{
	self.rpush (args);
	defer self.rpop()!!;

	Atom head = args;
	while (head != nil)
	{
		Atom symbol = head.car ();
		Atom value_expr = head.cdr().car();
		head = head.cdr().cdr();

		Atom value = self.evaluate (value_expr, env)!;
		self.rpush (value);
		defer self.rpop ()!!;
		
		env_define (env, self, symbol, value)!;
	}

	return nil;
}

fn Atom? State.evaluate_list (&self, Atom args, Env env)
{
	if (args == nil) return nil;

	self.rpush (args);

	Atom head = self.evaluate (args.car(), env)!;
	self.rpop()!!;

	self.rpush (head);
	defer self.rpop ()!!;

	Atom tail = self.evaluate_list (args.cdr(), env)!;
	self.rpush (tail);
	defer self.rpop ()!!;

	Atom result = self.pair (head, tail);

	return result;
}

fn Atom? State.apply (&self, Atom function, Atom args, Env parent)
{
	switch (function.tag)
	{
	case PRIM: return function.as.primitive (self, parent, args);
	default:
		return @errf (NOT_APPLICABLE, "Atom with tag `%d` can't be applied", function.tag);
	}
}

fn Atom? State.evaluate (&self, Atom expr, Env env)
{
	// Env *prev_env = self.gc.env;
	Env prev_env = self.gc.env;
	self.gc.env = env;
	defer self.gc.env = prev_env;

	switch (expr.tag)
	{
	case NIL:
	case NUM:
	case STR:
	case PRIM:
		return expr;
	case SYM:
		return env_get(env, expr)!;
	case PAIR:
		Atom head = expr.car();
		Atom tail = expr.cdr();

		if (head.tag == Tag.SYM)
		{
			if (head == self.symbol ("if")) return self.evaluate_if (tail, env);
			if (head == self.symbol ("do")) return self.evaluate_do (tail, env);
			if (head == self.symbol ("define")) return self.evaluate_define (tail, env);
		}

		Atom function = self.evaluate (head, env)!;
		self.rpush (function);
		defer self.rpop()!!;
		Atom args = self.evaluate_list (tail, env)!;
		self.rpush (args);
		defer self.rpop()!!;
		Atom result = self.apply (function, args, env)!;
		return result;
	}
}

fn Atom? prim_lt (State *self, Env env, Atom args)
{
	// already evaluated by `evaluate_list` called in `evaluate`
	Atom lhs = args.car();
	Atom rhs = args.cdr().car();

	if (lhs.tag != NUM || rhs.tag != NUM)
	{
		return @errf (TYPE_FUCKUP, "`lt` expected two `NUM` arguments, but it got something different ig");
	}

	if (lhs.as.number < rhs.as.number)
	{
		return self.number (420); // FIXME: maybe a `t` symbol?
	}
	else
	{
		return nil;
	}
}

fn Atom? prim_print (State *self, Env env, Atom value)
{
	// FIXME: replace with full serialization code
	switch (value.tag)
	{
	case STR: io::printf("%s", value.as.string);
	case SYM: io::printf("%s", value.as.symbol);
	case NUM: io::printf("%g", value.as.number);
	case NIL: io::print ("nil");
	case PRIM: io::printf ("prim<%p>", value.as.primitive);
	case PAIR:
		Atom cur = value;

		io::print ("(");
		while (cur != nil && cur.tag == PAIR)
		{
			Atom next = cur.cdr();
			
			prim_print (self, env, cur.car())!;

			if (!next || next == nil)
			{
				break;
			}
			else if (next.tag == PAIR)
			{
				io::print (" ");
				cur = next;
			}
			else
			{
				io::print (" . ");
				prim_print (self, env, next)!;
				break;
			}
		}
		io::print (")");
		break;
	}
	return nil;
}

faultdef TODO;
faultdef EOF;
faultdef INVALID_CHARACTER;
faultdef UNCLOSED_QUOTE;

struct ParsingError
{
	String msg;
	usz line, column;
}

enum TokenKind
{
	COMMENT,
	LPAREN,
	RPAREN,
	IDENTIFIER,
	NUMBER,
	STRING,
}

struct Token
{
	TokenKind kind;
	String view;
	usz loc;
}

fn Token token (TokenKind kind, char *ptr, usz len, usz loc)
{
	return { .kind=kind, .view=(String)ptr[:len], .loc=loc };
}

struct LexerData
{
	String src;
	usz pos;
}

fn void LexerData.init (&self, String src)
{
	self.src = src;
	self.pos = 0;
}

fn bool LexerData.at_end (&self)
{
	return self.pos == self.src.len;
}

fn char LexerData.get (&self, usz offset = 0)
{
	return self.src[self.pos + offset];
}

fn char *LexerData.get_ptr (&self, usz offset = 0)
{
	return &self.src[self.pos + offset];
}

fn char *LexerData.ptr_at (&self, usz pos)
{
	return &self.src[pos];
}

fn void LexerData.bump (&self, usz n = 1)
{
	self.pos = min((usz) self.src.len, self.pos + n);
}

fn void LexerData.skip_whitespace (&self)
{
	while (!self.at_end () && ascii::is_space (self.get ())) self.bump ();
}

fn Token? LexerData.lex_number (&self, ParsingError *out_err)
{
	usz start = self.pos;

	if (self.get () == '-' || self.get () == '+') self.bump ();
	while (!self.at_end () && ascii::is_digit (self.get ())) self.bump ();

	if (!self.at_end () && self.get () == '.')
	{
		self.bump ();

		while (!self.at_end () && ascii::is_digit (self.get ())) self.bump ();
	}

	if (!self.at_end () && ascii::to_lower (self.get ()) == 'e')
	{
		self.bump ();

		if (!self.at_end() && (self.get () == '-' || self.get () == '+')) self.bump ();
		while (!self.at_end () && ascii::is_digit (self.get ())) self.bump ();
	}

	return token (NUMBER, self.ptr_at (start), self.pos - start, start);
}

fn Token? LexerData.lex_string (&self, ParsingError *out_err)
{
	usz start = self.pos;
	self.bump ();

	while (!self.at_end () && self.get() != '"')
	{
		if (self.pos + 1 < self.src.len && self.get() == '\\') self.bump();
		self.bump();
	}

	if (self.at_end() && self.get() != '"')
	{
		if (out_err)
		{
			self.loc_to_err (out_err);
			out_err.msg = string::format (mem, "Unclosed string quote");
		}

		return UNCLOSED_QUOTE~;
	}

	self.bump();

	return token(STRING, self.ptr_at (start), self.pos - start, start);
}

fn bool LexerData.is_symbol_char (&self, char c)
{
	String chars = "!-_:./\\?~+=*^%";
	return (chars.contains_char(c) || ascii::is_alnum(c));
}

fn void LexerData.loc_to_err (&self, ParsingError *out_err)
{
	out_err.line = 1;
	out_err.column = 1;
	usz i = 0;

	foreach (char c : self.src) // fixme: can be done faster I think
	{
		if (c == '\n')
		{
			out_err.column = 1;
			out_err.line += 1;
		}
		else
		{
			out_err.column += 1;
		}

		i += 1;
		if (i >= self.pos) break;
	}
}

fn Token? LexerData.next (&self, ParsingError *out_err)
{
	self.skip_whitespace ();
	if (self.at_end ())
	{
		return EOF~;
	}

	char c = self.get ();
	Token t;

	switch (c)
	{
	case '(': t = token (LPAREN, self.get_ptr (), 1, self.pos); self.bump (); return t;
	case ')': t = token (RPAREN, self.get_ptr (), 1, self.pos); self.bump (); return t;
	}

	if (c == '-' || c == '+')
	{
		if (self.pos + 1 < self.src.len && ascii::is_digit (self.src[self.pos+1]))
		{
			return self.lex_number (out_err);
		}
	}

	if (ascii::is_digit (c) || c == '.') return self.lex_number (out_err);

	if (self.is_symbol_char (c))
	{
		usz start = self.pos;
		self.bump ();

		while (!self.at_end () && self.is_symbol_char (self.get())) self.bump ();

		return token (IDENTIFIER, self.ptr_at (start), self.pos - start, start);
	}

	if (c == '"')
	{
		return self.lex_string (out_err);
	}

	if (out_err)
	{
		self.loc_to_err (out_err);
		out_err.msg = string::format (mem, "Unparsable or unexpected character '%c", c);
	}

	return INVALID_CHARACTER~;
}

fn List{Token}? lex (String src, ParsingError *out_err = null)
{
	LexerData lexer;
	lexer.init (src);

	List{Token} tokens;
	tokens.init (mem);

	while (true)
	{
		Token? result = lexer.next (out_err);
		if (catch excuse = result)
		{
			if (excuse == EOF) break;
			return excuse~;
		}
		if (result.kind == COMMENT) continue;
		tokens.push (result);
	}

	return tokens;
}

struct ParserData
{
	List{Token} tokens;
	State *state;
	String source;
	sz head;
}

faultdef UNEXPECTED_EOF, UNBALANCED_PAREN, UNEXPECTED_TOKEN;

fn ParserData parser_init (State *state, List{Token} tokens, String source)
{
	return (ParserData) { tokens, state, source, 0 };
}

fn void ParserData.loc_to_err (&self, sz loc, ParsingError *out_err)
{
	out_err.line = 1;
	out_err.column = 1;
	sz i = 0;

	foreach (char c : self.source) // fixme: can be done faster I think
	{
		if (c == '\n')
		{
			out_err.column = 1;
			out_err.line += 1;
		}
		else
		{
			out_err.column += 1;
		}

		if (i >= loc) break;
		i += 1;
	}
}

fn Token? ParserData.peek (&self, ParsingError *out_err = null)
{
	if (self.head >= self.tokens.len())
	{
		if (out_err)
		{
			out_err.column = -1;
			out_err.line = -1;
			string::format (mem, "Unexpected end of file");
		}
		return UNEXPECTED_EOF~;
	}

	return self.tokens[self.head];
}

fn Token? ParserData.expect (&self, TokenKind kind, ParsingError *out_err = null)
{
	Token next = self.peek(out_err)!;
	TokenKind actual_kind = next.kind;
	if (actual_kind != kind)
	{
		if (out_err)
		{
			self.loc_to_err ((sz) next.loc, out_err);
			out_err.msg = string::format (mem, "Unexpected token: expected %s, got %s", TokenKind::names[kind], TokenKind::names[actual_kind]);
		}
		return UNEXPECTED_TOKEN~;
	}
	self.head += 1;
	return next;
}

fn Token? ParserData.next (&self, ParsingError *out_err = null)
{
	Token? t = self.peek ();
	self.head += 1;
	return t;
}

fn Atom? ParserData.parse_number (&self, ParsingError *out_err = null)
{
	Token num_tok = self.expect (NUMBER, out_err)!;
	double num = num_tok.view.to_double()!;
	return self.state.number (num);
}

fn Atom? ParserData.parse_string (&self, ParsingError *out_err = null)
{
	Token str_tok = self.expect (STRING, out_err)!;
	String str = str_tok.view.strip_prefix("\"").strip_suffix("\""); // FIXME: escaping
	return self.state.string (str);
}

fn Atom? ParserData.parse_symbol (&self, ParsingError *out_err = null)
{
	Token sym_tok = self.expect (IDENTIFIER, out_err)!;
	String sym = sym_tok.view;
	return self.state.symbol (sym);
}

fn Atom? ParserData.parse_list (&self, ParsingError *out_err = null)
{
	self.expect (LPAREN, out_err)!;
	return self.parse_list_tail (out_err);
}

fn Atom? ParserData.parse_list_tail (&self, ParsingError *out_err = null)
{
	Token tok = self.peek (out_err)!;

	if (tok.kind == RPAREN)
	{
		self.next (out_err)!;
		return self.state.pair (nil, nil);
	}
	
	Atom head_expr = self.parse (out_err)!;
	self.state.rpush (head_expr);

	tok = self.peek (out_err)!;
	if (tok.kind == RPAREN)
	{
		self.next (out_err)!;
		Atom result = self.state.pair (head_expr, nil);
		self.state.rpop ()!;
		return result;
	}

	Atom tail_expr = self.parse_list_tail (out_err)!;
	self.state.rpush (tail_expr);

	Atom result = self.state.pair (head_expr, tail_expr);
	self.state.rpop()!;
	self.state.rpop()!;

	return result;
}

fn Atom? ParserData.parse (&self, ParsingError *out_err = null)
{
	Token next = self.peek (out_err)!;

	switch (next.kind)
	{
	case COMMENT: return TODO~;
	case NUMBER: return self.parse_number (out_err);
	case STRING: return self.parse_string (out_err);
	case IDENTIFIER: return self.parse_symbol (out_err);
	case LPAREN: return self.parse_list (out_err);
	case RPAREN:
		if (out_err)
		{
			self.loc_to_err ((sz) next.loc, out_err);
			out_err.msg = "Unexpected trailing parenthesis";
		}
		return UNBALANCED_PAREN~;
	}
}

fn Atom? State.parse_string (&self, String sexpr, ParsingError *out_err = null)
{
	List{Token} tokens = lex (sexpr, out_err)!;
	ParserData parser = parser_init (self, tokens, sexpr);	
	Atom expr = parser.parse (out_err)!;
	tokens.free ();
	return expr;
}

fn void main ()
{
	/* Test expression */
	String sexpr = `(if (lt x 420)
		(print "uoooogghh~~!!")
		(print "lenin grzyb"))`;

	State s;
	
	s.init ();
	defer s.destroy ();

	Env e = s.global;

	s.define_symbol("nil", nil, e)!!;
	s.define_primitive("lt", &prim_lt, e)!!;
	s.define_primitive("print", &prim_print, e)!!;

	Atom fnnuy = s.number (2137);
	s.rpush (fnnuy);
	s.define_symbol("x", fnnuy, e)!!;
	s.rpop ()!!;

	Atom if_expr = s.parse_string (sexpr)!!;
	s.rpush (if_expr);

	Atom result = s.evaluate (if_expr, e)!!;
	s.rpop ()!!;
}

// TODO: closures
