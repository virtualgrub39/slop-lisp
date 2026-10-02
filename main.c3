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
alias PrimitiveCallback = fn Atom? (State*, Env*, Atom args);

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

struct Binding { Atom symbol; Atom value; }
struct Env
{
	Env *parent;
	List{Binding} bindings;
}

<* @require sym.tag == Tag.SYM *>
fn Atom? Env.get (Env* env, Atom sym)
{
	for (Env *e = env; e != null; e = e.parent)
	{
		foreach (b : e.bindings)
		{
			if (b.symbol == sym) return b.value; // pointer comparison
		}
	}
	return @errf(UNBOUND_SYMBOL, "symbol `%s` is unbound", sym.as.symbol);
}

<* @require sym.tag == Tag.SYM *>
fn void? Env.set (Env* env, Atom sym, Atom val)
{
	for (Env *e = env; e != null; e = e.parent)
	{
		foreach (b : e.bindings)
		{
			if (b.symbol == sym)
			{
				b.value = val;
				return;
			}
		}
	}
	return @errf(UNBOUND_SYMBOL, "symbol `%s` is undefined", sym.as.symbol);
}

fn void? Env.define (Env* env, Atom sym, Atom val)
{
	foreach (b : env.bindings)
	{
		if (b.symbol == sym)
		{
			return @errf(REDEFINED_SYMBOL, "tried to redefine symbol `%s`", sym.as.symbol);
		}
	}
	env.bindings.push_front((Binding){ sym, val });
}

const sz GC_SWEEP_FLOOR = 2;

struct State
{
	OrderedMap{String, Atom} intern_table; // makes sure, that only one identical symbol exists at once
	List{Atom} roots; // FIXME: replace with something, that doesn't allocate all the fucking time
	List{Atom} allocated; // for gc
	Env *env; // current env frame: for gc
}

fn void State.destroy (&self)
{
    self.roots.clear ();
    self.intern_table.clear ();

    GarbageCollector gc;
    gc.init (&self.allocated);
    gc.sweep ();

    self.roots.free ();
    self.allocated.free ();
    self.intern_table.free ();
}

faultdef UNBOUND_SYMBOL;
faultdef REDEFINED_SYMBOL;
faultdef TYPE_FUCKUP; // idk..

fn Atom State.atom (&self, Tag tag)
{
	if (self.allocated.size > GC_SWEEP_FLOOR) self.garbage_collect ();

	Atom new = mem::alloc (AtomData); // FIXME: dedicated atom area
	new.tag = tag;

	self.allocated.push_front (new);

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
	self.intern_table.set (sym, new);
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

struct GarbageCollector
{
	List{Atom} *allocated;
	HashSet{uptr} marked;
}

fn void GarbageCollector.init (&self, List{Atom} *allocated)
{
	self.allocated = allocated;
	self.marked.init (mem);
}

fn void GarbageCollector.mark (&self, Atom atom)
{
    Atom cur = atom;
    while (cur != nil && !self.marked.contains((uptr) cur))
    {
        self.marked.add ((uptr) cur);

        if (cur.tag == Tag.PAIR)
        {
            self.mark (cur.car());
            cur = cur.cdr();
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

        if (self.marked.contains((uptr) atom))
        {
			self.allocated.set_at(write_index, atom);
            write_index++;
        }
        else
        {
            atom.free();
        }
    }

    self.allocated.size = write_index;
	self.marked.free ();
}

fn void State.garbage_collect (&self)
{
	GarbageCollector gc;
	gc.init (&self.allocated);

	Env *e = self.env;
	while (e)
	{
		foreach (binding : e.bindings)
		{
			gc.mark (binding.symbol);
			gc.mark (binding.value);
		}
		e = e.parent;
	}

	foreach (atom : self.roots)
	{
		gc.mark (atom);
	}

	self.intern_table.@each(; key, value)
    {
        gc.mark (value);
    };

	gc.sweep ();
}

fn void? State.define_symbol (&self, String sym, Atom value, Env *env)
{
	Atom symbol_atom = self.symbol (sym);
	self.rpush (symbol_atom);
	fault result = @catch(env.define(symbol_atom, value));
	self.rpop ()!;
	return result~;
}

fn void? State.define_primitive (&self, String sym, PrimitiveCallback prim, Env *env)
{
	Atom symbol_atom = self.symbol (sym);
	self.rpush (symbol_atom);
	Atom prim_atom = self.primitive (prim);
	self.rpush (prim_atom);
	fault result = @catch (env.define (symbol_atom, prim_atom));
	self.rpop()!;
	self.rpop()!;
	return result~;
}

fn Atom? State.evaluate_if (&self, Atom args, Env *env)
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

fn Atom? State.evaluate_do (&self, Atom args, Env *env)
{
	Atom result = nil;
	while (args.tag == Tag.PAIR)
	{
		result = self.evaluate (args.car(), env)!;
		args = args.cdr();
	}
	return result;
}

fn Atom? State.evaluate_define (&self, Atom args, Env *env)
{
	Atom symbol = args.car ();
	Atom value_expr = args.cdr().car();

	Atom value = self.evaluate (value_expr, env)!;
	self.rpush (value);
	defer self.rpop ()!!;
	
	env.define (symbol, value)!;
	
	return nil;
}

fn Atom? State.evaluate_list (&self, Atom args, Env *env)
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

fn Atom? State.apply (&self, Atom function, Atom args, Env *parent)
{
	switch (function.tag)
	{
	case PRIM: return function.as.primitive (self, parent, args);
	default:
		unreachable("fuck you");
	}
}

fn Atom? State.evaluate (&self, Atom expr, Env *env)
{
	Env *prev_env = self.env;
	self.env = env;
	defer self.env = prev_env;

	switch (expr.tag)
	{
	case NIL:
	case NUM:
	case STR:
	case PRIM:
		return expr;
	case SYM:
		return env.get(expr)!;
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

fn Atom? prim_lt (State *self, Env *env, Atom args)
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

fn Atom? prim_print (State *self, Env *env, Atom value)
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

fn void main ()
{
	/* Test expression */
	/*
	(if (lt x 420)
		(print "uoooogghhh~~!!")
		(print "lenin grzyb"))
	*/
	/* this is about to be VERY ugly */

	State s;
	defer s.destroy ();

	Atom msg2 = s.string ("lenin grzyb");
	s.rpush (msg2);
	Atom else_print_arg = s.pair (msg2, nil);
	s.rpop()!!;
	s.rpush (else_print_arg);
	Atom else_print_sym = s.symbol("print");
	s.rpush (else_print_sym);
	Atom else_br = s.pair(else_print_sym, else_print_arg);
	s.rpop()!!;
	s.rpop()!!;
	s.rpush (else_br);
	Atom if_arg_1 = s.pair (else_br, nil);
	s.rpop ()!!;
	s.rpush (if_arg_1);
	Atom msg1 = s.string ("uoooogghhh~~!!");
	s.rpush (msg1);
	Atom then_print_arg = s.pair (msg1, nil);
	s.rpop()!!;
	s.rpush (then_print_arg);
	Atom then_print_sym = s.symbol("print");
	s.rpush (then_print_sym);
	Atom then_br = s.pair (then_print_sym, then_print_arg);
	s.rpop()!!;
	s.rpop()!!;
	s.rpush (then_br);
	Atom if_arg_2 = s.pair (then_br, if_arg_1);
	s.rpop()!!;
	s.rpop()!!;
	s.rpush (if_arg_2);
	Atom funny = s.number(420);
	s.rpush (funny);
	Atom cond_lt_arg_1 = s.pair (funny, nil);
	s.rpop()!!;
	s.rpush (cond_lt_arg_1);
	Atom cond_lt_x_sym = s.symbol ("x");
	s.rpush (cond_lt_x_sym);
	Atom cond_lt_args = s.pair (cond_lt_x_sym, cond_lt_arg_1);
	s.rpop()!!;
	s.rpop()!!;
	s.rpush (cond_lt_args);
	Atom cond_lt_sym = s.symbol("lt");
	s.rpush (cond_lt_sym);
	Atom cond = s.pair(cond_lt_sym, cond_lt_args);
	s.rpop()!!;
	s.rpop()!!;
	s.rpush (cond);
	Atom if_args = s.pair(cond, if_arg_2);
	s.rpop()!!;
	s.rpop()!!;
	s.rpush (if_args);
	Atom if_sym = s.symbol("if");
	s.rpush (if_sym);
	Atom if_expr = s.pair(if_sym, if_args);
	s.rpop()!!;
	s.rpop()!!;
	s.rpush (if_expr);

	// lmao

	Env e;
	s.env = &e;

	s.define_symbol("nil", nil, &e)!!;
	s.define_primitive("lt", &prim_lt, &e)!!;
	s.define_primitive("print", &prim_print, &e)!!;

	Atom fnnuy = s.number (2137);
	s.rpush (fnnuy);
	s.define_symbol("x", fnnuy, &e)!!;
	s.rpop ()!!;

	// I guess ready to evaluate?

	Atom result = s.evaluate (if_expr, &e)!!;
	s.rpop ()!!;
}
