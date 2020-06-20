#include <jsyn/sexpr.hpp>

namespace jsyn {

/* sexpr class */

sexpr::~sexpr()
{}

/* sexpr::terminal class */

sexpr::terminal::~terminal()
{}

/* sexpr::compound class */

sexpr::compound::~compound()
{}

/* sexpr format functions */

static void
collect_globrefs(const sexpr& e, std::unordered_set<std::string>& refs)
{
	if (const auto* c = dynamic_cast<const sexpr::compound*>(&e)) {
		if (c->kind() == "Global") {
			refs.insert(c->args()[0]->to_string());
		}
		for (const auto& arg : c->args()) {
			collect_globrefs(*arg, refs);
		}
	}
}

static std::unordered_set<std::string>
collect_globrefs(const sexpr& e)
{
	std::unordered_set<std::string> r;
	collect_globrefs(e, r);
	return r;
}

class varctx {
public:
	const std::string&
	lookup(int index) const;

	varctx
	push(std::string& name) const;

	std::size_t
	size() const noexcept
	{
		return names_by_index_.size();
	}

	std::string
	contextualize_global(std::string name) const;

	varctx
	enter_module(const std::string& name) const;

	bool
	has_name(const std::string& name) const noexcept;

	void
	push_globrefs(const std::unordered_set<std::string>& refs);

	bool
	name_available(const std::string& name) const noexcept;

private:
	void
	push_no_disambiguate(const std::string& name);

	std::vector<std::string> names_by_index_;
	std::unordered_set<std::string> names_;

	std::vector<std::string> mod_path_;

	std::unordered_set<std::string> globals_;
};

void
varctx::push_globrefs(const std::unordered_set<std::string>& refs)
{
	for (const auto& ref : refs) {
		globals_.insert(contextualize_global(ref));
	}
}

bool
varctx::has_name(const std::string& name) const noexcept
{
	return names_.find(name) != names_.end();
}

std::string
varctx::contextualize_global(std::string name) const
{
	for (const auto& part : mod_path_) {
		if (name.find(part) == 0) {
			name = name.substr(part.size() + 1);
		} else {
			break;
		}
	}
	return name;
}

varctx
varctx::enter_module(const std::string& name) const
{
	varctx res = *this;
	res.mod_path_.push_back(name);
	return res;
}

const std::string&
varctx::lookup(int index) const
{
	if (index <= names_.size()) {
		return names_by_index_[names_.size() - index];
	} else {
		static const std::string unknown = "???";
		return unknown;
	}
}

bool
varctx::name_available(const std::string& name) const noexcept
{
	return names_.find(name) == names_.end() && globals_.find(name) == globals_.end();
}

varctx
varctx::push(std::string& name) const
{
	varctx res = *this;
	if (name_available(name)) {
		res.push_no_disambiguate(name);
		return res;
	}
	for (std::size_t n = 0;; ++n) {
		std::string tmp = name + std::to_string(n);
		if (name_available(tmp)) {
			res.push_no_disambiguate(tmp);
			name = tmp;
			return res;
		}
	}

	return res;
}

void
varctx::push_no_disambiguate(const std::string& name)
{
	names_by_index_.push_back(name);
	names_.insert(name);
}

std::string format_name(const sexpr& e)
{
	const auto& name = dynamic_cast<const sexpr::compound&>(e);
	if (name.kind() == "Name") {
		return name.args()[0]->to_string();
	} else {
		return "_";
	}
}

static void
format_expr(
  const sexpr& e
, const varctx& ctx
, const std::string& indent
, std::string& out);

static void
format_prod(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	std::string name = format_name(*args[0]);
	varctx new_ctx = ctx.push(name);
	out += "forall (" + name + " : ";
	format_expr(*args[1], ctx, indent + "  ", out);
	out += "), ";
	format_expr(*args[2], new_ctx, indent, out);
}

static void
format_lambda(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	std::string name = format_name(*args[0]);
	varctx new_ctx = ctx.push(name);
	out += "fun (" + name + " : ";
	format_expr(*args[1], ctx, indent + "  ", out);
	out += ") => ";
	format_expr(*args[2], new_ctx, indent, out);
}

static void
format_branches(
  const sexpr& e
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	const auto& branches = dynamic_cast<const sexpr::compound&>(e);
	for (const auto& branch_e : branches.args()) {
		const auto& branch = dynamic_cast<const sexpr::compound&>(*branch_e);
		std::string constructor = branch.kind();
		std::size_t nargs = std::stoi(branch.args()[0]->to_string());
		const sexpr* e = branch.args()[1].get();
		constructor = ctx.contextualize_global(constructor);
		out += indent + "| " + constructor;

		varctx newctx = ctx;

		std::vector<std::string> unfold_args;

		for (std::size_t n = 0; n < nargs; ++n) {
			const auto& tmp = dynamic_cast<const sexpr::compound&>(*e);
			if (tmp.kind() == "Lambda") {
				std::string name = format_name(*tmp.args()[0]);
				newctx = newctx.push(name);
				out += " " + name;
				e = tmp.args()[2].get();
			} else {
				std::string name = "x" + std::to_string(n);
				while (newctx.has_name(name)) {
					name = name + '\'';
				}
				out += " " + name;
				unfold_args.push_back(name);
			}
		}
		out += " => ";
		for (const auto& arg : unfold_args) {
			out += "(";
		}
		format_expr(*e, newctx, indent + "  ", out);
		for (const auto& arg : unfold_args) {
			out += " " + arg + ")";
		}
		out += "\n";
	}
}

static void
format_case(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	const auto& ret = *args[1];
	const auto& match_expr = *args[2];
	const auto& branches = *args[3];
	out += "match ";
	format_expr(match_expr, ctx, indent + "    ", out);
	out += " return (";
	format_expr(ret, ctx, indent + "    ", out);
	out += ") (";
	format_expr(match_expr, ctx, indent + "    ", out);
	out += ") with\n";
	format_branches(branches, ctx, indent + "  ", out);
	out += indent + "end";
}

static void
format_global(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	out += ctx.contextualize_global(args[0]->to_string());
}

static void
format_local(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	int index = stoi(args[1]->to_string());
	out += ctx.lookup(index);
}

static void
format_sort(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	out += args[0]->to_string();
}

static void
format_letin(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	std::string name = format_name(*args[0]);
	varctx newctx = ctx.push(name);
	out += "let " + name + " := ";
	format_expr(*args[1], ctx, indent + "  ", out);
	out += " in\n" + indent;
	format_expr(*args[3], newctx, indent, out);
}

static void
format_app(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	bool first = true;
	for (const auto& arg : args) {
		if (first) {
			first = false;
		} else {
			out += " ";
		}
		out += "(";
		format_expr(*arg, ctx, indent + "  ", out);
		out += ")";
	}
}

static void
format_match(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	format_expr(*args[0], ctx, indent, out);
}

static void
format_fix_function(
  const std::string& name
, const sexpr::list& args
, const varctx& sigctx
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	auto type_sig = dynamic_cast<const sexpr::compound*>(args[1].get());
	auto body = dynamic_cast<const sexpr::compound*>(args[2].get());

	varctx newctx = ctx;
	varctx newsigctx = sigctx;
	out += name;
	while (type_sig->kind() == "Prod" && body->kind() == "Lambda") {
		std::string argname = format_name(*body->args()[0]);

		newctx = newctx.push(argname);
		out += " (" + argname + " : ";
		format_expr(*body->args()[1], newsigctx, indent + "  ", out);
		out += ")";
		newsigctx = newsigctx.push(argname);
		type_sig = dynamic_cast<const sexpr::compound*>(type_sig->args()[2].get());
		body = dynamic_cast<const sexpr::compound*>(body->args()[2].get());
	}
	out += " : ";
	format_expr(*type_sig, newsigctx, indent + "  ", out);
	out += " := ";
	format_expr(*body, newctx, indent + "  ", out);
}

static void
format_fix(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	std::string index = args[0]->to_string();

	varctx new_ctx = ctx;
	std::vector<std::string> names;
	for (std::size_t n = 1; n < args.size(); ++n) {
		const auto& fn = dynamic_cast<const sexpr::compound&>(*args[n]);
		std::string name = format_name(*fn.args()[0]);
		new_ctx = new_ctx.push(name);
		names.push_back(name);
	}

	out += "(fix ";
	bool first = true;
	for (std::size_t n = 1; n < args.size(); ++n) {
		if (first) {
			first = false;
		} else {
			out += " with ";
		}
		const auto& fn = dynamic_cast<const sexpr::compound&>(*args[n]);
		format_fix_function(names[n - 1], fn.args(), ctx, new_ctx, indent + "  ", out);
	}
	if (names.size() > 1) {
		out += " for " + names[std::stoi(index)];
	}
	out += ")";
}

static void
format_expr(
  const sexpr& e
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	const auto& expr = dynamic_cast<const sexpr::compound&>(e);
	if (expr.kind() == "Prod") {
		format_prod(expr.args(), ctx, indent, out);
	} else if (expr.kind() == "Global") {
		format_global(expr.args(), ctx, indent, out);
	} else if (expr.kind() == "Local") {
		format_local(expr.args(), ctx, indent, out);
	} else if (expr.kind() == "Sort") {
		format_sort(expr.args(), ctx, indent, out);
	} else if (expr.kind() == "App") {
		format_app(expr.args(), ctx, indent, out);
	} else if (expr.kind() == "Lambda") {
		format_lambda(expr.args(), ctx, indent, out);
	} else if (expr.kind() == "Case") {
		format_case(expr.args(), ctx, indent, out);
	} else if (expr.kind() == "Match") {
		format_match(expr.args(), ctx, indent, out);
	} else if (expr.kind() == "LetIn") {
		format_letin(expr.args(), ctx, indent, out);
	} else if (expr.kind() == "Fix") {
		format_fix(expr.args(), ctx, indent, out);
	} else {
		out += "UNHANDLED:" + expr.kind();
	}
}

static void
format_decl(
  const sexpr& e
, const varctx& ctx
, const std::string& indent
, std::string& out);

static void
format_module(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	std::string name = args[0]->to_string();
	out += indent + "Module " + name + ".\n";

	varctx newctx = ctx;
	size_t n = 0;
	while (n < name.size()) {
		std::size_t e = name.find('.', n + 1);
		newctx = newctx.enter_module(name.substr(n, e));
		if (e == std::string::npos) {
			break;
		} else {
			n = e + 1;
		}
	}
	const sexpr::compound& sb = dynamic_cast<const sexpr::compound&>(*args[1]);
	for (const auto& decl : sb.args()) {
		format_decl(*decl, newctx, indent + "  ", out);
	}
	out += indent + "End " + name + ".\n";
}

static void
format_definition(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	varctx newctx = ctx;
	for (const auto& arg : args) {
		newctx.push_globrefs(collect_globrefs(*arg));
	}

	std::string name = args[0]->to_string();
	out += indent + "Definition " + name + "\n";
	out += indent + "    : ";
	format_expr(*args[1], newctx, indent + "    ", out);
	out += " :=\n" + indent + "  ";
	format_expr(*args[2], newctx, indent + "  " , out);
	out += ".\n";
}

static void
format_one_constructor(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	std::string name = args[0]->to_string();
	out += "| " + name + " : ";
	format_expr(*args[1], ctx, indent + "  ", out);
}

static void
format_one_inductive(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	std::string name = args[0]->to_string();
	const auto& type = args[1];
	out += name + " : ";
	format_expr(*type, varctx{}, indent + "  ", out);
	out += " :=";
	for (size_t n = 2; n < args.size(); ++n) {
		const auto& constructor = dynamic_cast<const sexpr::compound&>(*args[n]);
		out += "\n" + indent;
		format_one_constructor(constructor.args(), ctx, indent + "  ", out);
	}
}

static void
format_inductive(
  const sexpr::list& args
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	varctx newctx = ctx;
	for (const auto& arg : args) {
		const auto& oib = dynamic_cast<const sexpr::compound&>(*arg);
		std::string name = oib.args()[0]->to_string();
		newctx = newctx.push(name);
	}

	for (const auto& arg : args) {
		newctx.push_globrefs(collect_globrefs(*arg));
	}

	bool first = true;
	for (const auto& arg : args) {
		const auto& oib = dynamic_cast<const sexpr::compound&>(*arg);
		if (first) {
			out += indent + "Inductive ";
			first = false;
		} else {
			out += "\n" + indent + "with ";
		}

		format_one_inductive(oib.args(), newctx, indent + "  ", out);
	}
	out += ".\n";
}

static void
format_decl(
  const sexpr& e
, const varctx& ctx
, const std::string& indent
, std::string& out)
{
	const sexpr::compound& decl = dynamic_cast<const sexpr::compound&>(e);
	if (decl.kind() == "Module") {
		format_module(decl.args(), ctx, indent, out);
	} else if (decl.kind() == "Definition") {
		format_definition(decl.args(), ctx, indent, out);
	} else if (decl.kind() == "Inductive") {
		format_inductive(decl.args(), ctx, indent, out);
	} else {
		out += "UNHANDLED:" + decl.kind();
	}
}

void
format_sexpr(
  const sexpr& e
, const std::string& indent
, std::string& out)
{
	if (auto c = dynamic_cast<const sexpr::compound*>(&e)) {
		if (c->kind() == "Module") {
			varctx ctx;
			format_module(c->args(), ctx, indent, out);
		}
	}
}

}
