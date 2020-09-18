#include <jsyn/ir/definition.hpp>
#include <jsyn/ir/module.hpp>
#include <jsyn/sexpr.hpp>
#include <jsyn/sexpr-rvsdg.hpp>
#include <jsyn/util/assert.hpp>
#include <jsyn/util/exception.hpp>

#include <stack>

namespace jsyn {

class context final {
public:
	context(jsyn::rvsdg & rvsdg)
	: rvsdg_(&rvsdg)
	{}

	context(const context&) = delete;

	context(context&&) = delete;

	context&
	operator=(const context&) = delete;

	context&
	operator=(context&&) = delete;

	jsyn::rvsdg &
	rvsdg() noexcept
	{
		return *rvsdg_;
	}

	jive::region *
	region() const noexcept
	{
		return regions_.top();
	}

	void
	push_region(jive::region * region)
	{
		regions_.push(region);
	}

	void
	pop_region()
	{
		regions_.pop();
	}

private:
	jsyn::rvsdg * rvsdg_;
	std::stack<jive::region*> regions_;
};

static void
convert_fix(const sexpr::compound & expr)
{
	JSYN_ASSERT(expr.kind() == "Fix");

	//FIXME
	JSYN_ASSERT(0 && "Unhandled");
}

static void
convert_letin(const sexpr::compound & expr)
{
	JSYN_ASSERT(expr.kind() == "LetIn");

	//FIXME
	JSYN_ASSERT(0 && "Unhandled");
}

static void
convert_match(const sexpr::compound & expr)
{
	JSYN_ASSERT(expr.kind() == "Match");

	//FIXME
	JSYN_ASSERT(0 && "Unhandled");
}

static void
convert_case(const sexpr::compound & expr)
{
	JSYN_ASSERT(expr.kind() == "Case");

	//FIXME
	JSYN_ASSERT(0 && "Unhandled");
}

static void
convert_lambda(const sexpr::compound & expr)
{
	JSYN_ASSERT(expr.kind() == "Lambda");

	//FIXME
	JSYN_ASSERT(0 && "Unhandled");
}

static void
convert_app(const sexpr::compound & expr)
{
	JSYN_ASSERT(expr.kind() == "App");

	//FIXME
	JSYN_ASSERT(0 && "Unhandled");
}

static void
convert_sort(const sexpr::compound & expr)
{
	JSYN_ASSERT(expr.kind() == "Sort");

	//FIXME
	JSYN_ASSERT(0 && "Unhandled");
}

static void
convert_local(const sexpr::compound & expr)
{
	JSYN_ASSERT(expr.kind() == "Local");

	// FIXME
	JSYN_ASSERT(0 && "Unhandled");
}

static void
convert_global(const sexpr::compound & expr)
{
	JSYN_ASSERT(expr.kind() == "Global");

	// FIXME
	JSYN_ASSERT(0 && "Undhandled");
}

static void
convert_prod(const sexpr::compound & expr)
{
	JSYN_ASSERT(expr.kind() == "Prod");

	// FIXME
	JSYN_ASSERT(0 && "Unhandled");
}

static void
convert_expr(const sexpr::compound & expr, context & ctx)
{
	std::unordered_map<std::string, void(*)(const sexpr::compound&)> map({
	  {"Prod",  convert_prod},  {"Global", convert_global}
	, {"Local", convert_local}, {"Sort",   convert_sort}
	, {"App",   convert_app},   {"Lambda", convert_lambda}
	, {"Case",  convert_case},  {"Match",  convert_match}
	, {"LetIn", convert_letin}, {"Fix",    convert_fix}
	});

	if (map.find(expr.kind()) == map.end())
		throw compilation_error("Unknown expression: " + expr.kind());

	map[expr.kind()](expr);
}

static void
convert_inductive(const sexpr::compound & decl, context & ctx)
{
	JSYN_ASSERT(decl.kind() == "Inductive");

	// FIXME
	JSYN_ASSERT(0 && "Unhandled");
}

static void
convert_definition(const sexpr::compound & decl, context & ctx)
{
	JSYN_ASSERT(decl.kind() == "Definition");
	JSYN_ASSERT(decl.args().size() == 3);

	auto name = decl.args()[0]->to_string();
//	auto & type = dynamic_cast<const sexpr::compound&>(*decl.args()[1]);
	auto & body = dynamic_cast<const sexpr::compound&>(*decl.args()[2]);

	auto definition = definition::node::create(ctx.region(), name);

	ctx.push_region(definition->subregion());
	convert_expr(body, ctx);
	ctx.pop_region();
}

static void
convert(const sexpr::compound & decl, context & ctx);

static void
convert_module(const sexpr::compound & decl, context & ctx)
{
	JSYN_ASSERT(decl.kind() == "Module");
	JSYN_ASSERT(decl.args().size() == 2);

	auto name = decl.args()[0]->to_string();
	auto & sb = dynamic_cast<const sexpr::compound&>(*decl.args()[1]); 

	auto module = module::node::create(ctx.region(), name);

	ctx.push_region(module->subregion());
	for (auto & decl : sb.args())
		convert(dynamic_cast<const sexpr::compound&>(*decl), ctx);
	ctx.pop_region();
}

static void
convert(const sexpr::compound & decl, context & ctx)
{
	static std::unordered_map<
		std::string
	, void(*)(const sexpr::compound&, context&)
	> map({
	  {"Module",     convert_module}
	, {"Definition", convert_definition}
	, {"Inductive",  convert_inductive}
	});

	if (map.find(decl.kind()) == map.end())
		throw jsyn::compilation_error("Unknown declaration: " + decl.kind());

	map[decl.kind()](decl, ctx);
}

std::unique_ptr<rvsdg>
convert_sexpr(const sexpr & e)
{
	auto rvsdg = rvsdg::create();

	context ctx(*rvsdg);
	convert(*dynamic_cast<const sexpr::compound*>(&e), ctx);

	return rvsdg;
}

}
