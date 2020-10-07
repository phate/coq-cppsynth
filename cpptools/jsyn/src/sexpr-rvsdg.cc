#include <jsyn/ir/case.hpp>
#include <jsyn/ir/constructor.hpp>
#include <jsyn/ir/definition.hpp>
#include <jsyn/ir/inductive.hpp>
#include <jsyn/ir/lambda.hpp>
#include <jsyn/ir/match.hpp>
#include <jsyn/ir/module.hpp>
#include <jsyn/sexpr.hpp>
#include <jsyn/sexpr-rvsdg.hpp>
#include <jsyn/util/assert.hpp>
#include <jsyn/util/exception.hpp>

#include <stack>

namespace jsyn {

class context final {
	class frame final {
		public:
			frame(jive::region * region)
			: region_(region)
			{}

			jive::region *
			region() const noexcept
			{
				return region_;
			}

			bool
			contains(const std::string & name) const noexcept
			{
				return outputs_.find(name) != outputs_.end();
			}

			jive::output *
			lookup(const std::string & name) const noexcept
			{
				JSYN_ASSERT(contains(name));
				return outputs_.at(name);
			}

			void
			insert(const std::string & name, jive::output * output)
			{
				JSYN_ASSERT(!contains(name));
				outputs_[name] = output;
			}

			static std::unique_ptr<frame>
			create(jive::region * region)
			{
				return std::make_unique<frame>(region);
			}

		private:
			jive::region * region_;
			std::unordered_map<std::string, jive::output*> outputs_;
	};

public:
	context(jsyn::rvsdg & rvsdg)
	: rvsdg_(&rvsdg)
	{
		push_region(rvsdg.graph().root());
	}

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
		JSYN_ASSERT(!frames_.empty());
		return frames_.back()->region();
	}

	void
	push_region(jive::region * region)
	{
		frames_.push_back(frame::create(region));
	}

	void
	pop_region()
	{
		JSYN_ASSERT(!frames_.empty());
		frames_.pop_back();
	}

	jive::output *
	lookup(const std::string & name)
	{
		for (auto it = frames_.rbegin(); it != frames_.rend(); it++) {
			if ((*it)->contains(name))
				return (*it)->lookup(name);
		}

		JSYN_ASSERT(0 && "FIXME: implement");
//		auto graph = frames_[0]->region()->graph();
		//auto output = graph->add_import(name, );
	}

	void
	insert(const std::string & name, jive::output * output)
	{
		frames_.back()->insert(name, output);
	}	

private:
	jsyn::rvsdg * rvsdg_;
	std::vector<std::unique_ptr<frame>> frames_;
};

std::string
get_id(const sexpr::compound & expr)
{
	JSYN_ASSERT(expr.kind() == "Name");
	JSYN_ASSERT(expr.args().size() == 1);

	return expr.args()[0]->to_string();
}

static jive::output *
convert_expr(const sexpr & expr, context & ctx);

static jive::output *
convert_fix(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Fix");

	//FIXME
	JSYN_ASSERT(0 && "Unhandled");
	return nullptr;
}

static jive::output *
convert_letin(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "LetIn");

	//FIXME
	JSYN_ASSERT(0 && "Unhandled");
	return nullptr;
}


static jive::output *
convert_branches(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Branches");

	for (auto & branch : expr.args())
		convert_expr(*branch, ctx);

//	JSYN_ASSERT(0 && "Unhandled");
	return nullptr;
}

static jive::output *
convert_match(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Match");
	JSYN_ASSERT(expr.args().size() == 1);

	auto & operand = *expr.args()[0];

	return convert_expr(operand, ctx);
}

static jive::output *
convert_case(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Case");
	JSYN_ASSERT(expr.args().size() == 4);

	auto & match = *expr.args()[2];
	auto & branches = *expr.args()[3];

	auto operand = convert_expr(match, ctx);
	auto node = match::node::create(ctx.region(), operand);

	ctx.push_region(node->subregion());

	convert_expr(branches, ctx);
	//convert_expr(match, ctx);

	ctx.pop_region();

	//FIXME
//	JSYN_ASSERT(0 && "Unhandled");
	return node->output(0);
}

static jive::output *
convert_lambda(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Lambda");
	JSYN_ASSERT(expr.args().size() == 3);

	auto argument = get_id(dynamic_cast<const sexpr::compound&>(*expr.args()[0]));
//	auto & type = dynamic_cast<const sexpr::compound&>(*decl.args()[1]);
	auto & body = *expr.args()[2];
//dynamic_cast<const sexpr::compound&>(*expr.args()[2]);

	//FIXME: to be removed
	auto vt = dummytype::create();
	auto ft = fcttype::create({vt.get()}, {vt.get()});

	/* FIXME: lambda node seem to have no name */
	auto lambda = lambda::node::create(ctx.region(), *dynamic_cast<const fcttype*>(ft.get()), "?");

	ctx.push_region(lambda->subregion());
	ctx.insert(argument, lambda->fctargument(0));

	auto result = convert_expr(body, ctx);

	ctx.pop_region();

	return nullptr;//lambda->finalize({result});
}

static jive::output *
convert_app(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "App");

	//FIXME
//	JSYN_ASSERT(0 && "Unhandled");
	return nullptr;
}

static jive::output *
convert_sort(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Sort");

	//FIXME
	JSYN_ASSERT(0 && "Unhandled");
	return nullptr;
}

static jive::output *
convert_local(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Local");
	JSYN_ASSERT(expr.args().size() == 2);

	auto id = expr.args()[0]->to_string();

	return ctx.lookup(id);
}

static jive::output *
convert_global(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Global");
	JSYN_ASSERT(expr.args().size() == 1);

	auto name = expr.args()[0]->to_string();

	//FIXME:
	JSYN_ASSERT(0 && "Undhandled");
	return nullptr;
}

static jive::output *
convert_prod(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Prod");

	// FIXME
	JSYN_ASSERT(0 && "Unhandled");
	return nullptr;
}

static jive::output *
convert_expr(const sexpr::compound & expr, context & ctx)
{
	static std::unordered_map<
		std::string
	, jive::output*(*)(const sexpr::compound&, context&)
	> map({
	  {"Prod",     convert_prod},     {"Global", convert_global}
	, {"Local",    convert_local},    {"Sort",   convert_sort}
	, {"App",      convert_app},      {"Lambda", convert_lambda}
	, {"Case",     convert_case},     {"Match",  convert_match}
	, {"LetIn",    convert_letin},    {"Fix",    convert_fix}
	, {"Branches", convert_branches}
	});

	if (map.find(expr.kind()) == map.end())
		throw compilation_error("Unknown expression: " + expr.kind());

	return map[expr.kind()](expr, ctx);
}

static jive::output *
convert_expr(const sexpr & expr, context & ctx)
{
	auto & compound = dynamic_cast<const sexpr::compound&>(expr);
	return convert_expr(compound, ctx);
}

static void
convert(const sexpr & expr, context & ctx);

static void
convert_constructor(const sexpr::compound & decl, context & ctx)
{
	JSYN_ASSERT(decl.kind() == "Constructor");
	JSYN_ASSERT(decl.args().size() == 2);

	auto name = decl.args()[0]->to_string();

	auto constructor = constructor::node::create(ctx.region(), name);
}

static void
convert_oneinductive(const sexpr::compound & decl, context & ctx)
{
	JSYN_ASSERT(decl.kind() == "OneInductive");
	JSYN_ASSERT(decl.args().size() >= 2);

	auto name = decl.args()[0]->to_string();

	auto inductive = inductive::node::create(ctx.region(), name);

	ctx.push_region(inductive->subregion());
	for (size_t n = 2; n < decl.args().size(); n++)
		convert(*decl.args()[n], ctx);
	ctx.pop_region();
}

static void
convert_inductive(const sexpr::compound & decl, context & ctx)
{
	JSYN_ASSERT(decl.kind() == "Inductive");
	JSYN_ASSERT(decl.args().size() == 1);

	auto & body = *decl.args()[0];


//	ctx.push_region()
	convert(body, ctx);
	// FIXME
//	JSYN_ASSERT(0 && "Unhandled");
}

static void
convert_definition(const sexpr::compound & decl, context & ctx)
{
	JSYN_ASSERT(decl.kind() == "Definition");
	JSYN_ASSERT(decl.args().size() == 3);

	auto name = decl.args()[0]->to_string();
//	auto & type = dynamic_cast<const sexpr::compound&>(*decl.args()[1]);
	auto & body = *decl.args()[2];

	auto definition = definition::node::create(ctx.region(), name);

	ctx.push_region(definition->subregion());
	convert_expr(body, ctx);
	ctx.pop_region();
}

static void
convert_structurebody(const sexpr::compound & decl, context & ctx)
{
	JSYN_ASSERT(decl.kind() == "StructureBody");

	for (auto & arg : decl.args())
		convert(*arg, ctx);
}

static void
convert_module(const sexpr::compound & decl, context & ctx)
{
	JSYN_ASSERT(decl.kind() == "Module");
	JSYN_ASSERT(decl.args().size() == 2);

	auto name = decl.args()[0]->to_string();
	auto & sb = *decl.args()[1];

	auto module = module::node::create(ctx.region(), name);

	ctx.push_region(module->subregion());
	convert(sb, ctx);
	ctx.pop_region();
}

static void
convert(const sexpr::compound & decl, context & ctx)
{
	static std::unordered_map<
		std::string
	, void(*)(const sexpr::compound&, context&)
	> map({
	  {"Module",        convert_module}
	, {"StructureBody", convert_structurebody}
	, {"Definition",    convert_definition}
	, {"Inductive",     convert_inductive}
	, {"OneInductive",  convert_oneinductive}
	, {"Constructor",   convert_constructor}
	});

	if (map.find(decl.kind()) == map.end())
		throw jsyn::compilation_error("Unknown declaration: " + decl.kind());

	map[decl.kind()](decl, ctx);
}

static void
convert(const sexpr & expr, context & ctx)
{
	auto & compound = dynamic_cast<const sexpr::compound&>(expr);
	convert(compound, ctx);
}

std::unique_ptr<rvsdg>
convert_sexpr(const sexpr & e)
{
	auto rvsdg = rvsdg::create();

	context ctx(*rvsdg);
	convert(e, ctx);

	return rvsdg;
}

}
