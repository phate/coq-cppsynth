#include <jsyn/ir/definition.hpp>
#include <jsyn/ir/lambda.hpp>
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

		auto graph = frames_[0]->region()->graph();
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
get_name(const sexpr::compound & expr)
{
	JSYN_ASSERT(expr.kind() == "Name");
	JSYN_ASSERT(expr.args().size() == 1);

	return expr.args()[0]->to_string();
}

static jive::output *
convert_expr(const sexpr::compound & expr, context & ctx);

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
convert_match(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Match");

	//FIXME
	JSYN_ASSERT(0 && "Unhandled");
	return nullptr;
}

static jive::output *
convert_case(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Case");

	//FIXME
//	JSYN_ASSERT(0 && "Unhandled");
	return nullptr;
}

static jive::output *
convert_lambda(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Lambda");
	JSYN_ASSERT(expr.args().size() == 3);

	auto argname = get_name(dynamic_cast<const sexpr::compound&>(*expr.args()[0]));
//	auto & type = dynamic_cast<const sexpr::compound&>(*decl.args()[1]);
	auto & body = dynamic_cast<const sexpr::compound&>(*expr.args()[2]);

	//FIXME: to be removed
	auto vt = dummytype::create();
	auto ft = fcttype::create({vt.get()}, {vt.get()});

	auto lambda = lambda::node::create(ctx.region(), *dynamic_cast<const fcttype*>(ft.get()), "?");

	ctx.push_region(lambda->subregion());
	ctx.insert(argname, lambda->fctargument(0));

	auto result = convert_expr(body, ctx);

	ctx.pop_region();

	return lambda->finalize({result});
	//FIXME
	//JSYN_ASSERT(0 && "Unhandled");
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

	// FIXME
	JSYN_ASSERT(0 && "Unhandled");
	return nullptr;
}

static jive::output *
convert_global(const sexpr::compound & expr, context & ctx)
{
	JSYN_ASSERT(expr.kind() == "Global");
	JSYN_ASSERT(expr.args().size() == 1);

	auto name = expr.args()[0]->to_string();

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
	  {"Prod",  convert_prod},  {"Global", convert_global}
	, {"Local", convert_local}, {"Sort",   convert_sort}
	, {"App",   convert_app},   {"Lambda", convert_lambda}
	, {"Case",  convert_case},  {"Match",  convert_match}
	, {"LetIn", convert_letin}, {"Fix",    convert_fix}
	});

	if (map.find(expr.kind()) == map.end())
		throw compilation_error("Unknown expression: " + expr.kind());

	return map[expr.kind()](expr, ctx);
}

static void
convert_inductive(const sexpr::compound & decl, context & ctx)
{
	JSYN_ASSERT(decl.kind() == "Inductive");

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
