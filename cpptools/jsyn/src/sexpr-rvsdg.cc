#include <jsyn/ir/case.hpp>
#include <jsyn/ir/constructor.hpp>
#include <jsyn/ir/definition.hpp>
#include <jsyn/ir/gamma.hpp>
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
	class scope final {
		public:
			scope(jive::region * region)
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

			static std::unique_ptr<scope>
			create(jive::region * region)
			{
				return std::make_unique<scope>(region);
			}

			// FIXME: provide iterators
			const std::unordered_map<std::string, jive::output*>
			map() const noexcept
			{
				return outputs_;
			}

		private:
			jive::region * region_;
			std::unordered_map<std::string, jive::output*> outputs_;
	};

public:
	context(jsyn::rvsdg & rvsdg)
	: rvsdg_(&rvsdg)
	{
		push_scope(rvsdg.graph().root());
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
		JSYN_ASSERT(!scopes_.empty());
		return scopes_.back()->region();
	}

	scope *
	last() const noexcept
	{
		JSYN_ASSERT(!scopes_.empty());
		return scopes_.back().get();
	}

	void
	push_scope(jive::region * region)
	{
		scopes_.push_back(scope::create(region));
	}

	void
	pop_scope()
	{
		JSYN_ASSERT(!scopes_.empty());
		scopes_.pop_back();
	}

	jive::output *
	lookup(const std::string & name)
	{
		auto mname = modulize_name(name);
		for (auto it = scopes_.rbegin(); it != scopes_.rend(); it++) {
			if ((*it)->contains(mname))
				return (*it)->lookup(mname);
		}

		JSYN_ASSERT(0 && "FIXME: implement");
//		auto graph = frames_[0]->region()->graph();
		//auto output = graph->add_import(name, );
	}

	void
	insert(const std::string & name, jive::output * output)
	{
		auto mname = modulize_name(name);
		scopes_.back()->insert(mname, output);
	}	

	void
	enter_module(jive::region * region, const std::string & name)
	{
		push_scope(region);
		module_paths_.push_back(name);
	}

	void
	exit_module()
	{
		JSYN_ASSERT(!module_paths_.empty());
		module_paths_.pop_back();
		pop_scope();
	}

private:
	std::string
	modulize_name(const std::string & name)
	{
		return module_paths_.back() + "." + name;
	}

	jsyn::rvsdg * rvsdg_;
	std::vector<std::string> module_paths_;
	std::vector<std::unique_ptr<scope>> scopes_;
};

static jive::output *
route(jive::output * output, jive::region * region)
{
	if (output->region() == region)
		return output;

	auto node = region->node();
	JSYN_ASSERT(node != nullptr);
	if (node->region() != region)
		output = route(output, node->region());

	//FIXME: implement as lookup table
	if (auto def = dynamic_cast<definition::node*>(node))
		return def->add_ctxvar(output);

	if (auto lambda = dynamic_cast<lambda::node*>(node))
		return lambda->add_ctxvar(output);

	if (auto gamma = dynamic_cast<gamma::node*>(node)) {
		auto input = gamma->add_entryvar(output);
		return input->argument(region->index());
	}

	JSYN_ASSERT(0 && "This should not have happened");
	return nullptr;
}

static std::string
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

	size_t nbranches = expr.args().size();

	for (auto & arg : expr.args()) {
		auto & branch = dynamic_cast<const sexpr::compound&>(*arg);
		JSYN_ASSERT(branch.args().size() == 2);

		std::string constname = branch.kind();
		auto nargs = std::stoi(branch.args()[0]->to_string());
//		auto & body = branch.args()[1];

		auto constructor = route(ctx.lookup(constname), ctx.region());

//		convert_expr(*branch, ctx);
	}

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

	/* FIXME: clean up */

	auto & match = *expr.args()[2];
	auto & branches = dynamic_cast<const sexpr::compound&>(*expr.args()[3]);

	auto cmp = convert_expr(match, ctx);

	std::vector<jive::output*> constructors;
	for (auto & arg : branches.args()) {
		auto & branch = dynamic_cast<const sexpr::compound&>(*arg);
		JSYN_ASSERT(branch.args().size() == 2);

		std::string constname = branch.kind();
		constructors.push_back(route(ctx.lookup(constname), ctx.region()));
	}

	auto predicate = match::operation::create(cmp, constructors);

	std::vector<jive::output*> results;
	auto gamma = gamma::node::create(predicate);
	for (size_t n = 0; n < gamma->nsubregions(); n++) {
		ctx.push_scope(gamma->subregion(n));

		auto & branch = dynamic_cast<const sexpr::compound&>(*branches.args()[n]);
		auto & body = branch.args()[1];

		results.push_back(convert_expr(*body, ctx));

		ctx.pop_scope();
	}

	return gamma->add_exitvar(results);
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

	ctx.push_scope(lambda->subregion());
	ctx.insert(argument, lambda->fctargument(0));

	auto result = convert_expr(body, ctx);

	ctx.pop_scope();

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
	JSYN_ASSERT(expr.args().size() == 1);

	auto sortname = expr.args()[0]->to_string();

	std::cout << sortname << "\n";

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

	/* FIXME: add de brujin index to name */

	return route(ctx.lookup(id), ctx.region());
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
	JSYN_ASSERT(expr.args().size() == 3);

	auto argname = expr.args()[0]->to_string();
	auto & argtype = *expr.args()[1];
//	auto & valtype = *expr.args()[2];

	std::cout << argname << "\n";

	convert_expr(argtype, ctx);
//	convert_expr(valtype, ctx);

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
	auto & type = *decl.args()[1];

	convert_expr(type, ctx);

	auto output = constructor::node::create(ctx.region(), name);
	ctx.insert(name, output);
}

static void
convert_oneinductive(const sexpr::compound & decl, context & ctx)
{
	JSYN_ASSERT(decl.kind() == "OneInductive");
	JSYN_ASSERT(decl.args().size() >= 2);

	auto name = decl.args()[0]->to_string();
//	auto & type = *decl.args()[1];

	auto inductive = inductive::node::create(ctx.region(), name);

	ctx.push_scope(inductive->subregion());
	for (size_t n = 2; n < decl.args().size(); n++)
		convert(*decl.args()[n], ctx);

	auto map = ctx.last()->map();	
	ctx.pop_scope();

	for (auto & pair : map) {
		auto output = inductive->add_constructor(dynamic_cast<constructor::output*>(pair.second));
		ctx.insert(pair.first, output);
	}
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
	auto & type = dynamic_cast<const sexpr::compound&>(*decl.args()[1]);
	auto & body = *decl.args()[2];

	convert_expr(type, ctx);

	auto definition = definition::node::create(ctx.region(), name);

	ctx.push_scope(definition->subregion());
	convert_expr(body, ctx);
	ctx.pop_scope();
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

	ctx.enter_module(module->subregion(), name);
	convert(sb, ctx);
	ctx.exit_module();
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
