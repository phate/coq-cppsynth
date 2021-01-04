#include <jsyn/ir/phi.hpp>

#include <jive/rvsdg/substitution.hpp>

namespace jsyn {
namespace phi {

/* phi operation */

operation::~operation()
{}

std::string
operation::debug_string() const
{
	return "PHI";
}

bool
operation::operator==(const jive::operation & other) const noexcept
{
	return this == &other;
}

std::unique_ptr<jive::operation>
operation::copy() const
{
	return std::unique_ptr<jive::operation>(new operation(*this));
}

/* phi node */

node::~node()
{}

node::cviterator
node::begin_cv()
{
	if (ninputs() == 0)
		return end_cv();

	return cviterator(input(0));
}

node::cvconstiterator
node::begin_cv() const
{
	if (ninputs() == 0)
		return end_cv();

	return cvconstiterator(input(0));
}

node::rviterator
node::begin_rv()
{
	if (noutputs() == 0)
		return end_rv();

	return rviterator(output(0));
}

node::rvconstiterator
node::begin_rv() const
{
	if (noutputs() == 0)
		return end_rv();

	return rvconstiterator(output(0));
}

node::cviterator
node::end_cv()
{
	return cviterator(nullptr);
}

node::cvconstiterator
node::end_cv() const
{
	return cvconstiterator(nullptr);
}

node::rviterator
node::end_rv()
{
	return rviterator(nullptr);
}

node::rvconstiterator
node::end_rv() const
{
	return rvconstiterator(nullptr);
}

cvinput *
node::input(size_t n) const noexcept
{
	return static_cast<cvinput*>(structural_node::input(n));
}

rvoutput *
node::output(size_t n) const noexcept
{
	return static_cast<rvoutput*>(structural_node::output(n));
}

rvoutput *
node::add_rvvar(const jive::type & type)
{
	auto argument = rvargument::create(subregion(), type);
	auto output = rvoutput::create(this, argument);
	rvresult::create(subregion(), argument, output);
	argument->output_ = output;

	return output;
}

cvargument *
node::add_ctxvar(jive::output * origin)
{
	auto input = cvinput::create(this, origin);
	return cvargument::create(subregion(), input);
}

phi::node *
node::create(jive::region * parent)
{
	phi::operation op;
	return new phi::node(parent, std::move(op));
}

phi::node *
node::create(
	jive::region * parent,
	const std::vector<const jive::type*> & rvtypes)
{
	auto node = create(parent);

	for (auto & rvtype : rvtypes)
		node->add_rvvar(*rvtype);

	return node;
}

phi::node *
node::copy(
	jive::region * region,
	const std::vector<jive::output*> & operands) const
{
	return static_cast<phi::node*>(jive::node::copy(region, operands));
}

phi::node *
node::copy(
	jive::region * region,
	jive::substitution_map & smap) const
{
	auto phi = create(region);

	/* add recursion variables */
	jive::substitution_map subregionmap;
	for (auto it = begin_rv(); it != end_rv(); it++) {
		auto output = phi->add_rvvar(it->type());
		subregionmap.insert(it->argument(), output->argument());
		smap.insert(it.value(), output);
	}

	/* add context variables */
	for (auto it = begin_cv(); it != end_cv(); it++) {
		auto & origin = smap.lookup(*it->origin());
		auto newcv = phi->add_ctxvar(&origin);
		subregionmap.insert(it->argument(), newcv);
	}

	/* copy subregion */
	subregion()->copy(phi->subregion(), subregionmap, false, false);

	/* finalize recursion variables */
	for (auto it = begin_rv(); it != end_rv(); it++) {
		auto & origin = subregionmap.lookup(*it->result()->origin());
		phi->output(it->index())->finalize(&origin);
	}

	return phi;
}

/* phi context variable input */

cvinput::~cvinput()
{}

/* phi context variable argument */

cvargument::~cvargument()
{}

/* phi recursion variable output */

rvoutput::~rvoutput()
{}

/* phi recursion variable argument */

rvargument::~rvargument()
{}

/* phi recursion variable result */

rvresult::~rvresult()
{}

}}
