#include <jsyn/util/gamma.hpp>

namespace jsyn {
namespace gamma {

/* gamma operation class */

operation::~operation()
{}

std::string
operation::debug_string() const
{
	return "GAMMA";
}

bool
operation::operator==(const jive::operation & other) const noexcept
{
	auto op = dynamic_cast<const gamma::operation*>(&other);
	return op
	    && op->nalternatives() == nalternatives();
}

std::unique_ptr<jive::operation>
operation::copy() const
{
	return std::unique_ptr<jive::operation>(new operation(*this));
}

/* gamma node class */

node::~node()
{}

node::eviterator
node::begin_ev()
{
	if (ninputs() < 2)
		return end_cv();

	return eviterator(input(1));
}

node::xviterator
node::being_xv()
{
	if (noutputs() == 0)
		return end_xv();

	return xviterator(output(0));
}

node::eviterator
node::end_ev()
{
	return eviterator(nullptr);
}

node::xviterator
node::end_xv()
{
	return xviterator(nullptr);
}

evinput *
node::input(size_t n) const noexcept
{
	return static_cast<evinput*>(structural_node::input(n));
}

xvoutput *
node::output(size_t n) const noexcept
{
	return static_cast<xvoutput*>(structural_node::output(n));
}

evinput *
node::add_entryvar(jive::output * origin)
{
	auto input = evinput::create(this, origin);
	for (size_t n = 0; n < nsubregions(); n++)
		evargument::create(subregion(n), input);

	return input;
}

xvoutput *
node::add_exitvar(const std::vector<jive::output*> & values)
{
	if (values.size() != nsubregions())
		throw compiler_error("Incorrect number of values.");

	auto output = xvoutput::create(this, values[0]->type());
	for (size_t n; n < nsubregions(); n++)
		xvresult::create(values[n], output);

	return output;
}

gamma::node *
node::create(jive::output * predicate)
{
	auto ct = dynamic_cast<const ctlttype*>(&predicate->type());
	if (!ct)
		throw compiler_error("Gamma predicate must be a control type.");

	gamma::operation op(ct->nalternatives());
	auto node = new gamma::node(predicate->region(), std::move(op));
	prdinput::create(node, predicate);

	return node;
}

gamma::node *
node::copy(
	jive::region * region,
	const std::vector<jive::output*> & operands) const
{
	return static_cast<gamma::node*>(jive::node::copy(region, operands));
}

gamma::node *
node::copy(jive::region * region, jive::substitution_map & smap) const
{
	auto gamma = create(region, smap.lookup(predicate()->origin()));

	/* add entry variables */
	std::vector<jive::substitution_map> rmaps(nsubregions());
	for (auto oev = begin_ev(); oev != end_ev(); oev++) {
		auto nev = gamma->add_entryvar(smap.lookup(oev->origin()));
		for (size_t n = 0; n < nev->narguments(); n++)
			rmaps[n].insert(oev->argument(n), nev->argument());
	}

	/* copy subregions */
	for (size_t n = 0; n < subregions(); n++)
		subregion(n)->copy(gamma->subregion(n), subregionmaps[n], false, false);

	/* add exit variables */
	for (auto oxv = begin_xv(); oxv != end_xv(); oxv++) {
		std::vector<jive::output*> operands;
		for (size_t n = 0; n < oxv->nresults(); n++)
			operands.push_back(rmaps[n].lookup(oxv->result(n)->origin()));
		auto nxv = gamma->add_exitvar(operands);
		smap.insert(oxv.output(), nxv);
	}

	return gamma;
}

}}
