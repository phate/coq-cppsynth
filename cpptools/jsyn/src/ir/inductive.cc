#include <jsyn/ir/inductive.hpp>

// FIXME: to be removed
#include <jsyn/util/assert.hpp>

namespace jsyn {
namespace inductive {

/* inductive operation class */

operation::~operation()
{}

std::string
operation::debug_string() const
{
	return "Inductive " + name();
}

bool
operation::operator==(const jive::operation & other) const noexcept
{
	auto op = dynamic_cast<const inductive::operation*>(&other);
	return op && op->name() == name();
}

std::unique_ptr<jive::operation>
operation::copy() const
{
	return std::unique_ptr<jive::operation>(new operation(*this));
}

/* inductive node class */

node::~node()
{}

inductive::node *
node::create(
	jive::region * parent,
	const std::string & name)
{
	inductive::operation op(name);
	auto node = new inductive::node(parent, std::move(op));

	return node;
}

inductive::node *
node::copy(
	jive::region * region,
	const std::vector<jive::output*> & operands) const
{
	return static_cast<inductive::node*>(jive::node::copy(region, operands));
}

inductive::node *
node::copy(jive::region * region, jive::substitution_map&) const
{
	auto inductive = create(this->region(), name());

	JSYN_ASSERT(0 && "FIXME: provide implementation");

	return inductive;
}

}}
