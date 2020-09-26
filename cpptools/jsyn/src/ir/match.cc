#include <jsyn/ir/match.hpp>

// FIXME: to be removed
#include <jsyn/util/assert.hpp>

namespace jsyn {
namespace match {

/* match operation class */

operation::~operation()
{}

std::string
operation::debug_string() const
{
	return "Match";
}

bool
operation::operator==(const jive::operation & other) const noexcept
{
	return dynamic_cast<const match::operation*>(&other);
}

std::unique_ptr<jive::operation>
operation::copy() const
{
	return std::unique_ptr<jive::operation>(new operation(*this));
}

/* match node class */

node::~node()
{}

match::node *
node::create(
	jive::region * parent,
	jive::output * operand)
{
	match::operation op;
	auto node = new match::node(parent, std::move(op));

	input::create(node, operand);

	/* FIXME: provide proper type */
	output::create(node, operand->type());

	return node;
}

match::node *
node::copy(
	jive::region * region,
	const std::vector<jive::output*> & operands) const
{
	return static_cast<match::node*>(jive::node::copy(region, operands));
}

match::node *
node::copy(jive::region * region, jive::substitution_map&) const
{
	//auto node = create(region, this->operation());

	JSYN_ASSERT(0 && "FIXME: provide implementation");

	return nullptr;//node;
}

/* match input class */

input::~input()
{}

/* match output class */

output::~output()
{}

}}

