#include <jsyn/ir/case.hpp>

// FIXME: to be removed
#include <jsyn/util/assert.hpp>

namespace jsyn {
namespace casee {

/* casee operation class */

operation::~operation()
{}

std::string
operation::debug_string() const
{
	return "Case";
}

bool
operation::operator==(const jive::operation & other) const noexcept
{
	return dynamic_cast<const casee::operation*>(&other);
}

std::unique_ptr<jive::operation>
operation::copy() const
{
	return std::unique_ptr<jive::operation>(new operation(*this));
}

/* casee node class */

node::~node()
{}

casee::node *
node::create(jive::region * parent)
{
	casee::operation op;
	auto node = new casee::node(parent, std::move(op));

	return node;
}

casee::node *
node::copy(
	jive::region * region,
	const std::vector<jive::output*> & operands) const
{
	return static_cast<casee::node*>(jive::node::copy(region, operands));
}

casee::node *
node::copy(jive::region * region, jive::substitution_map&) const
{
	auto node = create(this->region());

	JSYN_ASSERT(0 && "FIXME: provide implementation");

	return node;
}

}}
