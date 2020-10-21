#include <jsyn/ir/constructor.hpp>

// FIXME: to be removed
#include <jsyn/util/assert.hpp>

namespace jsyn {
namespace constructor {

/* constructor operation class */

operation::~operation()
{}

std::string
operation::debug_string() const
{
	return "Constructor " + name();
}

bool
operation::operator==(const jive::operation & other) const noexcept
{
	auto op = dynamic_cast<const constructor::operation*>(&other);
	return op && op->name() == name();
}

std::unique_ptr<jive::operation>
operation::copy() const
{
	return std::unique_ptr<jive::operation>(new operation(*this));
}

/* constructor node class */

node::~node()
{}

constructor::output *
node::create(
	jive::region * parent,
	const std::string & name)
{
	constructor::operation op(name);
	auto node = new constructor::node(parent, std::move(op));

	return output::create(node);
}

constructor::node *
node::copy(
	jive::region * region,
	const std::vector<jive::output*> & operands) const
{
	return static_cast<constructor::node*>(jive::node::copy(region, operands));
}

constructor::node *
node::copy(jive::region * region, jive::substitution_map&) const
{
	auto output = create(this->region(), name());

	JSYN_ASSERT(0 && "FIXME: provide implementation");

	return output->node();
}

/* constructor output class */

output::~output()
{}

}}

