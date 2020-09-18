#include <jsyn/ir/module.hpp>

//FIXME: to be removed
#include <jsyn/util/assert.hpp>

namespace jsyn {
namespace module {

/* module operation class */

operation::~operation()
{}

std::string
operation::debug_string() const
{
	return "Module " + name();
}

bool
operation::operator==(const jive::operation & other) const noexcept
{
	auto op = dynamic_cast<const module::operation*>(&other);
	return op && op->name() == name();
}

std::unique_ptr<jive::operation>
operation::copy() const
{
	return std::unique_ptr<jive::operation>(new operation(*this));
}

/* module node class */

node::~node()
{}

module::node *
node::create(
	jive::region * parent,
	const std::string & name)
{
	module::operation op(name);
	auto node = new module::node(parent, std::move(op));

	return node;
}

module::node *
node::copy(
	jive::region * region,
	const std::vector<jive::output*> & operands) const
{
	return static_cast<module::node*>(jive::node::copy(region, operands));
}

module::node *
node::copy(jive::region*, jive::substitution_map&) const
{
	auto module = create(this->region(), name());

	JSYN_ASSERT(0 && "FIXME: provide implementation");

	return module;
}

}}
