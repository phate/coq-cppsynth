#include <jsyn/ir/definition.hpp>

// FIXME: to be removed
#include <jsyn/util/assert.hpp>

namespace jsyn {
namespace definition {

/* definition operation class */

operation::~operation()
{}

std::string
operation::debug_string() const
{
	return "Definition " + name();
}

bool
operation::operator==(const jive::operation & other) const noexcept
{
	auto op = dynamic_cast<const definition::operation*>(&other);
	return op && op->name() == name();
}

std::unique_ptr<jive::operation>
operation::copy() const
{
	return std::unique_ptr<jive::operation>(new operation(*this));
}

/* definition node class */

node::~node()
{}

cvinput *
node::input(size_t n) const noexcept
{
	return static_cast<cvinput*>(structural_node::input(n));
}

definition::cvargument *
node::cvargument(size_t n) const noexcept
{
	return input(n)->argument();
}

cvargument *
node::add_ctxvar(jive::output * origin)
{
	auto input = cvinput::create(this, origin);
	return cvargument::create(subregion(), input);
}

definition::node *
node::create(
	jive::region * parent,
	const std::string & name)
{
	definition::operation op(name);
	auto node = new definition::node(parent, std::move(op));

	return node;
}

definition::node *
node::copy(
	jive::region * region,
	const std::vector<jive::output*> & operands) const
{
	return static_cast<definition::node*>(jive::node::copy(region, operands));
}

definition::node *
node::copy(jive::region * region, jive::substitution_map&) const
{
	auto definition = create(this->region(), name());

	JSYN_ASSERT(0 && "FIXME: provide implementation");

	return definition;
}

/* definition context variable input class */

cvinput::~cvinput()
{}

cvargument *
cvinput::argument() const noexcept
{
	return static_cast<cvargument*>(arguments.first());
}

/* lambda context variable argument class */

cvargument::~cvargument()
{}

}}
