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
	auto op = dynamic_cast<const match::operation*>(&other);
	return op
	    && op->type() == type();
}

std::unique_ptr<jive::operation>
operation::copy() const
{
	return std::unique_ptr<jive::operation>(new operation(*this));
}

}}
