#include "testnodes.hpp"

namespace jsyn {
namespace test {

/* simple operation */

simple_op::~simple_op()
{}

bool
simple_op::operator==(const operation & other) const noexcept
{
	auto op = dynamic_cast<const simple_op*>(&other);
	if (op == nullptr
	|| op->narguments() != narguments()
	|| op->nresults() != nresults())
		return false;

	for (size_t n = 0; n < narguments(); n++) {
		if (argument(n) != op->argument(n))
			return false;
	}

	for (size_t n = 0; n < nresults(); n++) {
		if (result(n) != op->result(n))
			return false;
	}

	return true;
}

std::string
simple_op::debug_string() const
{
	return "SIMPLE_TEST_NODE";
}

std::unique_ptr<jive::operation>
simple_op::copy() const
{
	return std::unique_ptr<jive::operation>(new simple_op(*this));
}

}}
