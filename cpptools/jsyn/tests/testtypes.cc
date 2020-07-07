#include "testtypes.hpp"

namespace jsyn {
namespace test {

/* valuetype class */

valuetype::~valuetype()
{}

std::string
valuetype::debug_string() const
{
	return "TESTVALUETYPE";
}

bool
valuetype::operator==(const jive::type & other) const noexcept
{
	return jive::is<valuetype>(other);
}

std::unique_ptr<jive::type>
valuetype::copy() const
{
	return std::unique_ptr<jive::type>(new valuetype(*this));
}

/* statetype class */

statetype::~statetype()
{}

std::string
statetype::debug_string() const
{
	return "TESTSTATETYPE";
}

bool
statetype::operator==(const jive::type & other) const noexcept
{
	return jive::is<statetype>(other);
}

std::unique_ptr<jive::type>
statetype::copy() const
{
	return std::unique_ptr<jive::type>(new statetype(*this));
}

}}
