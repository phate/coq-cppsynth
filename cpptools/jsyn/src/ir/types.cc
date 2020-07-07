#include <jsyn/ir/types.hpp>

namespace jsyn {

/* function type */

fcttype::~fcttype()
{}

fcttype::fcttype(
	const std::vector<const jive::type*> & operands,
	const std::vector<const jive::type*> & results)
: jive::valuetype()
{
	for (const auto & type : operands)
		operands_.push_back(type->copy());

	for (const auto & type : results)
		results_.push_back(type->copy());
}

fcttype::fcttype(
	const std::vector<std::unique_ptr<jive::type>> & operands,
	const std::vector<std::unique_ptr<jive::type>> & results)
: jive::valuetype()
{
	for (const auto & type : operands)
		operands_.push_back(type->copy());

	for (const auto & type : results)
		results_.push_back(type->copy());
}

fcttype::fcttype(const fcttype & other)
: fcttype(other.operands_, other.results_)
{}

fcttype::fcttype(fcttype && other)
: jive::valuetype(other)
, operands_(std::move(other.operands_))
, results_(std::move(other.results_))
{}

fcttype &
fcttype::operator=(const fcttype & other)
{
	if (this == &other)
		return *this;

	operands_.clear();
	for (const auto & type : other.operands_)
		operands_.push_back(type->copy());

	results_.clear();
	for (const auto & type : other.results_)
		results_.push_back(type->copy());

	return *this;
}

fcttype &
fcttype::operator=(fcttype && other)
{
	if (this == &other)
		return *this;

	operands_ = std::move(other.operands_);
	results_ = std::move(other.results_);

	return *this;
}

std::string
fcttype::debug_string() const
{
	return "fct";
}

bool
fcttype::operator==(const jive::type & other) const noexcept
{
	auto rhs = fcttype::cast(&other);
	if (rhs == nullptr)
		return false;

	if (noperands() != rhs->noperands())
		return false;

	if (nresults() != rhs->nresults())
		return false;

	auto it = begin_operand();
	auto it2 = rhs->begin_operand();
	for (; it != end_operand(); it++, it2++) {
		if (*it != *it2)
			return false;
	}

	it = begin_result();
	it2 = rhs->begin_result();
	for(; it != end_result(); it++, it2++) {
		if (*it != *it2)
			return false;
	}

	return true;
}

std::unique_ptr<jive::type>
fcttype::copy() const
{
	return std::unique_ptr<jive::type>(new fcttype(*this));
}

}
