#ifndef JSYN_TEST_TESTTYPES_HPP
#define JSYN_TEST_TESTTYPES_HPP

#include <jive/rvsdg/type.hpp>

namespace jsyn {
namespace test {

/* valuetype class */

class valuetype final : public jive::valuetype {
public:
	~valuetype() override;

	constexpr
	valuetype() noexcept
	: jive::valuetype()
	{}

	virtual std::string
	debug_string() const override;

	virtual bool
	operator==(const jive::type & other) const noexcept override;

	virtual std::unique_ptr<jive::type>
	copy() const override;

	static std::unique_ptr<jive::type>
	create()
	{
		return std::unique_ptr<jive::type>(new valuetype());
	}
};

/* statetype class */

class statetype final : public jive::statetype {
public:
	~statetype() override;

	constexpr
	statetype() noexcept
	: jive::statetype()
	{}

	virtual std::string
	debug_string() const override;

	virtual bool
	operator==(const jive::type & other) const noexcept override;

	virtual std::unique_ptr<jive::type>
	copy() const override;

	static std::unique_ptr<jive::type>
	create()
	{
		return std::unique_ptr<jive::type>(new statetype());
	}
};

}}

#endif
