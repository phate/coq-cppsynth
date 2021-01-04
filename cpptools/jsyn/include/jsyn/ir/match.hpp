#ifndef JSYN_IR_MATCH_HPP
#define JSYN_IR_MATCH_HPP

#include <jsyn/ir/types.hpp>

#include <jive/rvsdg/simple-node.hpp>
#include <jive/rvsdg/substitution.hpp>

namespace jsyn {
namespace match {

/**
* FIXME: write documentation
*/
class operation final : public jive::simple_op {
public:
	~operation() override;

private:
	operation(const std::vector<jive::port> & operands)
	: simple_op(operands, {ctltype(operands.size()-1)})
	{}

public:
	const ctltype &
	type() const noexcept
	{
		return *static_cast<const ctltype*>(&result(0).type());
	}

	virtual std::string
	debug_string() const override;

	virtual bool
	operator==(const jive::operation & other) const noexcept override;

	virtual std::unique_ptr<jive::operation>
	copy() const override;

	static jive::output *
	create(
		jive::output * operand,
		const std::vector<jive::output*> & constructors)
	{
		match::operation op(ports(operand, constructors));

		std::vector<jive::output*> operands(1, operand);
		operands.insert(operands.end(), constructors.begin(), constructors.end());

		return jive::simple_node::create(operand->region(), op, operands)->output(0);
	}

private:
	static std::vector<jive::port>
	ports(
		jive::output * operand,
		const std::vector<jive::output*> & constructors)
	{
		std::vector<jive::port> ports;
		ports.push_back(operand->type());
		for (auto constructor : constructors)
			ports.push_back(constructor->type());

		return ports;
	}
};

}}

#endif

