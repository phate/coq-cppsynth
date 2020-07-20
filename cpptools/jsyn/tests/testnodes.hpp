#ifndef JSYN_TEST_TESTNODES_HPP
#define JSYN_TEST_TESTNODES_HPP

#include <jive/rvsdg/simple-node.h>

namespace jsyn {
namespace test {

class simple_op final : public jive::simple_op {
public:
	~simple_op() override;

private:
	simple_op(
		const std::vector<jive::port> & operands,
		const std::vector<jive::port> & results)
	: jive::simple_op(operands, results)
	{}

public:
	virtual bool
	operator==(const operation & other) const noexcept override;

	virtual std::string
	debug_string() const override;

	virtual std::unique_ptr<jive::operation>
	copy() const override;

	static jive::node *
	create(
		jive::region * region,
		const std::vector<jive::output*> & operands,
		const std::vector<jive::port> & results)
	{
		simple_op op(ports(operands), results);
		return jive::simple_node::create(region, op, operands);
	}

	static std::vector<jive::output*>
	create_normalized(
		jive::region * region,
		const std::vector<jive::output*> & operands,
		const std::vector<jive::port> & results)
	{
		simple_op op(ports(operands), results);
		return jive::simple_node::create_normalized(region, op, operands);
	}

private:
	static std::vector<jive::port>
	ports(const std::vector<jive::output*> & outputs)
	{
		std::vector<jive::port> ports;
		for (auto & output: outputs)
			ports.push_back(output->port());

		return ports;
	}
};

}}

#endif
