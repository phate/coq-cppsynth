#ifndef JSYN_IR_CASE_HPP
#define JSYN_IR_CASE_HPP

#include <jive/rvsdg/structural-node.hpp>
#include <jive/rvsdg/substitution.hpp>

namespace jsyn {
namespace casee {

/**
* FIXME: write documentation
*/
class operation final : public jive::structural_op {
public:
	~operation() override;

	operation()
	{}

	virtual std::string
	debug_string() const override;

	virtual bool
	operator==(const jive::operation & other) const noexcept override;

	virtual std::unique_ptr<jive::operation>
	copy() const override;
};

/**
* FIXME: write documentation
*/
class node final : public jive::structural_node {
public:
	~node() override;

private:
	node(
		jive::region * parent,
		casee::operation && op)
	: structural_node(op, parent, 1)
	{}

public:
	jive::region *
	subregion() const noexcept
	{
		return structural_node::subregion(0);
	}

	const casee::operation &
	operation() const noexcept
	{
		return *static_cast<const casee::operation*>(&structural_node::operation());
	}

	virtual casee::node *
	copy(
		jive::region * region,
		const std::vector<jive::output*> & operands) const override;

	virtual casee::node *
	copy(
		jive::region * region,
		jive::substitution_map & smap) const override;

	static node *
	create(jive::region * parent);
};

}}

#endif
