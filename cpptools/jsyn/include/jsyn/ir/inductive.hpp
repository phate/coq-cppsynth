#ifndef JSYN_IR_INDUCTIVE_HPP
#define JSYN_IR_INDUCTIVE_HPP

#include <jive/rvsdg/structural-node.hpp>
#include <jive/rvsdg/substitution.hpp>

#include <jsyn/ir/types.hpp>

namespace jsyn {
namespace constructor {
	class output;
}

namespace inductive {

/**
*	FIXME: write documentation
*/
class operation final : public jive::structural_op {
public:
	~operation() override;

	operation(const std::string & name)
	: name_(name)
	{}

	const std::string &
	name() const noexcept
	{
		return name_;
	}

	virtual std::string
	debug_string() const override;

	virtual bool
	operator==(const jive::operation & other) const noexcept override;

	virtual std::unique_ptr<jive::operation>
	copy() const override;

private:
	std::string name_;
};


class output;

/**
*	FIXME: write documentation
*/
class node final : public jive::structural_node {
public:
	~node() override;

private:
	node(
		jive::region * parent,
		inductive::operation && op)
	: structural_node(op, parent, 1)
	{}

public:
	jive::region *
	subregion() const noexcept
	{
		return structural_node::subregion(0);
	}

	const inductive::operation &
	operation() const noexcept
	{
		return *static_cast<const inductive::operation*>(&structural_node::operation());
	}

	const std::string &
	name() const noexcept
	{
		return operation().name();
	}

	virtual inductive::node *
	copy(
		jive::region * region,
		const std::vector<jive::output*> & operands) const override;

	virtual inductive::node *
	copy(
		jive::region * region,
		jive::substitution_map & smap) const override;

	static node *
	create(
		jive::region * parent,
		const std::string & name);

	inductive::output *
	add_constructor(constructor::output * output);
};

/**
* FIXME: write documentation
*/
class output final : public jive::structural_output {
	friend ::jsyn::inductive::node;

public:
	~output() override;

private:
	output(inductive::node * node)
	: structural_output(node, *dummytype::create())
	{}

	static output *
	create(inductive::node * node)
	{
		auto output = std::unique_ptr<inductive::output>(new inductive::output(node));
		return static_cast<inductive::output*>(node->append_output(std::move(output)));
	}

public:
	inductive::node *
	node() const noexcept
	{
		return static_cast<inductive::node*>(structural_output::node());
	}
};

}}

#endif