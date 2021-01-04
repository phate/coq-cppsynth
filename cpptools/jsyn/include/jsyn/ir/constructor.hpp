#ifndef JSYN_IR_CONSTRUCTOR_HPP
#define JSYN_IR_CONSTRUCTOR_HPP

#include <jive/rvsdg/structural-node.hpp>
#include <jive/rvsdg/substitution.hpp>

#include <jsyn/ir/types.hpp>

namespace jsyn {
namespace constructor {

/**
* FIXME: write documentation
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
* FIXME: write documentation
*/
class node final : public jive::structural_node {
public:
	~node() override;

private:
	node(
		jive::region * parent,
		constructor::operation && op)
	: structural_node(op, parent, 1)
	{}

public:
	jive::region *
	subregion() const noexcept
	{
		return structural_node::subregion(0);
	}

	const constructor::operation &
	operation() const noexcept
	{
		return *static_cast<const constructor::operation*>(&structural_node::operation());
	}

	const std::string &
	name() const noexcept
	{
		return operation().name();
	}

	virtual constructor::node *
	copy(
		jive::region * region,
		const std::vector<jive::output*> & operands) const override;

	virtual constructor::node *
	copy(
		jive::region * region,
		jive::substitution_map & smap) const override;

	static constructor::output *
	create(
		jive::region * parent,
		const std::string & name);
};

/**
* FIXME: write documentation
*/
class output final : public jive::structural_output {
	friend ::jsyn::constructor::node;

public:
	~output() override;

private:
	output(constructor::node * node)
	: structural_output(node, *dummytype::create())
	{}

	static output *
	create(constructor::node * node)
	{
		auto output = std::unique_ptr<constructor::output>(new constructor::output(node));
		return static_cast<constructor::output*>(node->append_output(std::move(output)));
	}

public:
	constructor::node *
	node() const noexcept
	{
		return static_cast<constructor::node*>(structural_output::node());
	}
};

}}

#endif