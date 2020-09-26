#ifndef JSYN_IR_MATCH_HPP
#define JSYN_IR_MATCH_HPP

#include <jive/rvsdg/structural-node.h>
#include <jive/rvsdg/substitution.h>

namespace jsyn {
namespace match {

/**
* FIXME: write documentation
*/
class operation final : public jive::structural_op {
public:
	~operation() override;

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
		match::operation && op)
	: structural_node(op, parent, 1)
	{}

public:
	jive::region *
	subregion() const noexcept
	{
		return structural_node::subregion(0);
	}	

	const match::operation &
	operation() const noexcept
	{
		return *static_cast<const match::operation*>(&structural_node::operation());
	}

	virtual match::node *
	copy(
		jive::region * region,
		const std::vector<jive::output*> & operands) const override;

	virtual match::node *
	copy(
		jive::region * region,
		jive::substitution_map & smap) const override;

	static node *
	create(
		jive::region * parent,
		jive::output * operand);
};

/**
* FIXME: write documentation
*/
class input final : public jive::structural_input {
	friend ::jsyn::match::node;

public:
	~input() override;

private:
	input(
		match::node * node,
		jive::output * origin)
	: structural_input(node, origin, origin->port())
	{}

	static input *
	create(
		match::node * node,
		jive::output * origin)
	{
		auto input = std::unique_ptr<match::input>(new match::input(node, origin));
		return static_cast<match::input*>(node->append_input(std::move(input)));
	}

public:
	match::node *
	node() const noexcept
	{
		return static_cast<match::node*>(structural_input::node());
	}
};


/**
*	FIXME: write documentation
*/
class output final : public jive::structural_output {
	friend ::jsyn::match::node;

public:
	~output() override;

private:
	output(
		match::node * node,
		const jive::port & port)
	: structural_output(node, port)
	{}

	static output *
	create(
		match::node * node,
		const jive::port & port)
	{
		auto output = std::unique_ptr<match::output>(new match::output(node, port));

		return static_cast<match::output*>(node->append_output(std::move(output)));
	}

public:
	match::node *
	node() const noexcept
	{
		return static_cast<match::node*>(structural_output::node());
	}
};

}}

#endif

