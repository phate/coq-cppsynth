#ifndef JSYN_IR_DEFINITION_HPP
#define JSYN_IR_DEFINITION_HPP

#include <jive/rvsdg/structural-node.h>
#include <jive/rvsdg/substitution.h>

namespace jsyn {
namespace definition {

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


class cvargument;
class cvinput;

/**
* FIXME: write documentation
*/
class node final : public jive::structural_node {
public:
	~node() override;

private:
	node(
		jive::region * parent,
		definition::operation && op)
	: structural_node(op, parent, 1)
	{}

public:
	jive::region *
	subregion() const noexcept
	{
		return structural_node::subregion(0);
	}

	const definition::operation &
	operation() const noexcept
	{
		return *static_cast<const definition::operation*>(&structural_node::operation());
	}

	const std::string &
	name() const noexcept
	{
		return operation().name();
	}

	cvinput *
	input(size_t n) const noexcept;

	size_t
	ncvarguments() const noexcept
	{
		return ninputs();
	}

	definition::cvargument *
	cvargument(size_t n) const noexcept;

	/*
		FIXME: write documentation
	*/
	definition::cvargument *
	add_ctxvar(jive::output * origin);

	virtual definition::node *
	copy(
		jive::region * region,
		const std::vector<jive::output*> & operands) const override;

	virtual definition::node *
	copy(
		jive::region * region,
		jive::substitution_map & smap) const override;

	static node *
	create(
		jive::region * parent,
		const std::string & name);
};


/**
* FIXME: write documentation
*/
class cvinput final : public jive::structural_input {
	friend ::jsyn::definition::node;

public:
	~cvinput() override;

private:
	cvinput(
		definition::node * node,
		jive::output * origin)
	: structural_input(node, origin, origin->port())
	{}

	static cvinput *
	create(
		definition::node * node,
		jive::output * origin)
	{
		auto input = std::unique_ptr<cvinput>(new cvinput(node, origin));
		return static_cast<cvinput*>(node->append_input(std::move(input)));
	}

public:
	cvargument *
	argument() const noexcept;

	definition::node *
	node() const noexcept
	{
		return static_cast<definition::node*>(structural_input::node());
	}
};


/**
* FIXME: write documentation
*/
class cvargument final : public jive::argument {
	friend ::jsyn::definition::node;

public:
	~cvargument() override;

private:
	cvargument(
		jive::region * region,
		cvinput * input)
	: jive::argument(region, input, input->port())
	{}

	static cvargument *
	create(
		jive::region * region,
		definition::cvinput * input)
	{
		auto argument = new cvargument(region, input);
		region->append_argument(argument);
		return argument;
	}

public:
	cvinput *
	input() const noexcept
	{
		return static_cast<cvinput*>(jive::argument::input());
	}
};

}}

#endif
