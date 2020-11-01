#ifndef JSYN_IR_GAMMA_HPP
#define JSYN_IR_GAMMA_HPP

#include <jsyn/util/assert.hpp>

#include <jive/rvsdg/structural-node.h>
#include <jive/rvsdg/substitution.h>

namespace jsyn {
namespace gamma {

/**
* FIXME: write documentation
*/
class operation final : public jive::structural_op {
public:
	~operation() override;

	operation(size_t nalternatives)
	: nalternatives_(nalternatives)
	{}

	size_t
	nalternatives() const noexcept
	{
		return nalternatives_;
	}

	virtual std::string
	debug_string() const override;

	virtual bool
	operator==(const jive::operation & other) const noexcept override;

	virtual std::unique_ptr<jive::operation>
	copy() const override;

private:
	size_t nalternatives_;
};

class evargument;
class evinput;
class prdinput;
class xvoutput;
class xvresult;

/**
* FIXME: write documentation
*/
class node final : public jive::structural_node {
	class eviterator;
	class evconstiterator;

	class xviterator;
	class xvconstiterator;

public:
	~node() override;

private:
	node(
		jive::region * parent,
		gamma::operation && op)
	: structural_node(op, parent, op.nalternatives())
	{}

public:
	eviterator
	begin_ev();

	xviterator
	begin_xv();

	evconstiterator
	begin_ev() const;

	xvconstiterator
	begin_xv() const;

	eviterator
	end_ev();

	xviterator
	end_xv();

	evconstiterator
	end_ev() const;

	xvconstiterator
	end_xv() const;

	const gamma::operation &
	operation() const noexcept
	{
		return *static_cast<const gamma::operation*>(&structural_node::operation());
	}

	gamma::prdinput *
	predicate() const noexcept;

	gamma::evinput *
	input(size_t n) const noexcept;

	gamma::xvoutput *
	output(size_t n) const noexcept;

	gamma::evinput *
	add_entryvar(jive::output * origin);

	gamma::xvoutput *
	add_exitvar(const std::vector<jive::output*> & values);

	virtual gamma::node *
	copy(
		jive::region * region,
		const std::vector<jive::output*> & operands) const override;

	virtual gamma::node *
	copy(
		jive::region * region,
		jive::substitution_map & smap) const override;

	static node *
	create(jive::output * predicate);
};

/** \brief Gamma predicate input
*/
class prdinput final : public jive::structural_input {
	friend ::jsyn::gamma::node;

public:
	~prdinput() override;

private:
	prdinput(
		gamma::node * node,
		jive::output * origin)
	: structural_input(node, origin, origin->port())
	{}

	static prdinput *
	create(
		gamma::node * node,
		jive::output * origin)
	{
		auto input = std::unique_ptr<prdinput>(new prdinput(node, origin));
		return static_cast<prdinput*>(node->append_input(std::move(input)));
	}

public:
	gamma::node *
	node() const noexcept
	{
		return static_cast<gamma::node*>(structural_input::node());
	}
};

/** \brief Gamma entry variable input
*/
class evinput final : public jive::structural_input {
	friend ::jsyn::gamma::node;

public:
	~evinput() override;

private:
	evinput(
		gamma::node * node,
		jive::output * origin)
	: structural_input(node, origin, origin->port())
	{}

	static evinput *
	create(
		gamma::node * node,
		jive::output * origin)
	{
		auto input = std::unique_ptr<evinput>(new evinput(node, origin));
		return static_cast<evinput*>(node->append_input(std::move(input)));
	}

public:
	size_t
	narguments() const noexcept
	{
		return arguments.size();
	}

	evargument *
	argument(size_t n) const noexcept;

	/* FIXME: evargument iterators */

	gamma::node *
	node() const noexcept
	{
		return static_cast<gamma::node*>(structural_input::node());
	}
};

/** Gamma entry variable iterator
*/
class node::eviterator final : public jive::input::iterator<evinput> {
	friend ::jsyn::gamma::node;

	constexpr
	eviterator(evinput * input)
	: jive::input::iterator<evinput>(input)
	{}

	virtual evinput *
	next() const override
	{
		auto node = value()->node();
		auto index = value()->index();

		return node->ninputs() > index+1 ? node->input(index+1) : nullptr;
	}
};

/** Gamma entry variable const iterator
*/
class node::evconstiterator final : public jive::input::constiterator<evinput> {
	friend ::jsyn::gamma::node;

	constexpr
	evconstiterator(const evinput * input)
	: jive::input::constiterator<evinput>(input)
	{}

	virtual const evinput *
	next() const override
	{
		auto node = value()->node();
		auto index = value()->index();

		return node->ninputs() > index+1 ? node->input(index+1) : nullptr;
	}
};

/** Gamma exit variable output
*/
class xvoutput final : public jive::structural_output {
	friend ::jsyn::gamma::node;

public:
	~xvoutput() override;

private:
	xvoutput(
		gamma::node * node,
		const jive::port & port)
	: structural_output(node, port)
	{}

	static xvoutput *
	create(
		gamma::node * node,
		const jive::port & port)
	{
		auto output = std::unique_ptr<gamma::xvoutput>(new xvoutput(node, port));
		return static_cast<xvoutput*>(node->append_output(std::move(output)));
	}

public:
	size_t
	nresults() const noexcept
	{
		return results.size();
	}

	xvresult *
	result(size_t n) const noexcept;

	/* FIXME: xvresult iterators */

	gamma::node *
	node() const noexcept
	{
		return static_cast<gamma::node*>(structural_output::node());
	}
};

/** Gamma exit variable iterator
*/
class node::xviterator final : public jive::output::iterator<xvoutput> {
	friend ::jsyn::gamma::node;

	constexpr
	xviterator(xvoutput * output)
	: jive::output::iterator<xvoutput>(output)
	{}

	virtual xvoutput *
	next() const override
	{
		auto node = value()->node();
		auto index = value()->index();

		return node->noutputs() > index+1 ? node->output(index+1) : nullptr;
	}
};

/** Gamma exit variable const iterator
*/
class node::xvconstiterator final : public jive::output::iterator<xvoutput> {
	friend ::jsyn::gamma::node;

	constexpr
	xvconstiterator(xvoutput * output)
	: jive::output::iterator<xvoutput>(output)
	{}

	virtual xvoutput *
	next() const override
	{
		auto node = value()->node();
		auto index = value()->index();

		return node->noutputs() > index+1 ? node->output(index+1) : nullptr;
	}
};

/** \brief Gamma entry variable argument
*/
class evargument final : public jive::argument {
	friend ::jsyn::gamma::node;

public:
	~evargument() override;

private:
	evargument(
		jive::region * region,
		gamma::evinput * input)
	: jive::argument(region, input, input->type())
	{}

	static evargument *
	create(
		jive::region * region,
		gamma::evinput * input)
	{
		auto argument = new evargument(region, input);
		region->append_argument(argument);
		return argument;
	}
};

/** Gamma exit variable result
*/
class xvresult final : public jive::result {
	friend ::jsyn::gamma::node;

public:
	~xvresult() override;

private:
	xvresult(
		jive::output * origin,
		xvoutput * output)
	: jive::result(origin->region(), origin, output, output->port())
	{}

	static xvresult *
	create(
		jive::output * origin,
		xvoutput * output)
	{
		auto result = new xvresult(origin, output);
		origin->region()->append_result(result);
		return result;
	}

public:
	xvoutput *
	output() const noexcept
	{
		return static_cast<xvoutput*>(jive::result::output());
	}
};

}}

#endif
