#ifndef JSYN_IR_GAMMA_HPP
#define JSYN_IR_GAMMA_HPP

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
	begin_entryvar();

	xviterator
	begin_exitvar();

	eviterator
	end_entryvar();

	xviterator
	end_exitvar();

	const gamma::operation &
	operation() const noexcept
	{
		return *static_cast<const gamma::operation*>(&structrual_node::operation());
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
	create(jive::output * predicate)
	{

	}
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
	/* FIXME: evargument iterators */

	gamma::node *
	node() const noexcept
	{
		return static_cast<gamma::node*>(structural_input::node());
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

	static output *
	create(
		gamma::node * node,
		const jive::port & port)
	{
		auto output = std::unique_ptr<gamma::xvoutput>(new gamma::output(node, port));
		return static_cast<gamma::output*>(node->append_output(std::move(output)));
	}

public:
	/* FIXME: xvresult iterators */

	gamma::node *
	node() const noexcept
	{
		return static_cast<gamma::node*>(structural_output::node());
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
	~result() override;

private:
	result(
		jive::output * origin,
		xvoutput * output)
	: jive::result(origin->region(), origin, output, output->port())
	{}

	static xvresult *
	create(
		jive::output * origin,
		xvoutput * output)
	{
		auto result = new gamma::xvresult(origin, output);
		origin->region()->append_result(result);
		return result;
	}

public:
	gamma::output *
	output() const noexcept
	{
		return static_cast<gamma::output*>(jive::result::output());
	}
};

}}

#endif
