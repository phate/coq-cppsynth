#ifndef JSYN_IR_LAMBDA_HPP
#define JSYN_IR_LAMBDA_HPP

#include <jsyn/ir/types.hpp>

#include <jive/rvsdg/structural-node.h>
#include <jive/rvsdg/substitution.h>

namespace jsyn {
namespace lambda {

/** \brief Lambda operation
*
* A lamba operation determines a lambda's name and \ref fcttype "function type".
*/
class operation final : public jive::structural_op {
public:
	~operation() override;

	operation(
		const fcttype & type,
		const std::string & name)
	: type_(type)
	, name_(name)
	{}

	operation(const operation & other)
	: type_(other.type_)
	, name_(other.name_)
	{}

	operation(operation && other)
	: type_(std::move(other.type_))
	, name_(std::move(other.name_))
	{}

	operation &
	operator=(const operation & other)
	{
		if (this == &other)
			return *this;

		type_ = other.type_;
		name_ = other.name_;

		return *this;
	}

	operation &
	operator=(operation && other)
	{
		if (this == &other)
			return *this;

		type_ = std::move(other.type_);
		name_ = std::move(other.name_);

		return *this;
	}

	const fcttype &
	type() const noexcept
	{
		return type_;
	}

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
	fcttype type_;
	std::string name_;
};

class cvargument;
class cvinput;
class fctargument;
class output;
class result;

/** \brief Lambda node
*
* A lambda node represents a lambda expression in the RVSDG. Its creation requires the invocation
* of two functions: \ref create() and \ref finalize(). First, a node with only the function
* arguments is created by invoking \ref create(). The free variables of the lambda expression can
* then be added to the lambda node using the \ref add_ctxvar() method, and the body of the lambda
* node can be created. Finally, the lambda node can be finalized by invoking \ref finalize().
*
* The following snippet illustrates the creation of lambda nodes:
*
* \code{.cpp}
*   auto lambda = lambda::node::create(...);
*   ...
*   auto cv1 = lambda->add_ctxvar(...);
*   auto cv2 = lambda->add_ctxvar(...);
*   ...
*   // generate lambda body
*   ...
*   auto output = lambda->finalize(...);
* \endcode
*/
class node final : public jive::structural_node {
	class cviterator;
	class cvconstiterator;

	class fctargiterator;
	class fctargconstiterator;

	class fctresiterator;
	class fctresconstiterator;

public:
	~node() override;

private:
	node(
		jive::region * parent,
		lambda::operation && op)
	: structural_node(op, parent, 1)
	{}

public:
	cviterator
	begin_cv();

	fctargiterator
	begin_arg();

	fctresiterator
	begin_res();

	cvconstiterator
	begin_cv() const;

	fctargconstiterator
	begin_arg() const;

	fctresconstiterator
	begin_res() const;

	cviterator
	end_cv();

	fctargiterator
	end_arg();

	fctresiterator
	end_res();

	cvconstiterator
	end_cv() const;

	fctargconstiterator
	end_arg() const;

	fctresconstiterator
	end_res() const;

	jive::region *
	subregion() const noexcept
	{
		return structural_node::subregion(0);
	}

	const lambda::operation &
	operation() const noexcept
	{
		return *static_cast<const lambda::operation*>(&structural_node::operation());
	}

	const fcttype &
	type() const noexcept
	{
		return operation().type();
	}

	const std::string &
	name() const noexcept
	{
		return operation().name();
	}

	size_t
	ncvarguments() const noexcept
	{
		return ninputs();
	}

	size_t
	nfctarguments() const noexcept
	{
		return subregion()->narguments() - ninputs();
	}

	size_t
	nfctresults() const noexcept
	{
		return subregion()->nresults();
	}

	/**
	* Adds a context/free variable to the lambda node. The \p origin must be from the same region
	* as the lambda node.
	*
	* \return The context variable argument from the lambda region.
	*/
	lambda::cvargument *
	add_ctxvar(jive::output * origin);

	cvinput *
	input(size_t n) const noexcept;

	lambda::output *
	output() const noexcept;

	lambda::fctargument *
	fctargument(size_t n) const noexcept;

	lambda::cvargument *
	cvargument(size_t n) const noexcept;

	lambda::result *
	fctresult(size_t n) const noexcept;

	virtual lambda::node *
	copy(
		jive::region * region,
		const std::vector<jive::output*> & operands) const override;

	virtual lambda::node *
	copy(
		jive::region * region,
		jive::substitution_map & smap) const override;

	/**
	* Creates a lambda node in the region \p parent with the function type \p type and name \p name.
	* After the invocation of \ref create(), the lambda node only features the function arguments.
	* Free variables can be added to the function node using \ref add_ctxvar(). The generation of the
	* node can be finished using the \ref finalize() method.
	*
	* \param parent The region where the lambda node is created.
	* \param type The lambda node's type.
	* \param name The lambda node's name.
	*
	* \return A lambda node featuring only function arguments.
	*/
	static node *
	create(
		jive::region * parent,
		const fcttype & type,
		const std::string & name);

	/**
	* Finalizes the creation of a lambda node.
	*
	* \param results The result values of the lambda expression, originating from the lambda region.
	*
	* \return The output of the lambda node.
	*/
	lambda::output *
	finalize(const std::vector<jive::output*> & results);
};

/** \brief Lambda context variable input
*/
class cvinput final : public jive::structural_input {
	friend ::jsyn::lambda::node;

public:
	~cvinput() override;

private:
	cvinput(
		lambda::node * node,
		jive::output * origin)
	: structural_input(node, origin, origin->port())
	{}

	static cvinput *
	create(
		lambda::node * node,
		jive::output * origin)
	{
		auto input = std::unique_ptr<cvinput>(new cvinput(node, origin));
		return static_cast<cvinput*>(node->append_input(std::move(input)));
	}

public:
	cvargument *
	argument() const noexcept;

	lambda::node *
	node() const noexcept
	{
		return static_cast<lambda::node*>(structural_input::node());
	}
};

/** \brief Lambda context variable iterator
*/
class node::cviterator final : public jive::input::iterator<cvinput> {
	friend ::jsyn::lambda::node;

	constexpr
	cviterator(cvinput * input)
	: jive::input::iterator<cvinput>(input)
	{}

	virtual cvinput *
	next() const override
	{
		auto node = value()->node();
		auto index = value()->index();

		return node->ninputs() > index+1 ? node->input(index+1) : nullptr;
	}
};

/** \brief Lambda context variable const iterator
*/
class node::cvconstiterator final : public jive::input::constiterator<cvinput> {
	friend ::jsyn::lambda::node;

	constexpr
	cvconstiterator(const cvinput * input)
	: jive::input::constiterator<cvinput>(input)
	{}

	virtual const cvinput *
	next() const override
	{
		auto node = value()->node();
		auto index = value()->index();

		return node->ninputs() > index+1 ? node->input(index+1) : nullptr;
	}
};

/** \brief Lambda output
*/
class output final : public jive::structural_output {
	friend ::jsyn::lambda::node;

public:
	~output() override;

private:
	output(
		lambda::node * node,
		const jive::port & port)
	: structural_output(node, port)
	{}

	static output *
	create(
		lambda::node * node,
		const jive::port & port)
	{
		auto output = std::unique_ptr<lambda::output>(new lambda::output(node, port));
		return static_cast<lambda::output*>(node->append_output(std::move(output)));
	}

public:
	lambda::node *
	node() const noexcept
	{
		return static_cast<lambda::node*>(structural_output::node());
	}
};

/** \brief Lambda function argument
*/
class fctargument final : public jive::argument {
	friend ::jsyn::lambda::node;

public:
	~fctargument() override;

private:
	fctargument(
		jive::region * region,
		const jive::type & type)
	: jive::argument(region, nullptr, type)
	{}

	static fctargument *
	create(
		jive::region * region,
		const jive::type & type)
	{
		auto argument = new fctargument(region, type);
		region->append_argument(argument);
		return argument;
	}
};

/** \brief Lambda function argument iterator
*/
class node::fctargiterator final : public jive::output::iterator<lambda::fctargument> {
	friend ::jsyn::lambda::node;

	constexpr
	fctargiterator(lambda::fctargument * argument)
	: jive::output::iterator<lambda::fctargument>(argument)
	{}

	virtual lambda::fctargument *
	next() const override
	{
		auto index = value()->index();
		auto lambda = static_cast<lambda::node*>(value()->region()->node());

		/*
			This assumes that all function arguments were added to the lambda region
			before any context variable was added.
		*/
		return lambda->nfctarguments() > index+1
		     ? lambda->fctargument(index+1)
		     : nullptr;
	}
};

/** \brief Lambda function argument const iterator
*/
class node::fctargconstiterator final : public jive::output::constiterator<lambda::fctargument> {
	friend ::jsyn::lambda::node;

	constexpr
	fctargconstiterator(const lambda::fctargument * argument)
	: jive::output::constiterator<lambda::fctargument>(argument)
	{}

	virtual const lambda::fctargument *
	next() const override
	{
		auto index = value()->index();
		auto lambda = static_cast<lambda::node*>(value()->region()->node());

		/*
			This assumes that all function arguments were added to the lambda region
			before any context variable was added.
		*/
		return lambda->nfctarguments() > index+1
		     ? lambda->fctargument(index+1)
		     : nullptr;
	}
};

/** \brief Lambda context variable argument
*/
class cvargument final : public jive::argument {
	friend ::jsyn::lambda::node;

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
		lambda::cvinput * input)
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

/** \brief Lambda result
*/
class result final : public jive::result {
	friend ::jsyn::lambda::node;

public:
	~result() override;

private:
	result(jive::output * origin)
	: jive::result(origin->region(), origin, nullptr, origin->port())
	{}

	static result *
	create(jive::output * origin)
	{
		auto result = new lambda::result(origin);
		origin->region()->append_result(result);
		return result;
	}

public:
	lambda::output *
	output() const noexcept
	{
		return static_cast<lambda::output*>(jive::result::output());
	}
};

/** \brief Lambda result iterator
*/
class node::fctresiterator final : public jive::input::iterator<lambda::result> {
	friend ::jsyn::lambda::node;

	constexpr
	fctresiterator(lambda::result * result)
	: jive::input::iterator<lambda::result>(result)
	{}

	virtual lambda::result *
	next() const override
	{
		auto index = value()->index();
		auto lambda = static_cast<lambda::node*>(value()->region()->node());

		return lambda->nfctresults() > index+1
		     ? lambda->fctresult(index+1)
		     : nullptr;
	}
};

/** \brief Lambda result const iterator
*/
class node::fctresconstiterator final : public jive::input::constiterator<lambda::result> {
	friend ::jsyn::lambda::node;

	constexpr
	fctresconstiterator(const lambda::result * result)
	: jive::input::constiterator<lambda::result>(result)
	{}

	virtual const lambda::result *
	next() const override
	{
		auto index = value()->index();
		auto lambda = static_cast<lambda::node*>(value()->region()->node());

		return lambda->nfctresults() > index+1
		     ? lambda->fctresult(index+1)
		     : nullptr;
	}
};

}}

#endif
