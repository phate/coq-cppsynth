#ifndef JSYN_IR_PHI_HPP
#define JSYN_IR_PHI_HPP

#include <jsyn/util/assert.hpp>

#include <jive/rvsdg/structural-node.hpp>

namespace jsyn {
namespace phi {

/** \brief Phi operation
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

class cvargument;
class cvinput;
class rvoutput;

/** \brief Phi node
*
* A phi node represents a mutual recursive environment, such as recursive functions, in the RVSDG.
* Its creation is a two-step process and requires the invocation of two functions: \ref create()
* and \ref rvoutput::finalize(). First, an "empty" phi node, where only the recursion variables
* are present, is created by invoking \ref create(). The mutual recursive entities
* can then be added to the phi region along with the necessary context variables using the \ref
* add_ctxvar() method. Finally, the individual recursion variables can be redirected to the
* correct entities in the phi region using \ref rvoutput::finalize().
*
* The following snippet illustrates the creation of phi nodes:
*
* \code{.cpp}
*   auto phi = phi::node::create(...);
*   ...
*   auto cv1 = phi->add_ctxvar(...);
*   auto cv2 = phi->add_ctxvar(...);
*   ...
*   // create recursive entities
*   ...
*   phi->output(0)->finalize(...);
*   phi->output(1)->finalize(...);
*   ...
* \endcode
*/
class node final : public jive::structural_node {
	class cviterator;
	class cvconstiterator;

	class rviterator;
	class rvconstiterator;

public:
	~node() override;

private:
	node(
		jive::region * parent,
		phi::operation && op)
	: structural_node(op, parent, 1)
	{}

public:
	cviterator
	begin_cv();

	rviterator
	begin_rv();

	cvconstiterator
	begin_cv() const;

	rvconstiterator
	begin_rv() const;

	cviterator
	end_cv();

	rviterator
	end_rv();

	cvconstiterator
	end_cv() const;

	rvconstiterator
	end_rv() const;

	jive::region *
	subregion() const noexcept
	{
		return structural_node::subregion(0);
	}

	const phi::operation &
	operation() const noexcept
	{
		return *static_cast<const phi::operation*>(&structural_node::operation());
	}

	size_t
	ncvarguments() const noexcept
	{
		return ninputs();
	}

	size_t
	nrvarguments() const noexcept
	{
		return noutputs();
	}

	size_t
	nrvresults() const noexcept
	{
		return noutputs();
	}

	/**
	* Adds a context variable to the phi node. The \p origin must be from the same region as the phi
	* node.
	*
	* \return The context variable argument from the phi region.
	*/
	cvargument *
	add_ctxvar(jive::output * origin);

	cvinput *
	input(size_t n) const noexcept;

	rvoutput *
	output(size_t n) const noexcept;

	virtual phi::node *
	copy(
		jive::region * region,
		const std::vector<jive::output*> & operands) const override;

	virtual phi::node *
	copy(
		jive::region * region,
		jive::substitution_map & smap) const override;

	/**
	* Creates a phi node in the region \p parent with the number and types of recursion variables as
	* specified by \p rvtypes. The created phi node only contains the specified recursion variables.
	* Context variables can be added using \ref add_ctxvar(), and the recursive
	* entities can then be added to the phi region. Finally, the creation of the phi node can be
	* finished by redirecting the recursion variables to the corresponding recursive entities in the
	* phi region using \ref rvoutput::finalize().
	*
	* \param parent The region where the phi node is created.
	* \param rvtypes The number and types of the recursion variables.
	*
	* \return A phi node featuring only recursion variables.
	*/
	static phi::node *
	create(
		jive::region * parent,
		const std::vector<const jive::type*> & rvtypes);

private:
	static phi::node *
	create(jive::region * parent);

	rvoutput *
	add_rvvar(const jive::type & type);
};

/** \brief Phi context variable input
*/
class cvinput final : public jive::structural_input {
	friend ::jsyn::phi::node;

public:
	~cvinput() override;

private:
	cvinput(
		phi::node * node,
		jive::output * origin)
	: structural_input(node, origin, origin->port())
	{}

	static cvinput *
	create(
		phi::node * node,
		jive::output * origin)
	{
		auto input = std::unique_ptr<cvinput>(new cvinput(node, origin));
		return static_cast<cvinput*>(node->append_input(std::move(input)));
	}

public:
	cvargument *
	argument() const noexcept;

	phi::node *
	node() const noexcept
	{
		return static_cast<phi::node*>(structural_input::node());
	}
};

/** \brief Phi context variable iterator
*/
class node::cviterator final : public jive::input::iterator<cvinput> {
	friend ::jsyn::phi::node;

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

/** \brief Phi context variable const iterator
*/
class node::cvconstiterator final : public jive::input::constiterator<cvinput> {
	friend ::jsyn::phi::node;

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

class rvargument;
class rvresult;

/** \brief Phi recursion variable output
*/
class rvoutput final : public jive::structural_output {
	friend ::jsyn::phi::node;

public:
	~rvoutput() override;

private:
	rvoutput(
		phi::node * node,
		rvargument * argument);

	static rvoutput *
	create(
		phi::node * node,
		rvargument * argument)
	{
		auto output = std::unique_ptr<rvoutput>(new rvoutput(node, argument));
		return static_cast<rvoutput*>(node->append_output(std::move(output)));
	}

public:
	phi::node *
	node() const noexcept
	{
		return static_cast<phi::node*>(structural_output::node());
	}

	rvargument *
	argument() const noexcept
	{
		return argument_;
	}

	phi::rvresult *
	result() const noexcept;

	/**
	* Finalizes a recursion variable.
	*
	* \param origin The recursive entity from the phi region.
	*/
	void
	finalize(jive::output * origin);

private:
	rvargument * argument_;
};

/** \brief Phi context variable argument
*/
class cvargument final : public jive::argument {
	friend ::jsyn::phi::node;

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
		cvinput * input)
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

/** \brief Phi recursion variable argument
*/
class rvargument final : public jive::argument {
	friend ::jsyn::phi::node;
	friend ::jsyn::phi::rvoutput;

public:
	~rvargument() override;

private:
	rvargument(
		jive::region * region,
		const jive::port & port)
	: argument(region, nullptr, port)
	, output_(nullptr)
	{}

	static rvargument *
	create(
		jive::region * region,
		const jive::port & port)
	{
		auto argument = new rvargument(region, port);
		region->append_argument(argument);
		return argument;
	}

public:
	rvoutput *
	output() const noexcept
	{
		JSYN_ASSERT(output_ != nullptr);
		return output_;
	}

	rvresult *
	result() const noexcept
	{
		return output()->result();
	}

private:
	rvoutput * output_;
};

/** \brief Phi recursion variable result
*/
class rvresult final : public jive::result {
	friend ::jsyn::phi::node;

public:
	~rvresult() override;

private:
	rvresult(
		jive::region * region,
		jive::output * origin,
		rvoutput * output)
	: jive::result(region, origin, output, output->port())
	{}

	static rvresult *
	create(
		jive::region * region,
		jive::output * origin,
		rvoutput * output)
	{
		auto result = new rvresult(region, origin, output);
		region->append_result(result);
		return result;
	}

public:
	rvoutput *
	output() const noexcept
	{
		return static_cast<rvoutput*>(result::output());
	}

	rvargument *
	argument() const noexcept
	{
		return output()->argument();
	}
};

/** \brief Phi recursion variable iterator
*/
class node::rviterator final : public jive::output::iterator<rvoutput> {
	friend ::jsyn::phi::node;

	constexpr
	rviterator(rvoutput * output)
	: jive::output::iterator<rvoutput>(output)
	{}

	virtual rvoutput *
	next() const override
	{
		auto index = value()->index();
		auto node = value()->node();

		return node->noutputs() > index+1 ? node->output(index+1) : nullptr;
	}
};

/** \brief Phi recursion variable const iterator
*/
class node::rvconstiterator final : public jive::output::constiterator<rvoutput> {
	friend ::jsyn::phi::node;

	constexpr
	rvconstiterator(const rvoutput * output)
	: jive::output::constiterator<rvoutput>(output)
	{}

	virtual const rvoutput *
	next() const override
	{
		auto index = value()->index();
		auto node = value()->node();

		return node->noutputs() > index+1 ? node->output(index+1) : nullptr;
	}
};

/* method definitions */

inline cvargument *
cvinput::argument() const noexcept
{
	JSYN_ASSERT(arguments.size() == 1);
	return static_cast<cvargument*>(arguments.first());
}

inline
rvoutput::rvoutput(
	phi::node * node,
	rvargument * argument)
: structural_output(node, argument->port())
, argument_(argument)
{}

inline rvresult *
rvoutput::result() const noexcept
{
	JSYN_ASSERT(results.size() == 1);
	return static_cast<rvresult*>(results.first());
}

inline void
rvoutput::finalize(jive::output * origin)
{
	JSYN_ASSERT(result()->origin() == argument());
	result()->divert_to(origin);
}

}}

#endif
