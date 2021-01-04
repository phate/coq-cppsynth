#include "registry.hpp"
#include "testnodes.hpp"
#include "testtypes.hpp"

#include <jsyn/ir/phi.hpp>

#include <jive/rvsdg/graph.hpp>
#include <jive/view.hpp>

#include <assert.h>

static void
test_operation()
{
	using namespace jsyn;

	phi::operation op1, op2;

	auto op1copy = op1.copy();

	assert(op1 != op2);
	assert(op1 != *op1copy);
}

static void
test_node()
{
	using namespace jsyn;

	test::valuetype vt;

	jive::graph graph;
	auto imp1 = graph.add_import({vt, "imp1"});

	auto phi = phi::node::create(graph.root(), {&vt, &vt});
	assert(phi->ninputs() == 0);
	assert(phi->ncvarguments() == 0);
	assert(phi->noutputs() == 2);
	assert(phi->nrvarguments() == 2);
	assert(phi->nrvresults() == 2);

	phi->add_ctxvar(imp1);
	phi->add_ctxvar(imp1);
	assert(phi->ninputs() == 2);
	assert(phi->subregion()->narguments() == 4);

	assert(std::distance(phi->begin_cv(), phi->end_cv()) == 2);
	assert(std::distance(phi->begin_rv(), phi->end_rv()) == 2);

	auto tn1 = test::simple_op::create(phi->subregion(), {phi->output(0)->argument()}, {vt});
	phi->output(0)->finalize(tn1->output(0));

	auto tn2 = test::simple_op::create(phi->subregion(), {phi->output(1)->argument()}, {vt});
	phi->output(1)->finalize(tn2->output(0));

	assert(phi->output(0)->result()->origin() == tn1->output(0));
	assert(phi->output(1)->result()->origin() == tn2->output(0));

	auto copy = phi->copy(graph.root(), std::vector<jive::output*>({imp1}));
	assert(phi->ninputs() == copy->ninputs());
	assert(phi->ncvarguments() == copy->ncvarguments());
	assert(phi->noutputs() == copy->noutputs());
	assert(phi->nrvarguments() == copy->nrvarguments());
	assert(phi->nrvresults() == copy->nrvresults());

	jive::view(graph, stdout);
}

static int
test()
{
	test_operation();
	test_node();

	return 0;
}

JSYN_REGISTER_UNIT_TEST("ir/test-phi", test)
