#include "registry.hpp"
#include "testtypes.hpp"

#include <jsyn/ir/lambda.hpp>
#include <jsyn/ir/types.hpp>

#include <jive/rvsdg/graph.h>
#include <jive/view.h>

#include <assert.h>

static void
test_operation()
{
	using namespace jsyn;

	test::valuetype vt;
	test::statetype st;
	fcttype ft1({&vt}, {&vt});
	fcttype ft2({&vt}, {&st});

	lambda::operation op1(ft1, "f");
	lambda::operation op2(ft2, "g");
	lambda::operation op3(ft1, "h");

	auto op1copy = op1.copy();

	assert(op1.type() == ft1);
	assert(op1.name() == "f");

	assert(op1 == op1);
	assert(op1 == *op1copy);
	assert(op1 != op3);
	assert(op1 != op2);

	op2 = op1;
	assert(op1 == op2);

	lambda::operation op4(op3);
	assert(op4 == op3);
}

static void
test_node()
{
	using namespace jsyn;

	test::valuetype vt;
	fcttype ft1({&vt, &vt}, {&vt});

	jive::graph graph;
	auto imp1 = graph.add_import({vt, "imp1"});

	auto node = lambda::node::create(graph.root(), ft1, "f");
	assert(node->ninputs() == 0);
	assert(node->noutputs() == 0);
	assert(node->nfctarguments() == 2);

	node->add_ctxvar(imp1);
	node->add_ctxvar(imp1);
	node->add_ctxvar(imp1);
	assert(node->ninputs() == 3);
	assert(node->subregion()->narguments() == 5);

	assert(std::distance(node->begin_cv(), node->end_cv()) == 3);
	assert(std::distance(node->begin_arg(), node->end_arg()) == 2);

	auto output = node->finalize({node->fctargument(1)});
	assert(output->node() == node);

	assert(std::distance(node->begin_res(), node->end_res()) == 1);

	auto copy = node->copy(graph.root(), std::vector<jive::output*>({imp1}));
	assert(node->ninputs() == copy->ninputs());
	assert(node->noutputs() == copy->noutputs());
	assert(node->operation() == copy->operation());
	assert(node->nfctresults() == copy->nfctresults());
	assert(node->ncvarguments() == copy->ncvarguments());
	assert(node->nfctarguments() == copy->nfctarguments());

	jive::view(graph, stdout);
}

static int
test()
{
	test_operation();
	test_node();

	return 0;
}

JSYN_REGISTER_UNIT_TEST("ir/test-lambda", test)
