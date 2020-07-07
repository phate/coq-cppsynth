#include "registry.hpp"
#include "testtypes.hpp"

#include <jsyn/ir/types.hpp>

#include <assert.h>

static int
test()
{
	using namespace jsyn;

	test::valuetype vt;
	test::statetype st;
	fcttype ft1({&vt}, {&st});
	fcttype ft2({&vt, &vt}, {&vt, &st});

	assert(ft1.noperands() == 1 && ft1.nresults() == 1);
	assert(ft2.noperands() == 2 && ft2.nresults() == 2);

	assert(jive::is<test::valuetype>(ft1.operand(0)));
	assert(jive::is<test::statetype>(ft1.result(0)));

	assert(jive::is<test::valuetype>(ft2.operand(0)));
	assert(jive::is<test::valuetype>(ft2.operand(1)));
	assert(jive::is<test::valuetype>(ft2.result(0)));
	assert(jive::is<test::statetype>(ft2.result(1)));

	std::vector<const jive::type*> results;
	std::vector<const jive::type*> operands;
	for (auto it = ft2.begin_operand(); it != ft2.end_operand(); it++)
		operands.push_back(it.type());
	for (auto it = ft2.begin_result(); it != ft2.end_result(); it++)
		results.push_back(it.type());

	assert(operands.size() == 2 && results.size() == 2);
	assert(jive::is<test::valuetype>(*operands[0]) && jive::is<test::valuetype>(*operands[1]));
	assert(jive::is<test::valuetype>(*results[0]) && jive::is<test::statetype>(*results[1]));

	auto newft2 = jsyn::fcttype::create(operands, results);
	assert(ft2 == *newft2);

	newft2 = ft2.copy();
	assert(ft2 == *newft2 && ft1 != *newft2);

	return 0;
}

JSYN_REGISTER_UNIT_TEST("ir/test-fcttype", test)
