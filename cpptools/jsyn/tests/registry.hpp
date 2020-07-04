#ifndef JSYN_TEST_REGISTRY_HPP
#define JSYN_TEST_REGISTRY_HPP

#include <string>
#include <vector>

namespace jsyn {
namespace test {

void
register_test(const std::string & name, int (*f)(void));

#define JSYN_REGISTER_UNIT_TEST(name, function) \
	static void __attribute__((constructor)) register_##function(void) \
	{ \
		jsyn::test::register_test(name, function); \
	} \

int
run(const std::string & name);

std::vector<std::string>
list();

}}

#endif
