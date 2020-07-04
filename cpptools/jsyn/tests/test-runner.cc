#include "registry.hpp"

#include <jsyn/util/exception.hpp>

#include <iostream>

static void
print_usage(const std::string & appname)
{
	std::cout << "Usage: " + appname + " [TEST]\n\n";
	std::cout <<
		"The program takes a test as argument, executes it, and returns its result. "
		"If no test is provided, then it prints all available tests.\n\n";
}

static void
print_tests()
{
	for (auto & name : jsyn::test::list())
		std::cout << name << "\n";
}

static int
run_test(const std::string & appname, const std::string & test)
{
	try {
		return jsyn::test::run(test);
	} catch (jsyn::compilation_error & e) {
		std::cerr << "ERROR: " << e.what() << "\n\n";
		print_usage(appname);
		exit(EXIT_FAILURE);
	}
}

int
main(int argc, char ** argv)
{
	if (argc == 1) {
		print_tests();
		exit(EXIT_SUCCESS);
	}

	if (argc == 2)
		return run_test(argv[0], argv[1]);

	print_usage(argv[0]);
	exit(EXIT_FAILURE);
}
