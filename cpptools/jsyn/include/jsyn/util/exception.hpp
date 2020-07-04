#ifndef JSYN_EXCEPTION_HPP
#define JSYN_EXCEPTION_HPP

#include <stdexcept>

namespace jsyn {

class compilation_error : public std::runtime_error {
public:
	~compilation_error() noexcept override;

	compilation_error(const std::string & arg)
	: std::runtime_error(arg)
	{}
};

class type_error : public compilation_error {
public:
	~type_error() noexcept override;

	type_error(
		const std::string & expected_type,
		const std::string & received_type)
	: compilation_error(
			"Type error - expected " + expected_type
		+ ", received " + received_type)
	{}
};

}

#endif
