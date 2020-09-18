#ifndef JSYN_IR_RVSDG_HPP
#define JSYN_IR_RVSDG_HPP

#include <jive/rvsdg/graph.h>

namespace jsyn {

class rvsdg final {
public:
	rvsdg()
	{}

	rvsdg(const rvsdg&) = delete;

	rvsdg(rvsdg&&) = delete;

	rvsdg &
	operator=(const rvsdg&) = delete;

	rvsdg &
	operator=(rvsdg&&) = delete;

	const jive::graph &
	graph() const noexcept
	{
		return graph_;
	}

	jive::graph &
	graph() noexcept
	{
		return graph_;
	}

	static std::unique_ptr<rvsdg>
	create()
	{
		return std::make_unique<rvsdg>();
	}

private:
	jive::graph graph_;
};

}

#endif
