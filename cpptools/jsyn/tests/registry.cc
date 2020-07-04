#include "registry.hpp"

#include <jsyn/util/assert.hpp>
#include <jsyn/util/exception.hpp>

#include <algorithm>
#include <memory>
#include <unordered_map>

namespace {

typedef int(*testfct)(void);

class unittest final {
public:
	unittest(
		const std::string & name,
		testfct fct)
	: fct_(fct)
	, name_(name)
	{}

	unittest(const unittest&) = delete;

	unittest(unittest&&) = delete;

	unittest &
	operator=(const unittest&) = delete;

	unittest &
	operator=(unittest&&) = delete;

	static std::unique_ptr<unittest>
	create(
		const std::string & name,
		testfct fct)
	{
		return std::unique_ptr<unittest>(new unittest(name, fct));
	}

	const std::string &
	name() const noexcept
	{
		return name_;
	}

	int
	run() const
	{
		return fct_();
	}

private:
	testfct fct_;
	std::string name_;
};

class testregistry final {

	using mapconstiterator = std::unordered_map<std::string,
		std::unique_ptr<unittest>>::const_iterator;

private:
	testregistry() = default;

	testregistry(const testregistry&) = delete;

	testregistry(testregistry&&) = delete;

	testregistry &
	operator=(const testregistry&) = delete;

	testregistry &
	operator=(testregistry&&) = delete;

	class constiterator final : public std::iterator<std::forward_iterator_tag,
		const unittest*, ptrdiff_t> {

		friend testregistry;

		constiterator(const mapconstiterator & it)
		: it_(it)
		{}

	public:
		const std::string &
		name() const noexcept
		{
			return it_->first;
		}

		const unittest *
		test() const noexcept
		{
			return it_->second.get();
		}

		const unittest &
		operator*() const
		{
			return *test();
		}

		const unittest *
		operator->() const
		{
			return test();
		}

		constiterator &
		operator++()
		{
			++it_;
			return *this;
		}

		constiterator
		operator++(int)
		{
			constiterator tmp = *this;
			++*this;
			return tmp;
		}

		bool
		operator==(const constiterator & other) const
		{
			return it_ == other.it_;
		}

		bool
		operator!=(const constiterator & other) const
		{
			return !operator==(other);
		}

	private:
		mapconstiterator it_;
	};

public:
	static testregistry &
	registry()
	{
		static testregistry registry;
		return registry;
	}

	constiterator
	begin() const
	{
		return constiterator(map_.begin());
	}

	constiterator
	end() const
	{
		return constiterator(map_.end());
	}

	bool
	contains(const std::string & name) const noexcept
	{
		return map_.find(name) != map_.end();
	}

	void
	insert(std::unique_ptr<unittest> test)
	{
		JSYN_ASSERT(!contains(test->name()));
		map_[test->name()] = std::move(test);
	}

	const unittest &
	operator[](const std::string & name) const noexcept
	{
		JSYN_ASSERT(contains(name));
		return *map_.at(name);
	}

private:
	std::unordered_map<std::string, std::unique_ptr<unittest>> map_;
};

}

namespace jsyn {
namespace test {

void
register_test(const std::string & name, testfct fct)
{
	auto & registry = testregistry::registry();

	if (registry.contains(name))
		throw compilation_error("Unit test " + name + " already registered.");

	registry.insert(unittest::create(name, fct));
}

int
run(const std::string & name)
{
	auto & registry = testregistry::registry();

	if (!registry.contains(name))
		throw compilation_error("Unit test " + name + " not found.");

	return registry[name].run();
}

std::vector<std::string>
list()
{
	auto & registry = testregistry::registry();

	std::vector<std::string> names;
	for (const auto & test : registry)
		names.push_back(test.name());

	std::sort(names.begin(), names.end());
	return names;
}

}}
