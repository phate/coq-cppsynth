#ifndef JSYN_IR_TYPES_HPP
#define JSYN_IR_TYPES_HPP

#include <jsyn/util/assert.hpp>

#include <jive/rvsdg/type.h>

#include <vector>

namespace jsyn {

/** \brief Function type
*
* A function type consists of operand and result types, which determine the function's signature.
*/
class fcttype final : public jive::valuetype {
	class constiterator final : public std::iterator<std::forward_iterator_tag,
		const type *, ptrdiff_t> {

		friend class fcttype;

		constiterator(const std::vector<std::unique_ptr<jive::type>>::const_iterator & it)
		: it_(it)
		{}

	public:
		const jive::type *
		type() const
		{
			return it_->get();
		}

		const jive::type &
		operator*() const
		{
			JSYN_ASSERT(type() != nullptr);
			return *type();
		}

		const jive::type *
		operator->() const
		{
			return &operator*();
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
		std::vector<std::unique_ptr<jive::type>>::const_iterator it_;
	};

public:
	~fcttype() override;

	fcttype(
		const std::vector<const jive::type*> & operands,
		const std::vector<const jive::type*> & results);

	fcttype(
		const std::vector<std::unique_ptr<jive::type>> & operands,
		const std::vector<std::unique_ptr<jive::type>> & results);

	fcttype(const fcttype & other);

	fcttype(fcttype && other);

	fcttype &
	operator=(const fcttype & other);

	fcttype &
	operator=(fcttype && other);

	constiterator
	begin_operand() const
	{
		return constiterator(operands_.begin());
	}

	constiterator
	end_operand() const
	{
		return constiterator(operands_.end());
	}

	constiterator
	begin_result() const
	{
		return constiterator(results_.begin());
	}

	constiterator
	end_result() const
	{
		return constiterator(results_.end());
	}

	size_t
	noperands() const noexcept
	{
		return operands_.size();
	}

	size_t
	nresults() const noexcept
	{
		return results_.size();
	}

	const jive::type &
	operand(size_t index) const noexcept
	{
		JSYN_ASSERT(index < noperands());
		return *operands_[index];
	}

	const jive::type &
	result(size_t index) const noexcept
	{
		JSYN_ASSERT(index < nresults());
		return *results_[index];
	}

	virtual std::string
	debug_string() const override;

	virtual bool
	operator==(const jive::type & other) const noexcept override;

	virtual std::unique_ptr<jive::type>
	copy() const override;

	static const fcttype *
	cast(const jive::type * type) noexcept
	{
		return dynamic_cast<const fcttype*>(type);
	}

	static std::unique_ptr<jive::type>
	create(
		const std::vector<const jive::type*> & operands,
		const std::vector<const jive::type*> & results)
	{
		return std::unique_ptr<jive::type>(new fcttype(operands, results));
	}

	static std::unique_ptr<jive::type>
	create(
		const std::vector<std::unique_ptr<jive::type>> & operands,
		const std::vector<std::unique_ptr<jive::type>> & results)
	{
		return std::unique_ptr<jive::type>(new fcttype(operands, results));
	}

private:
	std::vector<std::unique_ptr<jive::type>> operands_;
	std::vector<std::unique_ptr<jive::type>> results_;
};

/** FIXME: to be removed
*/
class dummytype final : public jive::valuetype {
public:
	~dummytype() override;

	constexpr
	dummytype() noexcept
	: jive::valuetype()
	{}

	virtual std::string
	debug_string() const override;

	virtual bool
	operator==(const jive::type & other) const noexcept override;

	virtual std::unique_ptr<jive::type>
	copy() const override;

	static std::unique_ptr<jive::type>
	create()
	{
		return std::unique_ptr<jive::type>(new dummytype());
	}
};

}

#endif
