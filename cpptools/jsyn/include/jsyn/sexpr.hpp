#ifndef JSYN_SEXPR_HPP
#define JSYN_SEXPR_HPP

#include <iostream>
#include <memory>
#include <optional>
#include <string>
#include <unordered_set>
#include <vector>
#include <variant>

#include <unistd.h>

namespace jsyn {

class sexpr {
public:
	virtual
	~sexpr();

	virtual void
	app_str(std::string& out) const = 0;

	inline std::string to_string() const
	{
		std::string s;
		app_str(s);
		return s;
	}

	class terminal;
	class compound;
	using list = std::vector<std::shared_ptr<const sexpr>>;

private:
	sexpr()
	{}
};

class sexpr::terminal final : public sexpr {
public:
	~terminal() override;

	terminal(std::string id) noexcept
	: id_(std::move(id))
	{}

	inline const std::string&
	id() const noexcept
	{
		return id_;
	}

	void
	app_str(std::string& out) const override
	{
		out += id();
	}

private:
	std::string id_;
};

class sexpr::compound final : public sexpr {
public:
	~compound() override;

	void
	app_str(std::string& out) const override
	{
		out += "(";
		out += kind();
		for (const auto& arg : args()) {
			out += " ";
			arg->app_str(out);
		}
		out += ")";
	}

	compound(std::string kind, list args) noexcept
	: kind_(std::move(kind))
	, args_(std::move(args))
	{}

	inline const std::string&
	kind() const noexcept
	{
		return kind_;
	}

	inline const list&
	args() const noexcept
	{
		return args_;
	}

private:
	std::string kind_;
	list args_;
};

struct sexpr_token_open {};
struct sexpr_token_close {};
struct sexpr_token_id {
	inline sexpr_token_id(std::string init_id) noexcept : id(std::move(init_id)) {}
	std::string id;
};

using sexpr_token = std::variant<sexpr_token_open, sexpr_token_close, sexpr_token_id>;

class sexpr_tokenizer {
public:
	enum class state_type {
		none,
		open,
		close,
		id,
		id_escape
	};

	sexpr_tokenizer() noexcept
	: state_(state_type::none)
	{}

	std::optional<sexpr_token>
	process(char c)
	{
		if (state_ == state_type::id_escape) {
			if (c != '0') {
				id_ += c;
			}
			state_ = state_type::id;
			return {};
		} else if (c == '(') {
			auto result = flush_token();
			state_ = state_type::open;
			id_ = "";
			return result;
		} else if (c == ')') {
			auto result = flush_token();
			state_ = state_type::close;
			id_ = "";
			return result;
		} else if (c == ' ' || c == '\n' || c == '\r' || c == '\t') {
			auto result = flush_token();
			state_ = state_type::none;
			id_ = "";
			return result;
		} else if (c == '\\') {
			if (state_ == state_type::id) {
				state_ = state_type::id_escape;
				return {};
			} else {
				auto result = flush_token();
				state_ = state_type::id_escape;
				return result;
			}
		} else {
			if (state_ == state_type::id) {
				id_ += c;
				state_ = state_type::id;
				return {};
			} else {
				auto result = flush_token();
				id_ = c;
				state_ = state_type::id;
				return result;
			}
		}
	}

	std::optional<sexpr_token>
	finish()
	{
		auto tok = flush_token();
		state_ = state_type::none;
		id_.clear();
		return tok;
	}

	std::optional<sexpr_token>
	flush_token()
	{
		switch (state_) {
			case state_type::open: {
				return {sexpr_token_open()};
			}
			case state_type::close: {
				return {sexpr_token_close()};
			}
			case state_type::id: {
				return {sexpr_token_id(id_)};
			}
			default: {
				return {};
			}
		}
	}

private:
	state_type state_;
	std::string id_;
};

class sexpr_parser {
public:
	enum class state_kind {
		error,
		expression,
		head
	};
	struct layer {
		std::string head;
		sexpr::list args;
	};

	sexpr_parser()
	: state_(state_kind::expression)
	, stack_({{"", sexpr::list()}})
	{}

	void
	process_token(const sexpr_token& tok)
	{
		switch (state_) {
			case state_kind::error: {
				break;
			}
			case state_kind::expression: {
				if (std::holds_alternative<sexpr_token_open>(tok)) {
					state_ = state_kind::head;
				} else if (std::holds_alternative<sexpr_token_close>(tok)) {
					if (stack_.size() >= 2) {
						auto e = std::make_shared<sexpr::compound>(std::move(stack_.back().head),
							std::move(stack_.back().args));
						stack_.pop_back();
						stack_.back().args.push_back(e);
						state_ = state_kind::expression;
					} else {
						stack_.clear();
						state_ = state_kind::error;
					}
				} else {
					std::string id = std::get<sexpr_token_id>(tok).id;
					if (stack_.size() >= 1) {
						auto e = std::make_shared<sexpr::terminal>(std::move(id));
						stack_.back().args.push_back(e);
						state_ = state_kind::expression;
					} else {
						stack_.clear();
						state_ = state_kind::error;
					}
				}
				break;
			}
			case state_kind::head: {
				if (std::holds_alternative<sexpr_token_id>(tok)) {
					std::string id = std::get<sexpr_token_id>(tok).id;
					stack_.push_back(layer{std::move(id), {}});
					state_ = state_kind::expression;
				} else {
					state_ = state_kind::error;
					stack_.clear();
				}
				break;
			}
		}
	}

	std::shared_ptr<const sexpr>
	finalize()
	{
		switch (state_) {
			case state_kind::expression: {
				if (stack_.size() == 1) {
					if (stack_.front().args.size() == 1) {
						return stack_.front().args.front();
					} else {
						return {};
					}
				} else {
					return {};
				}
			}
			default: {
				return {};
			}
		}
	}

private:
	state_kind state_;
	std::vector<layer> stack_;
};

void
format_sexpr(const sexpr& e, const std::string& indent, std::string& out);

}

#endif
