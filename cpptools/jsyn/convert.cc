#include <jsyn/sexpr.hpp>

int main()
{
	jsyn::sexpr_tokenizer tokenizer;
	jsyn::sexpr_parser parser;

	for (;;) {
		char buffer[1024];
		ssize_t count = ::read(0, buffer, 1024);
		if (count <= 0) {
			break;
		}
		for (ssize_t index = 0; index < count; ++index) {
			char c = buffer[index];
			auto tok = tokenizer.process(c);
			if (tok) {
				parser.process_token(*tok);
			}
		}
	}
	auto tok = tokenizer.finish();
	if (tok) {
		parser.process_token(*tok);
	}

	auto e = parser.finalize();
	if (e) {
		std::string s;
		format_sexpr(*e, "", s);
		std::cout << s << "\n";
// 		std::cout << e->to_string() << "\n";
	}
}
