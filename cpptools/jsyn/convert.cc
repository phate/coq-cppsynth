#include <jsyn/sexpr.hpp>
#include <jsyn/sexpr-rvsdg.hpp>
#include <jsyn/util/assert.hpp>

#include <jive/view.h>

int main(int argc, char ** argv)
{
	jsyn::sexpr_tokenizer tokenizer;
	jsyn::sexpr_parser parser;

	JSYN_ASSERT(argc == 2);
	auto fd = fopen(argv[1], "r");

	for (;;) {
		char buffer[1024];
		ssize_t count = ::read(fileno(fd), buffer, 1024);
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

	fclose(fd);

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

	auto rvsdg = convert_sexpr(*e);
	jive::view(rvsdg->graph(), stdout);

}
