LIBJSYN_SRC = \
	src/sexpr.cc \
	src/sexpr-rvsdg.cc \
	\
	src/ir/case.cc \
	src/ir/constructor.cc \
	src/ir/definition.cc \
	src/ir/gamma.cc \
	src/ir/inductive.cc \
	src/ir/lambda.cc \
	src/ir/match.cc \
	src/ir/module.cc \
	src/ir/phi.cc \
	src/ir/types.cc \
	\
	src/util/exception.cc \

CFLAGS += -Wall -Wpedantic -Wextra --std=c++17 -g
CPPFLAGS += -DJIVE_DEBUG -DJSYN_ENABLE_ASSERTS -Iinclude


all: check convert

include tests/Makefile.sub

libjsyn.a: $(patsubst %.cc, %.la, $(LIBJSYN_SRC))

%.la: %.cc
	$(CXX) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<

%.a:
	rm -f $@
	ar clqv $@ $^
	ranlib $@

convert: CFLAGS += -O2
convert: LDFLAGS += -L. -L$(JIVE_ROOT) -ljsyn -ljive
convert: convert.cc libjsyn.a
	$(CXX) $(CFLAGS) $(CPPFLAGS) -o $@ $< $(LDFLAGS)

.PHONY: doc
doc: doxygen.conf
	doxygen $^

clean:
	find -name "*.la" | xargs rm -f
	rm -f check.log passed.log failed.log
	rm -f tests/test-runner
	rm -f libjsyn.a
	rm -f convert
	rm -rf doc

.PHONY: clean
