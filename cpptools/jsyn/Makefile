LIBJSYN_SRC = \
	src/sexpr.cc

all: libjsyn.a convert

libjsyn.a: CFLAGS += -Wall -Wpedantic -Wextra --std=c++17 -g
libjsyn.a: CPPFLAGS += -DJSYN_WITH_ASSERTS -Iinclude
libjsyn.a: $(patsubst %.cc, %.la, $(LIBJSYN_SRC))

%.la: %.cc
	$(CXX) -c $(CFLAGS) $(CPPFLAGS) -o $@ $<

%.a:
	rm -f $@
	ar clqv $@ $^
	ranlib $@

convert: CFLAGS += -Wall -Wpedantic -Wextra --std=c++17 -g -O2
convert: CPPFLAGS += -DJSYN_ENABLE_ASSERTS -Iinclude
convert: LDFLAGS += -L. -ljsyn
convert: convert.cc libjsyn.a
	$(CXX) $(CFLAGS) $(CPPFLAGS) -o $@ $< $(LDFLAGS)

clean:
	find -name "*.la" | xargs rm -f
	rm -f libjsyn.a
	rm -f convert

.PHONY: clean
