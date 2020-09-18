#ifndef JSYN_SEXPR_RVSDG_HPP
#define JSYN_SEXPR_RVSDG_HPP

#include <jsyn/ir/rvsdg.hpp>

namespace jsyn {

std::unique_ptr<rvsdg>
convert_sexpr(const sexpr & e);

}

#endif
