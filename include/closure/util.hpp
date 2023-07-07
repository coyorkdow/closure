//
// Created by Youtao Guo on 2022/12/27
//

#pragma once

#include <cstddef>
#include <tuple>

namespace closure {

template <class... Tps>
struct ArgList {
  using tuple_type = std::tuple<Tps...>;
  using forward_tuple = std::tuple<Tps&&...>;
  static constexpr size_t size = sizeof...(Tps);
};

template <class, class>
struct Concat;

template <class... Tps1, class... Tps2>
struct Concat<ArgList<Tps1...>, ArgList<Tps2...>> {
  using type = ArgList<Tps1..., Tps2...>;
};

template <class... Tps1, class... Tps2>
struct Concat<std::tuple<Tps1...>, std::tuple<Tps2...>> {
  using type = std::tuple<Tps1..., Tps2...>;
};

template <size_t... I1, size_t... I2>
struct Concat<std::index_sequence<I1...>, std::index_sequence<I2...>> {
  using type = std::index_sequence<I1..., I2...>;
};

template <class L1, class L2>
using ConcatT = typename Concat<L1, L2>::type;

// When the placeholders are discontinuous, the closure will have several superfluous parameters which can take any
// types. We use Any to express them.
struct Any {
  Any() = default;
  template <class Tp>
  constexpr Any(Tp&&) noexcept {}
};

}  // namespace closure
