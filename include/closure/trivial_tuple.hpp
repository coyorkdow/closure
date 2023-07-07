//
// Created by Youtao Guo on 2023/2/18.
//

#pragma once

#include <cstddef>
#include <type_traits>
#include <utility>

namespace closure {

namespace tuple {

template <size_t I, class Tp>
struct Component {
  static_assert(std::is_same<Tp, std::decay_t<Tp>>::value, "type Tp is not a decayed type");

 protected:
  constexpr Component() = default;
  template <class Up, std::enable_if_t<!std::is_same<std::decay_t<Up>, Component>::value, int> = 0>
  constexpr explicit Component(Up&& v) : value(std::forward<Up>(v)) {}

  Tp value;
};

static_assert(std::is_trivially_copyable<Component<123, int>>::value, "");

template <class Index, class... Tp>
class TupleImpl;

template <size_t... Is, class... Tp>
class TupleImpl<std::index_sequence<Is...>, Tp...> : private Component<Is, Tp>... {
  template <size_t F, size_t I, class Tp_, class... Tps_>
  struct ElementTypeImpl {
    static_assert(I < sizeof...(Is), "index out of bound");
    using type = typename ElementTypeImpl<F + 1, I, Tps_...>::type;
  };

  template <size_t I, class Tp_, class... Tps_>
  struct ElementTypeImpl<I, I, Tp_, Tps_...> {
    using type = Tp_;
  };

 protected:
  template <size_t I>
  struct ElementType {
    using type = typename ElementTypeImpl<0, I, Tp...>::type;
  };

  constexpr TupleImpl() = default;

  template <class... Ups>
  constexpr explicit TupleImpl(Ups&&... ups) : Component<Is, Tp>(std::forward<Ups>(ups))... {}

  template <size_t I>
  constexpr const typename ElementType<I>::type& Get() const noexcept {
    return Component<I, typename ElementType<I>::type>::value;
  }

  template <size_t I>
  typename ElementType<I>::type& Get() noexcept {
    return Component<I, typename ElementType<I>::type>::value;
  }
};

template <class... Tps>
class TrivialTuple : private TupleImpl<std::index_sequence_for<Tps...>, Tps...> {
  using base = TupleImpl<std::index_sequence_for<Tps...>, Tps...>;

 public:
  constexpr TrivialTuple() = default;

  template <class... Ups>
  constexpr explicit TrivialTuple(Ups&&... ups) : base(std::forward<Ups>(ups)...) {}

  using base::Get;
};

using std::get;

template <size_t I, class... Tps>
constexpr decltype(auto) get(const TrivialTuple<Tps...>& tuple) noexcept {
  return tuple.template Get<I>();
}

template <size_t I, class... Tps>
constexpr decltype(auto) get(TrivialTuple<Tps...>& tuple) noexcept {
  return tuple.template Get<I>();
}

}  // namespace tuple

}  // namespace closure