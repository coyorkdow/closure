//
// Created by Youtao Guo on 20/1/23.
//

#pragma once

#include "closure/traits.hpp"
#include "closure/util.hpp"

namespace closure {

namespace placeholders {
template <size_t>
struct PH {};

template <class>
struct IsPlaceHolder : std::false_type {};

template <size_t I>
struct IsPlaceHolder<PH<I>> : std::true_type {};

template <class Tp>
constexpr auto IsPlaceHolderV = IsPlaceHolder<Tp>{};

template <class>
struct HasPlaceHolder : std::false_type {};

template <size_t I, class... Tps>
struct HasPlaceHolder<ArgList<PH<I>, Tps...>> : std::true_type {};

template <class F, class... Os>
struct HasPlaceHolder<ArgList<F, Os...>> : HasPlaceHolder<ArgList<Os...>> {};

template <class... Tps>
constexpr auto HasPlaceHolderV = HasPlaceHolder<ArgList<Tps...>>{};

template <class... Tps>
constexpr auto HasPlaceHolderV<ArgList<Tps...>> = HasPlaceHolder<ArgList<Tps...>>{};

template <class>
struct FilterPlaceHolder;

template <>
struct FilterPlaceHolder<ArgList<>> {
  using type = ArgList<>;
};

template <size_t I, class... Tps>
struct FilterPlaceHolder<ArgList<PH<I>, Tps...>> {
  using type = ConcatT<ArgList<PH<I>>, typename FilterPlaceHolder<ArgList<Tps...>>::type>;
};

template <class F, class... Os>
struct FilterPlaceHolder<ArgList<F, Os...>> {
  using type = typename FilterPlaceHolder<ArgList<Os...>>::type;
};

template <class ArgL>
using FilterPlaceHolderT = typename FilterPlaceHolder<ArgL>::type;

// Agent stores the reference of the placeholder corresponding argument and the argument can be accessed by Getter.
// Initialize Agent with a temporary object (prvalue) will occur a dangling reference.
template <class Tp>
class Agent {
  class Wrapper {
   public:
    explicit Wrapper(Tp&& v) noexcept : data_(std::forward<Tp>(v)) {}
    Tp&& data_;
  };

 public:
  Agent() noexcept : ptr_(nullptr), mem_{} {}
  explicit Agent(Tp&& v) noexcept : ptr_(reinterpret_cast<Wrapper*>(mem_)), mem_{} {
    new (mem_) Wrapper(std::forward<Tp>(v));
  }
  Agent& operator=(Tp&& v) noexcept {
    ptr_ = reinterpret_cast<Wrapper*>(mem_);
    new (mem_) Wrapper(std::forward<Tp>(v));
    return *this;
  }
  Agent(Agent&& rhs) noexcept : ptr_(nullptr), mem_{} {
    if (rhs.ptr_) {
      new (mem_) Wrapper(std::move(*rhs.ptr_));
      rhs.ptr_ = nullptr;
      ptr_ = reinterpret_cast<Wrapper*>(mem_);
    }
  }
  Agent& operator=(Agent&& rhs) noexcept {
    ptr_ = nullptr;
    if (rhs.ptr_) {
      new (mem_) Wrapper(std::move(*rhs.ptr_));
      rhs.ptr_ = nullptr;
      ptr_ = reinterpret_cast<Wrapper*>(mem_);
    }
    return *this;
  }

  explicit operator bool() const noexcept { return ptr_; }
  decltype(auto) Get() const noexcept { return std::forward<Tp>(ptr_->data_); }

 private:
  Wrapper* ptr_;
  unsigned char mem_[sizeof(Wrapper)];
};

template <>
class Agent<Auto> {
 public:
  constexpr Agent() noexcept = default;
  template <class Tp>
  explicit constexpr Agent(Tp&&) noexcept {}
};

template <class... Tps>
auto MakeAgents(std::tuple<Tps...>) -> std::tuple<Agent<Tps>...>;

template <class... Tps>
auto MakeAgents(ArgList<Tps...>) -> std::tuple<Agent<Tps>...>;

template <class Tuple>
using MakeAgentsT = decltype(MakeAgents(std::declval<Tuple>()));

template <class>
struct IsAgent : std::false_type {};

template <class Tp>
struct IsAgent<Agent<Tp>> : std::true_type {};

template <class Tp>
constexpr auto IsAgentDecayV = IsAgent<std::decay_t<Tp>>{};

template <class AgentsTuple, size_t I>
class Getter {
  static_assert(I < __CLOSTD::tuple_size_v<AgentsTuple>, "the index of Getter is out of bounds");
  static_assert(IsAgentDecayV<decltype(std::get<I>(std::declval<AgentsTuple&>()))>, "Getter's target is not an agent");

 public:
  Getter() noexcept = default;
  Getter(PH<I>) noexcept {}  // allow that the PlaceHolder can be implicitly converted to the Getter.
  // Mapping a Getter to an Agent.
  void Map(AgentsTuple& tuple) { agents_tuple_ = tuple; }

  decltype(auto) Get() const noexcept { return std::get<I>(agents_tuple_.Get()).Get(); }

  explicit operator bool() const noexcept { return static_cast<bool>(agents_tuple_); }

 private:
  Agent<AgentsTuple&> agents_tuple_;
};

template <class>
struct IsGetter : std::false_type {};

template <class Tuple, size_t I>
struct IsGetter<Getter<Tuple, I>> : std::true_type {};

template <class Tp>
constexpr auto IsGetterDecayV = IsGetter<std::decay_t<Tp>>{};

template <size_t I, class GettersTuple, class AgentsTuple,
          std::enable_if_t<IsGetterDecayV<decltype(std::get<I>(std::declval<GettersTuple>()))>, int> = 0>
void TryMap(GettersTuple&& getters, AgentsTuple& agents, int& cnt) {
  std::get<I>(std::forward<GettersTuple>(getters)).Map(agents);
  cnt++;
}

template <size_t I, class... Tps>
constexpr void TryMap(Tps&&...) noexcept {}

template <size_t... I, class GettersTuple, class AgentsTuple>
int MapGetters(std::index_sequence<I...>, GettersTuple&& getters, AgentsTuple& agents) {
  using expander = int[];
  int cnt = 0;
  (void)expander{(TryMap<I>(std::forward<GettersTuple>(getters), agents, cnt), 0)...};
  return cnt;
}

template <class GettersTuple, class AgentsTuple>
int MapGetters(GettersTuple&& getters, AgentsTuple& agents) {
  constexpr auto size = __CLOSTD::tuple_size_v<std::decay_t<GettersTuple>>;
  return MapGetters(std::make_index_sequence<size>{}, std::forward<GettersTuple>(getters), agents);
}

template <size_t I, class Tuple,
          std::enable_if_t<IsGetterDecayV<decltype(std::get<I>(std::declval<Tuple>()))>, int> = 0>
decltype(auto) Get(Tuple&& tuple) noexcept {
  return std::get<I>(std::forward<Tuple>(tuple)).Get();
}

template <size_t I, class Tuple,
          std::enable_if_t<!IsGetterDecayV<decltype(std::get<I>(std::declval<Tuple>()))>, int> = 0>
decltype(auto) Get(Tuple&& tuple) noexcept {
  return std::get<I>(std::forward<Tuple>(tuple));
}

};  // namespace placeholders

}  // namespace closure
