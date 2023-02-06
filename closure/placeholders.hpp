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

template <class>
struct HasPlaceHolder : std::false_type {};

template <size_t I, class... Tps>
struct HasPlaceHolder<ArgList<PH<I>, Tps...>> : std::true_type {};

template <class F, class... Os>
struct HasPlaceHolder<ArgList<F, Os...>> : HasPlaceHolder<ArgList<Os...>> {};

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
 public:
  Agent() noexcept : ptr_(nullptr) {}
  explicit Agent(Tp&& v) noexcept : ptr_(&v) {}
  Agent& operator=(Tp&& v) noexcept {
    ptr_ = &v;
    return *this;
  }
  explicit operator bool() const noexcept { return ptr_; }
  Tp&& Target() const noexcept { return std::forward<Tp>(*ptr_); }

 private:
  std::remove_reference_t<Tp>* ptr_;
};

template <>
class Agent<Any> {
 public:
  constexpr Agent() noexcept = default;
  template <class Tp>
  explicit constexpr Agent(Tp&&) noexcept {}
  explicit constexpr operator bool() const noexcept { return false; }
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

template <class AgentsTuple, size_t... I, size_t... J, class... Args, class Callback, class... CallbackArgs>
decltype(auto) MakeAgentsTupleAndApply(std::index_sequence<I...>, std::index_sequence<J...>, std::tuple<Args...>&& args,
                                       Callback&& callback, CallbackArgs&&... callback_args) {
  return callback(std::forward<CallbackArgs>(callback_args)...,
                  AgentsTuple{std::get<I>(std::forward<decltype(args)>(args))...},
                  std::get<sizeof...(I) + J>(std::forward<decltype(args)>(args))...);
}

template <class AgentsTuple, size_t I>
class Getter {
  static_assert(!std::is_reference<AgentsTuple>::value, "the given AgentsTuple's type for Getter is reference type");
  static_assert(I < std::tuple_size<AgentsTuple>::value, "the index of Getter is out of bounds");
  static_assert(IsAgent<std::decay_t<decltype(std::get<I>(std::declval<AgentsTuple&>()))>>::value,
                "Getter's target is not an agent");

 public:
  using get_result_type = decltype(std::get<I>(std::declval<AgentsTuple&>()).Target());

  Getter() noexcept = default;
  explicit Getter(PH<I>) noexcept {}  // allow that the PlaceHolder can convert to the Getter.
  // Mapping a Getter to an Agent.
  get_result_type Get(AgentsTuple& agents) const noexcept { return std::get<I>(agents).Target(); }

  Getter(const Getter&) noexcept = default;  // A pseudo copy constructor, which is only used for copy traits
  Getter& operator=(const Getter&) = delete;
};

template <class>
struct IsGetter : std::false_type {};

template <class AgentsTuple, size_t I>
struct IsGetter<Getter<AgentsTuple, I>> : std::true_type {};

template <class AgentsTuple>
struct AgentsType {
  using agents_type = AgentsTuple;
};

template <class>
struct HasGetter : std::false_type, AgentsType<void> {};

template <size_t I, class AgentsTuple, class... Tps>
struct HasGetter<ArgList<Getter<AgentsTuple, I>, Tps...>> : std::true_type, AgentsType<AgentsTuple> {};

template <class F, class... Os>
struct HasGetter<ArgList<F, Os...>> : HasGetter<ArgList<Os...>> {};

template <size_t I, class Tuple, class Agents,
          std::enable_if_t<IsGetter<std::decay_t<decltype(std::get<I>(std::declval<Tuple>()))>>::value, int> = 0>
decltype(auto) TryMapAndGet(Tuple&& tuple, Agents&& agents) noexcept {
  return std::get<I>(std::forward<Tuple>(tuple)).Get(agents);
}

template <size_t I, class Tuple, class Agents,
          std::enable_if_t<!IsGetter<std::decay_t<decltype(std::get<I>(std::declval<Tuple>()))>>::value, int> = 0>
decltype(auto) TryMapAndGet(Tuple&& tuple, Agents&&) noexcept {
  return std::get<I>(std::forward<Tuple>(tuple));
}

};  // namespace placeholders

}  // namespace closure
