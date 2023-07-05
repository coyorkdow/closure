//
// Created by Youtao Guo on 20/1/23.
//

#pragma once

#include "closure/traits.hpp"
#include "closure/trivial_tuple.hpp"
#include "closure/util.hpp"

namespace closure {

namespace placeholders {
template <size_t>
struct PH {};

template <size_t First, size_t Last, class = std::enable_if_t<First<Last>> struct RangePH {};

template <class>
struct IsPlaceHolder : std::false_type {};

template <size_t I>
struct IsPlaceHolder<PH<I>> : std::true_type {};

template <class>
struct HasPlaceHolder : std::false_type {};

template <size_t I, class... Tps>
struct HasPlaceHolder<ArgList<PH<I>, Tps...>> : std::true_type {};

template <size_t I, size_t J, class... Tps>
struct HasPlaceHolder<ArgList<RangePH<I, J>, Tps...>> : std::true_type {};

template <class F, class... Os>
struct HasPlaceHolder<ArgList<F, Os...>> : HasPlaceHolder<ArgList<Os...>> {};

template <class>
struct HasRangePlaceHolder : std::false_type {};

template <size_t I, size_t J, class... Tps>
struct HasRangePlaceHolder<ArgList<RangePH<I, J>, Tps...>> : std::true_type {};
template <size_t I, size_t J, class... Tps>
struct HasRangePlaceHolder<ArgList<RangePH<I, J>&&, Tps...>> : std::true_type {};

template <class F, class... Os>
struct HasRangePlaceHolder<ArgList<F, Os...>> : HasRangePlaceHolder<ArgList<Os...>> {};

template <class Prefix>
struct ReplaceRangePlaceHolderWithPlaceHolders;

template <size_t I, class... Tps>
struct ReplaceRangePlaceHolderWithPlaceHolders<ArgList<RangePH<I, I + 1>, Tps...>> {
  using type =
      ConcatT<ArgList<PH<I>, PH<I + 1>>, typename ReplaceRangePlaceHolderWithPlaceHolders<ArgList<Tps...>>::type>;
};
template <size_t I, class... Tps>
struct ReplaceRangePlaceHolderWithPlaceHolders<ArgList<RangePH<I, I + 1>&&, Tps...>> {
  using type =
      ConcatT<ArgList<PH<I>, PH<I + 1>>, typename ReplaceRangePlaceHolderWithPlaceHolders<ArgList<Tps...>>::type>;
};

template <size_t I, size_t J, class... Tps>
struct ReplaceRangePlaceHolderWithPlaceHolders<ArgList<RangePH<I, J>, Tps...>> {
  using type = ConcatT<ArgList<PH<I>>,
                       typename ReplaceRangePlaceHolderWithPlaceHolders<ArgList<RangePH<I + 1, J>, Tps...>>::type>;
};
template <size_t I, size_t J, class... Tps>
struct ReplaceRangePlaceHolderWithPlaceHolders<ArgList<RangePH<I, J>&&, Tps...>> {
  using type = ConcatT<ArgList<PH<I>>,
                       typename ReplaceRangePlaceHolderWithPlaceHolders<ArgList<RangePH<I + 1, J>, Tps...>>::type>;
};

template <class Tp, class... Tps>
struct ReplaceRangePlaceHolderWithPlaceHolders<ArgList<Tp, Tps...>> {
  using type = ConcatT<ArgList<Tp>, typename ReplaceRangePlaceHolderWithPlaceHolders<ArgList<Tps...>>::type>;
};

template <>
struct ReplaceRangePlaceHolderWithPlaceHolders<ArgList<>> {
  using type = ArgList<>;
};

template <class Prefix>
using ReplaceRangePlaceHolderWithPlaceHoldersT = typename ReplaceRangePlaceHolderWithPlaceHolders<Prefix>::type;

namespace details {

template <class, class, class = void>
struct BoundPartitionImpl;

template <class... Before, class Tp, class... After>
struct BoundPartitionImpl<ArgList<Before...>, ArgList<Tp, After...>,
                          std::enable_if_t<!HasRangePlaceHolder<ArgList<Before...>>::value>>
    : BoundPartitionImpl<ArgList<Before..., Tp>, ArgList<After...>> {};

template <class... Before, size_t I, size_t J, class... After>
struct BoundPartitionImpl<ArgList<Before...>, ArgList<RangePH<I, J>, After...>,
                          std::enable_if_t<!HasRangePlaceHolder<ArgList<Before...>>::value>> {
  using former = ArgList<Before...>;
  using range_placeholders = ReplaceRangePlaceHolderWithPlaceHoldersT<ArgList<RangePH<I, J>>>;
  using latter = ArgList<After...>;
};

template <class... Before, size_t I, size_t J, class... After>
struct BoundPartitionImpl<ArgList<Before...>, ArgList<RangePH<I, J>&&, After...>,
                          std::enable_if_t<!HasRangePlaceHolder<ArgList<Before...>>::value>> {
  using former = ArgList<Before...>;
  using range_placeholders = ReplaceRangePlaceHolderWithPlaceHoldersT<ArgList<RangePH<I, J>>>;
  using latter = ArgList<After...>;
};

template <class... Before>
struct BoundPartitionImpl<ArgList<Before...>, ArgList<>,
                          std::enable_if_t<!HasRangePlaceHolder<ArgList<Before...>>::value>> {
  using former = ArgList<Before...>;
  using range_placeholders = ArgList<>;
  using latter = ArgList<>;
};

template <size_t I, size_t J, class... After>
struct BoundPartitionImpl<ArgList<RangePH<I, J>>, ArgList<After...>> {
  using former = ArgList<>;
  using range_placeholders = ReplaceRangePlaceHolderWithPlaceHoldersT<ArgList<RangePH<I, J>>>;
  using latter = ArgList<After...>;
};

template <size_t I, size_t J, class... After>
struct BoundPartitionImpl<ArgList<RangePH<I, J>&&>, ArgList<After...>> {
  using former = ArgList<>;
  using range_placeholders = ReplaceRangePlaceHolderWithPlaceHoldersT<ArgList<RangePH<I, J>>>;
  using latter = ArgList<After...>;
};

}  // namespace details

template <class Tp>
struct BoundPartition {};

template <class Tp, class... Tps>
struct BoundPartition<ArgList<Tp, Tps...>> : details::BoundPartitionImpl<ArgList<Tp>, ArgList<Tps...>> {};

template <>
struct BoundPartition<ArgList<>> {
  using former = ArgList<>;
  using range_placeholders = ArgList<>;
  using latter = ArgList<>;
};

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

// Agent stores the reference of the placeholder corresponding argument, and the argument can be accessed by Getter.
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

template <size_t I, class AgentsTuple, class... Os>
struct HasGetter<ArgList<Getter<AgentsTuple, I>, Os...>> : std::true_type, AgentsType<AgentsTuple> {};

template <class F, class... Os>
struct HasGetter<ArgList<F, Os...>> : HasGetter<ArgList<Os...>> {};

template <class>
struct HasNonGetter;

template <>
struct HasNonGetter<ArgList<>> : std::false_type {};

template <class F, class... Os>
struct HasNonGetter<ArgList<F, Os...>> : std::true_type {};

template <size_t I, class AgentsTuple, class... Os>
struct HasNonGetter<ArgList<Getter<AgentsTuple, I>, Os...>> : HasNonGetter<ArgList<Os...>> {};

template <size_t I, class Tuple, class Agents,
          std::enable_if_t<IsGetter<std::decay_t<decltype(tuple::get<I>(std::declval<Tuple>()))>>::value, int> = 0>
decltype(auto) TryMapAndGet(Tuple&& tuple, Agents&& agents) noexcept {
  return tuple::get<I>(std::forward<Tuple>(tuple)).Get(agents);
}

template <size_t I, class Tuple, class Agents,
          std::enable_if_t<!IsGetter<std::decay_t<decltype(tuple::get<I>(std::declval<Tuple>()))>>::value, int> = 0>
decltype(auto) TryMapAndGet(Tuple&& tuple, Agents&&) noexcept {
  return tuple::get<I>(std::forward<Tuple>(tuple));
}

};  // namespace placeholders

template <size_t I>
constexpr auto PlaceHolder() noexcept {
  return placeholders::PH<I>{};
}

template <size_t I, size_t J>
constexpr auto PlaceHolder() noexcept {
  return placeholders::RangePH<I, J>{};
}

}  // namespace closure
