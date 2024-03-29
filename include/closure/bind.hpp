//
// Created by Youtao Guo on 2023/1/20.
//

#pragma once

#include "placeholders.hpp"

namespace closure {

namespace closureimpl {

template <class, class>
struct IsPrefixWeak : std::false_type {};

template <class... Args2>
struct IsPrefixWeak<ArgList<>, ArgList<Args2...>> : std::true_type {};

template <class F1, class... Os1, class F2, class... Os2>
struct IsPrefixWeak<ArgList<F1, Os1...>, ArgList<F2, Os2...>>
    : std::conditional_t<placeholders::IsPlaceHolder<F1>::value || std::is_convertible<F1, F2>::value,
                         IsPrefixWeak<ArgList<Os1...>, ArgList<Os2...>>, std::false_type> {};

template <size_t N, class ArgL, class = void>
struct TailN;

template <size_t N, class F, class... Os>
struct TailN<N, ArgList<F, Os...>, std::enable_if_t<0 < N>> : TailN<N - 1, ArgList<Os...>> {};

template <class... Args>
struct TailN<0, ArgList<Args...>> {
  using type = ArgList<Args...>;
};

template <size_t N, class ArgL, class = void>
struct HeadN;

template <size_t N, class F, class... Os>
struct HeadN<N, ArgList<F, Os...>, std::enable_if_t<0 < N>> {
  using type = ConcatT<ArgList<F>, typename HeadN<N - 1, ArgList<Os...>>::type>;
};

template <class... Args>
struct HeadN<0, ArgList<Args...>> {
  using type = ArgList<>;
};

template <class Prefix, class ArgL>
struct RemovePrefixWeak;

template <>
struct RemovePrefixWeak<ArgList<>, ArgList<>> {
  using type = ArgList<>;
};

template <class... Args2>
struct RemovePrefixWeak<ArgList<>, ArgList<Args2...>> {
  using type = ArgList<Args2...>;
};

template <class F1, class... Os1, class F2, class... Os2>
struct RemovePrefixWeak<ArgList<F1, Os1...>, ArgList<F2, Os2...>> {
  using type = typename RemovePrefixWeak<ArgList<Os1...>, ArgList<Os2...>>::type;
};

template <class Prefix, class ArgL>
using RemovePrefixWeakT = typename RemovePrefixWeak<Prefix, ArgL>::type;

template <class Prefix, class ArgL>
struct GetPlaceHoldersCorrespondTypes;

template <size_t I, class... Os1, class F2, class... Os2>
struct GetPlaceHoldersCorrespondTypes<ArgList<placeholders::PH<I>, Os1...>, ArgList<F2, Os2...>> {
  using type = ConcatT<ArgList<F2>, typename GetPlaceHoldersCorrespondTypes<ArgList<Os1...>, ArgList<Os2...>>::type>;
};

template <class F1, class... Os1, class F2, class... Os2>
struct GetPlaceHoldersCorrespondTypes<ArgList<F1, Os1...>, ArgList<F2, Os2...>> {
  using type = typename GetPlaceHoldersCorrespondTypes<ArgList<Os1...>, ArgList<Os2...>>::type;
};

template <class... Tps>
struct GetPlaceHoldersCorrespondTypes<ArgList<>, ArgList<Tps...>> {
  using type = ArgList<>;
};

template <class Prefix, class ArgL>
using GetPlaceHoldersCorrespondTypesT = typename GetPlaceHoldersCorrespondTypes<Prefix, ArgL>::type;

namespace sort {

template <class PHs, size_t I>
struct Max;

template <size_t F, class... Os, size_t I>
struct Max<ArgList<placeholders::PH<F>, Os...>, I> : Max<ArgList<Os...>, (I < F ? F : I)> {};

template <size_t I>
struct Max<ArgList<>, I> : std::integral_constant<size_t, I> {};

template <class PHs>
struct MaxPlaceHolders;

template <size_t F, class... Os>
struct MaxPlaceHolders<ArgList<placeholders::PH<F>, Os...>> : Max<ArgList<Os...>, F> {};

enum class Cond { Unchecked, False, True };

template <size_t I, class Tp>
struct Component {};

template <class Com, class Sorted, Cond = Cond::Unchecked>
struct Insert;

template <size_t I, class Tp, size_t FI, class FT, class... Os>
struct Insert<Component<I, Tp>, ArgList<Component<FI, FT>, Os...>, Cond::Unchecked> {
  using type =
      typename Insert<Component<I, Tp>, ArgList<Component<FI, FT>, Os...>, (I <= FI) ? Cond::True : Cond::False>::type;
};

template <size_t I, class Tp, class... Os>
struct Insert<Component<I, Tp>, ArgList<Os...>, Cond::True> {
  using type = ArgList<Component<I, Tp>, Os...>;
};

template <size_t I, class Tp, size_t FI, class FT, class... Os>
struct Insert<Component<I, Tp>, ArgList<Component<FI, FT>, Os...>, Cond::False> {
  using type = ConcatT<ArgList<Component<FI, FT>>, typename Insert<Component<I, Tp>, ArgList<Os...>>::type>;
};

template <size_t I, class Tp>
struct Insert<Component<I, Tp>, ArgList<>> {
  using type = ArgList<Component<I, Tp>>;
};

template <class ArgL, class PHL>
struct Sort;

template <class Tp, class... Tps, size_t FI, class... Os>
struct Sort<ArgList<Tp, Tps...>, ArgList<placeholders::PH<FI>, Os...>> {
  using type = typename Insert<Component<FI, Tp>, typename Sort<ArgList<Tps...>, ArgList<Os...>>::type>::type;
};

template <class Tp, size_t FI>
struct Sort<ArgList<Tp>, ArgList<placeholders::PH<FI>>> {
  using type = ArgList<Component<FI, Tp>>;
};

template <class ArgL, class PHL>
using SortT = typename Sort<ArgL, PHL>::type;

template <class Sorted>
struct Unique;

template <size_t I, class Tp1, class Tp2, class... Os>
struct Unique<ArgList<Component<I, Tp1>, Component<I, Tp2>, Os...>> {
  using type = typename Unique<ArgList<Component<I, Tp1>, Os...>>::type;
};

template <size_t I, class Tp, class... Os>
struct Unique<ArgList<Component<I, Tp>, Os...>> {
  using type = ConcatT<ArgList<Component<I, Tp>>, typename Unique<ArgList<Os...>>::type>;
};

template <>
struct Unique<ArgList<>> {
  using type = ArgList<>;
};

template <class Sorted>
using UniqueT = typename Unique<Sorted>::type;

template <class Uniqued, class = void>
struct Fill;

template <size_t I, class Tp1, class Tp2, class... Os>
struct Fill<ArgList<Component<I, Tp1>, Component<I + 1, Tp2>, Os...>> {
  using type = ConcatT<ArgList<Component<I, Tp1>>, typename Fill<ArgList<Component<I + 1, Tp2>, Os...>>::type>;
};

template <size_t I1, class Tp1, size_t I2, class Tp2, class... Os>
struct Fill<ArgList<Component<I1, Tp1>, Component<I2, Tp2>, Os...>, std::enable_if_t<I1 + 1 < I2>> {
  using type = ConcatT<ArgList<Component<I1, Tp1>>,
                       typename Fill<ArgList<Component<I1 + 1, Any>, Component<I2, Tp2>, Os...>>::type>;
};

template <size_t I, class Tp>
struct Fill<ArgList<Component<I, Tp>>> {
  using type = ArgList<Component<I, Tp>>;
};

template <class Tp, class... Os>
auto FillFromZero(ArgList<Component<0, Tp>, Os...>) -> typename Fill<ArgList<Component<0, Tp>, Os...>>::type;

template <size_t I, class Tp, class... Os /*, class = std::enable_if_t<I != 0>*/>
auto FillFromZero(ArgList<Component<I, Tp>, Os...>) ->
    typename Fill<ArgList<Component<0, Any>, Component<I, Tp>, Os...>>::type;

template <class Uniqued>
using FillFromZeroT = decltype(FillFromZero(std::declval<Uniqued>()));

template <size_t... I, class... Tps>
auto RemoveIndices(ArgList<Component<I, Tps>...>) -> ArgList<Tps...>;

template <class Arg>
using RemoveIndicesT = decltype(RemoveIndices(std::declval<Arg>()));

// For sort testing

template <size_t... I, class... Tps>
auto TEST_RemoveArgs(ArgList<Component<I, Tps>...>) -> std::index_sequence<I...>;

template <size_t N>
struct TEST_MakeDummyArgs {
  using type = ConcatT<typename TEST_MakeDummyArgs<N - 1>::type, ArgList<void>>;
};

template <>
struct TEST_MakeDummyArgs<0> {
  using type = ArgList<>;
};

template <class... PHs>
struct TEST_Sort {
  using sorted = SortT<typename TEST_MakeDummyArgs<sizeof...(PHs)>::type, ArgList<PHs...>>;
  using type = decltype(TEST_RemoveArgs(std::declval<sorted>()));
};

}  // namespace sort

template <class ArgL, class PHL>
struct SortUniqueFillPlaceHoldersCorrespondTypes;

template <class... Tps, size_t... I>
struct SortUniqueFillPlaceHoldersCorrespondTypes<ArgList<Tps...>, ArgList<placeholders::PH<I>...>> {
  using type = sort::RemoveIndicesT<
      sort::FillFromZeroT<sort::UniqueT<sort::SortT<ArgList<Tps...>, ArgList<placeholders::PH<I>...>>>>>;
};

template <class ArgL, class PHL>
using SortUniqueFillPlaceHoldersCorrespondTypesT = typename SortUniqueFillPlaceHoldersCorrespondTypes<ArgL, PHL>::type;

template <class ArgL, class Tuple>
struct ReplacePlaceHoldersWithGettersImpl;

template <class F, class... Os, class Tuple>
struct ReplacePlaceHoldersWithGettersImpl<ArgList<F, Os...>, Tuple> {
  using type = ConcatT<ArgList<F>, typename ReplacePlaceHoldersWithGettersImpl<ArgList<Os...>, Tuple>::type>;
};

template <size_t I, class... Os, class Tuple>
struct ReplacePlaceHoldersWithGettersImpl<ArgList<placeholders::PH<I>, Os...>, Tuple> {
  using type = ConcatT<ArgList<placeholders::Getter<Tuple, I>>,
                       typename ReplacePlaceHoldersWithGettersImpl<ArgList<Os...>, Tuple>::type>;
};

template <class Tuple>
struct ReplacePlaceHoldersWithGettersImpl<ArgList<>, Tuple> {
  using type = ArgList<>;
};

template <class Prefix, class ArgL>
struct ReplacePlaceHoldersWithGetters {
  static_assert(IsPrefixWeak<Prefix, ArgL>::value, "template argument 1 is not the weak prefix of template argument 2");

 private:
  using ph_args = GetPlaceHoldersCorrespondTypesT<Prefix, ArgL>;
  using phl = placeholders::FilterPlaceHolderT<Prefix>;

 public:
  using agents_prototype = SortUniqueFillPlaceHoldersCorrespondTypesT<ph_args, phl>;
  using type = typename ReplacePlaceHoldersWithGettersImpl<Prefix, placeholders::MakeAgentsT<agents_prototype>>::type;
};

template <class PrePrefix, class ArgL>
using ReplacePlaceHoldersWithGettersT = typename ReplacePlaceHoldersWithGetters<PrePrefix, ArgL>::type;

template <class Prefix, class ArgL>
struct GenerateGettersFromClosureArgs {
 private:
  using phl = placeholders::FilterPlaceHolderT<Prefix>;
  static constexpr auto max = sort::MaxPlaceHolders<phl>::value;

 public:
  using agents_prototype = typename HeadN<max + 1, ArgL>::type;
  using type = typename ReplacePlaceHoldersWithGettersImpl<Prefix, placeholders::MakeAgentsT<agents_prototype>>::type;
};

template <class PrePrefix, class ArgL>
using GenerateGettersFromClosureArgsT = typename GenerateGettersFromClosureArgs<PrePrefix, ArgL>::type;

template <class... Args, std::enable_if_t<(sizeof...(Args) > 0), int> = 0>
auto FlatBoundArguments(std::tuple<Args...>&& args) noexcept ->
    typename placeholders::ReplaceRangePlaceHolderWithPlaceHoldersT<ArgList<Args...>>::forward_tuple;

auto FlatBoundArguments(std::tuple<>&& args) noexcept { return std::tuple<>{}; }

template <size_t... I, class... Args1, size_t... J, class... Args2>
auto ConcatTuple(std::index_sequence<I...>, std::index_sequence<J...>, std::tuple<Args1...>&& args1,
                 std::tuple<Args2...>&& args2) noexcept {
  static_assert(sizeof...(I) == sizeof...(Args1), "");
  static_assert(sizeof...(J) == sizeof...(Args2), "");
  return std::forward_as_tuple(std::move(std::get<I>(std::move(args1)))...,
                               std::move(std::get<J>(std::move(args2)))...);
}

template <class... Args, size_t... I, size_t... J, size_t... K>
auto ProcessPartition(std::index_sequence<I...>, std::index_sequence<J...>, std::tuple<Args...>&& args,
                      ArgList<placeholders::PH<K>...>) noexcept {
  constexpr size_t partition = sizeof...(K) == 0 ? 0 : 1;
  static_assert(sizeof...(I) + sizeof...(J) + partition == sizeof...(Args), "");
  auto former_part = std::forward_as_tuple(std::get<I>(std::move(std::move(args)))..., PlaceHolder<K>()...);
  auto latter_part =
      FlatBoundArguments(std::forward_as_tuple(std::move(std::get<J + sizeof...(I) + partition>(std::move(args)))...));
  return ConcatTuple(std::make_index_sequence<sizeof...(I) + sizeof...(K)>{},
                     std::make_index_sequence<std::tuple_size<decltype(latter_part)>::value>{}, std::move(former_part),
                     std::move(latter_part));
}

template <class... Args, std::enable_if_t<(sizeof...(Args) > 0), int>>
auto FlatBoundArguments(std::tuple<Args...>&& args) noexcept ->
    typename placeholders::ReplaceRangePlaceHolderWithPlaceHoldersT<ArgList<Args...>>::forward_tuple {
  using namespace placeholders;
  using args_list = ArgList<Args...>;
  using former = typename BoundPartition<args_list>::former;
  using latter = typename BoundPartition<args_list>::latter;
  using range_placeholders = typename BoundPartition<args_list>::range_placeholders;
  return ProcessPartition(std::make_index_sequence<former::size>{}, std::make_index_sequence<latter::size>{},  //
                          std::move(args), range_placeholders{});
}

}  // namespace closureimpl

}  // namespace closure
