//
// Created by Youtao Guo on 20/1/23.
//

#pragma once

#include <type_traits>

#ifdef __CLOSTD
#error "Macro __CLOSTD is already defined"
#endif

namespace closure {
#if __cplusplus < 201703L
#define __CLOSTD __clostd

namespace __clostd {
template <class Tuple>
constexpr auto tuple_size_v = std::tuple_size<Tuple>::value;

template <class From, class To>
constexpr auto is_convertible_v = std::is_convertible<From, To>::value;

template <class Tp1, class Tp2>
constexpr auto is_same_v = std::is_same<Tp1, Tp2>::value;

template <class Tp>
constexpr auto is_const_v = std::is_const<Tp>::value;

template <class Tp>
constexpr auto is_pointer_v = std::is_pointer<Tp>::value;

template <class Tp>
constexpr auto is_reference_v = std::is_reference<Tp>::value;

template <class...>
using void_t = void;

template <class Tp>
constexpr bool is_function_v = std::is_function<Tp>::value;

template <class Tp>
constexpr bool is_copy_constructible_v = std::is_copy_constructible<Tp>::value;

// Implementing invoke_result, source https://en.cppreference.com/w/cpp/types/result_of
namespace detail {
template <class T>
struct is_reference_wrapper : std::false_type {};
template <class U>
struct is_reference_wrapper<std::reference_wrapper<U>> : std::true_type {};

template <class T>
struct invoke_impl {
  template <class F, class... Args>
  static auto call(F&& f, Args&&... args) -> decltype(std::forward<F>(f)(std::forward<Args>(args)...));
};

template <class B, class MT>
struct invoke_impl<MT B::*> {
  template <class T, class Td = typename std::decay<T>::type,
            class = typename std::enable_if<std::is_base_of<B, Td>::value>::type>
  static auto get(T&& t) -> T&&;

  template <class T, class Td = typename std::decay<T>::type,
            class = typename std::enable_if<is_reference_wrapper<Td>::value>::type>
  static auto get(T&& t) -> decltype(t.get());

  template <class T, class Td = typename std::decay<T>::type,
            class = typename std::enable_if<!std::is_base_of<B, Td>::value>::type,
            class = typename std::enable_if<!is_reference_wrapper<Td>::value>::type>
  static auto get(T&& t) -> decltype(*std::forward<T>(t));

  template <class T, class... Args, class MT1, class = typename std::enable_if<std::is_function<MT1>::value>::type>
  static auto call(MT1 B::*pmf, T&& t, Args&&... args)
      -> decltype((invoke_impl::get(std::forward<T>(t)).*pmf)(std::forward<Args>(args)...));

  template <class T>
  static auto call(MT B::*pmd, T&& t) -> decltype(invoke_impl::get(std::forward<T>(t)).*pmd);
};

template <class F, class... Args, class Fd = typename std::decay<F>::type>
auto INVOKE(F&& f, Args&&... args) -> decltype(invoke_impl<Fd>::call(std::forward<F>(f), std::forward<Args>(args)...));

}  // namespace detail

// Conforming C++14 implementation (is also a valid C++11 implementation):
namespace detail {
template <typename AlwaysVoid, typename, typename...>
struct invoke_result {};
template <typename F, typename... Args>
struct invoke_result<decltype(void(detail::INVOKE(std::declval<F>(), std::declval<Args>()...))), F, Args...> {
  using type = decltype(detail::INVOKE(std::declval<F>(), std::declval<Args>()...));
};
}  // namespace detail

template <class F, class... ArgTypes>
struct invoke_result : detail::invoke_result<void, F, ArgTypes...> {};

template <class F, class... ArgTypes>
using invoke_result_t = typename invoke_result<F, ArgTypes...>::type;

// End of implementing invoke_result

}  // namespace __clostd
#else
#define __CLOSTD std
#endif

namespace traits {

template <class Tp, __CLOSTD::void_t<decltype(&Tp::operator->), decltype(&Tp::operator*)>* = nullptr>
auto IsDereferencableImpl(int) -> std::integral_constant<
    bool, __CLOSTD::is_pointer_v<decltype(std::declval<Tp>().operator->())> &&
              __CLOSTD::is_reference_v<decltype(std::declval<Tp>().operator*())> &&
              __CLOSTD::is_same_v<std::remove_pointer_t<decltype(std::declval<Tp>().operator->())>,
                                  std::remove_reference_t<decltype(std::declval<Tp>().operator*())>>>;

template <class Tp>
auto IsDereferencableImpl(...) -> std::is_pointer<Tp>;

template <class Ptr, class Tar>
struct IsDereferencable
    : std::integral_constant<bool,
                             decltype(IsDereferencableImpl<Ptr>(0))::value &&
                                 __CLOSTD::is_same_v<std::remove_reference_t<decltype(*std::declval<Ptr>())>, Tar>> {};

template <class Dereferencable, class Method, class... Args>
auto TryCallMethod(int, Dereferencable&&, Method&&, Args&&...)
    -> decltype((std::declval<Dereferencable>()->*std::declval<Method>())(std::declval<Args>()...), std::true_type{});

auto TryCallMethod(float, ...) -> std::false_type;

template <class Ptr, class Tar>
constexpr auto IsDereferencableV = IsDereferencable<Ptr, Tar>::value;

template <class Dereferencable, class Method>
struct CanUsePointerToMemberFunction : std::false_type {};

template <class Dereferencable, class R, class Class, class... Args>
struct CanUsePointerToMemberFunction<Dereferencable, R (Class::*)(Args...)>
    : decltype(TryCallMethod(0, std::declval<Dereferencable>(), std::declval<R (Class::*)(Args...)>(),
                             std::declval<Args>...)) {};

template <class Dereferencable, class R>
constexpr auto CanUsePointerToMemberFunctionV = CanUsePointerToMemberFunction<Dereferencable, R>::value;

template <class Tp>
auto IsFunctorImpl(int) -> decltype(&Tp::operator(), std::true_type{});

template <class Tp>
auto IsFunctorImpl(...) -> std::false_type;

template <class Tp>
struct IsFunctor : decltype(IsFunctorImpl<Tp>(0)) {};

template <class Tp>
constexpr auto IsFunctorV = IsFunctor<Tp>::value;

}  // namespace traits

}  // namespace closure
