//
// Created by Youtao Guo on 2022/12/27
//

#pragma once

#include <cstring>
#include <memory>
#include <tuple>

#include "closure/bind.hpp"
#include "closure/placeholders.hpp"
#include "closure/traits.hpp"

namespace closure {

template <size_t I>
constexpr auto PlaceHolder() noexcept {
  return placeholders::PH<I>{};
}

namespace __closure {

template <class>
class ClosureImplBase;

template <class R, class... Args>
class ClosureImplBase<R(Args...)> {
 public:
  using result_type = R;
  using arguments_type = ArgList<Args...>;
  using closure_type = R(Args...);
  ClosureImplBase() = default;
  virtual ~ClosureImplBase() = default;
  virtual result_type Run(Args&&...) = 0;
  virtual ClosureImplBase<R(Args...)>* Copy() const { return nullptr; }
  virtual bool Copyable() const { return false; }
};

template <class ClosureArg, class Callee, class CalleeArgsList, class = void>
class ClosureImpl;

// Overload for function pointer and functor (copy constructible)
// It is guaranteed that the Callable and all the StoredArgs... are non-reference types.
template <class R, class... Args, class Callable, class... StoredArgs>
class ClosureImpl<R(Args...), Callable, ArgList<StoredArgs...>,
                  std::enable_if_t<(__CLOSTD::is_function_v<std::remove_pointer_t<Callable>> ||
                                    traits::IsFunctorV<Callable>)&&/* requires the class is copy constructible */
                                   __CLOSTD::is_copy_constructible_v<Callable> &&
                                   __CLOSTD::is_copy_constructible_v<std::tuple<StoredArgs...>>>>
    : public ClosureImplBase<R(Args...)> {
  using base = ClosureImplBase<R(Args...)>;

 public:
  using callee_type = Callable;
  using stored_types = std::tuple<StoredArgs...>;

  static_assert(
      __CLOSTD::is_convertible_v<
          __CLOSTD::invoke_result_t<
              callee_type, std::add_lvalue_reference_t<StoredArgs>... /*stored args are passed by lvalue*/, Args...>,
          typename base::result_type>,
      "the result type of the given callee is not match");

  template <class Callee, class... BoundArgs,
            std::enable_if_t<!placeholders::HasPlaceHolderV<std::decay_t<BoundArgs>...>, int> = 0>
  explicit ClosureImpl(Callee&& f, BoundArgs&&... args)
      : callable_(std::forward<Callee>(f)), stored_list_(std::forward<BoundArgs>(args)...) {}

  ClosureImpl(const ClosureImpl& rhs) = default;
  base* Copy() const override { return new ClosureImpl(*this); }
  bool Copyable() const override { return true; }

  typename base::result_type Run(Args&&... args) override {
    return RunHelper(std::index_sequence_for<StoredArgs...>{}, std::forward<Args>(args)...);
  }

 private:
  template <std::size_t... I>
  decltype(auto) RunHelper(std::index_sequence<I...>, Args&&... args) {
    return callable_(std::get<I>(stored_list_)..., std::forward<Args>(args)...);
  }

  callee_type callable_;
  stored_types stored_list_;
};

// Overload for function pointer and functor (non copy constructible)
// It is guaranteed that the Callable and all the StoredArgs... are non-reference types.
template <class R, class... Args, class Callable, class... StoredArgs>
class ClosureImpl<R(Args...), Callable, ArgList<StoredArgs...>,
                  std::enable_if_t<(__CLOSTD::is_function_v<std::remove_pointer_t<Callable>> ||
                                    traits::IsFunctorV<Callable>)&&/* requires the class is non copy constructible */
                                   !(__CLOSTD::is_copy_constructible_v<Callable> &&
                                     __CLOSTD::is_copy_constructible_v<std::tuple<StoredArgs...>>)>>
    : public ClosureImplBase<R(Args...)> {
  using base = ClosureImplBase<R(Args...)>;

 public:
  using callee_type = Callable;
  using stored_types = std::tuple<StoredArgs...>;

  static_assert(
      __CLOSTD::is_convertible_v<
          __CLOSTD::invoke_result_t<
              callee_type, std::add_lvalue_reference_t<StoredArgs>... /*stored args are passed by lvalue*/, Args...>,
          typename base::result_type>,
      "the result type of the given callee is not match");

  template <class Callee, class... BoundArgs,
            std::enable_if_t<!placeholders::HasPlaceHolderV<std::decay_t<BoundArgs>...>, int> = 0>
  explicit ClosureImpl(Callee&& f, BoundArgs&&... args)
      : callable_(std::forward<Callee>(f)), stored_list_(std::forward<BoundArgs>(args)...) {}

  typename base::result_type Run(Args&&... args) override {
    return RunHelper(std::index_sequence_for<StoredArgs...>{}, std::forward<Args>(args)...);
  }

 private:
  template <std::size_t... I>
  decltype(auto) RunHelper(std::index_sequence<I...>, Args&&... args) {
    return callable_(std::get<I>(stored_list_)..., std::forward<Args>(args)...);
  }

  callee_type callable_;
  stored_types stored_list_;
};

// TODO Overload for class method
// It is guaranteed that Method, the CPtr, and all the StoredArgs... are non-reference types.
template <class R, class... Args, class Method, class CPtr, class... StoredArgs>
class ClosureImpl<R(Args...), Method, ArgList<CPtr, StoredArgs...>,
                  std::enable_if_t<__CLOSTD::is_member_function_pointer_v<Method> && traits::IsDereferencableV<CPtr> &&
                                   traits::CanUsePointerToMemberFunctionV<CPtr, Method>>>
    : public ClosureImplBase<R(Args...)> {
  using base = ClosureImplBase<R(Args...)>;

  static_assert(traits::CanUsePointerToMemberFunctionV<CPtr, Method>, "");

 public:
  using callee_type = Method;
  using stored_types = std::tuple<CPtr, StoredArgs...>;

  template <class CArg, class... BoundArgs,
            std::enable_if_t<!placeholders::HasPlaceHolderV<std::decay_t<BoundArgs>...>, int> = 0>
  explicit ClosureImpl(callee_type f, CArg&& carg, BoundArgs&&... args)
      : callable_(f), stored_list_(std::forward<CArg>(carg), std::forward<BoundArgs>(args)...) {}

  typename base::result_type Run(Args&&... args) override {
    return RunHelper(std::index_sequence_for<StoredArgs...>{}, std::forward<Args>(args)...);
  }

 private:
  template <std::size_t... I>
  typename base::result_type RunHelper(std::index_sequence<I...>, Args&&... args) {
    return ((*std::get<0>(stored_list_)).*callable_)(std::get<I + 1>(stored_list_)..., std::forward<Args>(args)...);
  }

 private:
  callee_type callable_;
  stored_types stored_list_;
};

// Overload for function pointer
template <class R, class... ClosureArgs, class... Args, class... Bounds,
          std::enable_if_t<!placeholders::HasPlaceHolderV<Bounds...>, int> = 0>
decltype(auto) MakeClosureImpl(ArgList<ClosureArgs...>, R (*func)(Args...), Bounds&&... bound_args) {
  return new ClosureImpl<R(ClosureArgs...), R (*)(Args...), ArgList<std::remove_reference_t<Bounds>...>>(
      func, std::forward<Bounds>(bound_args)...);
}

// Overload for functor
template <
    class R, class... ClosureArgs, class Functor, class... Bounds,
    std::enable_if_t<traits::IsFunctorV<std::remove_reference_t<Functor>> && !placeholders::HasPlaceHolderV<Bounds...>,
                     int> = 0>
decltype(auto) MakeClosureImpl(ArgList<ClosureArgs...>, Functor&& functor, Bounds&&... bound_args) {
  return new ClosureImpl<R(ClosureArgs...), std::remove_reference_t<Functor>,
                         ArgList<std::remove_reference_t<Bounds>...>>(std::forward<Functor>(functor),
                                                                      std::forward<Bounds>(bound_args)...);
}

// Overload for class method
template <class R, class... ClosureArgs, class Method, class CPtr, class... Bounds,
          std::enable_if_t<__CLOSTD::is_member_function_pointer_v<Method> && !placeholders::HasPlaceHolderV<Bounds...>,
                           int> = 0>
decltype(auto) MakeClosureImpl(ArgList<ClosureArgs...>, Method method, CPtr&& cptr, Bounds&&... bound_args) {
  return new ClosureImpl<R(ClosureArgs...), Method,
                         ArgList<std::remove_reference_t<CPtr>, std::remove_reference_t<Bounds>...>>(
      method, std::forward<CPtr>(cptr), std::forward<Bounds>(bound_args)...);
}

// TODO Overload for closure which has placeholders.
template <class R, class... ClosureArgs, class... Args, class... Bounds,
          std::enable_if_t<placeholders::HasPlaceHolderV<Bounds...>, int> = 0>
decltype(auto) MakeClosureImpl(ArgList<ClosureArgs...>, R (*func)(Args...), Bounds&&... bind_args) {}

}  // namespace __closure

template <class>
class Closure;

template <class R, class... Args>
class Closure<R(Args...)> {
  using impl_base_type = __closure::ClosureImplBase<R(Args...)>;

 public:
  using result_type = R;
  using arguments_type = typename impl_base_type::arguments_type;

  Closure() = default;

  template <class... FuncArgs, class... Bounds>
  explicit Closure(R (*func)(FuncArgs...), Bounds&&... bound_args)
      : pimpl_(__closure::MakeClosureImpl(arguments_type{}, func, std::forward<Bounds>(bound_args)...)) {}

  template <class... FuncArgs>
  Closure& operator=(R (*func)(FuncArgs...)) {
    pimpl_.reset(__closure::MakeClosureImpl(arguments_type{}, func));
    return *this;
  }

  template <class Functor, class... Bounds>
  explicit Closure(Functor&& functor, Bounds&&... bound_args)
      : pimpl_(__closure::MakeClosureImpl<R>(arguments_type{}, std::forward<Functor>(functor),
                                             std::forward<Bounds>(bound_args)...)) {
    static_assert(!__CLOSTD::is_same_v<std::decay_t<Functor>, Closure>, "this is not copy/move constructor");
  }

  // We have to add a SFINAE argument to avoid hiding the copy assignment operator. The parameter of the copy assignment
  // operator is const qualified, which may make the `Functor&&` prior to the `const Closure&`.
  template <class Functor, class = std::enable_if_t<!__CLOSTD::is_same_v<std::decay_t<Functor>, Closure>>>
  Closure& operator=(Functor&& functor) {
    pimpl_.reset(__closure::MakeClosureImpl<R>(arguments_type{}, std::forward<Functor>(functor)));
    return *this;
  }

  explicit Closure(impl_base_type* pimpl) : pimpl_(pimpl) {}
  Closure(Closure&&) noexcept = default;
  Closure& operator=(Closure&&) noexcept = default;

  Closure(const Closure& rhs) : pimpl_(rhs ? rhs.pimpl_->Copy() : nullptr) {}
  Closure& operator=(const Closure& rhs) {
    pimpl_.reset();
    if (rhs) {
      pimpl_.reset(rhs.pimpl_->Copy());
    }
    return *this;
  }

  bool Copyable() const { return !pimpl_ || pimpl_->Copyable(); }

  // The parameters cannot be Args&&. We must enable pass-by-value in the first layer forwarding to ensure that an
  // lvalue can be accepted by a closure argument of non-reference type.
  result_type Run(Args... args) const { return pimpl_->Run(std::forward<Args>(args)...); }
  result_type operator()(Args... args) const { return Run(std::forward<Args>(args)...); }

  explicit operator bool() const noexcept { return static_cast<bool>(pimpl_); }

  std::unique_ptr<impl_base_type> pimpl_;
};

// MakeClosure for function pointer, remove cv-qualifier and noexcept
template <class R, class... Args, class... Bounds>
decltype(auto) MakeClosure(R (*func)(Args...), Bounds&&... bound_args) {
  using closure_args = details::RemovePrefixWeakT<ArgList<Bounds...>, ArgList<Args...>>;
  auto res = __closure::MakeClosureImpl(closure_args{}, func, std::forward<Bounds>(bound_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(static_cast<__closure::ClosureImplBase<closure_res_t>*>(res));
}

// TODO noexcept had been a part of type system since c++17. make a specialization so that we can call noexcept function
// in noexcept.

// MakeClosure for functor
template <class R, class... ClosureArgs, class Functor, class... Bounds>
decltype(auto) MakeClosure(Functor&& functor, Bounds&&... bound_args) {
  auto res = __closure::MakeClosureImpl<R>(ArgList<ClosureArgs...>{}, std::forward<Functor>(functor),
                                           std::forward<Bounds>(bound_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(static_cast<__closure::ClosureImplBase<closure_res_t>*>(res));
}

template <class R, class Class, class... Args, class CPtr, class... Bounds>
decltype(auto) MakeClosure(R (Class::*method)(Args...), CPtr&& cptr, Bounds&&... bound_args) {
  using closure_args = details::RemovePrefixWeakT<ArgList<Bounds...>, ArgList<Args...>>;
  auto res = __closure::MakeClosureImpl<R>(closure_args{}, method, std::forward<CPtr>(cptr),
                                           std::forward<Bounds>(bound_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(static_cast<__closure::ClosureImplBase<closure_res_t>*>(res));
}

template <class R, class Class, class... Args, class CPtr, class... Bounds>
decltype(auto) MakeClosure(R (Class::*method)(Args...) const, CPtr&& cptr, Bounds&&... bound_args) {
  using closure_args = details::RemovePrefixWeakT<ArgList<Bounds...>, ArgList<Args...>>;
  auto res = __closure::MakeClosureImpl<R>(closure_args{}, method, std::forward<CPtr>(cptr),
                                           std::forward<Bounds>(bound_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(static_cast<__closure::ClosureImplBase<closure_res_t>*>(res));
}

}  // namespace closure
