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
  using args_type = ArgList<Args...>;
  using closure_type = R(Args...);
  ClosureImplBase() = default;
  virtual ~ClosureImplBase() = default;
  virtual result_type Run(Args&&...) = 0;
  virtual ClosureImplBase<R(Args...)>* Copy() const { return nullptr; }
  virtual bool Copyable() const { return false; }
};

template <class ClosureArg, class Callee, class CalleeArgsList, class = void>
class ClosureImpl;

template <class Callable, class StoredArgs, class Args>
class Validator;

template <class Callable, class... Args, class... StoredArgs>
class Validator<Callable, ArgList<StoredArgs...>, ArgList<Args...>> {
 public:
  struct ErrType;

 private:
  /*
   * A gcc bug makes full template specialization in the class scope cannot be compiled. Use function overload
   * instead.
   * Related bug report: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=85282
   */

  template <class Tp>
  struct RealInvokeArg {
    using type = Tp&;
  };

  template <class AgentsTuple, size_t I>
  struct RealInvokeArg<placeholders::Getter<AgentsTuple, I>> {
    using type = typename placeholders::Getter<AgentsTuple, I>::result_type;
  };

  template <class Agents>
  static auto GetRealArgs(int) -> typename __closure::TailN<std::tuple_size<Agents>::value, ArgList<Args...>>::type;
  template <class Agent>
  static auto GetRealArgs(...) -> ArgList<Args...>;

  template <class... RealArgs>
  static auto TryInvoke(ArgList<RealArgs...>, int)  // stored args are passed by lvalue
      -> traits::invoke_result_t<Callable, typename RealInvokeArg<StoredArgs>::type..., RealArgs...>;
  static auto TryInvoke(...) -> ErrType;

  template <class Tp>
  static auto Invokable(Tp) -> std::true_type;
  static auto Invokable(ErrType*) -> std::false_type;

 public:
  using maybe_has_placeholder = placeholders::HasGetter<ArgList<StoredArgs...>>;
  using agents_type = typename maybe_has_placeholder::agents_type;
  using real_args = decltype(GetRealArgs<agents_type>(0));
  using invoke_result = decltype(TryInvoke(GetRealArgs<agents_type>(0), 0));
  // invoke_result might be void, std::declval<void>() is invalid as an argument, use pointer type instead.
  static constexpr bool is_invokable = decltype(Invokable(std::declval<invoke_result*>()))::value;
};

// Overload for function pointer and functor
// It is guaranteed that the Callable and all the StoredArgs... are non-reference types.
// If it has placeholders, all placeholders must have been already replaced with getters.
template <class R, class... Args, class Callable, class... StoredArgs>
class ClosureImpl<R(Args...), Callable, ArgList<StoredArgs...>/*,
                  std::enable_if_t<!std::is_member_function_pointer<Callable>::value>*/>
    : public ClosureImplBase<R(Args...)>, private decltype(std::make_tuple(std::declval<StoredArgs>()...)) {
  using base = ClosureImplBase<R(Args...)>;

  using validator = Validator<Callable, ArgList<StoredArgs...>, ArgList<Args...>>;
  static_assert(validator::is_invokable, "the given arguments don't match the arguments of callee");
  static_assert(!validator::is_invokable ||
                    std::is_convertible<typename validator::invoke_result, typename base::result_type>::value,
                "the result type of the given callee is not match");

 public:
  using callee_type = Callable;
  using stored_types = decltype(std::make_tuple(std::declval<StoredArgs>()...)); /*std::tuple<StoredArgs...>*/

  using is_copyable = std::integral_constant<bool, std::is_copy_constructible<Callable>::value &&
                                                       std::is_copy_constructible<std::tuple<StoredArgs...>>::value>;

  using maybe_has_placeholder = typename validator::maybe_has_placeholder;
  using agents_type = typename maybe_has_placeholder::agents_type;
  static_assert(maybe_has_placeholder::value || std::is_same<agents_type, void>::value,
                "agents type is not void when closure doesn't have placeholder");

  template <class Callee, class... BoundArgs>
  explicit ClosureImpl(Callee&& f, BoundArgs&&... args)
      : stored_types(std::forward<BoundArgs>(args)...), callable_(std::forward<Callee>(f)) {}

  ClosureImpl(const ClosureImpl& rhs) = default;  // maybe deleted
  template <class MaybeCopyable, std::enable_if_t<MaybeCopyable::value, int> = 0>
  base* TryCopy() const {
    return new ClosureImpl(*this);
  }
  template <class MaybeCopyable, std::enable_if_t<!MaybeCopyable::value, int> = 0>
  constexpr base* TryCopy() const noexcept {
    return nullptr;
  }

  base* Copy() const override { return TryCopy<is_copyable>(); }
  bool Copyable() const override { return is_copyable::value; }

  typename base::result_type Run(Args&&... args) override {
    return RunHelper<maybe_has_placeholder>(std::index_sequence_for<StoredArgs...>{}, std::forward<Args>(args)...);
  }

 private:
  struct MapAndRun {
    template <size_t... I, class... RestArgs>
    typename base::result_type operator()(std::index_sequence<I...>, ClosureImpl* this_ptr, agents_type&& agents,
                                          RestArgs&&... args) const {
      return invoke::INVOKE(this_ptr->callable_,
                            placeholders::TryMapAndGet<I>(static_cast<stored_types&>(*this_ptr), agents)...,
                            std::forward<RestArgs>(args)...);
    }
  };

  template <class MaybeHasPlaceHolder, std::enable_if_t<MaybeHasPlaceHolder::value, int> = 0, std::size_t... I>
  typename base::result_type RunHelper(std::index_sequence<I...> seq, Args&&... args) {
    static_assert(!std::is_same<agents_type, void>::value, "agents type is void");
    static_assert(sizeof...(Args) >= std::tuple_size<agents_type>::value,
                  "the number of the given arguments is less than the agents size");
    constexpr auto agents_size = std::tuple_size<agents_type>::value;
    return placeholders::MakeAgentsTupleAndApply<agents_type>(
        std::make_index_sequence<agents_size>{}, std::make_index_sequence<sizeof...(Args) - agents_size>{},
        std::forward_as_tuple(std::forward<Args>(args)...), MapAndRun{}, seq, this);
  }

  template <class MaybeHasPlaceHolder, std::enable_if_t<!MaybeHasPlaceHolder::value, int> = 0, std::size_t... I>
  typename base::result_type RunHelper(std::index_sequence<I...>, Args&&... args) {
    return invoke::INVOKE(callable_, std::get<I>(static_cast<stored_types&>(*this))..., std::forward<Args>(args)...);
  }

  callee_type callable_;
};

// Overload for non placeholders
template <class R, class... ClosureArgs, class Callable, class... Bounds,
          std::enable_if_t<!placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
auto MakeClosureImpl(ArgList<ClosureArgs...>, Callable&& callable, Bounds&&... bound_args) {
  return new ClosureImpl<R(ClosureArgs...), std::remove_reference_t<Callable>,
                         ArgList<std::remove_reference_t<Bounds>...>>(std::forward<Callable>(callable),
                                                                      std::forward<Bounds>(bound_args)...);
}

// Overload for placeholders
template <class R, class... ClosureArgs, class... ConvertedBounds, class Callable, class... Bounds,
          std::enable_if_t<placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
auto MakeClosureImpl(ArgList<ClosureArgs...>, ArgList<ConvertedBounds...>, Callable&& callable,
                     Bounds&&... bound_args) {
  return new ClosureImpl<R(ClosureArgs...), std::remove_reference_t<Callable>,
                         ArgList<std::remove_reference_t<ConvertedBounds>...>>(std::forward<Callable>(callable),
                                                                               std::forward<Bounds>(bound_args)...);
}

}  // namespace __closure

template <class>
class Closure;

template <class R, class... Args>
class Closure<R(Args...)> {
  using impl_base_type = __closure::ClosureImplBase<R(Args...)>;

 public:
  using result_type = R;
  using args_type = typename impl_base_type::args_type;

  Closure() = default;

  template <class FP, class... Bounds,
            std::enable_if_t<std::is_function<FP>::value && !placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                             int> = 0>
  explicit Closure(FP* func, Bounds&&... bound_args)
      : pimpl_(__closure::MakeClosureImpl<R>(args_type{}, func, std::forward<Bounds>(bound_args)...)) {}

  template <
      class FP, class... Bounds,
      std::enable_if_t<std::is_function<FP>::value && placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
  explicit Closure(FP* func, Bounds&&... bound_args) {
    using bounds_l = ArgList<Bounds...>;
    using replaced_types = __closure::GenerateGettersFromClosureArgsT<bounds_l, args_type>;
    pimpl_.reset(
        __closure::MakeClosureImpl<R>(args_type{}, replaced_types{}, func, std::forward<Bounds>(bound_args)...));
  }

  template <class FP, std::enable_if_t<std::is_function<FP>::value, int> = 0>
  Closure& operator=(FP* func) {
    pimpl_.reset(__closure::MakeClosureImpl<R>(args_type{}, func));
    return *this;
  }

  template <class Callable, class... Bounds,
            std::enable_if_t<!std::is_same<std::decay_t<Callable>, Closure>::value &&
                                 !std::is_function<std::remove_reference_t<Callable>>::value &&
                                 !placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                             int> = 0>
  explicit Closure(Callable&& callable, Bounds&&... bound_args)
      : pimpl_(__closure::MakeClosureImpl<R>(args_type{}, std::forward<Callable>(callable),
                                             std::forward<Bounds>(bound_args)...)) {}

  template <class Callable, class... Bounds,
            std::enable_if_t<!std::is_same<std::decay_t<Callable>, Closure>::value &&
                                 !std::is_function<std::remove_reference_t<Callable>>::value &&
                                 placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                             int> = 0>
  explicit Closure(Callable&& callable, Bounds&&... bound_args) {
    using bounds_l = ArgList<Bounds...>;
    using replaced_types = __closure::GenerateGettersFromClosureArgsT<bounds_l, args_type>;
    pimpl_.reset(__closure::MakeClosureImpl<R>(args_type{}, replaced_types{}, std::forward<Callable>(callable),
                                               std::forward<Bounds>(bound_args)...));
  }

  template <class Callable, std::enable_if_t<!std::is_same<std::decay_t<Callable>, Closure>::value &&
                                                 !std::is_function<std::remove_reference_t<Callable>>::value,
                                             int> = 0>
  Closure& operator=(Callable&& callable) {
    pimpl_.reset(__closure::MakeClosureImpl<R>(args_type{}, std::forward<Callable>(callable)));
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
  result_type operator()(Args... args) const { return pimpl_->Run(std::forward<Args>(args)...); }

  explicit operator bool() const noexcept { return static_cast<bool>(pimpl_); }

 private:
  std::unique_ptr<impl_base_type> pimpl_;
};

// MakeClosure for function pointer, remove cv-qualifier and noexcept
template <class R, class... Args, class... Bounds,
          std::enable_if_t<!placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
auto MakeClosure(R (*func)(Args...), Bounds&&... bound_args) {
  using closure_args = __closure::RemovePrefixWeakT<ArgList<Bounds...>, ArgList<Args...>>;
  auto res = __closure::MakeClosureImpl<R>(closure_args{}, func, std::forward<Bounds>(bound_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(static_cast<__closure::ClosureImplBase<closure_res_t>*>(res));
}

template <class R, class... Args, class... Bounds,
          std::enable_if_t<placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
auto MakeClosure(R (*func)(Args...), Bounds&&... bound_args) {
  using bounds_l = ArgList<Bounds...>;
  using args_l = ArgList<Args...>;
  using replaced_types = __closure::ReplacePlaceHoldersWithGettersT<bounds_l, args_l>;
  using closure_args = ConcatT<typename __closure::ReplacePlaceHoldersWithGetters<bounds_l, args_l>::agents_prototype,
                               __closure::RemovePrefixWeakT<bounds_l, args_l>>;
  auto res = __closure::MakeClosureImpl<R>(closure_args{}, replaced_types{}, func, std::forward<Bounds>(bound_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(static_cast<__closure::ClosureImplBase<closure_res_t>*>(res));
}

// TODO noexcept had been a part of type system since c++17. make a specialization so that we can call noexcept function
// in noexcept.

// MakeClosure for simple functor
template <class Functor, class... Bounds,
          std::enable_if_t<traits::IsSimpleFunctor<std::remove_reference_t<Functor>>::value &&
                               !placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                           int> = 0>
auto MakeClosure(Functor&& functor, Bounds&&... bound_args) {
  using functor_traits = traits::SimpleFunctorTraits<std::remove_reference_t<Functor>>;
  auto res = __closure::MakeClosureImpl<typename functor_traits::return_type>(
      typename functor_traits::args_type{}, std::forward<Functor>(functor), std::forward<Bounds>(bound_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(static_cast<__closure::ClosureImplBase<closure_res_t>*>(res));
}

template <class Functor, class... Bounds,
          std::enable_if_t<traits::IsSimpleFunctor<std::remove_reference_t<Functor>>::value &&
                               placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                           int> = 0>
auto MakeClosure(Functor&& functor, Bounds&&... bound_args) {
  using functor_traits = traits::SimpleFunctorTraits<std::remove_reference_t<Functor>>;
  using bounds_l = ArgList<Bounds...>;
  using args_l = typename functor_traits::args_type;
  using replaced_types = __closure::ReplacePlaceHoldersWithGettersT<bounds_l, args_l>;
  using closure_args = ConcatT<typename __closure::ReplacePlaceHoldersWithGetters<bounds_l, args_l>::agents_prototype,
                               __closure::RemovePrefixWeakT<bounds_l, args_l>>;
  auto res = __closure::MakeClosureImpl<typename functor_traits::return_type>(
      closure_args{}, replaced_types{}, std::forward<Functor>(functor), std::forward<Bounds>(bound_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(static_cast<__closure::ClosureImplBase<closure_res_t>*>(res));
}

// MakeClosure for member function pointer
template <class Method, class... Bounds,
          std::enable_if_t<std::is_member_function_pointer<Method>::value &&
                               !placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                           int> = 0>
auto MakeClosure(Method method, Bounds&&... bound_args) {
  using mfp_traits = traits::MemberFunctionPointerTraits<Method>;
  using class_type = typename mfp_traits::class_type;
  using args_type = typename mfp_traits::args_type;
  using return_type = typename mfp_traits::return_type;
  using closure_args = __closure::RemovePrefixWeakT<ArgList<Bounds...>, ConcatT<ArgList<class_type*>, args_type>>;
  auto res = __closure::MakeClosureImpl<return_type>(closure_args{}, method, std::forward<Bounds>(bound_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(static_cast<__closure::ClosureImplBase<closure_res_t>*>(res));
}

template <class Method, class... Bounds,
          std::enable_if_t<std::is_member_function_pointer<Method>::value &&
                               placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                           int> = 0>
auto MakeClosure(Method method, Bounds&&... bound_args) {
  using mfp_traits = traits::MemberFunctionPointerTraits<Method>;
  using bounds_l = ArgList<Bounds...>;
  using class_type = typename mfp_traits::class_type;
  using args_l = ConcatT<ArgList<class_type*>, typename mfp_traits::args_type>;
  using return_type = typename mfp_traits::return_type;
  using replaced_types = __closure::ReplacePlaceHoldersWithGettersT<bounds_l, args_l>;
  using closure_args = ConcatT<typename __closure::ReplacePlaceHoldersWithGetters<bounds_l, args_l>::agents_prototype,
                               __closure::RemovePrefixWeakT<bounds_l, args_l>>;
  auto res = __closure::MakeClosureImpl<return_type>(closure_args{}, replaced_types{}, method,
                                                     std::forward<Bounds>(bound_args)...);
  using closure_res_t = typename std::remove_pointer_t<decltype(res)>::closure_type;
  return Closure<closure_res_t>(static_cast<__closure::ClosureImplBase<closure_res_t>*>(res));
}

}  // namespace closure
