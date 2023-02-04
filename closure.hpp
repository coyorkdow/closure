//
// Created by Youtao Guo on 2022/12/27
//

#pragma once

#include <cstring>
#include <memory>
#include <new>
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

class StoragePool;
enum class ManagerOperation;

template <class>
class ClosureImplBase;

template <class R, class... Args>
class ClosureImplBase<R(Args...)> {
 public:
  using result_type = R;
  using args_type = ArgList<Args...>;
  using closure_type = result_type(Args...);
  using invoker_type = result_type(const StoragePool*, Args&&...);
  using manager_type = bool(StoragePool*, const StoragePool*, ManagerOperation);
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

template <class Impl>
class ClosureImplHandler;

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

  friend class ClosureImplHandler<ClosureImpl>;

 public:
  using callee_type = Callable;
  using stored_types = decltype(std::make_tuple(std::declval<StoredArgs>()...)); /*std::tuple<StoredArgs...>*/

  using maybe_has_placeholder = typename validator::maybe_has_placeholder;
  using agents_type = typename maybe_has_placeholder::agents_type;
  static_assert(maybe_has_placeholder::value || std::is_same<agents_type, void>::value,
                "agents type is not void when closure doesn't have placeholder");

  template <class Callee, class... BoundArgs,
            class = std::enable_if_t<!std::is_same<std::decay_t<Callee>, ClosureImpl>::value>>
  explicit ClosureImpl(Callee&& f, BoundArgs&&... args)
      : stored_types(std::forward<BoundArgs>(args)...), callable_(std::forward<Callee>(f)) {}

  ClosureImpl(const ClosureImpl& rhs) = default;  // maybe deleted

  typename base::result_type Invoke(Args&&... args) {
    return InvokeHelper<maybe_has_placeholder>(std::index_sequence_for<StoredArgs...>{}, std::forward<Args>(args)...);
  }

 private:
  struct MapAndInvoke {
    template <size_t... I, class... RestArgs>
    typename base::result_type operator()(std::index_sequence<I...>, ClosureImpl* this_ptr, agents_type&& agents,
                                          RestArgs&&... args) const {
      return invoke::INVOKE(this_ptr->callable_,
                            placeholders::TryMapAndGet<I>(static_cast<stored_types&>(*this_ptr), agents)...,
                            std::forward<RestArgs>(args)...);
    }
  };

  template <class MaybeHasPlaceHolder, std::enable_if_t<MaybeHasPlaceHolder::value, int> = 0, std::size_t... I>
  typename base::result_type InvokeHelper(std::index_sequence<I...> seq, Args&&... args) {
    static_assert(!std::is_same<agents_type, void>::value, "agents type is void");
    static_assert(sizeof...(Args) >= std::tuple_size<agents_type>::value,
                  "the number of the given arguments is less than the agents size");
    constexpr auto agents_size = std::tuple_size<agents_type>::value;
    return placeholders::MakeAgentsTupleAndApply<agents_type>(
        std::make_index_sequence<agents_size>{}, std::make_index_sequence<sizeof...(Args) - agents_size>{},
        std::forward_as_tuple(std::forward<Args>(args)...), MapAndInvoke{}, seq, this);
  }

  template <class MaybeHasPlaceHolder, std::enable_if_t<!MaybeHasPlaceHolder::value, int> = 0, std::size_t... I>
  typename base::result_type InvokeHelper(std::index_sequence<I...>, Args&&... args) {
    return invoke::INVOKE(callable_, std::get<I>(static_cast<stored_types&>(*this))..., std::forward<Args>(args)...);
  }

  callee_type callable_;
};

namespace soo {

class Undefined;

union SmallStorage {
  void* object;
  const void* const_object;
  void (*fptr)();
  void (Undefined::*mfp)();
};

constexpr auto small_object_size = sizeof(SmallStorage);
constexpr auto small_object_alignment = alignof(SmallStorage);

template <class Tp>
struct IsSmallObject
    : std::integral_constant<bool, std::is_trivially_copyable<Tp>::value && sizeof(Tp) <= small_object_size &&
                                       alignof(Tp) <= small_object_alignment &&
                                       small_object_alignment % alignof(Tp) == 0> {};

};  // namespace soo

class StoragePool {
 public:
  StoragePool() : mem_{} {}

  template <class Tp, class... Args, std::enable_if_t<soo::IsSmallObject<Tp>::value, int> = 0>
  void emplace(Args&&... args) noexcept(noexcept(Tp(std::forward<Args>(args)...))) {
    new (mem_) Tp(std::forward<Args>(args)...);
  }

  template <class Tp, class... Args, std::enable_if_t<!soo::IsSmallObject<Tp>::value, int> = 0>
  void emplace(Args&&... args) {
    auto tmp = new (mem_) Tp*;
    *tmp = new Tp(std::forward<Args>(args)...);
  }

  template <class Tp, std::enable_if_t<soo::IsSmallObject<Tp>::value, int> = 0>
  Tp* get() const noexcept {
#if !(__cplusplus < 201703L)
    auto cptr = std::launder(reinterpret_cast<const Tp*>(mem_));
#else
    auto cptr = reinterpret_cast<const Tp*>(mem_);
#endif
    return const_cast<Tp*>(cptr);
  }

  template <class Tp, std::enable_if_t<!soo::IsSmallObject<Tp>::value, int> = 0>
  Tp* get() const noexcept {
    using ptr_t = Tp*;
#if !(__cplusplus < 201703L)
    return *std::launder(reinterpret_cast<const ptr_t*>(mem_));
#else
    return *reinterpret_cast<const ptr_t*>(mem_);
#endif
  }

  template <class Tp,
            std::enable_if_t<soo::IsSmallObject<Tp>::value && !std::is_trivially_destructible<Tp>::value, int> = 0>
  void erase() noexcept(std::is_nothrow_destructible<Tp>::value) {
#if !(__cplusplus < 201703L)
    auto ptr = std::launder(reinterpret_cast<Tp*>(mem_));
#else
    auto ptr = reinterpret_cast<Tp*>(mem_);
#endif
    ptr->~Tp();
  }

  template <class Tp,
            std::enable_if_t<soo::IsSmallObject<Tp>::value && std::is_trivially_destructible<Tp>::value, int> = 0>
  constexpr void erase() noexcept {}

  template <class Tp, std::enable_if_t<!soo::IsSmallObject<Tp>::value, int> = 0>
  void erase() {
#if !(__cplusplus < 201703L)
    auto ptr = *std::launder(reinterpret_cast<Tp**>(mem_));
#else
    auto ptr = *reinterpret_cast<Tp**>(mem_);
#endif
    delete ptr;
  }

 private:
  alignas(soo::small_object_alignment) unsigned char mem_[soo::small_object_size];
};

static_assert(sizeof(StoragePool) == soo::small_object_size, "");
static_assert(std::is_trivially_copyable<StoragePool>::value, "");

enum class ManagerOperation { COPYABLE, COPY, DESTROY, GET_PTR };

template <class R, class... Args, class Callable, class... StoredArgs>
class ClosureImplHandler<ClosureImpl<R(Args...), Callable, ArgList<StoredArgs...>>> {
 public:
  using impl_type = ClosureImpl<R(Args...), Callable, ArgList<StoredArgs...>>;

  static typename impl_type::result_type Invoke(const StoragePool* storage, Args&&... args) {
    return storage->template get<impl_type>()->Invoke(std::forward<Args>(args)...);
  }

  static bool Manage(StoragePool* dst, const StoragePool* src, ManagerOperation op) {
    switch (op) {
      case ManagerOperation::COPYABLE:
        return std::is_copy_constructible<impl_type>::value;
      case ManagerOperation::COPY:
        Copy<std::is_copy_constructible<impl_type>>(dst, src);
        break;
      case ManagerOperation::DESTROY:
        dst->template erase<impl_type>();
        break;
      case ManagerOperation::GET_PTR:
        auto pimpl = const_cast<impl_type*>(src->template get<impl_type>());
        dst->template emplace<typename impl_type::callee_type*>(&pimpl->callable_);
        break;
    }
    return false;
  }

  static_assert(std::is_same<decltype(Invoke), typename impl_type::invoker_type>::value, "");
  static_assert(std::is_same<decltype(Manage), typename impl_type::manager_type>::value, "");

 private:
  template <class MaybeCopyable, std::enable_if_t<MaybeCopyable::value, int> = 0>
  static void Copy(StoragePool* dst, const StoragePool* src) {
    dst->template emplace<impl_type>(*(src->template get<impl_type>()));
  }

  template <class MaybeCopyable, std::enable_if_t<!MaybeCopyable::value, int> = 0>
  static constexpr void Copy(StoragePool* dst, const StoragePool* src) {}
};

// Overload for non placeholders
template <class R, class... ClosureArgs, class Callable, class... Bounds,
          std::enable_if_t<!placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
auto MakeClosureImpl(StoragePool* dst, ArgList<ClosureArgs...>, Callable&& callable, Bounds&&... bound_args) {
  using impl_type =
      ClosureImpl<R(ClosureArgs...), std::remove_reference_t<Callable>, ArgList<std::remove_reference_t<Bounds>...>>;
  dst->template emplace<impl_type>(std::forward<Callable>(callable), std::forward<Bounds>(bound_args)...);
  impl_type* p = nullptr;
  return p;
}

// Overload for placeholders
template <class R, class... ClosureArgs, class... ConvertedBounds, class Callable, class... Bounds,
          std::enable_if_t<placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
auto MakeClosureImpl(StoragePool* dst, ArgList<ClosureArgs...>, ArgList<ConvertedBounds...>, Callable&& callable,
                     Bounds&&... bound_args) {
  using impl_type = ClosureImpl<R(ClosureArgs...), std::remove_reference_t<Callable>,
                                ArgList<std::remove_reference_t<ConvertedBounds>...>>;
  dst->template emplace<impl_type>(std::forward<Callable>(callable), std::forward<Bounds>(bound_args)...);
  impl_type* p = nullptr;
  return p;
}

}  // namespace __closure

template <class>
class Closure;

template <class R, class... Args>
class Closure<R(Args...)> {
  using impl_base_type = __closure::ClosureImplBase<R(Args...)>;

 public:
  using result_type = typename impl_base_type::result_type;
  using args_type = typename impl_base_type::args_type;

 private:
  using invoker_type = typename impl_base_type::invoker_type;
  using manager_type = typename impl_base_type::manager_type;

 public:
  Closure() noexcept : invoker_(nullptr), manager_(nullptr) {}

  ~Closure() {
    if (manager_) {
      manager_(&storage_, nullptr, __closure::ManagerOperation::DESTROY);
    }
  }

  template <class FP, class... Bounds,
            std::enable_if_t<std::is_function<FP>::value && !placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                             int> = 0>
  explicit Closure(FP* fptr, Bounds&&... bound_args) : invoker_(nullptr), manager_(nullptr) {
    auto type_holder = __closure::MakeClosureImpl<R>(&storage_, args_type{}, fptr, std::forward<Bounds>(bound_args)...);
    using impl_type = std::remove_pointer_t<decltype(type_holder)>;
    invoker_ = __closure::ClosureImplHandler<impl_type>::Invoke;
    manager_ = __closure::ClosureImplHandler<impl_type>::Manage;
  }

  template <
      class FP, class... Bounds,
      std::enable_if_t<std::is_function<FP>::value && placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
  explicit Closure(FP* fptr, Bounds&&... bound_args) : invoker_(nullptr), manager_(nullptr) {
    using bounds_l = ArgList<Bounds...>;
    using replaced_types = __closure::GenerateGettersFromClosureArgsT<bounds_l, args_type>;
    auto type_holder = __closure::MakeClosureImpl<R>(&storage_, args_type{}, replaced_types{}, fptr,
                                                     std::forward<Bounds>(bound_args)...);
    using impl_type = std::remove_pointer_t<decltype(type_holder)>;
    invoker_ = __closure::ClosureImplHandler<impl_type>::Invoke;
    manager_ = __closure::ClosureImplHandler<impl_type>::Manage;
  }

  template <class Callable, class... Bounds,
            std::enable_if_t<!std::is_same<std::decay_t<Callable>, Closure>::value &&
                                 !std::is_function<std::remove_reference_t<Callable>>::value &&
                                 !placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                             int> = 0>
  explicit Closure(Callable&& callable, Bounds&&... bound_args) : invoker_(nullptr), manager_(nullptr) {
    auto type_holder = __closure::MakeClosureImpl<R>(&storage_, args_type{}, std::forward<Callable>(callable),
                                                     std::forward<Bounds>(bound_args)...);
    using impl_type = std::remove_pointer_t<decltype(type_holder)>;
    invoker_ = __closure::ClosureImplHandler<impl_type>::Invoke;
    manager_ = __closure::ClosureImplHandler<impl_type>::Manage;
  }

  template <class Callable, class... Bounds,
            std::enable_if_t<!std::is_same<std::decay_t<Callable>, Closure>::value &&
                                 !std::is_function<std::remove_reference_t<Callable>>::value &&
                                 placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                             int> = 0>
  explicit Closure(Callable&& callable, Bounds&&... bound_args) : invoker_(nullptr), manager_(nullptr) {
    using bounds_l = ArgList<Bounds...>;
    using replaced_types = __closure::GenerateGettersFromClosureArgsT<bounds_l, args_type>;
    auto type_holder =
        __closure::MakeClosureImpl<R>(&storage_, args_type{}, replaced_types{}, std::forward<Callable>(callable),
                                      std::forward<Bounds>(bound_args)...);
    using impl_type = std::remove_pointer_t<decltype(type_holder)>;
    invoker_ = __closure::ClosureImplHandler<impl_type>::Invoke;
    manager_ = __closure::ClosureImplHandler<impl_type>::Manage;
  }

  void swap(Closure& rhs) noexcept {
    using std::swap;
    swap(invoker_, rhs.invoker_);
    swap(manager_, rhs.manager_);
    swap(storage_, rhs.storage_);
  }

  template <class FP, std::enable_if_t<std::is_function<FP>::value, int> = 0>
  Closure& operator=(FP* fptr) {
    Closure(fptr).swap(*this);
    return *this;
  }

  template <class Callable, std::enable_if_t<!std::is_same<std::decay_t<Callable>, Closure>::value &&
                                                 !std::is_function<std::remove_reference_t<Callable>>::value,
                                             int> = 0>
  Closure& operator=(Callable&& callable) {
    Closure(std::forward<Callable>(callable)).swap(*this);
    return *this;
  }

  Closure(Closure&& rhs) noexcept : invoker_(rhs.invoker_), manager_(rhs.manager_) {
    if (rhs) {
      storage_ = rhs.storage_;
      rhs.manager_ = nullptr;
      rhs.invoker_ = nullptr;
    }
  }

  Closure& operator=(Closure&& rhs) noexcept {
    Closure(std::move(rhs)).swap(*this);
    return *this;
  }

  Closure(const Closure& rhs) : invoker_(nullptr), manager_(nullptr) {
    if (rhs.Copyable()) {
      invoker_ = rhs.invoker_;
      manager_ = rhs.manager_;
      manager_(&storage_, &rhs.storage_, __closure::ManagerOperation::COPY);
    }
  }

  Closure& operator=(const Closure& rhs) {
    if (&rhs == this) {
      return *this;
    }
    Closure(rhs).swap(*this);
    return *this;
  }

  bool Copyable() const { return !manager_ || manager_(nullptr, nullptr, __closure::ManagerOperation::COPYABLE); }

  // The parameters cannot be Args&&. We must enable pass-by-value in the first layer forwarding to ensure that an
  // lvalue can be accepted by a closure argument of non-reference type.
  result_type operator()(Args... args) const { return invoker_(&storage_, std::forward<Args>(args)...); }

  explicit operator bool() const noexcept { return manager_; }

 private:
  invoker_type* invoker_;
  manager_type* manager_;
  __closure::StoragePool storage_;
};

// MakeClosure for function pointer, remove cv-qualifier and noexcept
template <class R, class... Args, class... Bounds,
          std::enable_if_t<!placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
auto MakeClosure(R (*fptr)(Args...), Bounds&&... bound_args) {
  using closure_args = __closure::RemovePrefixWeakT<ArgList<Bounds...>, ArgList<Args...>>;
  using closure_res_t = typename std::remove_pointer_t<decltype(__closure::MakeClosureImpl<R>(
      nullptr, closure_args{}, fptr, std::forward<Bounds>(bound_args)...))>::closure_type;
  return Closure<closure_res_t>(fptr, std::forward<Bounds>(bound_args)...);
}

template <class R, class... Args, class... Bounds,
          std::enable_if_t<placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
auto MakeClosure(R (*func)(Args...), Bounds&&... bound_args) {
  using bounds_l = ArgList<Bounds...>;
  using args_l = ArgList<Args...>;
  using replaced_types = __closure::ReplacePlaceHoldersWithGettersT<bounds_l, args_l>;
  using closure_args = ConcatT<typename __closure::ReplacePlaceHoldersWithGetters<bounds_l, args_l>::agents_prototype,
                               __closure::RemovePrefixWeakT<bounds_l, args_l>>;
  using closure_res_t = typename std::remove_pointer_t<decltype(__closure::MakeClosureImpl<R>(
      nullptr, closure_args{}, replaced_types{}, func, std::forward<Bounds>(bound_args)...))>::closure_type;
  return Closure<closure_res_t>(func, std::forward<Bounds>(bound_args)...);
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
  using closure_res_t =
      typename std::remove_pointer_t<decltype(__closure::MakeClosureImpl<typename functor_traits::return_type>(
          nullptr, typename functor_traits::args_type{}, std::forward<Functor>(functor),
          std::forward<Bounds>(bound_args)...))>::closure_type;
  return Closure<closure_res_t>(std::forward<Functor>(functor), std::forward<Bounds>(bound_args)...);
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
  using closure_res_t =
      typename std::remove_pointer_t<decltype(__closure::MakeClosureImpl<typename functor_traits::return_type>(
          nullptr, closure_args{}, replaced_types{}, std::forward<Functor>(functor),
          std::forward<Bounds>(bound_args)...))>::closure_type;
  return Closure<closure_res_t>(std::forward<Functor>(functor), std::forward<Bounds>(bound_args)...);
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
  using closure_res_t = typename std::remove_pointer_t<decltype(__closure::MakeClosureImpl<return_type>(
      nullptr, closure_args{}, method, std::forward<Bounds>(bound_args)...))>::closure_type;
  return Closure<closure_res_t>(method, std::forward<Bounds>(bound_args)...);
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
  using closure_res_t = typename std::remove_pointer_t<decltype(__closure::MakeClosureImpl<return_type>(
      nullptr, closure_args{}, replaced_types{}, method, std::forward<Bounds>(bound_args)...))>::closure_type;
  return Closure<closure_res_t>(method, std::forward<Bounds>(bound_args)...);
}

}  // namespace closure
