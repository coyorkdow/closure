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
#include "closure/trivial_tuple.hpp"

namespace closure {

template <size_t I>
constexpr auto PlaceHolder() noexcept {
  return placeholders::PH<I>{};
}

namespace closureimpl {

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
    using type = typename placeholders::Getter<AgentsTuple, I>::get_result_type;
  };

  template <class Agents>
  static auto GetRealArgs(int) -> typename closureimpl::TailN<std::tuple_size<Agents>::value, ArgList<Args...>>::type;
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

template <class... Args>
struct MaybeStores {
 public:
  static constexpr bool value = placeholders::HasNonGetter<ArgList<Args...>>::value;
  using type = std::conditional_t<value, tuple::TrivialTuple<std::decay_t<Args>...>, tuple::TrivialTuple<>>;
};

// Overload for function pointer and functor
// It is guaranteed that the Callable is decayed type, and all the StoredArgs... are non-reference types.
// If it has placeholders, all placeholders must have been already replaced with getters.
template <class R, class... Args, class Callable, class... StoredArgs>
class ClosureImpl<R(Args...), Callable, ArgList<StoredArgs...>> : public ClosureImplBase<R(Args...)>,
                                                                  private MaybeStores<StoredArgs...>::type {
  using base = ClosureImplBase<R(Args...)>;

  using validator = Validator<Callable, ArgList<StoredArgs...>, ArgList<Args...>>;
  static_assert(validator::is_invokable, "the given arguments don't match the arguments of callee");
  static_assert(!validator::is_invokable ||
                    std::is_convertible<typename validator::invoke_result, typename base::result_type>::value,
                "the result type of the given callee is not match");

  friend class ClosureImplHandler<ClosureImpl>;

 public:
  using callee_type = Callable;
  using stored_types = tuple::TrivialTuple<std::decay_t<StoredArgs>...>;
  using maybe_stores = MaybeStores<StoredArgs...>;
  static_assert(!maybe_stores::value || std::is_same<stored_types, typename maybe_stores::type>::value,
                "maybe stored type is not same as the stored_types");

  using maybe_has_placeholder = typename validator::maybe_has_placeholder;
  using agents_type = typename maybe_has_placeholder::agents_type;
  static_assert(maybe_has_placeholder::value || std::is_same<agents_type, void>::value,
                "agents type is not void when closure doesn't have placeholder");

  template <class Callee, class... BoundArgs,
            std::enable_if_t<!std::is_same<std::decay_t<Callee>, ClosureImpl>::value && maybe_stores::value, int> = 0>
  explicit ClosureImpl(Callee&& f, BoundArgs&&... args)
      : stored_types(std::forward<BoundArgs>(args)...), callable_(std::forward<Callee>(f)) {}

  template <class Callee, class... BoundArgs,
            std::enable_if_t<!std::is_same<std::decay_t<Callee>, ClosureImpl>::value && !maybe_stores::value, int> = 0>
  explicit ClosureImpl(Callee&& f, BoundArgs&&... args) : callable_(std::forward<Callee>(f)) {}

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
                            placeholders::TryMapAndGet<I>(GetStored<stored_types>(this_ptr), agents)...,
                            std::forward<RestArgs>(args)...);
    }

    template <class Tp, class CloImpl, std::enable_if_t<std::is_base_of<Tp, CloImpl>::value, int> = 0>
    static Tp& GetStored(CloImpl* this_ptr) {
      return static_cast<Tp&>(*this_ptr);
    }

    template <class Tp, class CloImpl, std::enable_if_t<!std::is_base_of<Tp, CloImpl>::value, int> = 0>
    static constexpr Tp GetStored(CloImpl*) {
      return Tp{};
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
    return invoke::INVOKE(callable_, tuple::get<I>(static_cast<stored_types&>(*this))..., std::forward<Args>(args)...);
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
    Tp* ptr = nullptr;
    std::memcpy(&ptr, mem_, sizeof(Tp*));
    return ptr;
  }

  template <class Tp, std::enable_if_t<soo::IsSmallObject<Tp>::value, int> = 0>
  constexpr void erase() noexcept {}

  template <class Tp, std::enable_if_t<!soo::IsSmallObject<Tp>::value, int> = 0>
  void erase() {
    Tp* ptr = nullptr;
    std::memcpy(&ptr, mem_, sizeof(Tp*));
    delete ptr;
  }

 private:
  alignas(soo::small_object_alignment) unsigned char mem_[soo::small_object_size];
};

static_assert(sizeof(StoragePool) == soo::small_object_size, "");
static_assert(std::is_trivially_copyable<StoragePool>::value, "");

enum class ManagerOperation { COPYABLE, COPY, DESTROY, GET_PTR };

template <class Callable>
struct CallableTypeHelper {
  static void Id() {}
};

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
        auto fptr = &CallableTypeHelper<Callable>::Id;
        if (*(dst->template get<decltype(fptr)>()) == fptr) {
          auto pimpl = const_cast<impl_type*>(src->template get<impl_type>());
          dst->template emplace<Callable*>(&pimpl->callable_);
          return true;
        }
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
  static constexpr void Copy(StoragePool* dst, const StoragePool* src) noexcept {}
};

// Overload for non placeholders
template <class R, class... ClosureArgs, class Callable, class... Bounds,
          std::enable_if_t<!placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
auto MakeClosureImpl(StoragePool* dst, ArgList<ClosureArgs...>, Callable&& callable, Bounds&&... bound_args) {
  using impl_type = ClosureImpl<R(ClosureArgs...), std::decay_t<Callable>, ArgList<std::remove_reference_t<Bounds>...>>;
  dst->template emplace<impl_type>(std::forward<Callable>(callable), std::forward<Bounds>(bound_args)...);
  impl_type* p = nullptr;
  return p;
}

// Overload for placeholders
template <class R, class... ClosureArgs, class... ConvertedBounds, class Callable, class... Bounds,
          std::enable_if_t<placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
auto MakeClosureImpl(StoragePool* dst, ArgList<ClosureArgs...>, ArgList<ConvertedBounds...>, Callable&& callable,
                     Bounds&&... bound_args) {
  using impl_type =
      ClosureImpl<R(ClosureArgs...), std::decay_t<Callable>, ArgList<std::remove_reference_t<ConvertedBounds>...>>;
  dst->template emplace<impl_type>(std::forward<Callable>(callable), std::forward<Bounds>(bound_args)...);
  impl_type* p = nullptr;
  return p;
}

}  // namespace closureimpl

template <class>
class Closure;

template <class R, class... Args>
class Closure<R(Args...)> {
  using impl_base_type = closureimpl::ClosureImplBase<R(Args...)>;

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
      manager_(&storage_, nullptr, closureimpl::ManagerOperation::DESTROY);
    }
  }

  template <class Callable, class... Bounds,
            std::enable_if_t<!std::is_same<std::decay_t<Callable>, Closure>::value &&
                                 !placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                             int> = 0>
  Closure(Callable&& callable, Bounds&&... bound_args) : invoker_(nullptr), manager_(nullptr) {
    auto type_holder = closureimpl::MakeClosureImpl<R>(&storage_, args_type{}, std::forward<Callable>(callable),
                                                       std::forward<Bounds>(bound_args)...);
    using impl_type = std::remove_pointer_t<decltype(type_holder)>;
    invoker_ = closureimpl::ClosureImplHandler<impl_type>::Invoke;
    manager_ = closureimpl::ClosureImplHandler<impl_type>::Manage;
  }

  template <class Callable, class... Bounds,
            std::enable_if_t<!std::is_same<std::decay_t<Callable>, Closure>::value &&
                                 placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                             int> = 0>
  Closure(Callable&& callable, Bounds&&... bound_args) : invoker_(nullptr), manager_(nullptr) {
    using bounds_l = ArgList<Bounds...>;
    using replaced_types = closureimpl::GenerateGettersFromClosureArgsT<bounds_l, args_type>;
    auto type_holder =
        closureimpl::MakeClosureImpl<R>(&storage_, args_type{}, replaced_types{}, std::forward<Callable>(callable),
                                        std::forward<Bounds>(bound_args)...);
    using impl_type = std::remove_pointer_t<decltype(type_holder)>;
    invoker_ = closureimpl::ClosureImplHandler<impl_type>::Invoke;
    manager_ = closureimpl::ClosureImplHandler<impl_type>::Manage;
  }

  void swap(Closure& rhs) noexcept {
    using std::swap;
    swap(invoker_, rhs.invoker_);
    swap(manager_, rhs.manager_);
    swap(storage_, rhs.storage_);
  }

  template <class Callable, std::enable_if_t<!std::is_same<std::decay_t<Callable>, Closure>::value, int> = 0>
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
    if (rhs.copyable()) {
      invoker_ = rhs.invoker_;
      manager_ = rhs.manager_;
      manager_(&storage_, &rhs.storage_, closureimpl::ManagerOperation::COPY);
    }
  }

  Closure& operator=(const Closure& rhs) {
    if (&rhs == this) {
      return *this;
    }
    Closure(rhs).swap(*this);
    return *this;
  }

  bool copyable() const { return !manager_ || manager_(nullptr, nullptr, closureimpl::ManagerOperation::COPYABLE); }

  // The parameters cannot be Args&&. We must enable pass-by-value in the first layer forwarding to ensure that an
  // lvalue can be accepted by a closure argument of non-reference type.
  result_type operator()(Args... args) const { return invoker_(&storage_, std::forward<Args>(args)...); }

  explicit operator bool() const noexcept { return manager_; }

  template <class Callable>
  Callable* target() noexcept {
    const Closure* cptr = this;
    auto callable = cptr->template target<Callable>();
    return *const_cast<Callable**>(&callable);
  }

  template <class Callable>
  const Callable* target() const noexcept {
    return TargetImpl<std::remove_cv_t<Callable>>();
  }

 private:
  template <class Tp, std::enable_if_t<std::is_object<Tp>::value, int> = 0>
  const Tp* TargetImpl() const noexcept {
    closureimpl::StoragePool tmp;
    auto fptr = &closureimpl::CallableTypeHelper<Tp>::Id;
    tmp.template emplace<decltype(fptr)>(fptr);
    if (manager_ && manager_(&tmp, &storage_, closureimpl::ManagerOperation::GET_PTR)) {
      return *tmp.template get<Tp*>();
    }
    return nullptr;
  }

  template <class Tp, std::enable_if_t<!std::is_object<Tp>::value, int> = 0>
  constexpr const Tp* TargetImpl() const noexcept {
    return nullptr;
  }

  invoker_type* invoker_;
  manager_type* manager_;
  closureimpl::StoragePool storage_;
};

// MakeClosure for function pointer, remove cv-qualifier and noexcept

template <class R, class... Args, class... Bounds,
          std::enable_if_t<!placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
auto MakeClosure_ClosureImplType(R (*fptr)(Args...), Bounds&&... bound_args) {
  using closure_args = closureimpl::RemovePrefixWeakT<ArgList<Bounds...>, ArgList<Args...>>;
  using type =
      decltype(closureimpl::MakeClosureImpl<R>(nullptr, closure_args{}, fptr, std::forward<Bounds>(bound_args)...));
  return (type)(nullptr);
}

template <class R, class... Args, class... Bounds,
          std::enable_if_t<placeholders::HasPlaceHolder<ArgList<Bounds...>>::value, int> = 0>
auto MakeClosure_ClosureImplType(R (*fptr)(Args...), Bounds&&... bound_args) {
  using bounds_l = ArgList<Bounds...>;
  using args_l = ArgList<Args...>;
  using replaced_types = closureimpl::ReplacePlaceHoldersWithGettersT<bounds_l, args_l>;
  using closure_args = ConcatT<typename closureimpl::ReplacePlaceHoldersWithGetters<bounds_l, args_l>::agents_prototype,
                               closureimpl::RemovePrefixWeakT<bounds_l, args_l>>;
  using type = decltype(closureimpl::MakeClosureImpl<R>(nullptr, closure_args{}, replaced_types{}, fptr,
                                                        std::forward<Bounds>(bound_args)...));
  return (type)(nullptr);
}

template <class R, class... Args, class... Bounds>
auto MakeClosure(R (*fptr)(Args...), Bounds&&... bound_args) {
  using impl_type = decltype(MakeClosure_ClosureImplType(fptr, std::forward<Bounds>(bound_args)...));
  using closure_res_t = typename std::remove_pointer_t<impl_type>::closure_type;
  return Closure<closure_res_t>(fptr, std::forward<Bounds>(bound_args)...);
}

// TODO noexcept had been a part of type system since c++17. make a specialization so that we can call noexcept function
// in noexcept.

// MakeClosure for simple functor
template <class Functor, class... Bounds,
          std::enable_if_t<traits::IsSimpleFunctor<std::remove_reference_t<Functor>>::value &&
                               !placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                           int> = 0>
auto MakeClosure_ClosureImplType(Functor&& functor, Bounds&&... bound_args) {
  using functor_traits = traits::SimpleFunctorTraits<std::remove_reference_t<Functor>>;
  using closure_args = closureimpl::RemovePrefixWeakT<ArgList<Bounds...>, typename functor_traits::args_type>;
  using type = decltype(closureimpl::MakeClosureImpl<typename functor_traits::return_type>(
      nullptr, closure_args{}, std::forward<Functor>(functor), std::forward<Bounds>(bound_args)...));
  return (type)(nullptr);
}

template <class Functor, class... Bounds,
          std::enable_if_t<traits::IsSimpleFunctor<std::remove_reference_t<Functor>>::value &&
                               placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                           int> = 0>
auto MakeClosure_ClosureImplType(Functor&& functor, Bounds&&... bound_args) {
  using functor_traits = traits::SimpleFunctorTraits<std::remove_reference_t<Functor>>;
  using bounds_l = ArgList<Bounds...>;
  using args_l = typename functor_traits::args_type;
  using replaced_types = closureimpl::ReplacePlaceHoldersWithGettersT<bounds_l, args_l>;
  using closure_args = ConcatT<typename closureimpl::ReplacePlaceHoldersWithGetters<bounds_l, args_l>::agents_prototype,
                               closureimpl::RemovePrefixWeakT<bounds_l, args_l>>;
  using type = decltype(closureimpl::MakeClosureImpl<typename functor_traits::return_type>(
      nullptr, closure_args{}, replaced_types{}, std::forward<Functor>(functor), std::forward<Bounds>(bound_args)...));
  return (type)(nullptr);
}

template <class Functor, class... Bounds,
          std::enable_if_t<traits::IsSimpleFunctor<std::remove_reference_t<Functor>>::value, int> = 0>
auto MakeClosure(Functor&& functor, Bounds&&... bound_args) {
  using impl_type =
      decltype(MakeClosure_ClosureImplType(std::forward<Functor>(functor), std::forward<Bounds>(bound_args)...));
  using closure_res_t = typename std::remove_pointer_t<impl_type>::closure_type;
  return Closure<closure_res_t>(std::forward<Functor>(functor), std::forward<Bounds>(bound_args)...);
}

// MakeClosure for member function pointer
template <class Method, class... Bounds,
          std::enable_if_t<std::is_member_function_pointer<Method>::value &&
                               !placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                           int> = 0>
auto MakeClosure_ClosureImplType(Method method, Bounds&&... bound_args) {
  using mfp_traits = traits::MemberFunctionPointerTraits<Method>;
  using class_type = typename mfp_traits::class_type;
  using args_type = typename mfp_traits::args_type;
  using return_type = typename mfp_traits::return_type;
  using closure_args = closureimpl::RemovePrefixWeakT<ArgList<Bounds...>, ConcatT<ArgList<class_type*>, args_type>>;
  using type = decltype(closureimpl::MakeClosureImpl<return_type>(nullptr, closure_args{}, method,
                                                                  std::forward<Bounds>(bound_args)...));
  return (type)(nullptr);
}

template <class Method, class... Bounds,
          std::enable_if_t<std::is_member_function_pointer<Method>::value &&
                               placeholders::HasPlaceHolder<ArgList<Bounds...>>::value,
                           int> = 0>
auto MakeClosure_ClosureImplType(Method method, Bounds&&... bound_args) {
  using mfp_traits = traits::MemberFunctionPointerTraits<Method>;
  using bounds_l = ArgList<Bounds...>;
  using class_type = typename mfp_traits::class_type;
  using args_l = ConcatT<ArgList<class_type*>, typename mfp_traits::args_type>;
  using return_type = typename mfp_traits::return_type;
  using replaced_types = closureimpl::ReplacePlaceHoldersWithGettersT<bounds_l, args_l>;
  using closure_args = ConcatT<typename closureimpl::ReplacePlaceHoldersWithGetters<bounds_l, args_l>::agents_prototype,
                               closureimpl::RemovePrefixWeakT<bounds_l, args_l>>;
  using type = decltype(closureimpl::MakeClosureImpl<return_type>(nullptr, closure_args{}, replaced_types{}, method,
                                                                  std::forward<Bounds>(bound_args)...));
  return type(nullptr);
}

template <class Method, class... Bounds, std::enable_if_t<std::is_member_function_pointer<Method>::value, int> = 0>
auto MakeClosure(Method method, Bounds&&... bound_args) {
  using impl_type = decltype(MakeClosure_ClosureImplType(method, std::forward<Bounds>(bound_args)...));
  using closure_res_t = typename std::remove_pointer_t<impl_type>::closure_type;
  return Closure<closure_res_t>(method, std::forward<Bounds>(bound_args)...);
}

// Overload for closure type, do nothing
template <class R, class... Args>
decltype(auto) MakeClosure(Closure<R(Args...)>&& closure) {
  return std::forward<decltype(closure)>(closure);
}

}  // namespace closure
