# Closure

 **Closure is a c++ functional object implementation. It integrates the std::function and std::bind, and even more powerful.**

[![Linux Status](https://github.com/coyorkdow/closure/actions/workflows/linux.yml/badge.svg)](https://github.com/coyorkdow/closure/actions/workflows/linux.yml)
[![macOS Status](https://github.com/coyorkdow/closure/actions/workflows/macos.yml/badge.svg)](https://github.com/coyorkdow/closure/actions/workflows/macos.yml)
[![C++ Standard](https://img.shields.io/badge/cpp14-minimum%20required-success?logo=cplusplus)](https://en.cppreference.com/w/cpp/14)

Closure is header-only. To use Closure, simply copy `closure.hpp` and dicectory `closure` into your project.

## Features

- Support almost all the methods of `std::function`, except `target_type()` (need RTTI).
- Support arguments binding, therefore it can replace `std::bind`.
- It can stores non-copyable object, like `std::unique_ptr`. An extra method `copyable()` is provided to check if the object currently stored in a `Closure` instance is copyable. If it returns `false`, then trying copy this instance (construct or assign) will get an empty `Closure`.
- Support small object optimization. On x64 machines, any objects of the type which is trivially copyable and `sizeof` not greater than 16 will be stored locally. No dynamical memory allocated.
- Helper function `MakeClosure` can create an instance of `Closure` and deduce its type, you can use `auto` instead of manually writing the `Closure`'s template arguments. `MakeClosure` also supports arguments binding.

Read more details in

[Compare to std::function](#compare-to-stdfunction)

[Compare to std::bind](#compare-to-stdbind)

[closure::Any](#closureany)

## Basic Usage

Use like `std::function`. Store a function pointer or any callable object (including pointer to member function).
```C++
int calculate_sum(const std::string& exp) {
  int ans = 0;
  int cur_num = 0;
  for (auto iter = exp.begin(); iter < exp.end(); ++iter) {
    if (*iter == '+') {
      assert(iter != exp.begin());
      ans += cur_num;
      cur_num = 0;
    } else {
      assert('0' <= *iter && *iter <= '9');
      cur_num = cur_num * 10 + *iter - '0';
    }
  }
  ans += cur_num;
  return ans;
}

using namespace closure;
Closure<int(const std::string&)> closure1;
closure1 = calculate_sum;
closure1("1+2+3"); // result is 6

std::string exp = "1+2+3";
auto wrap_sum = [=] (const std::string& exp2) {
  return calculate_sum(exp + "+" + exp2);
};
closure1 = wrap_sum;
closure1("4"); // result is 10
```

## Binding

You can fast bind the first n arguments by passing them to the constructor or `MakeClosure`.

```C++
std::size_t sum(const int& v1, double v2, int v3, int v4) noexcept { return v1 + v2 + v3 + v4; }

using namespace closure;
auto closure1 = MakeClosure(sum, 1); // bind 1 to arg v1
// Alternatively, Closure<std::size_t(double, int, int)> closure1(sum, 1);
static_assert(std::is_same<decltype(closure1), Closure<std::size_t(double, int, int)>>::value);
closure1(2, 3, 4); // result is 10
```

Or, you can use placeholders to process more complicatedly binding. The number of placeholders is unlimited.

```C++
// Change the order of arguments.
auto lambda1 = [](int v1, int v2) { return v1 - v2; };

auto closure1 = closure::MakeClosure(lambda1, closure::PlaceHolder<1>(), closure::PlaceHolder<0>());
closure1(5, 3); // result is -2
```
## Compare to `std::function`

- `std::function` can only store the copyable object. Before c++23 introduced `std::move_only_function`, using only the standard library you cannot create a generic functional type which can hold a non-copyable functor.

```C++
class TestClassBindMethod {
 public:
  int ResIntArg1NonConst(int v) { return v; }
};

auto ptr = std::make_unique<TestClassBindMethod>();
auto closure4 = closure::MakeClosure(&TestClassBindMethod::ResIntArg1NonConst, std::move(ptr));
closure4(123); // result is 123
auto bounded = [capture0 = std::make_unique<TestClassBindMethod>()](int v) {
  return capture0->ResIntArg1NonConst(v);
};
//  std::function<int(int)> _ = std::move(bounded);  // can't compile
closure4 = std::move(bounded);

assert(!closure4.copyable()); // cannot copy a std::unique_ptr
auto closure5 = closure4;
assert(!closure5); // trying copy a non-copyable closure will get an empty result.
```

- Before c++17, when using `std::function` you have to correctly write the complete type of the object you want to construct. `MakeClosure` can help you omit this step.

```C++
template <class C, class... Args>
auto MakeClosure(C&&, Args&&...); // deduce the return type
```

If `C` is a function pointer, or a member function pointer (a.k.a. pointer to class method), or a "simple functor". Then `MakeClosure` can be applied, and it will return a `Closure` instance with the proper type.

A simple functor is a non-template, non-generic lambda, or a class type with one and only one `operator()` overloading, while this `operator()` is not a template. More formly, a class `F` is a functor if and only if `decltype(&F::operator())` is a valid expression, and it is a type of member function pointer.

These rules are similar to the deduction guides of `std::function` that introduced in c++17. However, the deduction rules don't contain the deduction for member function pointers, moreover, `MakeClosure` doesn't need c++17.

```C++
struct NonSimple {
  std::string operator()() const { return "empty"; }
  int operator()(int a, int b) const { return a + b; }
};
//  MakeClosure(NonSimple{}); // can't compile
closure::Closure<std::string()> closure1 = NonSimple{};
closure1(); // result is "empty";
Closure<int(int, int)> closure2 = NonSimple{};
closure2(1, 2); // result is 3

struct Simple {
  int operator()(int a, int b) const { return a + b; }
};
closure2 = MakeClosure(Simple{}); // ok
```

## Compare to `std::bind`

`std::bind` is somehow a bad design and is considered to be deprecated. But `Closure` makes up many drawbacks of `std::bind`. 

The type of the object return from `std::bind` is unspecified, which means you have to store it in the `std::function` to save it elsewhere. `Closure` integrates the arguments binding, and each `Closure` instance has a determined type.

Using `std::bind` you can even create a "callable" object that cannot call at all. Later when you try to call it, IDE and compiler will give you a lot of errors that hard to read. But using `Closure` you can never create a closure that unable to call. And the error messages are more human-friendly because it's incurred by a `static_assert`.

```C++
auto lambda = [](std::unique_ptr<int>) {};
auto b = std::bind(lambda, std::make_unique<int>()); // you can create b, even if it's wrong from the beginning.
b(); // the compiler will only complain error when you try to call it.
closure::MakeClosure(lambda, std::make_unique<int>()); // error, and the compiler will give an error message that easy to read.
```

If you use `std::bind`, when `b()`, the error message given by the gcc maybe very long, for example like (only a small part of the front, since it's too long)

```C++
: error: no match for call to '(std::_Bind<TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>(std::unique_ptr<int>)>) ()'
  620 |   b();
      |   ~^~
In file included from googletest/include/gtest/gtest-printers.h:104,
                 from googletest/include/gtest/gtest-matchers.h:48,
                 from googletest/include/gtest/internal/gtest-death-test-internal.h:46,
                 from googletest/include/gtest/gtest-death-test.h:43,
                 from googletest/include/gtest/gtest.h:61,
                 from :
/usr/local/Cellar/gcc/12.2.0/include/c++/12/functional:565:9: note: candidate: 'template<class ... _Args, class _Result> _Result std::_Bind<_Functor(_Bound_args ...)>::operator()(_Args&& ...) [with _Args = {_Args ...}; _Functor = TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>; _Bound_args = {std::unique_ptr<int, std::default_delete<int> >}]'
  565 |         operator()(_Args&&... __args)
      |         ^~~~~~~~
/usr/local/Cellar/gcc/12.2.0/include/c++/12/functional:565:9: note:   template argument deduction/substitution failed:
/usr/local/Cellar/gcc/12.2.0/include/c++/12/functional: In substitution of 'template<class _Functor, class ... _Bound_args> template<class _Fn, class _CallArgs, class ... _BArgs> using _Res_type_impl = typename std::result_of<_Fn&(decltype (std::_Mu<typename std::remove_cv<_BArgs>::type, std::is_bind_expression<typename std::remove_cv<_BArgs>::type>::value, (std::is_placeholder<typename std::remove_cv<_BArgs>::type>::value > 0)>()(declval<_BArgs&>(), declval<_CallArgs&>()))&& ...)>::type [with _Fn = TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>; _CallArgs = std::tuple<>; _BArgs = {std::unique_ptr<int, std::default_delete<int> >}; _Functor = TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>; _Bound_args = {std::unique_ptr<int, std::default_delete<int> >}]':
/usr/local/Cellar/gcc/12.2.0/include/c++/12/functional:532:8:   required by substitution of 'template<class _Functor, class ... _Bound_args> template<class _CallArgs> using _Res_type = std::_Bind<_Functor(_Bound_args ...)>::_Res_type_impl<_Functor, _CallArgs, _Bound_args ...> [with _CallArgs = std::tuple<>; _Functor = TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>; _Bound_args = {std::unique_ptr<int, std::default_delete<int> >}]'
/usr/local/Cellar/gcc/12.2.0/include/c++/12/functional:562:9:   required from here
/usr/local/Cellar/gcc/12.2.0/include/c++/12/functional:528:15: error: no type named 'type' in 'struct std::result_of<TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>&(std::unique_ptr<int>&)>'
  528 |         using _Res_type_impl
      |               ^~~~~~~~~~~~~~
```

But the error message caused by `closure::MakeClosure(lambda, std::make_unique<int>())` is much shorter, and will tell you "the given arguments don't match the arguments of callee".

```C++
In file included from :
closure.hpp: In instantiation of 'class closure::closureimpl::ClosureImpl<void(std::unique_ptr<int>), TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>, closure::ArgList<std::unique_ptr<int, std::default_delete<int> > >, void>':
/usr/local/Cellar/gcc/12.2.0/include/c++/12/type_traits:734:38:   required from 'struct std::is_trivially_copyable<closure::closureimpl::ClosureImpl<void(std::unique_ptr<int>), TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>, closure::ArgList<std::unique_ptr<int, std::default_delete<int> > >, void> >'
closure.hpp:201:68:   required from 'struct closure::closureimpl::soo::IsSmallObject<closure::closureimpl::ClosureImpl<void(std::unique_ptr<int>), TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>, closure::ArgList<std::unique_ptr<int, std::default_delete<int> > >, void> >'
closure.hpp:216:80:   required by substitution of 'template<class Tp, class ... Args, typename std::enable_if<(! closure::closureimpl::soo::IsSmallObject<Tp>::value), int>::type <anonymous> > void closure::closureimpl::StoragePool::emplace(Args&& ...) [with Tp = closure::closureimpl::ClosureImpl<void(std::unique_ptr<int>), TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>, closure::ArgList<std::unique_ptr<int, std::default_delete<int> > >, void>; Args = {TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int, std::default_delete<int> >)>&, std::unique_ptr<int, std::default_delete<int> >}; typename std::enable_if<(! closure::closureimpl::soo::IsSmallObject<Tp>::value), int>::type <anonymous> = <missing>]'
closure.hpp:312:35:   required from 'auto closure::closureimpl::MakeClosureImpl(StoragePool*, closure::ArgList<Tps2 ...>, Callable&&, Bounds&& ...) [with R = void; ClosureArgs = {std::unique_ptr<int, std::default_delete<int> >}; Callable = TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>&; Bounds = {std::unique_ptr<int, std::default_delete<int> >}; typename std::enable_if<(! closure::placeholders::HasPlaceHolder<closure::ArgList<Os2 ...> >::value), int>::type <anonymous> = 0]'
closure.hpp:500:113:   required from 'auto closure::MakeClosure(Functor&&, Bounds&& ...) [with Functor = TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>&; Bounds = {std::unique_ptr<int, std::default_delete<int> >}; typename std::enable_if<(traits::IsSimpleFunctor<typename std::remove_reference<_Tp>::type>::value && (! placeholders::HasPlaceHolder<ArgList<Tps2 ...> >::value)), int>::type <anonymous> = 0]'
:   required from here
closure.hpp:111:28: error: static assertion failed: the given arguments don't match the arguments of callee
  111 |   static_assert(validator::is_invokable, "the given arguments don't match the arguments of callee");
      |                            ^~~~~~~~~~~~
closure.hpp:111:28: note: 'closure::closureimpl::Validator<TestClosureWithPlaceHolders_Method_Test::TestBody()::<lambda(std::unique_ptr<int>)>, closure::ArgList<std::unique_ptr<int, std::default_delete<int> > >, closure::ArgList<std::unique_ptr<int, std::default_delete<int> > > >::is_invokable' evaluates to false
```

## closure::Any

Sometimes you may create a closure with the discontinuous placeholders, thus some parameters are useless and will be abandoned when calling. For example. Object `c` takes 4 arguments, but only the 2nd and 4th arguments are meaningful. As for the 1st and 3rd arguments, they can be anything.

```C++
auto lambda = [](int a, int b) { return a + b; };
auto c = MakeClosure(lambda, PlaceHolder<1>(), PlaceHolder<3>());

closure("123", 4, "567", 8); // ok, result is 12
closure(std::vector<int>{1, 2}, 3, std::vector<long>{4, 5}, 6); // ok, result is 9
```

When the placeholders are discontinuous, `MakeClosure` will let `closure::Any` be the type of the useless parameters. So the type of `c` is `closure::Closure<int(closure::Any, int, closure::Any, int)`.

```C++
std::is_same<decltype(c), closure::Closure<int(closure::Any, int, closure::Any, int)>>::value; // equals to true
```

Since `closure::Any` means any type, you can assign `c` to any other closure with the type that the useful parameters are matched. Like

```C++
// it's ok that the 4th parameter is float type, because float can implicitly convert to int
Closure<int(int, int, std::string, float)> c2(lambda, PlaceHolder<1>(), PlaceHolder<3>());
c2 = closure;
c2(1, 2, "3", 4.2); // result is 6
```
