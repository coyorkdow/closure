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
- Helper function `MakeClosure` can create a closure and deduce its type, you can use `auto` instead of manually writing the `Closure`'s template arguments. `MakeClosure` also supports arguments binding.

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

- `std::function` can only store the copyable object. Before c++23 introduces the `std::move_only_function`, using only the standard library you cannot create a generic functional type which can hold a non-copyable functor.

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
auto MakeClosure(C&&, Args&&...);
```

If `C` is a function pointer, or a member function pointer (a.k.a. pointer to class method), or a "simple functor". Then `MakeClosure` can be applied, and it will return a closure instance with the proper type.

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