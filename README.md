# Closure

 **Closure is a c++ functional object implementation. It integrates the std::function and std::bind, and even more powerful.**

[![Linux Status](https://github.com/coyorkdow/closure/actions/workflows/linux.yml/badge.svg)](https://github.com/coyorkdow/closure/actions/workflows/linux.yml)
[![macOS Status](https://github.com/coyorkdow/closure/actions/workflows/macos.yml/badge.svg)](https://github.com/coyorkdow/closure/actions/workflows/macos.yml)

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

Or, you can use placeholders. The number of placeholders is unlimited.

```C++
auto lambda1 = [](int v1, int v2) { return v1 - v2; };

auto closure1 = closure::MakeClosure(lambda1, closure::PlaceHolder<1>(), closure::PlaceHolder<0>());
closure1(5, 3); // result is -2
```
