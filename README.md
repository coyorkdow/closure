# Closure

 **Closure is a c++ functional object implementation. It integrates the std::function and std::bind, and even more powerful.**

[![Linux Status](https://github.com/coyorkdow/closure/actions/workflows/linux.yml/badge.svg)](https://github.com/coyorkdow/closure/actions/workflows/linux.yml)

Closure is header-only. To use Closure, simply copy `closure.hpp` and dicectory `closure` into your project.

## Basic Usage

Use Closure like std::function. Store the function pointer or any callable object (include lambda).
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

TEST(TestClosure, UseLikeSTDFunction) {
  Closure<int(const std::string&)> closure1;
  closure1 = calculate_sum;
  EXPECT_EQ(closure1("1+2+3"), 6);

  std::string exp = "1+2+3";
  auto wrap_sum = [=] (const std::string& exp2) {
    return calculate_sum(exp + "+" + exp2);
  };
  closure1 = wrap_sum;
  EXPECT_EQ(closure1("4"), 10);
  std::function<int(std::string)> wrap_twice(wrap_sum);
  EXPECT_TRUE(wrap_twice);
  closure1 = std::move(wrap_twice);
  EXPECT_FALSE(wrap_twice); // should be empty after moved
  EXPECT_EQ(closure1("4"), 10);
}
```

## Binding

Still work in progress.

```C++
std::size_t sum(const int& v1, double v2, int v3, int v4) noexcept { return v1 + v2 + v3 + v4; }

auto closure1 = MakeClosure(sum, 1); // bind 1 to arg v1
static_assert(std::is_same<decltype(closure1), Closure<std::size_t(double, int, int)>>::value);
EXPECT_EQ(closure1(2, 3, 4), 10);
```
