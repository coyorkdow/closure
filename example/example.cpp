#include <cassert>
#include <iostream>

#include "closure.hpp"

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

std::size_t sum(const int& v1, double v2, int v3, int v4) noexcept { return v1 + v2 + v3 + v4; }

int main() {
  {
    closure::Closure<int(const std::string&)> closure1;
    closure1 = calculate_sum;
    std::cout << closure1("1+2+3") << '\n';  // result is 6

    std::string exp = "1+2+3";
    auto wrap_sum = [=](const std::string& exp2) { return calculate_sum(exp + "+" + exp2); };
    closure1 = wrap_sum;
    std::cout << closure1("4") << '\n';  // result is 10
  }
  {
    auto closure1 = closure::MakeClosure(sum, 1);  // bind 1 to arg v1
    // Or Closure<std::size_t(double, int, int)> closure1(sum, 1);
    static_assert(std::is_same<decltype(closure1), closure::Closure<std::size_t(double, int, int)>>::value, "");
    std::cout << closure1(2, 3, 4) << '\n';  // result is 10
  }
  {
    auto lambda1 = [](int v1, int v2) { return v1 - v2; };
    auto closure1 = closure::MakeClosure(lambda1, closure::PlaceHolder<1>(), closure::PlaceHolder<0>());
    std::cout << closure1(5, 3) << '\n'; // result is -2
  }
}