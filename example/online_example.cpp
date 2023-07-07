/*
 * online example. load this file into a compiler explorer page.
 */
#include <https://raw.githubusercontent.com/coyorkdow/closure/main/include/closure/util.hpp>
#include <https://raw.githubusercontent.com/coyorkdow/closure/main/include/closure/traits.hpp>
#include <https://raw.githubusercontent.com/coyorkdow/closure/main/include/closure/bind.hpp>
#include <https://raw.githubusercontent.com/coyorkdow/closure/main/include/closure/placeholders.hpp>
#include <https://raw.githubusercontent.com/coyorkdow/closure/main/include/closure/trivial_tuple.hpp>
#include <https://raw.githubusercontent.com/coyorkdow/closure/main/include/closure/closure.hpp>

#include <cassert>
#include <iostream>

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

int main() {
  closure::Closure<int(const std::string&)> closure1;
  closure1 = calculate_sum;
  auto res = closure1("1+2+3");
  std::cout << res << '\n';  // result is 6
  return 0;
}
