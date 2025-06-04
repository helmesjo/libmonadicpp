#include <monadicpp/monad.hxx>
#include <monadicpp/version.hxx>

#include <format>
#include <iostream>
#include <string>

#undef NDEBUG
#include <cassert>

auto
main() -> int
{
  using namespace fho;

  // constexpr auto m1 = pure(
  //   []()
  //   {
  //     return 0.0;
  //   });

  constexpr auto m1 = monad2(
    []
    {
      return 1;
    });
  constexpr auto m2 = [](int x)
  {
    return x + 4;
  };
  constexpr auto mx = [](int x)
  {
    return monad2(
      [=]()
      {
        return x + 1;
      });
  };
  constexpr auto my = monad2(
    [](auto x)
    {
      return monad2(x + 1);
    });
  constexpr auto derp1 = m1.value();
  // constexpr auto m4    = m1.fmap(m2);
  // constexpr auto m5 = m1.bind(mx);
  // constexpr auto m6    = m1.ap(mx);
  constexpr auto m7 = m1.ap(my);

  // constexpr auto final_value  = m4.value(); // == 5
  // constexpr auto final_value2 = m5.value(); // == 2

  // constexpr auto res = fho::ap_compose{}(my, m1);
  // constexpr auto val = res.value()().value()().value();
  // std::cout << std::format("val: {}", );
  // constexpr auto final_value3 = m6.value(); // == 2
  constexpr auto final_value4 = m7.value(); // == 2
  // constexpr auto lhs          = std::get<0>(m7.computes_);
  // constexpr auto comp         = std::get<1>(m7.computes_);
  // constexpr auto rhs          = std::get<2>(m7.computes_);
  // constexpr auto derp  = comp(lhs, rhs);
  // constexpr auto perp = smerg.value();
  // (void)m1;
  // constexpr auto m2 = pure(2);
  constexpr auto m3 = m1.fmap(
    [](int x)
    {
      return std::to_string(x);
    });

  constexpr auto m4 = m3.fmap(
    [](std::string x) -> double
    {
      return std::stoi(x);
    });

  constexpr auto m5 = m4.bind(
    [](double x)
    {
      return monad2(
        [x]
        {
          return x + 1;
        });
    });

  std::cout << std::format("m4.value() == {}\n", m4.value());
  std::cout << std::format("m5.value() == {}\n", m5.value());
  std::cout << std::format("m5.value() + m4.value() == {}\n", m5.value() + m4.value());
  assert(m5.value() + m4.value() == 3.0);
  return 0;
}
