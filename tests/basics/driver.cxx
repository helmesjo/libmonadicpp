#include <libmonadicpp/monad.hxx>
#include <libmonadicpp/version.hxx>

#undef NDEBUG
#include <cassert>

auto
main() -> int
{
  using namespace fho;

  constexpr auto m1 = pure(
    []()
    {
      return 0.0;
    });
  (void)m1;
  constexpr auto m2 = pure(2);
  constexpr auto m3 = m2.map(
    [](int x)
    {
      return std::to_string(x);
    });

  constexpr auto m4 = m3.map(
    [](std::string x) -> double
    {
      return std::stoi(x);
    });

  constexpr auto m5 = m4.bind(
    [](double x)
    {
      return pure(x + 1);
    });

  assert(m5.value() + m4.value() == 5.0);
  return 0;
}
