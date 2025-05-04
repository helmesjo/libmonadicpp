#include <monadicpp/monad.hxx>

#include <concepts>

#undef NDEBUG
#include <cassert>

auto
main() -> int
{
  using namespace fho;
  using namespace std;

  // Assembly: Setup a value and monadic function
  auto x = 42;
  auto f = [](int v)
  {
    return pure(v + 1);
  };
  auto m = pure(x);

  { // Act & Assert: Left Identity Law: pure(x) >>= f == f(x)
    auto left  = bind(m, f);
    auto right = f(x);
    assert(left.value() == right.value());
    static_assert(same_as<decltype(left.value()), decltype(right.value())>);
  }

  { // Act & Assert: Right Identity Law: m >>= pure == m
    auto boundPure = bind(m,
                          [](int v)
                          {
                            return pure(v);
                          });
    assert(boundPure.value() == m.value());
    static_assert(same_as<decltype(boundPure.value()), int>);
  }

  { // Act & Assert: Associativity Law: (m >>= f) >>= g == m >>= (x -> f(x) >>= g)
    auto g = [](int v)
    {
      return pure(v * 2);
    };
    auto lhs = bind(bind(m, f), g);
    auto fg  = [f, g](int v)
    {
      return bind(f(v), g);
    };
    auto rhs = bind(m, fg);
    assert(lhs.value() == rhs.value());
    static_assert(same_as<decltype(lhs.value()), decltype(rhs.value())>);
  }

  return 0;
}
