#include <monadicpp/monad.hxx>

#include <concepts>

#undef NDEBUG
#include <cassert>

auto
main() -> int
{
  using namespace fho;
  using namespace std;

  // Assembly: Create a monad with a value
  auto m = pure(42);

  { // Act & Assert: Functor Identity Law: fmap id == id
    auto f = [](int x)
    {
      return x;
    };
    auto mappedId = fmap(f, m);

    assert(mappedId.value() == m.value());
    static_assert(same_as<decltype(mappedId.value()), int>);
  }

  { // Act & Assert: Functor Composition Law: fmap (f . g) == fmap f . fmap g
    auto f = [](int x)
    {
      return x + 1;
    };
    auto g = [](int x)
    {
      return x * 2;
    };
    auto composed = fmap(
      [f, g](int x)
      {
        return f(g(x));
      },
      m);
    auto chained = fmap(f, fmap(g, m));

    assert(composed.value() == chained.value());
    static_assert(same_as<decltype(composed.value()), decltype(chained.value())>);
  }
  return 0;
}
