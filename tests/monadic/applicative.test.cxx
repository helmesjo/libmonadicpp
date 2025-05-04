#include <monadicpp/monad.hxx>

#include <concepts>

#undef NDEBUG
#include <cassert>

auto
main() -> int
{
  using namespace fho;
  using namespace std;

  // Assembly: Create monads with a value and a function
  auto m  = pure(42);
  auto mf = pure(
    [](int x)
    {
      return x + 1;
    });

  // Act: Apply the function in the monad to the value in the monad
  auto result = ap(mf, m);

  // Assert: Check the result of applying the function
  assert(result.value() == 43);
  static_assert(same_as<decltype(result.value()), int>);

  return 0;
}
