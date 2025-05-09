#include <monadicpp/monad.hxx>

#undef NDEBUG
#include <cassert>

auto
main() -> int
{
  using namespace fho;
  using namespace std;

  // Test Applicative Laws for a monad
  // Assuming 'fho' namespace provides a monad implementation with 'pure' and 'ap'

  // Test data
  auto x = 42;
  auto f = [](int n)
  {
    return n + 1;
  }; // f: int -> int
  auto g = [](int n)
  {
    return n * 2;
  }; // g: int -> int
  auto h = [](int n)
  {
    return n - 1;
  }; // h: int -> int

  // Monads
  auto m  = pure(x); // pure 42
  auto mf = pure(f); // pure (int -> int)
  auto mg = pure(g); // pure (int -> int)
  auto mh = pure(h); // pure (int -> int)

  // Law 1: Identity
  // pure id <*> v = v
  {
    auto id = [](auto x)
    {
      return x;
    };
    auto mid    = pure(id);
    auto result = ap(mid, m);
    assert(result.value() == x);
    static_assert(same_as<decltype(result.value()), int>);
  }

  // Law 2: Homomorphism
  // pure f <*> pure x = pure (f x)
  {
    auto result1 = ap(pure(f), pure(x));
    auto result2 = pure(f(x));
    assert(result1.value() == result2.value());
    assert(result1.value() == 43);
    static_assert(same_as<decltype(result1.value()), int>);
  }

  // Law 3: Interchange
  // u <*> pure y = pure ($ y) <*> u
  // where ($ y) f = f y
  // {
  //   auto y       = 10;
  //   auto u       = pure(f);
  //   auto result1 = ap(u, pure(y));
  //   auto apply_y = [y](auto func) // NOLINT
  //   {
  //     return func(y);
  //   };
  //   auto result2 = ap(pure(apply_y), u);
  //   // assert(result1.value() == result2.value());
  //   // assert(result1.value() == f(y));
  //   // static_assert(same_as<decltype(result1.value()), int>);
  // }

  // Law 4: Composition
  // u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
  // where (.) f g x = f (g x)
  // {
  //   auto u       = pure(f); // f: +1
  //   auto v       = pure(g); // g: *2
  //   auto w       = pure(x); // x: 42
  //   auto compose = [](auto f, auto g)
  //   {
  //     return [f, g](auto x)
  //     {
  //       return f(g(x));
  //     };
  //   };
  //   auto result1 = ap(u, ap(v, w));
  //   auto result2 = ap(ap(ap(pure(compose), u), v), w);
  //   assert(result1.value() == result2.value());
  //   assert(result1.value() == f(g(x))); // f(g(42)) = 43
  //   static_assert(same_as<decltype(result1.value()), int>);
  // }

  // Original test
  {
    auto result = ap(mf, m);
    assert(result.value() == 43);
    static_assert(same_as<decltype(result.value()), int>);
  }

  return 0;
}
