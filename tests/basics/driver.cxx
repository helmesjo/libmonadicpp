#include <type_traits>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

// Free function pointer
template<typename... Args, typename... Inferred, typename R>
constexpr auto
disambiguate(R (*f)(Args..., Inferred...)) -> R (*)(Args..., Inferred...)
{
  return f;
}

// Non-const member function pointer
template<typename... Args, typename... Inferred, typename R, typename C>
constexpr auto
disambiguate(R (C::*f)(Args..., Inferred...)) -> R (C::*)(Args..., Inferred...)
{
  return f;
}

// Const member function pointer
template<typename... Args, typename... Inferred, typename R, typename C>
constexpr auto
disambiguate(R (C::*f)(Args..., Inferred...) const) -> R (C::*)(Args..., Inferred...) const
{
  return f;
}

// Functor/lambda: Resolve operator() implicitly with partial signature
template<typename... Args, typename F>
constexpr auto
disambiguate(F&&) -> decltype(disambiguate<Args...>(&std::remove_reference_t<F>::operator()))
{
  return nullptr;
}

struct Dummy
{
  auto
  operator()(int a, float b) -> int
  {
    return a + static_cast<int>(b);
  }

  auto
  operator()(char a, float b) -> int
  {
    return static_cast<int>(a) + static_cast<int>(b);
  }

  auto
  operator()(int a, double b, int c) -> int
  {
    return a + static_cast<int>(b) + static_cast<int>(c);
  }
};

struct Class
{
  auto
  method(int a, float b) -> int
  {
    return a + static_cast<int>(b);
  }

  [[nodiscard]] auto
  const_method(int a, float b) const -> int
  {
    return a + static_cast<int>(b);
  }
};

auto
func(int a, float b) -> int
{
  return a + static_cast<int>(b);
}

auto
main() -> int
{
  Dummy d;
  auto  p1 = disambiguate<int, float>(d);  // int (Dummy::*)(int, float)
  auto  p2 = disambiguate<char, float>(d); // int (Dummy::*)(char, float)
  auto  p3 = disambiguate<int, double>(d); // int (Dummy::*)(int, double)
  auto  p4 = disambiguate<int>(func);      // int (*)(int, float)
  // Class c;
  auto p5 = disambiguate<int>(&Class::method);       // int (Class::*)(int, float)
  auto p6 = disambiguate<int>(&Class::const_method); // int (Class::*)(int, float) const
  return 0;
}
