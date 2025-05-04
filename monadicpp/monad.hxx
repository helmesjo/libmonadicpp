#pragma once

#include <monadicpp/monad_traits.hxx>
#include <monadicpp/detail/func_traits.hxx>

#include <functional>
#include <utility>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

namespace fho
{
  /// @brief Concept for monadic types.
  /// @details Ensures a type has a value type, supports map and bind operations, and provides a
  /// value extraction method.
  template<typename T>
  concept monadic = requires (T t) {
                      typename T::value_type;
                      t.map(std::declval<detail::null_pure_func>());
                      t.bind(std::declval<detail::null_monadic_func>());
                      { t.value() } -> std::convertible_to<typename T::value_type>;
                    };

  /// @brief Concept for functor types.
  /// @details Checks if a type supports map operation returning a monadic type.
  template<typename T>
  concept functor = requires (T t) {
                      { t.map(std::declval<detail::null_pure_func>()) } -> monadic;
                    };

  /// @brief Concept for functions returning monadic types.
  /// @details Verifies that a function, when invoked with arguments, produces a monadic result.
  template<typename T, typename... Args>
  concept monadic_func = requires (T&& t, Args&&... args) {
                           { std::invoke(t, std::forward<Args>(args)...) } -> monadic;
                         };

  /// @brief Concept for pure functions.
  /// @details Ensures a function is invocable and does not return a monadic type.
  template<typename T, typename... Args>
  concept pure_func = std::invocable<T, Args...> && (!monadic<std::invoke_result_t<T, Args...>>);

  /// @brief Concept for invocable types with specific return type.
  /// @details Checks if a function, when invoked, returns a type convertible to the specified
  /// return type.
  template<typename T, typename R, typename... Args>
  concept invocable_r = requires (T&& t, Args&&... args) {
                          { std::invoke(t, std::forward<Args>(args)...) } -> std::convertible_to<R>;
                        };

  static_assert(monadic<detail::null_monad<int>>);
  static_assert(functor<detail::null_monad<int>>);
  static_assert(functor<detail::null_functor<int>>);
  static_assert(monadic_func<detail::null_monadic_func, int>);
  static_assert(pure_func<detail::null_pure_func, int>);

  /// @brief Identity functor for monad transformations.
  /// @details Acts as a default morphism in monadic operations, passing values through computations
  /// and bindings without modification.
  struct identity
  {
    template<pure_func C, pure_func<std::invoke_result_t<C>> B>
    constexpr auto
    operator()(C&& compute, B&& bind) const
    {
      return [c = FWD(compute), b = FWD(bind)]()
      {
        return b(std::forward<C>(c)());
      };
    }

    template<pure_func C, monadic_func<std::invoke_result_t<C>> B>
    constexpr auto
    operator()(C&& compute, B&& bind) const
    {
      return [c = FWD(compute), b = FWD(bind)]()
      {
        return b(std::forward<C>(c)()).value();
      };
    }
  };

  /// @brief A monad class for functional programming in C++.
  /// @details Wraps a computation (function) and supports operations like mapping and binding,
  /// using a morphism to transform results.
  template<typename Morphism, typename T, invocable_r<T> Func,
           typename Sig = detail::signature_t<Func>>
  class monad
  {
  public:
    using value_type   = T;
    using compute_type = Func;

    static constexpr auto morphism = Morphism{};

    /// @brief Constructs a monad with a computation function.
    /// @param func The function producing the monad's value.
    constexpr explicit monad(Func func)
      : comp_(std::move(func))
    {}

    constexpr monad(monad const&)                    = delete;
    constexpr monad(monad&&)                         = default;
    constexpr ~monad()                               = default;
    constexpr auto operator=(monad const&) -> monad& = delete;
    constexpr auto operator=(monad&&) -> monad&      = default;

    /// @brief Applies a function to the monad’s value, returning a new monad.
    /// @param f Function to transform the value.
    /// @return New monad with the transformed result.
    template<pure_func<value_type> F>
    [[nodiscard]] constexpr auto
    map(F f) const
    {
      auto l  = morphism(comp_, f);
      using U = std::invoke_result_t<F, value_type>;
      using M = monad<Morphism, U, decltype(l), detail::signature_t<decltype(l), Sig>>;
      return M(std::move(l));
    }

    /// @brief Chains a monad-producing function to the monad’s value.
    /// @param f Function that takes the value and returns a new monad.
    /// @return New monad with the chained result.
    template<monadic_func<value_type> F>
    [[nodiscard]] constexpr auto
    bind(F f) const
    {
      auto l  = morphism(comp_, f);
      using U = std::invoke_result_t<F, value_type>;
      using V = typename U::value_type;
      using M = monad<Morphism, V, decltype(l), detail::signature_t<decltype(l), Sig>>;
      return M(std::move(l));
    }

    /// @brief Extracts the monad’s value by running the computation.
    /// @return The computed value.
    [[nodiscard]] constexpr auto
    value() const -> value_type
    {
      return comp_();
    }

  private:
    compute_type comp_;
  };

  /// @brief Creates a monad from a function.
  /// @param f Function producing the monad’s value.
  /// @return A monad wrapping the function.
  template<typename Morphism = identity, std::invocable Func>
  static constexpr auto
  pure(Func f) noexcept
  {
    using value_t = std::invoke_result_t<Func>;
    return monad<Morphism, value_t, Func, detail::signature_t<Func>>(std::move(f));
  }

  /// @brief Creates a monad from a plain value.
  /// @param v Value to wrap in the monad.
  /// @return A monad wrapping a function that returns the value.
  template<typename Morphism = identity, typename T>
    requires (!std::invocable<T>)
  static constexpr auto
  pure(T v) noexcept
  {
    auto f = [v = std::move(v)]() -> T
    {
      return v;
    };
    return monad<Morphism, T, decltype(f), detail::signature_t<decltype(f)>>{std::move(f)};
  }

  /// @brief Verifies monad laws through static assertions.
  /// @details Ensures the monad implementation satisfies left identity, right identity,
  /// associativity, and functor laws.

  /// @brief Left Identity Law: pure(x) >>= f == f(x).
  /// @details Checks that wrapping a value and binding it to a function yields the same result as
  /// applying the function directly.
  static_assert(
    []
    {
      auto x = 42;
      auto f = [](int v)
      {
        return pure(v + 1);
      };
      return pure(x).bind(f).value() == f(x).value();
    }());

  /// @brief Right Identity Law: m >>= pure == m.
  /// @details Verifies that binding a monad to the pure function returns the original monad.
  static_assert(
    []
    {
      auto m = pure(42);
      return m.bind(
                [](int v)
                {
                  return pure(v);
                })
               .value() == m.value();
    }());

  /// @brief Associativity Law: (m >>= f) >>= g == m >>= (x -> f(x) >>= g).
  /// @details Ensures that chaining monad operations is consistent regardless of grouping.
  static_assert(
    []
    {
      auto m = pure(42);
      auto f = [](int v)
      {
        return pure(v + 1);
      };
      auto g = [](int v)
      {
        return pure(v * 2);
      };
      auto lhs = m.bind(f).bind(g);
      auto rhs = m.bind(
        [=](int x)
        {
          return f(x).bind(g);
        });
      return lhs.value() == rhs.value();
    }());

  /// @brief Functor Identity Law: fmap id == id.
  /// @details Confirms that mapping the identity function over a monad leaves it unchanged.
  static_assert(
    []
    {
      auto m  = pure(42);
      auto id = [](int v)
      {
        return v;
      };
      return m.map(id).value() == m.value();
    }());

  /// @brief Functor Composition Law: fmap (f . g) == fmap f . fmap g.
  /// @details Verifies that mapping a composed function is equivalent to mapping functions
  /// sequentially.
  static_assert(
    []
    {
      auto m = pure(42);
      auto f = [](int v)
      {
        return v + 1;
      };
      auto g = [](int v)
      {
        return v * 2;
      };
      auto lhs = m.map(
        [=](int v)
        {
          return f(g(v));
        });
      auto rhs = m.map(g).map(f);
      return lhs.value() == rhs.value();
    }());
}

#undef FWD
