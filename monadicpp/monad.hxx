#pragma once

#include <monadicpp/applicative.hxx>
#include <monadicpp/concepts.hxx>
#include <monadicpp/traits.hxx>
#include <monadicpp/detail/func_traits.hxx>

#include <utility>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

namespace fho
{
  /// @struct compose
  /// @brief Constructs a morphism by composing a computation and a binding function.
  ///
  /// @details Combines a pure computation and a binding function into a new pure
  ///          function (morphism). Provides overloads for pure and monadic bindings.
  struct compose
  {
    /// @brief Composes a pure computation with a pure binding function.
    ///
    /// @tparam Compute Pure function type of the computation (pure_func constraint).
    /// @tparam Bind Pure binding function type (pure_func constraint).
    ///
    /// @param comp Pure function to compute a value.
    /// @param binder Pure function to transform the computed value.
    /// @return A lambda (morphism) that applies `comp` and passes its result to `binder`,
    ///         returning `binder(comp())`.
    template<pure_func Compute, pure_func<std::invoke_result_t<Compute>> Bind>
    constexpr auto
    operator()(Compute&& comp, Bind&& binder) const
    {
      return [c = FWD(comp), b = FWD(binder)]()
      {
        return b(std::forward<Compute>(c)());
      };
    }

    /// @brief Composes a pure computation with a monadic binding function.
    ///
    /// @tparam Compute Pure function type of the computation (pure_func constraint).
    /// @tparam Bind Monadic binding function type (monadic_func constraint).
    ///
    /// @param comp Pure function to compute a value.
    /// @param binder Monadic function to transform the computed value.
    /// @return A lambda (morphism) that applies `comp`, passes its result to `binder`,
    ///         and unwraps the monadic result, returning `binder(comp()).value()`.
    template<pure_func Compute, chainable_func<std::invoke_result_t<Compute>> Bind>
    constexpr auto
    operator()(Compute&& comp, Bind&& binder) const
    {
      return [c = FWD(comp), b = FWD(binder)]()
      {
        return b(std::forward<Compute>(c)()).value();
      };
    }
  };

  /// @brief A monad class for functional programming in C++.
  /// @details Encapsulates a computation that produces a value of type T, enabling functional
  /// operations like mapping (Functor), application (Applicative), and chaining (Monad).
  /// The Morphism template parameter defines how computations are composed.
  /// Sig is an optional type for debugging, representing the computation's signature.
  template<typename Morphism, typename T, invocable_r<T> Func,
           typename Sig = detail::signature_t<Func>>
  class monad
  {
  public:
    using value_type   = T;                      ///< Type of the value produced by the computation.
    using compute_type = Func;                   ///< Type of the computation function.

    static constexpr auto morphism = Morphism{}; ///< Morphism instance for composing computations.

    /// @brief Constructs a monad with a computation function.
    /// @param func The computation function that produces a value of type T.
    constexpr explicit monad(Func func)
      : comp_(std::move(func))
    {}

    constexpr monad(monad const&)                    = default;
    constexpr monad(monad&&)                         = default;
    constexpr ~monad()                               = default;
    constexpr auto operator=(monad const&) -> monad& = default;
    constexpr auto operator=(monad&&) -> monad&      = default;

    /// @brief Maps a function over the monad’s value (Functor operation, `<$>`
    ///        in Haskell).
    /// @tparam F Pure function type taking value_type and returning a new value.
    /// @param f The function to apply to the monad's value.
    /// @return A new monad containing the computation composed with f.
    template<pure_func<value_type> F>
    [[nodiscard]] constexpr auto
    fmap(F f) const
    {
      auto l  = morphism(comp_, f);
      using U = std::invoke_result_t<F, value_type>;
      using M = monad<Morphism, U, decltype(l), detail::signature_t<Sig, decltype(l)>>;
      return M(std::move(l));
    }

    /// @brief Chains a function to the monad’s value (Monad operation, `>>=` in Haskell).
    /// @tparam F Function type taking value_type and returning a new monad or a callable.
    /// @param f The function to chain, producing a new monad or callable from the value.
    /// @return A new monad with the chained computation.
    template<chainable_func<value_type> F>
    [[nodiscard]] constexpr auto
    bind(F f) const
    {
      auto l  = morphism(comp_, f);
      using U = std::invoke_result_t<F, value_type>;
      using V = typename U::value_type;
      using M = monad<Morphism, V, decltype(l), detail::signature_t<Sig, decltype(l)>>;
      return M(std::move(l));
    }

    /// @brief Applies a function wrapped in a monad to this monad's value (Applicative
    ///        operation, `<*>` in Haskell).
    /// @tparam MF The monad type containing the function to apply.
    /// @param mf The monad containing a function to apply to this monad's value.
    /// @return A new monad with the result of applying the function.
    template<monadic MF>
    [[nodiscard]] constexpr auto
    ap(MF&& mf) const
    {
      using self_t = decltype(*this);
      return FWD(mf).bind(
        [self = *this](pure_func<value_type> auto&& f)
        {
          return static_cast<self_t>(self).fmap(FWD(f));
        });
    }

    /// @brief Extracts the monad’s value by executing the computation.
    /// @return The computed value of type `value_type`.
    [[nodiscard]] constexpr auto
    value() const -> value_type
    {
      return comp_();
    }

  private:
    compute_type comp_; ///< The computation function producing the value.
  };

  /// @brief Creates a monad from a function.
  /// @param f Function producing the monad’s value.
  /// @return A monad wrapping the function.
  template<typename Morphism = compose, std::invocable Func>
  static constexpr auto
  pure(Func f) noexcept
  {
    using value_t = std::invoke_result_t<Func>;
    return monad<Morphism, value_t, Func, detail::signature_t<Func>>(std::move(f));
  }

  /// @brief Creates a monad from a plain value.
  /// @param v Value to wrap in the monad.
  /// @return A monad wrapping a function that returns the value.
  template<typename Morphism = compose, typename T>
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

  /// @brief Maps a pure function over a functor (`fmap` in Haskell).
  /// @tparam F The type of the pure function.
  /// @tparam M The functor/monad type.
  /// @param f The pure function to apply.
  /// @param m The functor/monad to map over.
  /// @return A new functor/monad with the transformed value.
  template<functor M, mappable<M> F>
  constexpr auto
  fmap(F&& f, M&& m) noexcept
  {
    return FWD(m).fmap(FWD(f));
  }

  /// @brief Binds a monadic function to a monad (`>>=` in Haskell).
  /// @tparam F The type of the monadic function.
  /// @tparam M The monad type.
  /// @param m The monad to bind.
  /// @param f The monadic function to apply.
  /// @return A new monad with the result of the monadic function.
  template<monadic M, bindable<M> F>
  constexpr auto
  bind(M&& m, F&& f) noexcept
  {
    return FWD(m).bind(FWD(f));
  }

  /// @brief Applies a function wrapped in a monad to a value wrapped in a monad (`<*>` in Haskell).
  /// @tparam MF The monad type containing the function.
  /// @tparam M The monad type containing the value.
  /// @param mf The monad containing the function.
  /// @param m The monad containing the value.
  /// @return A new monad with the result of applying the function to the value.
  template<monadic MF, monadic M>
  constexpr auto
  ap(MF&& mf, M&& m) noexcept
  {
    return FWD(m).ap(FWD(mf));
  }

  /// @brief Verifies monad laws through static assertions.
  /// @details Ensures the monad implementation satisfies left identity, right identity,
  /// associativity, and functor laws.

  /// @brief Left Identity Law: `pure(x) >>= f == f(x)`.
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

  /// @brief Right Identity Law: `m >>= pure == m`.
  /// @details Verifies that binding a monad to the pure function returns the original monad.
  static_assert(
    []
    {
      auto m = pure(42);
      auto f = [](int v)
      {
        return pure(v);
      };
      return m.bind(f).value() == m.value();
    }());

  /// @brief Associativity Law: `(m >>= f) >>= g == m >>= (x -> f(x) >>= g)`.
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

  /// @brief Functor Identity Law: `fmap id == id`.
  /// @details Confirms that mapping the identity function over a monad leaves it unchanged.
  static_assert(
    []
    {
      auto m  = pure(42);
      auto id = [](int v)
      {
        return v;
      };
      return m.fmap(id).value() == m.value();
    }());

  /// @brief Functor Composition Law: `fmap (f ∘ g) == fmap f ∘ fmap g`.
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
      auto lhs = m.fmap(
        [=](int v)
        {
          return f(g(v));
        });
      auto rhs = m.fmap(g).fmap(f);
      return lhs.value() == rhs.value();
    }());
}

#undef FWD
