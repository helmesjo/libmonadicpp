#pragma once

#include <concepts>
#include <functional>
#include <type_traits>
#include <utility>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

namespace fho
{
  namespace detail
  {
    template<typename T>
    struct function_signature;

    // Specialization for function pointers
    template<typename R, typename... Args>
    struct function_signature<R (*)(Args...)>
    {
      using type = R(Args...);
    };

    // Specialization for std::function
    template<typename R, typename... Args>
    struct function_signature<std::function<R(Args...)>>
    {
      using type = R(Args...);
    };

    // Specialization for member functions (e.g., operator() in functors)
    template<typename Class, typename R, typename... Args>
    struct function_signature<R (Class::*)(Args...) const>
    {
      using type = R(Args...);
    };

    // Primary template for callables (lambdas, functors)
    template<typename T>
    struct function_signature
    {
      // Extract operator() signature
      using type = typename function_signature<decltype(&T::operator())>::type;
    };

    template<typename T>
    using function_signature_t = typename function_signature<T>::type;

    template<typename... Computations>
    struct monad_signature
    {};

    template<typename... Sig>
    struct signature;

    template<std::invocable T>
    struct signature<T>
    {
      using type = monad_signature<function_signature_t<T>>;
    };

    template<typename T, typename... Computations>
    struct signature<T, monad_signature<Computations...>>
    {
      using type = monad_signature<Computations..., function_signature_t<T>>;
    };

    template<typename T, typename... Computations>
    using signature_t = typename detail::signature<T, Computations...>::type;

    template<typename T>
    struct null_monad
    {
      using value_type = T;

      constexpr auto map(auto&&) const -> null_monad;
      constexpr auto bind(auto&&) const -> null_monad;

      [[nodiscard]] constexpr auto
      value() const -> value_type
      {
        return {};
      }
    };

    template<typename T>
    struct null_functor
    {
      constexpr auto map(auto&&) const -> null_monad<T>;
    };

    struct null_monadic_func
    {
      template<typename T>
      constexpr auto
      operator()(T&&) const -> null_monad<T>
      {
        return {};
      }
    };

    struct null_pure_func
    {
      template<typename T>
      constexpr auto
      operator()(T&&) const -> T
      {
        return {};
      }
    };
  }

  template<typename T>
  concept monadic = requires (T t) {
                      typename T::value_type;
                      t.map(std::declval<detail::null_pure_func>());
                      t.bind(std::declval<detail::null_monadic_func>());
                      { t.value() } -> std::convertible_to<typename T::value_type>;
                    };

  template<typename T>
  concept functor = requires (T t) {
                      { t.map(std::declval<detail::null_pure_func>()) } -> monadic;
                    };

  template<typename T, typename... Args>
  concept monadic_func = requires (T&& t, Args&&... args) {
                           { std::invoke(t, std::forward<Args>(args)...) } -> monadic;
                         };

  template<typename T, typename... Args>
  concept pure_func = std::invocable<T, Args...> && (!monadic<std::invoke_result_t<T, Args...>>);

  template<typename T, typename R, typename... Args>
  concept invocable_r = requires (T&& t, Args&&... args) {
                          { std::invoke(t, std::forward<Args>(args)...) } -> std::convertible_to<R>;
                        };

  static_assert(monadic<detail::null_monad<int>>);
  static_assert(functor<detail::null_monad<int>>);
  static_assert(functor<detail::null_functor<int>>);
  static_assert(monadic_func<detail::null_monadic_func, int>);
  static_assert(pure_func<detail::null_pure_func, int>);

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

  template<typename Morphism, typename T, invocable_r<T> Func,
           typename Sig = detail::signature_t<Func>>
  class monad
  {
  public:
    using value_type   = T;
    using compute_type = Func;

    static constexpr auto morphism = Morphism{};

    constexpr monad(monad const&)                    = delete;
    constexpr monad(monad&&)                         = default;
    constexpr ~monad()                               = default;
    constexpr auto operator=(monad const&) -> monad& = delete;
    constexpr auto operator=(monad&&) -> monad&      = default;

    constexpr explicit monad(Func func)
      : comp_(std::move(func))
    {}

    template<pure_func<value_type> F>
    [[nodiscard]] constexpr auto
    map(F f) const
    {
      auto l  = morphism(comp_, f);
      using U = std::invoke_result_t<F, value_type>;
      using M = monad<Morphism, U, decltype(l), detail::signature_t<decltype(l), Sig>>;
      return M(std::move(l));
    }

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

    [[nodiscard]] constexpr auto
    value() const -> value_type
    {
      return comp_();
    }

  private:
    compute_type comp_;
  };

  template<typename Morphism = identity, std::invocable Func>
  static constexpr auto
  pure(Func f) noexcept
  {
    using value_t = std::invoke_result_t<Func>;
    return monad<Morphism, value_t, Func, detail::signature_t<Func>>(std::move(f));
  }

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

  // Static assertions for monad laws
  //
  // 1. Left Identity: pure(x) >>= f == f(x)
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

  // 2. Right Identity: m >>= pure == m
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

  // 3. Associativity: (m >>= f) >>= g == m >>= (x -> f(x) >>= g)
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

  // Bonus: Functor identity law for map: fmap id == id
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

  // Bonus: Functor composition law for map: fmap (f . g) == fmap f . fmap g
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
