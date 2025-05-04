#pragma once

#include <monadicpp/detail/func_traits.hxx>

#include <concepts>

namespace fho::detail
{
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
