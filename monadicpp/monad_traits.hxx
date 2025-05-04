#pragma once

#include <monadicpp/detail/func_traits.hxx>

#include <concepts>

namespace fho::detail
{
  /// @brief Stores a sequence of computation signatures for monads.
  /// @details Used to track the types of computations in a monad’s signature chain.
  template<typename... Computations>
  struct monad_signature
  {};

  template<typename... Sig>
  struct signature;

  /// @brief Defines a signature for a single invocable type.
  /// @details Specifies a monad signature based on the function’s signature.
  template<std::invocable T>
  struct signature<T>
  {
    using type = monad_signature<function_signature_t<T>>;
  };

  /// @brief Extends a monad signature with additional computations.
  /// @details Appends a new function’s signature to an existing monad signature.
  template<typename T, typename... Computations>
  struct signature<T, monad_signature<Computations...>>
  {
    using type = monad_signature<Computations..., function_signature_t<T>>;
  };

  template<typename T, typename... Computations>
  using signature_t = typename detail::signature<T, Computations...>::type;

  /// @brief A null monad for type T.
  /// @details Acts as a placeholder monad that supports map, bind, and value operations, returning
  /// default-constructed values.
  template<typename T>
  struct null_monad
  {
    using value_type = T;

    constexpr auto fmap(auto&&) const -> null_monad;
    constexpr auto bind(auto&&) const -> null_monad;

    [[nodiscard]] constexpr auto
    value() const -> value_type
    {
      return {};
    }
  };

  /// @brief A null functor for type T.
  /// @details Provides a placeholder functor that supports map, returning a null monad.
  template<typename T>
  struct null_functor
  {
    constexpr auto fmap(auto&&) const -> null_monad<std::remove_cvref_t<T>>;
  };

  /// @brief A null monadic function.
  /// @details Placeholder function that takes any input and returns a null monad.
  struct null_monadic_func
  {
    template<typename T>
    constexpr auto
    operator()(T&&) const -> null_monad<std::remove_cvref_t<T>>
    {
      return {};
    }
  };

  /// @brief A null pure function.
  /// @details Placeholder function that returns a default-constructed value of the input type.
  struct null_pure_func
  {
    template<typename T>
    constexpr auto
    operator()(T&&) const -> std::remove_cvref_t<T>
    {
      return {};
    }
  };
}
