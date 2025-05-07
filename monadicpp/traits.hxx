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
}
