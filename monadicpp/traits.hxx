#pragma once

#include <monadicpp/detail/func_traits.hxx>
#include <monadicpp/detail/traits/applied_tuple.hxx>
#include <monadicpp/detail/traits/subtuple.hxx>
#include <monadicpp/detail/traits/tuple_cat.hxx>

#include <concepts>
#include <tuple>
#include <type_traits>
#include <utility>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

namespace fho::detail
{
  /// @brief Stores a sequence of computation signatures for monads.
  /// @details Used to track the types of computations in a monad’s signature chain.
  template<typename... Computations>
  struct monad_signature
  {};

  /// @brief Primary type trait for extracting composed `monad_signature`.
  template<typename... Sig>
  struct signature
  {
    using type = monad_signature<Sig...>;
  };

  /// @brief Defines a signature for a single invocable type.
  /// @details Specifies a monad signature based on the function’s signature.
  template<std::invocable T>
  struct signature<T>
  {
    using type = monad_signature<function_signature_t<T>>;
  };

  /// @brief Defines a `monad_signature` for a function type composed with a `monad_signature`.
  /// @details Appends a new function’s signature to an existing monad signature.
  template<typename F, typename... Computations>
  struct signature<F, monad_signature<Computations...>>
  {
    using type = monad_signature<F, Computations...>;
  };

  /// @brief Defines a `monad_signature` for two composed `monad_signature` types.
  /// @details Appends a new function’s signature to an existing monad signature.
  template<typename... LComputations, typename... Computations>
  struct signature<monad_signature<LComputations...>, monad_signature<Computations...>>
  {
    using type = monad_signature<LComputations..., Computations...>;
  };

  template<typename T, typename... Computations>
  using signature_t = typename detail::signature<T, Computations...>::type;
}

/// @brief Specializations of `tuple_element_t` & `tuple_size_v` for `monad_signature`.
namespace std
{
  template<size_t I, class... Ts>
  struct tuple_element<I, fho::detail::monad_signature<Ts...>>
  {
    using type = tuple_element_t<I, tuple<Ts...>>;
  };

  template<class... Ts>
  struct tuple_size<fho::detail::monad_signature<Ts...>> : integral_constant<size_t, sizeof...(Ts)>
  {};
}

namespace fho::detail::tests::signature
{
  /// TEST: Signature Composition
  static_assert(std::same_as<monad_signature<int(), float()>, signature_t<int(), float()>>);

  static_assert(
    std::same_as<monad_signature<float(), int()>, signature_t<float(), monad_signature<int()>>>);

  static_assert(
    []
    {
      using monad1_sig_t = monad_signature<int(), float()>;
      using monad2_sig_t = monad_signature<double(), char()>;
      return std::same_as<monad_signature<int(), float(), double(), char()>,
                          signature_t<monad1_sig_t, monad2_sig_t>>;
    }());

  /// TEST: Tuple specializations
  static_assert(std::same_as<int(), std::tuple_element_t<0, monad_signature<int(), float()>>>);
  static_assert(std::same_as<float(), std::tuple_element_t<1, monad_signature<int(), float()>>>);
  static_assert(2 == std::tuple_size_v<monad_signature<int(), float()>>);
}

#undef FWD
