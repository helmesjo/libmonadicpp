#pragma once

#include <monadicpp/detail/func_traits.hxx>

#include <concepts>
#include <utility>

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

  /// @brief Type trait to extract a subtuple from a tuple starting at Offset with Count elements.
  /// @tparam Offset Starting index of the subtuple.
  /// @tparam Count Number of elements in the subtuple.
  /// @tparam Ts Types in the input tuple.
  template<size_t Offset, size_t Count, typename... Ts>
  struct tuple_subtypes;

  /// @brief Specialization for tuple input.
  /// @tparam Offset Starting index.
  /// @tparam Count Number of elements.
  /// @tparam Ts Types in the tuple.
  /// @note Static assert ensures Offset + Count does not exceed tuple size.
  template<size_t Offset, size_t Count, typename... Ts>
  struct tuple_subtypes<Offset, Count, std::tuple<Ts...>>
  {
    static_assert(Offset + Count <= sizeof...(Ts), "Offset + Count exceeds tuple size");

    /// @brief Helper to create subtuple type from index sequence.
    /// @tparam Is Index sequence for subtuple elements.
    template<size_t... Is>
    static auto make_subtuple(std::index_sequence<Is...>)
      -> std::tuple<std::tuple_element_t<Offset + Is, std::tuple<Ts...>>...>;

    /// @brief Resulting subtuple type.
    using type = decltype(make_subtuple(std::make_index_sequence<Count>{}));
  };

  /// @brief Alias for accessing subtuple type.
  /// @tparam Offset Starting index.
  /// @tparam Count Number of elements.
  /// @tparam T Input tuple type.
  template<size_t Offset, size_t Count, typename T>
  using subtuple_t = typename tuple_subtypes<Offset, Count, T>::type;

  /// TEST: Sub-tuple
  static_assert(std::same_as<std::tuple<int>, subtuple_t<0, 1, std::tuple<int, float, double>>>);
  static_assert(std::same_as<std::tuple<double>, subtuple_t<2, 1, std::tuple<int, float, double>>>);
  static_assert(std::same_as<std::tuple<float>, subtuple_t<1, 1, std::tuple<int, float, double>>>);
  static_assert(
    std::same_as<std::tuple<int, float, double>, subtuple_t<0, 3, std::tuple<int, float, double>>>);
  static_assert(
    std::same_as<std::tuple<float, double>, subtuple_t<1, 2, std::tuple<int, float, double>>>);
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

namespace fho::detail
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
