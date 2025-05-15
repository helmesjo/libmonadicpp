#pragma once

#include <monadicpp/detail/func_traits.hxx>
#include <monadicpp/detail/traits/subtuple.hxx>

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

  /// @brief Concept to detect tuple-like types.
  template<typename T>
  concept tuple_like = requires { typename std::tuple_size<std::remove_cvref_t<T>>::type; };

  /// @brief Tuple concatination.
  /// @param tp1 First tuple.
  /// @param types Variadic arguments (non-empty).
  template<typename... Types>
    requires (sizeof...(Types) > 0 && !tuple_like<std::tuple_element_t<0, std::tuple<Types...>>>)
  constexpr auto
  tuple_concat(tuple_like auto&& tp, Types&&... types)
  {
    return std::tuple_cat(FWD(tp), std::tuple<Types...>(FWD(types)...));
  }

  /// @brief Tuple concatination.
  /// @param tp1 First tuple.
  /// @param types Variadic arguments (non-empty).
  constexpr auto
  tuple_concat(tuple_like auto&& tp)
  {
    return FWD(tp);
  }

  /// @brief Tuple concatination.
  /// @param tp1 First tuple.
  /// @param tp2 Second tuple.
  constexpr auto
  tuple_concat(tuple_like auto&& tp1, tuple_like auto&& tp2)
  {
    return std::tuple_cat(FWD(tp1), FWD(tp2));
  }

  /// @brief Concatenated tuple type of `Tuple` and `Types...`.
  /// @detailed `Types...` is a tuple-like or variadic types.
  template<typename Tuple, typename... Types>
  using tuple_concat_t = decltype(tuple_concat(std::declval<Tuple>(), std::declval<Types>()...));

  /// TEST: Tuple concatination
  static_assert(
    std::same_as<std::tuple<int, float>, tuple_concat_t<std::tuple<int, float>, std::tuple<>>>);
  static_assert(std::same_as<std::tuple<int, float>, tuple_concat_t<std::tuple<int, float>>>);
  static_assert(std::same_as<std::tuple<int, float, double, int>,
                             tuple_concat_t<std::tuple<int, float>, double, int>>);
  static_assert(std::same_as<std::tuple<int, float, double, int>,
                             tuple_concat_t<std::tuple<int, float>, std::tuple<double, int>>>);

  /// @brief Applies a trait to unpacked tuple types and additional arguments.
  template<template<typename...> typename Trait, typename Arg1, typename Tuple, typename... Rest>
  struct applied_tuple;

  template<template<typename...> typename Trait, typename Arg1, typename... Bound, typename... Rest>
  struct applied_tuple<Trait, Arg1, std::tuple<Bound...>, Rest...>
  {
    using type                  = typename Trait<Arg1, Bound..., Rest...>::type; // NOLINT
    static constexpr bool value = type::value;
  };

  /// @brief Applies `Tuple` & `Rest...` to `Trait`.
  template<template<typename...> typename Trait, typename Arg1, typename Tuple, typename... Rest>
  using applied_t = typename applied_tuple<Trait, Arg1, Tuple, Rest...>::type;

  template<template<typename...> typename Trait, typename Arg1, typename Tuple, typename... Rest>
  static constexpr auto applied_v = applied_t<Trait, Arg1, Tuple, Rest...>::value;

  /// TEST: Tuple applied to type trait.
  static_assert(
    []
    {
      constexpr auto l = [](auto a, float&)
      {
        return a;
      };
      return std::same_as<std::invoke_result_t<decltype(l), int, float&>,
                          applied_t<std::invoke_result, decltype(l), std::tuple<int, float&>>>;
    }());

  static_assert(
    []
    {
      constexpr auto l = [](auto& a, float)
      {
        return a;
      };
      return std::same_as<std::invoke_result_t<decltype(l), int&, float>,
                          applied_t<std::invoke_result, decltype(l), std::tuple<int&>, float>>;
    }());

  // Test 1: Void return type
  static_assert(
    []
    {
      constexpr auto l = [](int, float&) {};
      return std::same_as<std::invoke_result_t<decltype(l), int, float&>,
                          applied_t<std::invoke_result, decltype(l), std::tuple<int, float&>>>;
    }());

  // Test 2: Empty tuple
  static_assert(
    []
    {
      constexpr auto l = [](int)
      {
        return 42;
      };
      return std::same_as<std::invoke_result_t<decltype(l), int>,
                          applied_t<std::invoke_result, decltype(l), std::tuple<>, int>>;
    }());

  // Test 3: Rvalue reference
  static_assert(
    []
    {
      constexpr auto l = []([[maybe_unused]] int&&, float)
      {
        return 42;
      };
      return std::same_as<std::invoke_result_t<decltype(l), int&&, float>,
                          applied_t<std::invoke_result, decltype(l), std::tuple<int&&>, float>>;
    }());

  // Test 4: is_invocable trait
  static_assert(
    []
    {
      constexpr auto l = [](int, float) {};
      return std::is_invocable_v<decltype(l), int, float> ==
             applied_v<std::is_invocable, decltype(l), std::tuple<int>, float>;
    }());

  // Test 5: Non-invocable case
  static_assert(
    []
    {
      constexpr auto l = [](int, float) {};
      // Use a non-convertible type, e.g., struct
      struct x
      {};
      return std::is_invocable_v<decltype(l), int, x> ==
             applied_v<std::is_invocable, decltype(l), std::tuple<int>, x>;
    }());
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

#undef FWD
