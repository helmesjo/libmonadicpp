#pragma once

#include <tuple>
#include <type_traits>

namespace fho::detail
{
  /// @brief Applies a trait to unpacked tuple types and additional arguments.
  template<template<typename...> typename Trait, typename... Rest>
  struct applied_tuple
  {
    using type                  = typename Trait<Rest...>::type; // NOLINT
    static constexpr bool value = type::value;
  };

  template<template<typename...> typename Trait, typename Arg1, typename... Ts, typename... Rest>
  struct applied_tuple<Trait, Arg1, std::tuple<Ts...>, Rest...>
  {
    using type                  = typename Trait<Arg1, Ts..., Rest...>::type; // NOLINT
    static constexpr bool value = type::value;
  };

  template<template<typename...> typename Trait, typename... Ts>
  struct applied_tuple<Trait, std::tuple<Ts...>>
  {
    using type                  = typename Trait<Ts...>::type; // NOLINT
    static constexpr bool value = type::value;
  };

  /// @brief Applies `Tuple` & `Rest...` to `Trait`.
  template<template<typename...> typename Trait, typename... Rest>
  using applied_t = typename applied_tuple<Trait, Rest...>::type;

  template<template<typename...> typename Trait, typename... Rest>
  static constexpr auto applied_v = applied_t<Trait, Rest...>::value;
}

namespace fho::detail::tests::applied_tuple
{
  /// @test Empty Tuple with Arg1 (std::is_same)
  /// @brief Verifies std::is_same boolean result with empty tuple and initial argument.
  static_assert(applied_v<std::is_same, int, std::tuple<int>>, "Empty tuple with matching Arg1");
  static_assert(!applied_v<std::is_same, int, std::tuple<double>>,
                "Empty tuple with non-matching Arg1");

  /// @test Single Element Tuple with Arg1 (std::is_same)
  /// @brief Verifies std::is_same boolean result with single-element tuple and initial argument.
  static_assert(applied_v<std::is_same, int, std::tuple<int>>,
                "Single element tuple with matching Arg1");
  static_assert(!applied_v<std::is_same, int, std::tuple<double>>,
                "Single element tuple with non-matching Arg1");

  /// @test Multiple Element Tuple with Arg1 (std::is_same)
  /// @brief Verifies std::is_same boolean result with multiple-element tuple and initial argument.
  static_assert(applied_v<std::is_same, int, std::tuple<int>>,
                "Type & Multiple element tuple with matching types");
  static_assert(applied_v<std::is_same, std::tuple<int, int>>,
                "Multiple element tuple with matching types");
  static_assert(!applied_v<std::is_same, int, std::tuple<double>>,
                "Type & Multiple element tuple with non-matching types");
  static_assert(!applied_v<std::is_same, std::tuple<int, double>>,
                "Multiple element tuple with non-matching types");

  /// @test CV-Qualified Types (std::is_same)
  /// @brief Checks std::is_same boolean result preserves const and volatile qualifiers.
  static_assert(applied_v<std::is_same, int const, std::tuple<int const>>,
                "CV-qualified matching types");
  static_assert(!applied_v<std::is_same, int const, std::tuple<double volatile>>,
                "CV-qualified non-matching types");

  /// @test Reference Types (std::is_same)
  /// @brief Verifies std::is_same boolean result handles reference types.
  static_assert(applied_v<std::is_same, int&, std::tuple<int&>>, "Reference matching types");
  static_assert(!applied_v<std::is_same, int&, std::tuple<double&&>>,
                "Reference non-matching types");

  /// @test Callable with Empty Tuple (std::invoke_result)
  /// @brief Verifies std::invoke_result type with empty tuple and callable.
  static_assert(std::is_same_v<std::invoke_result_t<int (*)()>,
                               applied_t<std::invoke_result, int (*)(), std::tuple<>>>,
                "Empty tuple with callable type");

  /// @test Callable with Arguments (std::invoke_result)
  /// @brief Verifies std::invoke_result type with tuple and matching arguments.
  static_assert(std::is_same_v<int, applied_t<std::invoke_result, int (*)(int, float, double),
                                              std::tuple<int, float>, double>>,
                "Callable with matching arguments type");

  /// @test Reference Types
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

  /// @test Reference Types
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

  /// @test Void return type
  static_assert(
    []
    {
      constexpr auto l = [](int, float&) {};
      return std::same_as<std::invoke_result_t<decltype(l), int, float&>,
                          applied_t<std::invoke_result, decltype(l), std::tuple<int, float&>>>;
    }());

  /// @test Empty tuple
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

  /// @test Rvalue reference
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

  /// @test is_invocable trait
  static_assert(
    []
    {
      constexpr auto l = [](int, float) {};
      return std::is_invocable_v<decltype(l), int, float> ==
             applied_v<std::is_invocable, decltype(l), std::tuple<int>, float>;
    }());

  /// @test Non-invocable case
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
