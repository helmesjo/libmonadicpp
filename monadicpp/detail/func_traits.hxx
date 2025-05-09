#pragma once

#include <functional>
#include <tuple>

namespace fho::detail
{
  template<typename T>
  struct function_signature;

  /// @breif Specialization for function pointers.
  template<typename R, typename... Args>
  struct function_signature<R (*)(Args...)>
  {
    using type = R(Args...);

    template<auto I>
    using arg_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arg_count = sizeof...(Args);
  };

  template<typename R, typename... Args>
  struct function_signature<R(Args...)>
  {
    using type = R(Args...);

    template<auto I>
    using arg_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arg_count = sizeof...(Args);
  };

  template<typename R, typename... Args>
  struct function_signature<R(Args...) const>
  {
    using type = R(Args...);

    template<auto I>
    using arg_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arg_count = sizeof...(Args);
  };

  /// @breif  Specialization for member functions (e.g., `operator()` in functors).
  template<typename Class, typename R, typename... Args>
  struct function_signature<R (Class::*)(Args...)>
  {
    using type = R(Args...);

    template<auto I>
    using arg_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arg_count = sizeof...(Args);
  };

  /// @breif  Specialization for member functions (e.g., `operator()` in functors).
  template<typename Class, typename R, typename... Args>
  struct function_signature<R (Class::*)(Args...) const>
  {
    using type = R(Args...) const;

    template<auto I>
    using arg_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arg_count = sizeof...(Args);
  };

  /// @breif  Specialization for `std::function`.
  template<typename R, typename... Args>
  struct function_signature<std::function<R(Args...)>>
  {
    using type = R(Args...);

    template<auto I>
    using arg_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arg_count = sizeof...(Args);
  };

  /// @breif  Specialization for callables (lambdas, functors, ...).
  template<typename T>
  struct function_signature
  {
    /// @breif  Extract `operator()` signature.
    using type = typename function_signature<decltype(&T::operator())>::type;

    template<auto I>
    using arg_type_t =
      typename function_signature<decltype(&T::operator())>::template arg_type_t<I>;

    static constexpr auto arg_count = function_signature<decltype(&T::operator())>::arg_count;
  };

  /// @breif Type trait to extract a types function signature.
  template<typename T>
  using function_signature_t = typename function_signature<T>::type;

  template<typename T, auto I>
  using function_arg_type_t = typename function_signature<T>::template arg_type_t<I>;

  template<typename T>
  static constexpr auto function_arg_count =
    function_signature<std::remove_reference_t<T>>::arg_count;

  /// @brief TEST: Function Signature
  static_assert(
    []
    {
      using func_t = int (*)(int, float const&, double);
      return std::same_as<int(int, float const&, double), function_signature_t<func_t>>;
    }());

  static_assert(
    []
    {
      using func_t = decltype([](int, float const&, double) -> int{ return 1; });
      return std::same_as<int(int, float const&, double) const, function_signature_t<func_t>>;
    }());

  static_assert(
    []
    {
      using func_t = std::function<int(int, float const&, double)>;
      return std::same_as<int(int, float const&, double), function_signature_t<func_t>>;
    }());

  static_assert(
    []
    {
      using func_t = function_signature_t<int (*)(int, float const&, double)>;
      // TODO: Figure out why both of these work
      return std::same_as<int(int, float const&, double), function_signature_t<func_t>>;
    }());

  /// @brief TEST: Argument type at index `I`
  static_assert(
    []
    {
      using func_t = int (*)(int, float const&, double);
      return std::same_as<int, function_arg_type_t<func_t, 0>> &&
             std::same_as<float const&, function_arg_type_t<func_t, 1>> &&
             std::same_as<double, function_arg_type_t<func_t, 2>>;
    }());

  static_assert(
    []
    {
      using func_t = decltype([](int, float const&, double) -> int{ return 1; });
      return std::same_as<int, function_arg_type_t<func_t, 0>> &&
             std::same_as<float const&, function_arg_type_t<func_t, 1>> &&
             std::same_as<double, function_arg_type_t<func_t, 2>>;
    }());

  static_assert(
    []
    {
      using func_t = std::function<int(int, float const&, double)>;
      return std::same_as<int, function_arg_type_t<func_t, 0>> &&
             std::same_as<float const&, function_arg_type_t<func_t, 1>> &&
             std::same_as<double, function_arg_type_t<func_t, 2>>;
    }());

  /// @brief TEST: Argument count
  static_assert(1 == function_arg_count<int (*)(int)>);
  static_assert(2 == function_arg_count<int (*)(int, float)>);
  static_assert(3 == function_arg_count<int (*)(int, float, double)>);

  static_assert(1 == function_arg_count<decltype([](int) -> int{ return 1; })>);
  static_assert(2 == function_arg_count<decltype([](int, float) -> int{ return 1; })>);
  static_assert(3 == function_arg_count<decltype([](int, float, double) -> int{ return 1; })>);

  static_assert(1 == function_arg_count<std::function<int(int)>>);
  static_assert(2 == function_arg_count<std::function<int(int, float)>>);
  static_assert(3 == function_arg_count<std::function<int(int, float, double)>>);

  // @brief Type used in `pairwise` matcher to always equal true.
  struct match_any
  {};

  // @brief Recursively applies `Trait` to pack types from `0` to `min(sizeof...(LPack),
  // sizeof...(RPack))`. Type `match_any` in either `LPack[I]` and/or `RPack[I]` is considered a
  // match.
  template<template<typename, typename> typename Trait, typename LPack, typename RPack,
           size_t I = 0, size_t N = std::min(std::tuple_size_v<LPack>, std::tuple_size_v<RPack>)>
  constexpr auto
  check_pairwise_recursive() -> bool
  {
    if constexpr (I == N)
    {
      return true; // Base case: reached the end
    }
    else
    {
      using lhs_t = std::tuple_element_t<I, LPack>;
      using rhs_t = std::tuple_element_t<I, RPack>;
      if constexpr (std::same_as<match_any, lhs_t> || std::same_as<match_any, rhs_t>)
      {
        return check_pairwise_recursive<Trait, LPack, RPack, I + 1, N>();
      }
      else
      {
        return Trait<lhs_t, rhs_t>::value &&
               check_pairwise_recursive<Trait, LPack, RPack, I + 1, N>();
      }
    }
  }
}
