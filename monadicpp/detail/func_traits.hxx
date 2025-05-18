#pragma once

#include <functional>
#include <tuple>

namespace fho::detail
{
  /// @brief Primary template.
  template<typename... T>
  struct function_signature;

  /// @breif Fallback specialization for plain signature.
  template<typename R, typename... Args>
  struct function_signature<R, Args...>
  {
    using type           = R(Args...);
    using argument_types = std::tuple<Args...>;

    template<auto I>
    using argument_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arity = sizeof...(Args);
  };

  /// @brief Specialization where `T` has expected aliases.
  template<typename T>
    requires requires {
               typename std::remove_reference_t<T>::function_type;
               typename std::remove_reference_t<T>::argument_types;
               typename std::remove_reference_t<T>::result_type;
               std::remove_reference_t<T>::arity;
             }
  struct function_signature<T>
  {
    using type           = typename std::remove_reference_t<T>::function_type;
    using argument_types = typename std::remove_reference_t<T>::argument_types;

    template<auto I>
    using argument_type_t = std::tuple_element_t<I, argument_types>;

    static constexpr auto arity = std::remove_reference_t<T>::arity;
  };

  /// @brief Specialization for callables (lambdas, functors with operator()).
  template<typename T>
    requires requires { &std::remove_reference_t<T>::operator(); } &&
             (!requires {
                 typename std::remove_reference_t<T>::function_type;
                 typename std::remove_reference_t<T>::argument_types;
                 typename std::remove_reference_t<T>::result_type;
                 std::remove_reference_t<T>::arity;
               })
  struct function_signature<T>
  {
    /// @breif  Extract `operator()` signature.
    using type           = typename function_signature<decltype(&T::operator())>::type;
    using argument_types = typename function_signature<decltype(&T::operator())>::argument_types;

    template<auto I>
    using argument_type_t =
      typename function_signature<decltype(&T::operator())>::template argument_type_t<I>;

    static constexpr auto arity = function_signature<decltype(&T::operator())>::arity;
  };

  /// @breif Specialization for function pointers.
  template<typename R, typename... Args>
  struct function_signature<R (*)(Args...)>
  {
    using type           = R(Args...);
    using argument_types = std::tuple<Args...>;

    template<auto I>
    using argument_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arity = sizeof...(Args);
  };

  /// @breif Specialization for function types.
  template<typename R, typename... Args>
  struct function_signature<R(Args...)>
  {
    using type           = R(Args...);
    using argument_types = std::tuple<Args...>;

    template<auto I>
    using argument_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arity = sizeof...(Args);
  };

  /// @breif Specialization for `const` function types.
  template<typename R, typename... Args>
  struct function_signature<R(Args...) const>
  {
    using type           = R(Args...);
    using argument_types = std::tuple<Args...>;

    template<auto I>
    using argument_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arity = sizeof...(Args);
  };

  /// @breif  Specialization for member functions (e.g., `operator()` in functors).
  template<typename Class, typename R, typename... Args>
  struct function_signature<R (Class::*)(Args...)>
  {
    using type           = R(Args...);
    using argument_types = std::tuple<Args...>;

    template<auto I>
    using argument_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arity = sizeof...(Args);
  };

  /// @breif  Specialization for `const` member functions (e.g., `operator()` in functors).
  template<typename Class, typename R, typename... Args>
  struct function_signature<R (Class::*)(Args...) const>
  {
    using type           = R(Args...) const;
    using argument_types = std::tuple<Args...>;

    template<auto I>
    using argument_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arity = sizeof...(Args);
  };

  /// @breif  Specialization for `std::function`.
  template<typename R, typename... Args>
  struct function_signature<std::function<R(Args...)>>
  {
    using type           = R(Args...);
    using argument_types = std::tuple<Args...>;

    template<auto I>
    using argument_type_t = std::tuple_element_t<I, std::tuple<Args...>>;

    static constexpr auto arity = sizeof...(Args);
  };

  /// @breif Type trait to extract `T` function signature.
  template<typename... Ts>
  using function_signature_t = typename function_signature<Ts...>::type;

  /// @breif Type trait to extract argument types for callable `T`.
  template<typename T>
  using argument_types_t = typename function_signature<T>::argument_types;

  /// @breif Type trait to extract argument type `I` for callable `T`.
  template<typename T, auto I>
  using argument_type_t = typename function_signature<T>::template argument_type_t<I>;

  /// @breif Number of arguments for callable `T`.
  template<typename T>
  static constexpr auto arity = function_signature<std::remove_reference_t<T>>::arity;

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
      return std::same_as<int(int, float const&, double), function_signature_t<func_t>>;
    }());

  /// @brief TEST: Argument pack
  static_assert(
    []
    {
      using func_t = int (*)(int, float const&, double);
      return std::same_as<std::tuple<int, float const&, double>, argument_types_t<func_t>>;
    }());

  static_assert(
    []
    {
      using func_t = decltype([](int, float const&, double) -> int{ return 1; });
      return std::same_as<std::tuple<int, float const&, double>, argument_types_t<func_t>>;
    }());

  static_assert(
    []
    {
      using func_t = std::function<int(int, float const&, double)>;
      return std::same_as<std::tuple<int, float const&, double>, argument_types_t<func_t>>;
    }());

  static_assert(
    []
    {
      using func_t = function_signature_t<int (*)(int, float const&, double)>;
      return std::same_as<std::tuple<int, float const&, double>, argument_types_t<func_t>>;
    }());

  /// @brief TEST: Argument type at index `I`
  static_assert(
    []
    {
      using func_t = int (*)(int, float const&, double);
      return std::same_as<int, argument_type_t<func_t, 0>> &&
             std::same_as<float const&, argument_type_t<func_t, 1>> &&
             std::same_as<double, argument_type_t<func_t, 2>>;
    }());

  static_assert(
    []
    {
      using func_t = decltype([](int, float const&, double) -> int{ return 1; });
      return std::same_as<int, argument_type_t<func_t, 0>> &&
             std::same_as<float const&, argument_type_t<func_t, 1>> &&
             std::same_as<double, argument_type_t<func_t, 2>>;
    }());

  static_assert(
    []
    {
      using func_t = std::function<int(int, float const&, double)>;
      return std::same_as<int, argument_type_t<func_t, 0>> &&
             std::same_as<float const&, argument_type_t<func_t, 1>> &&
             std::same_as<double, argument_type_t<func_t, 2>>;
    }());

  /// @brief TEST: Argument count
  static_assert(1 == arity<int (*)(int)>);
  static_assert(2 == arity<int (*)(int, float)>);
  static_assert(3 == arity<int (*)(int, float, double)>);

  static_assert(1 == arity<decltype([](int) -> int{ return 1; })>);
  static_assert(2 == arity<decltype([](int, float) -> int{ return 1; })>);
  static_assert(3 == arity<decltype([](int, float, double) -> int{ return 1; })>);

  static_assert(1 == arity<std::function<int(int)>>);
  static_assert(2 == arity<std::function<int(int, float)>>);
  static_assert(3 == arity<std::function<int(int, float, double)>>);
}
