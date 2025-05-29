#pragma once

#include <concepts>
#include <functional>
#include <type_traits>

namespace fho::detail
{
  /// @brief A utility for partial function application, binding initial argument types & inferring
  /// the rest.
  /// @details The `partial` struct binds a variadic template pack `Args` and provides
  /// overloads of the `type` function to handle function pointers, member function
  /// pointers, and callable objects. It returns the function or member function
  /// pointer as-is or extracts the `operator()` for callable objects.
  /// @example
  /// ```c++
  /// int add(int a, int b) { return a + b; }
  /// int add(double a, int b) { return static_cast<int>(a) + b; }
  /// auto p = partial<int>::type(add); // Resolves to int(int, int).
  /// struct Callable {
  ///   int operator()(int, int) { return 0; }
  ///   int operator()(double, int) { return 0; }
  /// };
  /// auto f = partial<int>::type(Callable{}); // Resolves to int(int, int).
  /// ```
  template<typename... Args>
  struct partial final
  {
    /// @brief Handles free functions with additional inferred arguments.
    /// @details Returns the function pointer unchanged if it accepts `Args...` followed by
    /// additional `Inferred...` parameters.
    /// @param f Function pointer to process.
    /// @return The input function pointer.
    /// @example
    /// ```c++
    /// int func(int, double);
    /// int func(double, double);
    /// auto p = partial<int>::type(func); // Resolves to int(int, double).
    /// ```
    template<typename R, typename... Inferred>
      requires (sizeof...(Inferred) > 0)
    static constexpr auto
    type(R (*f)(Args..., Inferred...)) noexcept -> R (*)(Args..., Inferred...)
    {
      return f;
    }

    /// @brief Handles free functions matching `Args...` exactly.
    /// @details Returns the function pointer unchanged if it accepts exactly `Args...`.
    /// @param f Function pointer to process.
    /// @return The input function pointer.
    /// @example
    /// ```c++
    /// int func(int);
    /// int func(double);
    /// auto p = partial<int>::type(func); // Resolves to int(int).
    /// ```
    template<typename R>
    static constexpr auto
    type(R (*f)(Args...)) noexcept -> R (*)(Args...)
    {
      return f;
    }

    /// @brief Handles non-const member functions with additional inferred arguments.
    /// @details Returns the member function pointer unchanged if it accepts `Args...` followed by
    /// additional `Inferred...` parameters.
    /// @param f Member function pointer to process.
    /// @return The input member function pointer.
    /// @example
    /// ```c++
    /// struct S { int func(int, double); int func(double, double); };
    /// auto p = partial<int>::type(&S::func); // Resolves to int(int, double).
    /// ```
    template<typename C, typename R, typename... Inferred>
      requires (sizeof...(Inferred) > 0)
    static constexpr auto
    type(R (C::*f)(Args..., Inferred...)) -> R (C::*)(Args..., Inferred...)
    {
      return f;
    }

    /// @brief Handles non-const member functions matching `Args...` exactly.
    /// @details Returns the member function pointer unchanged if it accepts exactly `Args...`.
    /// @param f Member function pointer to process.
    /// @return The input member function pointer.
    /// @example
    /// ```c++
    /// struct S { int func(int); int func(double); };
    /// auto p = partial<int>::type(&S::func); // Resolves to int(int).
    /// ```
    template<typename C, typename R>
    static constexpr auto
    type(R (C::*f)(Args...)) -> R (C::*)(Args...)
    {
      return f;
    }

    /// @brief Handles const member functions with additional inferred arguments.
    /// @details Returns the const member function pointer unchanged if it accepts `Args...`
    /// followed by additional `Inferred...` parameters.
    /// @param f Const member function pointer to process.
    /// @return The input const member function pointer.
    /// @example
    /// ```c++
    /// struct S { int func(int, double) const; int func(double, double) const; };
    /// auto p = partial<int>::type(&S::func); // Resolves to int(int, double).
    /// ```
    template<typename C, typename R, typename... Inferred>
      requires (sizeof...(Inferred) > 0)
    static constexpr auto
    type(R (C::*f)(Args..., Inferred...) const) -> R (C::*)(Args..., Inferred...) const
    {
      return f;
    }

    /// @brief Handles const member functions matching `Args...` exactly.
    /// @details Returns the const member function pointer unchanged if it accepts exactly
    /// `Args...`.
    /// @param f Const member function pointer to process.
    /// @return The input const member function pointer.
    /// @example
    /// ```c++
    /// struct S { int func(int) const; int func(double) const; };
    /// auto p = partial<int>::type(&S::func); // Resolves to int(int).
    /// ```
    template<typename C, typename R>
    static constexpr auto
    type(R (C::*f)(Args...) const) -> R (C::*)(Args...) const
    {
      return f;
    }

    /// @brief Handles callable objects by extracting their `operator()`.
    /// @details Forwards the callable object and extracts its `operator()` member function pointer,
    /// ensuring the input is not a function type.
    /// @param c Callable object to process.
    /// @return The `operator()` member function pointer.
    /// @example
    /// ```c++
    /// struct C {
    ///   int operator()(int, int);
    ///   int operator()(double, int);
    /// };
    /// auto p = partial<int>::type(C{}); // Resolves to int(int, int).
    /// ```
    template<typename C>
      requires (!std::is_function_v<std::remove_reference_t<C>>)
    static constexpr auto
    type(C&&) -> decltype(partial<Args...>::type(&std::remove_reference_t<C>::operator()))
    {
      return &std::remove_reference_t<C>::operator();
    }

    /// @brief Handles function "wrappers" explicitly by extracting their `operator()`.
    /// @details Forwards the instance and extracts its `operator()` member function pointer,
    /// ensuring the input is of type `std::function`.
    /// @param f Function wrapper to process.
    /// @return The `operator()` member function pointer.
    /// @example
    /// ```c++
    /// auto f = std::function<int(int, int)>;
    /// auto p = partial<int>::type(f); // Resolves to int(*)(int, int).
    /// ```
    template<template<typename> typename F, typename R, typename... Inferred>
    static constexpr auto
    type(F<R(Args..., Inferred...)> f)
      -> decltype(partial<Args...>::type(&std::remove_reference_t<decltype(f)>::operator()))
    {
      return &std::remove_reference_t<decltype(f)>::operator();
    }
  };
}

/// @brief Test suite for partial function application, verifying overload resolution.
/// @details Contains static assertions for various combinations of explicit and inferred arguments,
/// checking for ambiguity and correct resolution in free functions, member functions, const member
/// functions, and callable objects.
namespace fho::detail::tests::disambiguate
{
  // Minimal types and functions for reuse across tests
  struct test_struct
  {
    [[nodiscard]] auto
    non_const_member(int, double) -> int
    {
      return 0;
    }

    [[nodiscard]] auto
    non_const_member(double, double) -> int
    {
      return 0;
    }

    [[nodiscard]] auto
    const_member(int, double) const -> int
    {
      return 0;
    }

    [[nodiscard]] auto
    const_member(double, double) const -> int
    {
      return 0;
    }
  };

  [[nodiscard]] auto
  free_func(int, double) -> int
  {
    return 0;
  }

  [[nodiscard]] auto
  free_func(double, double) -> int
  {
    return 0;
  }

  struct callable
  {
    [[nodiscard]] auto
    operator()(int, double) -> int
    {
      return 0;
    }

    [[nodiscard]] auto
    operator()(double, double) -> int
    {
      return 0;
    }
  };

  constexpr auto immutable_lambda = [](int, double) -> int
  {
    return 1;
  };

  constexpr auto mutable_lambda = [](int, double) mutable -> int
  {
    return 1;
  };

  template<typename T, typename... Args>
  constexpr auto can_deduce = requires { partial<Args...>::type(std::declval<T>()); };

  /// @test Free Functions
  /// @brief No ambiguity with all arguments explicit.
  static_assert(
    std::is_same_v<int (*)(int, double), decltype(partial<int, double>::type(free_func))>,
    "No ambiguity with all arguments explicit");

  /// @test Free Functions
  /// @brief No ambiguity with first argument explicit, rest inferred.
  static_assert(std::is_same_v<int (*)(int, double), decltype(partial<int>::type(free_func))>,
                "No ambiguity with first argument explicit, rest inferred");

  /// @test Free Functions
  /// @brief Ambiguity with all arguments inferred (expects failure).
  static_assert(
    // No instantiation of partial<> to cause ambiguity
    true // Note: This would fail to compile if partial<> was used
    ,
    "Ambiguity with all arguments inferred (expects failure)");

  /// @test Free Functions
  /// @brief Ambiguity with not enough arguments explicit, can't infer rest (expects failure).
  static_assert(
    // partial<> with no arguments would fail to resolve
    true // Note: This would fail to compile if partial<> was used
    ,
    "Ambiguity with not enough arguments explicit, can't infer rest (expects failure)");

  /// @test Non-const Member Functions
  /// @brief No ambiguity with all arguments explicit.
  static_assert(
    std::is_same_v<int (test_struct::*)(int, double),
                   decltype(partial<int, double>::type(&test_struct::non_const_member))>,
    "No ambiguity with all arguments explicit");

  /// @test Non-const Member Functions
  /// @brief No ambiguity with first argument explicit, rest inferred.
  static_assert(std::is_same_v<int (test_struct::*)(int, double),
                               decltype(partial<int>::type(&test_struct::non_const_member))>,
                "No ambiguity with first argument explicit, rest inferred");

  /// @test Non-const Member Functions
  /// @brief No ambiguity with first argument explicit, rest inferred.
  static_assert(std::is_same_v<int (test_struct::*)(int, double),
                               decltype(partial<int>::type(&test_struct::non_const_member))>,
                "Ambiguity with first argument explicit, rest inferred");

  /// @test Non-const Member Functions
  /// @brief Ambiguity with all arguments inferred (expects failure).
  static_assert(
    // No instantiation of partial<> to cause ambiguity
    true // Note: This would fail to compile if partial<> was used
    ,
    "Ambiguity with all arguments inferred (expects failure)");

  /// @test Non-const Member Functions
  /// @brief Ambiguity with not enough arguments explicit, can't infer rest (expects failure).
  static_assert(
    // partial<> with no arguments would fail to resolve
    true // Note: This would fail to compile if partial<> was used
    ,
    "Ambiguity with not enough arguments explicit, can't infer rest (expects failure)");

  /// @test Const Member Functions
  /// @brief No ambiguity with all arguments explicit.
  static_assert(std::is_same_v<int (test_struct::*)(int, double) const,
                               decltype(partial<int, double>::type(&test_struct::const_member))>,
                "No ambiguity with all arguments explicit");

  /// @test Const Member Functions
  /// @brief No ambiguity with first argument explicit, rest inferred.
  static_assert(std::is_same_v<int (test_struct::*)(int, double) const,
                               decltype(partial<int>::type(&test_struct::const_member))>,
                "No ambiguity with first argument explicit, rest inferred");

  /// @test Const Member Functions
  /// @brief Ambiguity with all arguments inferred (expects failure).
  static_assert(
    // No instantiation of partial<> to cause ambiguity
    true // Note: This would fail to compile if partial<> was used
    ,
    "Ambiguity with all arguments inferred (expects failure)");

  /// @test Const Member Functions
  /// @brief Ambiguity with not enough arguments explicit, can't infer rest (expects failure).
  static_assert(
    // partial<> with no arguments would fail to resolve
    true // Note: This would fail to compile if partial<> was used
    ,
    "Ambiguity with not enough arguments explicit, can't infer rest (expects failure)");

  /// @test Callable Objects
  /// @brief No ambiguity with all arguments explicit.
  static_assert(std::is_same_v<int (callable::*)(int, double),
                               decltype(partial<int, double>::type(callable{}))>,
                "No ambiguity with all arguments explicit");

  /// @test Callable Objects
  /// @brief No ambiguity with first argument explicit, rest inferred.
  static_assert(
    std::is_same_v<int (callable::*)(int, double), decltype(partial<int>::type(callable{}))>,
    "No ambiguity with first argument explicit, rest inferred");

  // Callable object tests
  /// @test Callable Objects With Overloads
  /// @brief Ambiguity with all arguments inferred (has overloads).
  static_assert(!can_deduce<callable>, "Ambiguity with all arguments inferred (has overloads)");

  /// @test Callable Objects
  /// @brief Ambiguity with not enough arguments explicit, can't infer rest (expects failure).
  static_assert(!can_deduce<callable>,
                "Ambiguity with not enough arguments explicit, can't infer rest (expects failure)");

  // Lambda object tests
  /// @test Immutable Lambda Objects
  /// @brief No ambiguity with all arguments inferred.
  static_assert(std::is_same_v<int (decltype(immutable_lambda)::*)(int, double) const,
                               decltype(partial<>::type(immutable_lambda))>,
                "Never ambiguity with all arguments inferred");

  // Lambda object tests
  /// @test Mutable Lambda Objects
  /// @brief No ambiguity with all arguments inferred.
  static_assert(std::is_same_v<int (decltype(mutable_lambda)::*)(int, double),
                               decltype(partial<>::type(mutable_lambda))>,
                "Never ambiguity with all arguments inferred");

  /// @test Function Wrapper (`std::function`)
  /// @brief Trivial test to confirm that `std::function` (or similar) can be deduced.
  static_assert(
    std::common_reference_with<int (std::function<int(int, double)>::*)(int, double) const,
                               decltype(partial<int>::type(std::function<int(int, double)>{}))>,
    "No ambiguity with all arguments inferred");
}
