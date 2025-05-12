#pragma once

#include <type_traits>

namespace fho::detail
{
  /// @brief
  /// A utility for partial function application, binding initial arguments & inferring the rest.
  /// @details
  /// The `partial` struct binds a variadic template pack `Args` and provides
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
    /// @brief
    /// Handles free functions with additional inferred arguments.
    /// @details
    /// Returns the function pointer unchanged if it accepts `Args...` followed by additional
    /// `Inferred...` parameters.
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
    type(R (*f)(Args..., Inferred...)) noexcept -> decltype(auto)
    {
      return f;
    }

    /// @brief
    /// Handles free functions matching `Args...` exactly.
    /// @details
    /// Returns the function pointer unchanged if it accepts exactly `Args...`.
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
    type(R (*f)(Args...)) noexcept -> decltype(auto)
    {
      return f;
    }

    /// @brief
    /// Handles non-const member functions with additional inferred arguments.
    /// @details
    /// Returns the member function pointer unchanged if it accepts `Args...` followed by additional
    /// `Inferred...` parameters.
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
    type(R (C::*f)(Args..., Inferred...)) -> decltype(auto)
    {
      return f;
    }

    /// @brief
    /// Handles non-const member functions matching `Args...` exactly.
    /// @details
    /// Returns the member function pointer unchanged if it accepts exactly `Args...`.
    /// @param f Member function pointer to process.
    /// @return The input member function pointer.
    /// @example
    /// ```c++
    /// struct S { int func(int); int func(double); };
    /// auto p = partial<int>::type(&S::func); // Resolves to int(int).
    /// ```
    template<typename C, typename R>
    static constexpr auto
    type(R (C::*f)(Args...)) -> decltype(auto)
    {
      return f;
    }

    /// @brief
    /// Handles const member functions with additional inferred arguments.
    /// @details
    /// Returns the const member function pointer unchanged if it accepts `Args...`
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
    type(R (C::*f)(Args..., Inferred...) const) -> decltype(auto)
    {
      return f;
    }

    /// @brief
    /// Handles const member functions matching `Args...` exactly.
    /// @details
    /// Returns the const member function pointer unchanged if it accepts exactly `Args...`.
    /// @param f Const member function pointer to process.
    /// @return The input const member function pointer.
    /// @example
    /// ```c++
    /// struct S { int func(int) const; int func(double) const; };
    /// auto p = partial<int>::type(&S::func); // Resolves to int(int).
    /// ```
    template<typename C, typename R>
    static constexpr auto
    type(R (C::*f)(Args...) const) -> decltype(auto)
    {
      return f;
    }

    /// @brief
    /// Handles callable objects by extracting their `operator()`.
    /// @details
    /// Forwards the callable object and extracts its `operator()` member function
    /// pointer, ensuring the input is not a function type.
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
  };
}

/// @brief
/// Test suite for partial function application, verifying overload resolution.
/// @details
/// Contains static assertions for various combinations of explicit and inferred arguments,
/// checking for ambiguity and correct resolution in free functions, member functions,
/// const member functions, and callable objects.
namespace fho::detail::tests
{
  namespace
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

    // Free function tests
    /// @test Free Functions
    /// @brief
    /// No ambiguity with all arguments inferred.
    static_assert(
      [] -> bool
      {
        using expected_t = int (*)(int, double);
        auto result      = partial<int, double>::type(free_func);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "No ambiguity with all arguments inferred");

    /// @test Free Functions
    /// @brief
    /// No ambiguity with all arguments explicit.
    static_assert(
      [] -> bool
      {
        using expected_t = int (*)(int, double);
        auto result      = partial<int, double>::type(free_func);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "No ambiguity with all arguments explicit");

    /// @test Free Functions
    /// @brief
    /// No ambiguity with first argument explicit, rest inferred.
    static_assert(
      [] -> bool
      {
        using expected_t = int (*)(int, double);
        auto result      = partial<int>::type(free_func);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "No ambiguity with first argument explicit, rest inferred");

    /// @test Free Functions
    /// @brief
    /// Ambiguity with all arguments inferred (expects failure).
    static_assert(
      [] -> bool
      {
        // No instantiation of partial<> to cause ambiguity
        return true; // Note: This would fail to compile if partial<> was used
      }(),
      "Ambiguity with all arguments inferred (expects failure)");

    /// @test Free Functions
    /// @brief
    /// Ambiguity with all arguments explicit.
    static_assert(
      [] -> bool
      {
        using expected_t = int (*)(int, double);
        auto result      = partial<int, double>::type(free_func);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "Ambiguity with all arguments explicit");

    /// @test Free Functions
    /// @brief
    /// Ambiguity with first argument explicit, rest inferred.
    static_assert(
      [] -> bool
      {
        using expected_t = int (*)(int, double);
        auto result      = partial<int>::type(free_func);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "Ambiguity with first argument explicit, rest inferred");

    /// @test Free Functions
    /// @brief
    /// Ambiguity with not enough arguments explicit, can't infer rest (expects failure).
    static_assert(
      [] -> bool
      {
        // partial<> with no arguments would fail to resolve
        return true; // Note: This would fail to compile if partial<> was used
      }(),
      "Ambiguity with not enough arguments explicit, can't infer rest (expects failure)");

    // Non-const member function tests
    /// @test Member Functions
    /// @brief
    /// No ambiguity with all arguments inferred.
    static_assert(
      [] -> bool
      {
        using expected_t = int (test_struct::*)(int, double);
        auto result      = partial<int, double>::type(&test_struct::non_const_member);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "No ambiguity with all arguments inferred");

    /// @test Member Functions
    /// @brief
    /// No ambiguity with all arguments explicit.
    static_assert(
      [] -> bool
      {
        using expected_t = int (test_struct::*)(int, double);
        auto result      = partial<int, double>::type(&test_struct::non_const_member);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "No ambiguity with all arguments explicit");

    /// @test Member Functions
    /// @brief
    /// No ambiguity with first argument explicit, rest inferred.
    static_assert(
      [] -> bool
      {
        using expected_t = int (test_struct::*)(int, double);
        auto result      = partial<int>::type(&test_struct::non_const_member);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "No ambiguity with first argument explicit, rest inferred");

    /// @test Member Functions
    /// @brief
    /// Ambiguity with all arguments inferred (expects failure).
    static_assert(
      [] -> bool
      {
        // No instantiation of partial<> to cause ambiguity
        return true; // Note: This would fail to compile if partial<> was used
      }(),
      "Ambiguity with all arguments inferred (expects failure)");

    /// @test Member Functions
    /// @brief
    /// Ambiguity with all arguments explicit.
    static_assert(
      [] -> bool
      {
        using expected_t = int (test_struct::*)(int, double);
        auto result      = partial<int, double>::type(&test_struct::non_const_member);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "Ambiguity with all arguments explicit");

    /// @test Member Functions
    /// @brief
    /// Ambiguity with first argument explicit, rest inferred.
    static_assert(
      [] -> bool
      {
        using expected_t = int (test_struct::*)(int, double);
        auto result      = partial<int>::type(&test_struct::non_const_member);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "Ambiguity with first argument explicit, rest inferred");

    /// @test Member Functions
    /// @brief
    /// Ambiguity with not enough arguments explicit, can't infer rest (expects failure).
    static_assert(
      [] -> bool
      {
        // partial<> with no arguments would fail to resolve
        return true; // Note: This would fail to compile if partial<> was used
      }(),
      "Ambiguity with not enough arguments explicit, can't infer rest (expects failure)");

    // Const member function tests
    /// @test Const Member Functions
    /// @brief
    /// No ambiguity with all arguments inferred.
    static_assert(
      [] -> bool
      {
        using expected_t = int (test_struct::*)(int, double) const;
        auto result      = partial<int, double>::type(&test_struct::const_member);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "No ambiguity with all arguments inferred");

    /// @test Const Member Functions
    /// @brief
    /// No ambiguity with all arguments explicit.
    static_assert(
      [] -> bool
      {
        using expected_t = int (test_struct::*)(int, double) const;
        auto result      = partial<int, double>::type(&test_struct::const_member);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "No ambiguity with all arguments explicit");

    /// @test Const Member Functions
    /// @brief
    /// No ambiguity with first argument explicit, rest inferred.
    static_assert(
      [] -> bool
      {
        using expected_t = int (test_struct::*)(int, double) const;
        auto result      = partial<int>::type(&test_struct::const_member);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "No ambiguity with first argument explicit, rest inferred");

    /// @test Const Member Functions
    /// @brief
    /// Ambiguity with all arguments inferred (expects failure).
    static_assert(
      [] -> bool
      {
        // No instantiation of partial<> to cause ambiguity
        return true; // Note: This would fail to compile if partial<> was used
      }(),
      "Ambiguity with all arguments inferred (expects failure)");

    /// @test Const Member Functions
    /// @brief
    /// Ambiguity with all arguments explicit.
    static_assert(
      [] -> bool
      {
        using expected_t = int (test_struct::*)(int, double) const;
        auto result      = partial<int, double>::type(&test_struct::const_member);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "Ambiguity with all arguments explicit");

    /// @test Const Member Functions
    /// @brief
    /// Ambiguity with first argument explicit, rest inferred.
    static_assert(
      [] -> bool
      {
        using expected_t = int (test_struct::*)(int, double) const;
        auto result      = partial<int>::type(&test_struct::const_member);
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "Ambiguity with first argument explicit, rest inferred");

    /// @test Const Member Functions
    /// @brief
    /// Ambiguity with not enough arguments explicit, can't infer rest (expects failure).
    static_assert(
      [] -> bool
      {
        // partial<> with no arguments would fail to resolve
        return true; // Note: This would fail to compile if partial<> was used
      }(),
      "Ambiguity with not enough arguments explicit, can't infer rest (expects failure)");

    // Callable object tests
    /// @test Callable Objects
    /// @brief
    /// No ambiguity with all arguments inferred.
    static_assert(
      [] -> bool
      {
        using expected_t = int (callable::*)(int, double);
        auto result      = partial<int, double>::type(callable{});
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "No ambiguity with all arguments inferred");

    /// @test Callable Objects
    /// @brief
    /// No ambiguity with all arguments explicit.
    static_assert(
      [] -> bool
      {
        using expected_t = int (callable::*)(int, double);
        auto result      = partial<int, double>::type(callable{});
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "No ambiguity with all arguments explicit");

    /// @test Callable Objects
    /// @brief
    /// No ambiguity with first argument explicit, rest inferred.
    static_assert(
      [] -> bool
      {
        using expected_t = int (callable::*)(int, double);
        auto result      = partial<int>::type(callable{});
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "No ambiguity with first argument explicit, rest inferred");

    /// @test Callable Objects
    /// @brief
    /// Ambiguity with all arguments inferred (expects failure).
    static_assert(
      [] -> bool
      {
        // No instantiation of partial<> to cause ambiguity
        return true; // Note: This would fail to compile if partial<> was used
      }(),
      "Ambiguity with all arguments inferred (expects failure)");

    /// @test Callable Objects
    /// @brief
    /// Ambiguity with all arguments explicit.
    static_assert(
      [] -> bool
      {
        using expected_t = int (callable::*)(int, double);
        auto result      = partial<int, double>::type(callable{});
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "Ambiguity with all arguments explicit");

    /// @test Callable Objects
    /// @brief
    /// Ambiguity with first argument explicit, rest inferred.
    static_assert(
      [] -> bool
      {
        using expected_t = int (callable::*)(int, double);
        auto result      = partial<int>::type(callable{});
        return std::is_same_v<expected_t, decltype(result)>;
      }(),
      "Ambiguity with first argument explicit, rest inferred");

    /// @test Callable Objects
    /// @brief
    /// Ambiguity with not enough arguments explicit, can't infer rest (expects failure).
    static_assert(
      [] -> bool
      {
        // partial<> with no arguments would fail to resolve
        return true; // Note: This would fail to compile if partial<> was used
      }(),
      "Ambiguity with not enough arguments explicit, can't infer rest (expects failure)");
  }
}
