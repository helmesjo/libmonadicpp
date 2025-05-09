#pragma once

#include <monadicpp/concepts.hxx>
#include <monadicpp/detail/func_traits.hxx>

#include <concepts>
#include <type_traits>
#include <utility>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

namespace fho
{
  /// @brief Creates a curried version of a function, enabling partial application.
  ///
  /// @tparam F The type of the function to curry.
  /// @tparam Args Variadic template for arguments to partially apply.
  /// @param f The function to curry, forwarded as an rvalue reference.
  /// @param args Arguments to bind, forwarded as rvalue references.
  /// @return Either a zero-parameter callable wrapping the result of invoking `f` with `args` if
  /// invocable, otherwise a _partially applied_ non-zero parameter callable for further
  /// application.
  ///
  /// @details This function implements (perfectly forwarded) currying, a technique rooted in
  /// applicative programming, notably inspired by Haskell. In Haskell, functions are curried by
  /// default — e.g., a function `add :: Int -> Int -> Int` is inherently a function that takes one
  /// `Int` and returns a function awaiting another `Int`. This C++ implementation mimics that
  /// behavior using templates and lambdas.
  ///
  /// The process works as follows:
  /// 1. It checks if `f` is invocable with the provided `args` using `std::invocable`. If true, it
  /// returns a zero-parameter callable wrapping the invocation `f(args...)`, forwarded with
  /// `std::forward` (aliased as `FWD` for brevity) to preserve their value category.
  /// 2. If `f` cannot be invoked yet (i.e., more arguments are needed), it captures `FWD(f)` and
  /// `FWD(args)` in a lambda that accepts additional arguments (`As...`) and recursively calls
  /// `curry`, using `std::forward_like` to maintain the value category relative to the lambda's
  /// `Self` type.
  /// 3. The (curried) lambda requires at least one argument, no more than the remaining
  /// arguments expected by `f`, and that the passed arguments are convertible to the remaining
  /// arguments expected by `f` (respectivelly), enforcing the currying contract.
  /// 4. This recursive application continues until enough arguments are provided to invoke `f`,
  /// mirroring Haskell's automatic currying. The use of `constexpr` ensures compile-time evaluation
  /// where possible, and `decltype(auto)` preserves the exact return type.
  ///
  /// This approach enables applicative-style programming in C++, allowing functions to be partially
  /// applied and composed, much like Haskell's functional paradigm.
  template<typename F, typename... Args>
  constexpr auto
  curry(F&& f, Args&&... args) -> decltype(auto)
  {
    /// 1.
    if constexpr (std::invocable<F, Args...>)
    {
      return [f        = FWD(f),
              ... args = FWD(args)]<typename Self, typename... As>(this Self&&) -> decltype(auto)
      {
        return std::forward_like<Self>(f)(std::forward_like<Self>(args)...);
      };
    }
    /// 2.
    else
    {
      static_assert(detail::function_arg_count<F> > 0);
      return [f        = FWD(f),
              ... args = FWD(args)]<typename Self, typename... As>(this Self&&,
                                                                   As&&... as) -> decltype(auto)
             /// 3. Aligns comparison with `detail::match_any` (always true).
               requires (sizeof...(As) > 0 && (sizeof...(As) <= detail::function_arg_count<F>)) &&
                        pairwise<std::is_convertible, std::tuple<Args...>,
                                 std::tuple<detail::match_any, As...>>

      {
        /// 4.
        return curry(std::forward_like<Self>(f), std::forward_like<Self>(args)...,
                     std::forward<As>(as)...);
      };
    }
  }

  namespace detail
  {
    template<typename F, typename... Args>
    using curry_t =
      std::remove_cvref_t<decltype(curry(std::declval<F>(), std::declval<Args>()...))>;
  }

  /// @brief TEST: Direct & indirect (full) invocation.
  static_assert(
    []
    {
      constexpr auto add = [](int x, int y)
      {
        return x + y;
      };
      constexpr auto direct      = curry(add, 5, 10);
      constexpr auto add_curried = curry(add);
      constexpr auto indirect    = add_curried(1, 2);

      return direct() == 15 && indirect() == 3;
    }(),
    "constexpr evaluation of curry() failed");

  /// @brief TEST: Ensure `constexpr` evaluation.
  static_assert(
    []
    {
      constexpr auto add = [](int x, int y)
      {
        return x + y;
      };
      constexpr auto add_5  = curry(add, 5);
      constexpr int  result = add_5(10)();
      return result == 15;
    }(),
    "constexpr evaluation of curry() failed");

  /// @brief TEST: Verify value category forwarding (lvalue).
  static_assert(
    []
    {
      constexpr auto add = [](int x, int y)
      {
        return x + y;
      };
      constexpr int  x     = 10;
      constexpr auto add_x = curry(add, x);
      return std::is_invocable_v<decltype(add_x), int> && add_x(20)() == 30;
    }(),
    "Value category forwarding for lvalue failed");

  /// @brief TEST: Verify value category forwarding (rvalue)
  static_assert(
    []
    {
      auto add = [](int x, int y)
      {
        return x + y;
      };
      constexpr auto add_rvalue = curry(add, 5);
      return std::is_invocable_v<decltype(add_rvalue), int> && add_rvalue(10)() == 15;
    }(),
    "Value category forwarding for rvalue failed");

  /// @brief TEST: Ensure partially applied function requires at least one argument
  static_assert(
    []
    {
      constexpr auto add = [](int x, int y)
      {
        return x + y;
      };
      constexpr auto partially_applied = curry(add, 5);
      return !std::invocable<decltype(partially_applied)>;
    }(),
    "Partially applied function should be invocable with at least one argument");

  /// @brief TEST: Ensure currying works with multiple arguments incrementally
  static_assert(
    []
    {
      constexpr auto three_args = [](int a, int b, int c)
      {
        return a + b + c;
      };
      constexpr auto curried_three_1 = curry(three_args, 1);
      constexpr auto curried_three_2 = curried_three_1(2);
      return curried_three_2(3)() == 6;
    }(),
    "Incremental currying failed");

  /// @brief TEST: Ensure type safety with complex types
  static_assert(
    []
    {
      struct not_an_int
      {
        int value;
      };
      constexpr auto takes_not_an_int_and_int = [](not_an_int, int)
      {
        return 0;
      };
      constexpr auto curried_only_int = curry(takes_not_an_int_and_int, not_an_int{5});
      return std::invocable<decltype(curried_only_int), not_an_int> && curried_only_int(10)() == 0;
    }(),
    "Type safety with custom types failed");
}
