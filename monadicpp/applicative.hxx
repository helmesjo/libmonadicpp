#pragma once

#include <monadicpp/concepts.hxx>
#include <monadicpp/traits.hxx>
#include <monadicpp/detail/func_traits.hxx>

#include <concepts>
#include <tuple>
#include <type_traits>
#include <utility>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

namespace fho
{
  namespace detail
  {
    template<typename Func, typename Bound, typename... Unbound>
    struct curried
    {
      using function_type  = Func;
      using argument_types = tuple_concat_t<Bound, Unbound...>;
      using unbound_types  = std::tuple<Unbound...>;
      using result_type =
        typename applied_t<std::invoke_result, Func, Bound, Unbound...>::type; // NOLINT
      using signature_type =
        typename applied_t<function_signature, result_type, Bound, Unbound...>::type;
      static constexpr auto arity       = std::tuple_size_v<argument_types>;
      static constexpr auto bound_arity = std::tuple_size_v<Bound>;
      static constexpr auto unbound_arity =
        (std::tuple_size_v<argument_types> - sizeof...(Unbound) - 1);

      constexpr explicit curried(function_type f)
        : func_(std::move(f))
      {}

      constexpr explicit curried(Func f, Bound args)
        : func_(std::move(f))
        , bound_(std::move(args))
      {}

      template<typename... _Bound> // NOLINT
      constexpr auto
      operator()(_Bound&&... args)
        requires (fho::pairwise<std::is_convertible, std::tuple<_Bound...>, unbound_types>)
      {
        return std::apply(
          [this, f = std::forward<Func>(func_),
           ... args = FWD(args)]<typename Self>(this Self&&, auto&&... bound)
          {
            if constexpr (std::invocable<Func, decltype(FWD(bound))..., _Bound...>)
            {
              return std::invoke(f, bound..., args...);
            }
            else
            {
              constexpr auto _bound_arity = bound_arity + sizeof...(_Bound); // NOLINT
              constexpr auto _remain      = arity - _bound_arity;            // NOLINT
              return apply(
                [this, f = std::forward_like<Self>(f),
                 ... args = std::forward_like<Self>(args)]<typename... Remain>(Remain&&...)
                {
                  return curried<Func, tuple_concat_t<Bound, _Bound...>, Remain...>(
                    std::forward_like<Self>(f),
                    tuple_concat(bound_, std::forward_like<Self>(args)...));
                },
                subtuple_t<_bound_arity, _remain, argument_types>{});
            }
          },
          bound_);
      }

    private:
      std::remove_reference_t<function_type> func_;
      Bound                                  bound_;
    };

    // template<typename Func, typename... Bound>
    // curried(Func&&, Bound&&...)-> typename detail::applied_t<curried, Func,
    // std::tuple<Bound...>>::type;

    // template<typename Func, typename... Args>
    // curried(Func&&, Args&&...)

    template<typename T>
    struct detect;

    // constexpr auto fyo = [](int, float, double)
    // {
    //   return 1;
    // };
    // using detect<
    //   typename detail::applied_t<curried, decltype(fyo), std::tuple<std::tuple<int>>>::type>
    //   sadasdsa;

    constexpr auto fyo = [](int a, float b, double c)
    {
      return a + b + c;
    };
    auto derp = curried<decltype(fyo), std::tuple<>, int, float, double>(fyo);
    // // using smerg = typename decltype(derp)::signature_type;
    // // detect<smerg> asdsad;
    constexpr auto wut  = derp(1, 2.0f, 3.0);
    constexpr auto wut1 = derp(1);
    constexpr auto wut2 = derp(1)(2);
    constexpr auto wut3 = derp(1)(2)(3);
    // detect<decltype(wut2)> asdsad;
  }

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
  /// 1. It checks if `f` is invocable with the provided `args`. If true, it invokes `f(args...)`.
  /// (`std::forward`, aliased as `FWD` for brevity, to preserve their value category).
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
      return std::invoke(FWD(f), FWD(args)...);
    }
    /// 2.
    else
    {
      static_assert(detail::arity<F> > 0);
      return [f        = FWD(f),
              ... args = FWD(args)]<typename Self, typename... As>(this Self&&,
                                                                   As&&... as) -> decltype(auto)
             /// 3. Aligns comparison with `detail::match_any` (always true).
               requires (sizeof...(As) > 0 && (sizeof...(As) <= detail::arity<F>)) &&
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

      return direct == 15 && indirect == 3;
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
      constexpr int  result = add_5(10);
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
      return std::is_invocable_v<decltype(add_x), int> && add_x(20) == 30;
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
      return std::is_invocable_v<decltype(add_rvalue), int> && add_rvalue(10) == 15;
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
      return curried_three_2(3) == 6;
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
      return std::invocable<decltype(curried_only_int), not_an_int> && curried_only_int(10) == 0;
    }(),
    "Type safety with custom types failed");
}
