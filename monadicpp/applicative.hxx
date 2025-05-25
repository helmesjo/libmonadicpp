#pragma once

#include <monadicpp/concepts.hxx>
#include <monadicpp/traits.hxx>
#include <monadicpp/detail/disambiguate.hxx>

#include <concepts>
#include <tuple>
#include <type_traits>
#include <utility>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

namespace fho
{
  /// @brief A curried function object for partial application of arguments.
  /// @tparam T The type of the callable object.
  /// @tparam Bound The type of the tuple holding bound arguments.
  /// @tparam Unbound The types of the remaining unbound arguments.
  template<promoted T, typename Bound, typename... Unbound>
  struct curried
  {
    /// @brief The types of the unbound arguments.
    using argument_types = std::tuple<Unbound...>;

    /// @brief The result type when the function is fully applied.
    using result_type = detail::applied_t<std::invoke_result, T, Bound, Unbound...>;

    /// @brief The function type for the applied function.
    using function_type = result_type(Unbound...);

    /// @brief The full signature type, including bound and unbound arguments.
    using full_signature_type =
      detail::applied_t<detail::function_signature, result_type, Bound, Unbound...>;

    /// @brief The number of unbound arguments.
    static constexpr auto arity = sizeof...(Unbound);

    /// @brief Constructs a curried object with the given callable and bound arguments.
    /// @param t The callable object.
    /// @param b The tuple of bound arguments.
    constexpr explicit curried(T t, Bound b)
      : obj_(std::move(t))
      , bound_(std::move(b))
    {}

    /// @brief Applies provided arguments to the curried function.
    /// @tparam Self The type of the curried object (deduced).
    /// @tparam Params The types of the provided arguments.
    /// @param self The curried object (lvalue or rvalue).
    /// @param args The arguments to apply.
    /// @return If all arguments are provided, the result of invoking the callable.
    ///         Otherwise, a new curried object with additional bound arguments.
    /// @details
    /// The call operator checks if the provided arguments, combined with the bound arguments,
    /// are sufficient to invoke the callable (saturation). Saturation is determined by
    /// `detail::applied_v<std::is_invocable, T, Bound, Params...>`, which unpacks the bound
    /// tuple and checks invocability with the provided arguments. If saturated, the operator:
    /// 1. Concatenates the bound arguments (`self.bound_`) with the provided arguments (`args`)
    ///    using `detail::tuple_concat`, preserving value categories via `std::forward_like`.
    /// 2. Uses `std::apply` to unpack the concatenated tuple and invoke the callable (`self.obj_`)
    ///    via `std::invoke`, forwarding arguments and the callable with correct value categories.
    ///    A static assert ensures invocability, guarding against library bugs.
    /// If not saturated, the operator:
    /// 1. Computes the remaining unbound argument types using `detail::subtuple_t`, extracting
    ///    a subtuple of `argument_types` starting at the number of provided arguments.
    /// 2. Concatenates the bound and provided arguments into a new bound tuple.
    /// 3. Uses `std::apply` to construct a new `curried` object with the updated bound tuple
    ///    and remaining unbound types, deduced from the subtuple.
    /// The implementation supports constexpr evaluation, perfect forwarding with
    /// `std::forward_like`, and enforces type convertibility via the `pairwise` constraint.
    /// @example
    /// ```c++
    /// int add(int a, int b) { return a + b; }
    /// auto curried_add = curried<decltype(add), std::tuple<>, int, int>(add, std::tuple<>());
    /// auto partial = curried_add(5); // curried<decltype(add), std::tuple<int>, int>
    /// auto result = partial(3); // result is 8
    /// ```
    template<typename Self, typename... Params>
      requires (sizeof...(Params) <= arity) &&
               pairwise<std::tuple<Params...>, std::is_convertible, std::tuple<Unbound...>>
    constexpr auto
    operator()([[maybe_unused]] this Self&& self, Params&&... args) -> decltype(auto)
    {
      constexpr auto saturated = detail::applied_v<std::is_invocable, T, Bound, Params...>;
      if constexpr (saturated)
      {
        return std::apply(
          [obj = std::forward_like<Self>(self.obj_)](auto&&... args) constexpr mutable
          {
            static_assert(std::invocable<T, decltype(args)...>,
                          "library bug (1000): Partial application saturated, but not invocable.");
            return std::invoke(std::forward_like<Self>(obj), FWD(args)...);
          },
          detail::tuple_concat(std::forward_like<Self>(self.bound_), FWD(args)...));
      }
      else
      {
        static_assert(
          arity >= sizeof...(Params),
          "library bug (1001): Partial application not saturated, but attempting to apply "
          "too many arguments.");
        using unbound_t =
          detail::subtuple_t<sizeof...(Params), arity - sizeof...(Params), argument_types>;
        auto bound = detail::tuple_concat(std::forward_like<Self>(self.bound_), FWD(args)...);
        return std::apply(
          [obj   = std::forward_like<Self>(self.obj_),
           bound = std::move(bound)]<typename... Remains>(Remains&&...) constexpr
          {
            return curried<T, decltype(bound), Remains...>(std::forward_like<Self>(obj),
                                                           std::move(bound));
          },
          unbound_t{});
      }
    }

  private:
    T     obj_;   ///< The callable object.
    Bound bound_; ///< The tuple of bound arguments.
  };

  /// @brief Promotes a callable into a first-class citizen with a fully resolved signature.
  /// @details Transforms a callable (function, functor, or member function pointer) into a
  /// first-class citizen by making it a value that can be passed, returned, or assigned and has a
  /// resolved function signature by disambiguating overloads using the provided argument types.
  /// @tparam `Args` Parameter pack of types used for overload resolution if the callable has
  /// ambiguous overloads.
  /// @tparam `C` Type of the callable to promote (e.g., function, functor, or member function
  /// pointer).
  /// @param `simpleton` The callable to transform into a first-class citizen.
  /// @return A new callable that is a first-class citizen with a fully resolved signature.
  /// @example
  /// ```c++
  /// auto add(int a, int b)   -> int { return a + b; }
  /// auto add(float a, int b) -> int { return a + b; }
  /// auto p = promote<int>(add); // disambiguate overloads, pick add(int,int)
  /// auto result = p(5, 6);      // add(5,6) -> 11
  /// ```
  template<typename... Args, std::move_constructible C>
  constexpr auto
  promote(C&& simpleton)
  {
    if constexpr (promoted<C>)
    {
      return FWD(simpleton);
    }
    else
    {
      auto vip = detail::partial<Args...>::type(simpleton);
      // unwrap the full signature (from tuple)
      return std::apply(
        [c   = FWD(simpleton),
         vip = std::move(vip)]<typename... Params>(Params&&...) constexpr mutable
        {
          // return callable with fully resolved signature.
          return [c   = std::move(c),
                  vip = std::move(vip)]<typename Self>(this Self&&,
                                                       Params&&... args) constexpr -> decltype(auto)
          {
            if constexpr (std::is_function_v<std::remove_reference_t<C>>)
            {
              return std::invoke(std::forward_like<Self>(vip), FWD(args)...);
            }
            else
            {
              return std::invoke(std::forward_like<Self>(vip), std::forward_like<Self>(c),
                                 FWD(args)...);
            }
          };
        },
        detail::argument_types_t<decltype(vip)>{});
    }
  }

  /// @brief Creates a curried version of a function, enabling partial application.
  ///
  /// @tparam `F` The type of the function to curry.
  /// @tparam `Args` Variadic template for arguments to partially apply.
  /// @param `f` The function to curry, forwarded as an rvalue reference.
  /// @param `args` Arguments to bind, forwarded as rvalue references.
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
  template<promoted F, typename... Args>
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
                        pairwise<std::tuple<Args...>, std::is_convertible,
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
      std::remove_reference_t<decltype(curry(std::declval<F>(), std::declval<Args>()...))>;
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
