#pragma once

#include <monadicpp/traits.hxx>

#include <concepts>
#include <type_traits>
#include <utility>

namespace fho
{
  namespace detail
  {
    /// @brief A null monad for type T.
    /// @details Acts as a placeholder monad that supports map, bind, and value operations,
    /// returning default-constructed values.
    template<typename T>
    struct null_monad
    {
      using value_type = T;

      static constexpr auto pure(auto&&) -> null_monad;
      constexpr auto        fmap(auto&&) const -> null_monad;
      constexpr auto        bind(auto&&) const -> null_monad;

      [[nodiscard]] constexpr auto
      value() const -> value_type
      {
        return {};
      }
    };

    /// @brief A null functor for type T.
    /// @details Provides a placeholder functor that supports map, returning a null monad.
    template<typename T>
    struct null_functor
    {
      using value_type = T;
      constexpr auto fmap(auto&&) const -> null_monad<std::remove_cvref_t<T>>;
    };

    /// @brief A null monadic function.
    /// @details Placeholder function that takes any input and returns a null monad.
    struct null_monadic_func
    {
      template<typename T>
      constexpr auto
      operator()(T&&) const -> null_monad<std::remove_cvref_t<T>>
      {
        return {};
      }
    };

    /// @brief A null pure function.
    /// @details Placeholder function that returns a default-constructed value of the input type.
    struct null_pure_func
    {
      template<typename T>
      constexpr auto
      operator()(T&&) const -> std::remove_cvref_t<T>
      {
        return {};
      }
    };
  }

  template<template<typename, typename> typename Concept, typename LPack, typename RPack,
           size_t I = 0>
  concept pairwise = detail::check_pairwise_recursive<Concept, LPack, RPack, I>();

  static_assert(pairwise<std::is_same, std::tuple<int, float>, std::tuple<int>>);
  static_assert(pairwise<std::is_same, std::tuple<int, float>, std::tuple<int, float>>);
  static_assert(!pairwise<std::is_same, std::tuple<float, float>, std::tuple<int>>);
  static_assert(pairwise<std::is_convertible, std::tuple<int, float>, std::tuple<float>>);
  static_assert(pairwise<std::is_convertible, std::tuple<float>, std::tuple<int, float>>);

  /// @brief Concept for monadic types.
  /// @details Ensures a type has a value type, supports map and bind operations, and provides a
  /// value extraction method.
  template<typename T>
  concept monadic = requires (T t) {
                      typename std::remove_cvref_t<T>::value_type;
                      t.fmap(std::declval<detail::null_pure_func>());
                      t.bind(std::declval<detail::null_monadic_func>());
                      {
                        t.value()
                      } -> std::convertible_to<typename std::remove_cvref_t<T>::value_type>;
                    };

  /// @brief Simple negation of `monadic` concept.
  template<typename T>
  concept not_monadic = !monadic<T>;

  /// @brief Concept for functor types.
  /// @details Checks if a type supports map operation returning a monadic type.
  template<typename T>
  concept functor = requires (T t) {
                      { t.fmap(std::declval<detail::null_pure_func>()) } -> monadic;
                    };

  /// @concept chainable_func
  /// @brief Checks if a type is invocable and its result is either monadic or another callable.
  ///        Used for functions in monadic chaining (bind) or partial application (ap).
  template<typename T, typename... Args>
  concept chainable_func = requires {
                             { std::declval<T>()(std::declval<Args>()...) } -> monadic;
                           };

  /// @brief Concept for pure functions.
  /// @details Ensures a function is invocable with `Args...` and does not return a monadic type.
  template<typename T, typename... Args>
  concept pure_func =
    chainable_func<T, Args...> || requires {
                                    { std::declval<T const>()(std::declval<Args>()...) };
                                  };

  /// @brief Concept for invocable types with specific return type.
  /// @details Checks if a function, when invoked, returns a type convertible to the specified
  ///          return type.
  template<typename T, typename R, typename... Args>
  concept invocable_r = requires (T&& t, Args&&... args) {
                          { std::invoke(t, std::forward<Args>(args)...) } -> std::convertible_to<R>;
                        };

  template<typename F, typename M>
  concept mappable = functor<M> && pure_func<F, typename std::remove_cvref_t<M>::value_type>;

  template<typename F, typename M>
  concept bindable = monadic<M> && chainable_func<F, typename std::remove_cvref_t<M>::value_type>;

  template<typename MF, typename M>
  concept applicative = functor<MF> && mappable<M, typename std::remove_cvref_t<MF>::value_type>;

  static_assert(monadic<detail::null_monad<int>>);
  static_assert(functor<detail::null_monad<int>>);
  static_assert(functor<detail::null_functor<int>>);
  static_assert(chainable_func<detail::null_monadic_func, int>);
  static_assert(pure_func<detail::null_pure_func, int>);
  static_assert(mappable<detail::null_pure_func, detail::null_monad<int>>);
}
