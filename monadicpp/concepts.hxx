#pragma once

#include <monadicpp/traits.hxx>
#include <monadicpp/detail/cmacros.hxx>
#include <monadicpp/detail/disambiguate.hxx>

#include <concepts>
#include <type_traits>
#include <utility>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

FHO_PUSH_OFF(FHO_W_UNUSED, FHO_W_UNDEF_INLINE)

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

    /// @brief A pure null function.
    /// @details Placeholder function that returns a default-constructed value of the input type.
    template<typename R, typename... Args>
    struct null_pure_func
    {
      constexpr auto
      operator()(Args...) const -> R
      {
        return {};
      }
    };

    /// @brief A null-type convertible to/from any other type.
    /// @details Placeholder type that returns a default-constructed value of the input type.
    struct null_any
    {
      /// @brief Allow conversion from any type to null_any
      template<typename T>
      constexpr null_any(T&&) noexcept // NOLINT
      {}

      constexpr null_any() noexcept = default;

      /// @brief Allow conversion from null_any to any type
      template<typename T>
      constexpr
      operator T() const noexcept
      {
        return {};
      }
    };

    /// @brief Type used in `pairwise` matcher to always equal true.
    struct match_any
    {};

    /// @brief Recursively applies `Trait` to pack types from `0` to
    /// `min(sizeof...(LPack), sizeof...(RPack))`.
    /// Type `match_any` in either `LPack[I]` and/or `RPack[I]` is considered a match.
    template<typename LPack, template<typename, typename> typename Trait, typename RPack,
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
          return check_pairwise_recursive<LPack, Trait, RPack, I + 1, N>();
        }
        else
        {
          return Trait<lhs_t, rhs_t>::value &&
                 check_pairwise_recursive<LPack, Trait, RPack, I + 1, N>();
        }
      }
    }
  }

  template<typename LPack, template<typename, typename> typename Concept, typename RPack,
           size_t I = 0>
  concept pairwise = detail::check_pairwise_recursive<LPack, Concept, RPack, I>();

  static_assert(pairwise<std::tuple<int, float>, std::is_same, std::tuple<int>>);
  static_assert(pairwise<std::tuple<int, float>, std::is_same, std::tuple<int, float>>);
  static_assert(!pairwise<std::tuple<float, float>, std::is_same, std::tuple<int>>);
  static_assert(pairwise<std::tuple<int, float>, std::is_convertible, std::tuple<float>>);
  static_assert(pairwise<std::tuple<float>, std::is_convertible, std::tuple<int, float>>);

  /// @brief Concept for monadic types.
  /// @details Ensures type `T` has a value type, supports map and bind operations, and provides a
  /// value extraction method.
  template<typename T>
  concept monadic =
    requires (T&& t) {
      typename std::remove_cvref_t<T>::value_type;
      t.fmap(
        std::declval<detail::null_pure_func<int, typename std::remove_cvref_t<T>::value_type>>());
      t.bind(std::declval<detail::null_monadic_func>());
      { t.value() } -> std::convertible_to<typename std::remove_cvref_t<T>::value_type>;
    };

  /// @brief Simple negation of `monadic` concept.
  template<typename T>
  concept not_monadic = !monadic<T>;

  /// @brief Concept for functor types.
  /// @details Checks if type `T` has a value type and supports map operation returning a monadic
  /// type.
  template<typename T>
  concept functor =
    requires (T&& t) {
      {
        FWD(t).fmap(std::declval<detail::null_pure_func<detail::null_any, detail::null_any>>())
      } -> monadic;
    };

  /// @brief Concept for pure functions.
  /// @details Ensures a function is invocable with `Args...` and does not return a monadic type.
  template<typename T, typename... Args>
  concept pure_func = requires (T const t) {
                        { t(std::declval<Args>()...) };
                      };

  /// @brief Concept for invocable types with specific return type.
  /// @details Checks if a function, when invoked, returns a type convertible to the specified
  ///          return type.
  template<typename T, typename R, typename... Args>
  concept invocable_r = requires (T&& t, Args&&... args) {
                          { std::invoke(FWD(t), FWD(args)...) } -> std::convertible_to<R>;
                        };

  template<typename F, typename M>
  concept mappable = pure_func<F, typename std::remove_cvref_t<M>::value_type>;

  template<typename F, typename M>
  concept bindable = requires (F&& f) {
                       {
                         FWD(f)(std::declval<typename std::remove_cvref_t<M>::value_type>())
                       } -> monadic;
                     };

  template<typename MF, typename M>
  concept applicative = functor<MF> && mappable<M, typename std::remove_cvref_t<MF>::value_type>;

  /// @brief Concept for invocable types considered first-class citizens.
  /// @details Checks if `T` is an un-ambiguous callable (single signature, no overload ambiguity).
  template<typename T>
  concept promoted = std::move_constructible<T> && requires (T&& t) {
                                                     { detail::partial<>::type(FWD(t)) };
                                                   };

  static_assert(monadic<detail::null_monad<int>>);
  static_assert(functor<detail::null_monad<int>>);
  static_assert(functor<detail::null_functor<int>>);
  static_assert(bindable<detail::null_monadic_func, detail::null_monad<int>>);
  static_assert(pure_func<detail::null_pure_func<int, int>, int>);
  static_assert(mappable<detail::null_pure_func<int, int>, detail::null_monad<int>>);
}

FHO_POP_OFF

#undef FWD
