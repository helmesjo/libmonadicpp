#pragma once

#include <monadicpp/detail/func_traits.hxx>
#include <monadicpp/detail/traits/applied_tuple.hxx>
#include <monadicpp/detail/traits/flatten.hxx>
#include <monadicpp/detail/traits/subtuple.hxx>
#include <monadicpp/detail/traits/tuple_cat.hxx>

#include <concepts>
#include <tuple>
#include <type_traits>
#include <utility>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

namespace fho::detail
{
  /// @brief Stores a sequence of computation signatures for monads.
  /// @details Used to track the types of computations in a monad’s signature chain.
  template<typename... Computations>
  struct monad_signature
  {};

  template<typename T, typename... Computations>
  using signature_t =
    typename detail::flatten_pack_t<monad_signature, T,
                                    decltype(partial<>::type(std::declval<Computations>()))...>;
}

/// @brief Specializations of `tuple_element_t` & `tuple_size_v` for `monad_signature`.
namespace std
{
  template<size_t I, class... Ts>
  struct tuple_element<I, fho::detail::monad_signature<Ts...>>
  {
    using type = tuple_element_t<I, tuple<Ts...>>;
  };

  template<class... Ts>
  struct tuple_size<fho::detail::monad_signature<Ts...>> : integral_constant<size_t, sizeof...(Ts)>
  {};
}

namespace fho::detail::tests::signature
{
  /// @test `monad_signature` tuple specializations
  static_assert(std::same_as<int(), std::tuple_element_t<0, monad_signature<int(), float()>>>);
  static_assert(std::same_as<float(), std::tuple_element_t<1, monad_signature<int(), float()>>>);
  static_assert(2 == std::tuple_size_v<monad_signature<int(), float()>>);
}

#undef FWD
