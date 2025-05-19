#pragma once

#include <tuple>
#include <type_traits>

namespace fho::detail
{
  // Helper to concatenate tuples
  template<typename... Tuples>
  struct tuple_cat_helper;

  template<>
  struct tuple_cat_helper<>
  {
    using type = std::tuple<>;
  };

  template<typename First, typename... Rest>
  struct tuple_cat_helper<First, Rest...>
  {
    using type = decltype(std::tuple_cat(std::declval<First>(),
                                         std::declval<typename tuple_cat_helper<Rest...>::type>()));
  };

  // Convert tuple to user-specified signature
  template<template<typename...> class Pack, typename Tuple>
  struct tuple_to_pack;

  template<template<typename...> class Pack, typename... Ts>
  struct tuple_to_pack<Pack, std::tuple<Ts...>>
  {
    using type = Pack<Ts...>;
  };

  // Flatten a single type
  template<template<typename...> class Pack, typename T>
  struct flatten_one
  {
    using type = std::tuple<T>;
  };

  template<template<typename...> class Pack, typename... Ts>
  struct flatten_one<Pack, Pack<Ts...>>
  {
    using type = typename tuple_cat_helper<typename flatten_one<Pack, Ts>::type...>::type;
  };

  // Flatten all input types
  template<template<typename...> class Pack, typename... Ts>
  struct flatten_all
  {
    using type = typename tuple_cat_helper<typename flatten_one<Pack, Ts>::type...>::type;
  };

  /// @brief Flattens a pack/sequence of types, potentially containing nested packs, into a single
  /// pack containing all plain types in order.
  /// @details This type trait processes a variadic list of types, where each type may be a plain
  /// type or a nested instance of the specified `Pack` template. It recursively flattens all
  /// nested `Pack` instances and concatenates all plain types into a single `Pack` containing them
  /// in order.
  /// The implementation uses helper templates to handle the flattening and concatenation
  /// of types, ensuring that no nested `Pack` instances remain in the final result. It also
  /// correctly handles empty signatures and nested empty signatures by ignoring them.
  /// @tparam `Pack` The template template parameter specifying the signature type to use (e.g.,
  /// `monad_signature`, `std::tuple` etc).
  /// @tparam `Ts...` The variadic list of types to flatten, which can include plain types and
  /// nested `Pack` instances.
  template<template<typename...> class Pack, typename... Ts>
  using flatten_pack_t =
    typename tuple_to_pack<Pack, typename flatten_all<Pack, Ts...>::type>::type;
}

namespace fho::detail::tests::flatten
{
  template<typename...>
  struct pack;
  // Assume pack and flatten_pack_t definitions are provided
  /// @test Flatten Type Trait - No Arguments
  /// @brief Check that flatten_pack_t with no arguments resolves to pack
  static_assert(std::is_same_v<pack<>, flatten_pack_t<pack>>, "Flatten Type Trait - No Arguments");

  /// @test Flatten Type Trait - Single Plain Type
  /// @brief Check that flatten_pack_t with a single plain type resolves to pack with that
  /// type
  static_assert(std::is_same_v<pack<int>, flatten_pack_t<pack, int>>,
                "Flatten Type Trait - Single Plain Type");

  /// @test Flatten Type Trait - Multiple Plain Types
  /// @brief Check that flatten_pack_t with multiple plain types resolves to pack with
  /// those types
  static_assert(std::is_same_v<pack<int, char, float>, flatten_pack_t<pack, int, char, float>>,
                "Flatten Type Trait - Multiple Plain Types");

  /// @test Flatten Type Trait - Single Nested pack
  /// @brief Check that flatten_pack_t with a single nested pack resolves correctly
  static_assert(std::is_same_v<pack<int>, flatten_pack_t<pack, pack<int>>>,
                "Flatten Type Trait - Single Nested pack");

  /// @test Flatten Type Trait - Multiple Nested packs
  /// @brief Check that flatten_pack_t with multiple nested packs resolves correctly
  static_assert(std::is_same_v<pack<int, char>, flatten_pack_t<pack, pack<int>, pack<char>>>,
                "Flatten Type Trait - Multiple Nested packs");

  /// @test Flatten Type Trait - Mixed Plain and Nested
  /// @brief Check that flatten_pack_t with a mix of plain types and nested packs resolves
  /// correctly
  static_assert(
    std::is_same_v<pack<int, char, float>, flatten_pack_t<pack, int, pack<char>, float>>,
    "Flatten Type Trait - Mixed Plain and Nested");

  /// @test Flatten Type Trait - Deeper Nesting
  /// @brief Check that flatten_pack_t handles deeper nested packs correctly
  static_assert(
    std::is_same_v<pack<int, char, float>, flatten_pack_t<pack, pack<int, pack<char>>, float>>,
    "Flatten Type Trait - Deeper Nesting");

  /// @test Flatten Type Trait - With Empty packs
  /// @brief Check that empty packs are ignored during flattening
  static_assert(std::is_same_v<pack<int, float>, flatten_pack_t<pack, int, pack<>, float>>,
                "Flatten Type Trait - With Empty packs");

  /// @test Flatten Type Trait - All Empty packs
  /// @brief Check that multiple empty packs result in an empty pack
  static_assert(std::is_same_v<pack<>, flatten_pack_t<pack, pack<>, pack<>>>,
                "Flatten Type Trait - All Empty packs");

  /// @test Flatten Type Trait - Nested Empty packs
  /// @brief Check that nested empty packs still result in an empty pack
  static_assert(std::is_same_v<pack<>, flatten_pack_t<pack, pack<pack<>>>>,
                "Flatten Type Trait - Nested Empty packs");

  /// @test Flatten Type Trait - Ignoring Nested Empty Inside Non-Empty
  /// @brief Check that nested empty packs inside non-empty ones are ignored
  static_assert(std::is_same_v<pack<int, float>, flatten_pack_t<pack, pack<int, pack<>>, float>>,
                "Flatten Type Trait - Ignoring Nested Empty Inside Non-Empty");

  /// @test Flatten Type Trait - Mixed Levels With Empties
  /// @brief Check that flatten_pack_t correctly handles mixed levels of nesting with empty
  /// packs
  static_assert(std::is_same_v<pack<int, float>, flatten_pack_t<pack, int, pack<pack<>>, float>>,
                "Flatten Type Trait - Mixed Levels With Empties");
}
