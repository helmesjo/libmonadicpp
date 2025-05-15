#pragma once

#include <tuple>
#include <type_traits>

#define FWD(...) ::std::forward<decltype(__VA_ARGS__)>(__VA_ARGS__)

namespace fho::detail
{
  /// @brief Concatenates a tuple-like object with no additional arguments.
  /// @param t First tuple-like object.
  /// @return The forwarded tuple-like object.
  template<typename T>
  constexpr auto
  tuple_concat(T&& t)
  {
    return FWD(t);
  }

  /// @brief Concatenates a tuple-like object with additional arguments.
  /// @param t First tuple-like object.
  /// @param us Additional arguments to concatenate.
  /// @return A tuple containing elements of t followed by us.
  template<typename T, typename... Us>
  constexpr auto
  tuple_concat(T&& t, Us&&... us)
  {
    return std::tuple_cat(FWD(t), std::tuple(FWD(us)...));
  }

  /// @brief Type of concatenated tuple from Tuple and Types...
  /// @tparam Tuple First tuple-like type.
  /// @tparam Types Additional types to concatenate.
  template<typename Tuple, typename... Types>
  using tuple_concat_t = decltype(tuple_concat(std::declval<Tuple>(), std::declval<Types>()...));
}

namespace fho::detail::tests::tuple_cat
{
  struct my_struct
  {};

  /// @test Tuple with No Additional Types
  /// @brief Verifies tuple with no additional types returns same tuple.
  static_assert(std::is_same_v<tuple_concat_t<std::tuple<int, double>>, std::tuple<int, double>>,
                "Tuple with no additional types");

  /// @test Empty Tuple with No Additional Types
  /// @brief Verifies empty tuple with no additional types returns empty tuple.
  static_assert(std::is_same_v<tuple_concat_t<std::tuple<>>, std::tuple<>>,
                "Empty tuple with no additional types");

  /// @test Tuple with Additional Types
  /// @brief Verifies tuple concatenated with additional types includes all elements.
  static_assert(
    std::is_same_v<tuple_concat_t<std::tuple<int>, double, char>, std::tuple<int, double, char>>,
    "Tuple with additional types");

  /// @test Empty Tuple with Additional Types
  /// @brief Verifies empty tuple with additional types results in tuple of those types.
  static_assert(std::is_same_v<tuple_concat_t<std::tuple<>, int, double>, std::tuple<int, double>>,
                "Empty tuple with additional types");

  /// @test Concatenation with std::pair
  /// @brief Verifies concatenation using std::pair as tuple-like type.
  static_assert(
    std::is_same_v<tuple_concat_t<std::tuple<int, double>, char>, std::tuple<int, double, char>>,
    "Concatenation with std::pair");

  /// @test CV-Qualified Types
  /// @brief Checks const and volatile qualifiers are preserved.
  static_assert(std::is_same_v<tuple_concat_t<std::tuple<int const>, double volatile>,
                               std::tuple<int const, double>>,
                "CV-qualified types");

  /// @test Reference Types
  /// @brief Verifies reference types are correctly handled.
  static_assert(
    std::is_same_v<tuple_concat_t<std::tuple<int&>, double&&>, std::tuple<int&, double>>,
    "Reference types");

  /// @test tuple_concat Function Return Type
  /// @brief Confirms tuple_concat function return type.
  static_assert(std::is_same_v<decltype(tuple_concat(std::declval<std::tuple<int>>(),
                                                     std::declval<double>(), std::declval<char>())),
                               std::tuple<int, double, char>>,
                "tuple_concat return type");
}
