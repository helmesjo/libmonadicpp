#pragma once

#include <cstddef>
#include <tuple>
#include <utility>

#define FWD(x) std::forward<decltype(x)>(x)

namespace fho::detail
{
  /// @brief Type trait to extract a subtuple from a tuple starting, at Offset with Count elements.
  /// @tparam Offset Starting index of the subtuple.
  /// @tparam Count Number of elements in the subtuple.
  /// @tparam Ts Types in the input tuple.
  template<size_t Offset, size_t Count, typename... Ts>
  struct tuple_subtypes;

  /// @brief Type trait to extract a subtuple from a tuple starting, at Offset with Count elements.
  /// @details Specialization for tuple input.
  /// @tparam Offset Starting index.
  /// @tparam Count Number of elements.
  /// @tparam Ts Types in the tuple.
  /// @note Static assert ensures Offset + Count does not exceed tuple size.
  template<size_t Offset, size_t Count, typename... Ts>
  struct tuple_subtypes<Offset, Count, std::tuple<Ts...>>
  {
    static_assert(Offset + Count <= sizeof...(Ts), "Offset + Count exceeds tuple size");

    /// @brief Helper to create subtuple type from index sequence.
    /// @tparam Is Index sequence for subtuple elements.
    template<size_t... Is>
    static constexpr auto make_subtuple(std::index_sequence<Is...>)
      -> std::tuple<std::tuple_element_t<Offset + Is, std::tuple<Ts...>>...>;

    /// @brief Helper to create subtuple from tuple.
    /// @tparam Is Index sequence for subtuple elements.
    /// @param outerTuple Input tuple.
    template<size_t... Is>
    static constexpr auto
    make_subtuple(std::index_sequence<Is...>, std::tuple<Ts...> const& outerTuple)
      -> std::tuple<std::tuple_element_t<Offset + Is, std::tuple<Ts...>>...>
    {
      return std::make_tuple(std::get<Offset + Is>(outerTuple)...);
    }

    /// @brief Resulting subtuple type.
    using type = decltype(make_subtuple(std::make_index_sequence<Count>{}));
  };

  /// @brief Alias for accessing subtuple type.
  /// @details Type trait to extract a subtuple from a tuple, starting at Offset with Count
  /// elements.
  /// @tparam Offset Starting index.
  /// @tparam Count Number of elements.
  /// @tparam T Input tuple type.
  template<size_t Offset, size_t Count, typename T>
  using subtuple_t = typename tuple_subtypes<Offset, Count, T>::type;

  /// @brief Creates a subtuple from a tuple.
  /// @tparam Offset Starting index.
  /// @tparam Count Number of elements.
  /// @tparam T Input tuple type.
  /// @param tp Input tuple.
  template<size_t Offset, size_t Count, typename T>
  constexpr auto
  make_subtuple(T const& tp)
  {
    return tuple_subtypes<Offset, Count, T>::make_subtuple(std::make_index_sequence<Count>{}, tp);
  }

  /// @brief Creates a subtuple from variadic arguments.
  /// @tparam Offset Starting index.
  /// @tparam Count Number of elements.
  /// @tparam Ts Input argument types.
  /// @param ts Input arguments.
  template<size_t Offset, size_t Count, typename... Ts>
  constexpr auto
  forward_as_subtuple(Ts&&... ts)
  {
    return make_subtuple<Offset, Count>(std::forward_as_tuple(FWD(ts)...));
  }
}

/// @brief Test suite for tuple_subtypes and related utilities.
/// @details Contains static assertions for various valid cases of subtuple extraction,
///          checking both the type trait and the make_subtuple function.
namespace fho::detail::tests
{
  namespace
  {
    // Define a user-defined type for testing
    struct my_struct
    {};

    /// @test Empty Tuple
    /// @brief Verifies that extracting a subtuple from an empty tuple results in an empty tuple.
    using EmptyTuple = std::tuple<>;
    static_assert(std::is_same_v<std::tuple<>, subtuple_t<0, 0, EmptyTuple>>,
                  "Empty subtuple from empty tuple");

    /// @test Single Element Tuple
    /// @brief Verifies subtuple extraction from a tuple with one element, including both empty and
    /// full subtuples.
    using SingleTuple = std::tuple<int>;
    static_assert(std::is_same_v<std::tuple<>, subtuple_t<0, 0, SingleTuple>>,
                  "Empty subtuple from single element tuple");
    static_assert(std::is_same_v<std::tuple<int>, subtuple_t<0, 1, SingleTuple>>,
                  "Full subtuple from single element tuple");

    /// @test Multiple Elements Tuple
    /// @brief Verifies subtuple extraction from a tuple with multiple elements for various offset
    /// and count combinations.
    using MultiTuple = std::tuple<int, double, char>;
    static_assert(std::is_same_v<std::tuple<int, double>, subtuple_t<0, 2, MultiTuple>>,
                  "First two elements");
    static_assert(std::is_same_v<std::tuple<double>, subtuple_t<1, 1, MultiTuple>>,
                  "Middle element");
    static_assert(std::is_same_v<std::tuple<char>, subtuple_t<2, 1, MultiTuple>>, "Last element");
    static_assert(std::is_same_v<std::tuple<int, double, char>, subtuple_t<0, 3, MultiTuple>>,
                  "All elements");
    static_assert(std::is_same_v<std::tuple<double, char>, subtuple_t<1, 2, MultiTuple>>,
                  "Last two elements");

    /// @test User-Defined Type
    /// @brief Ensures that subtuple extraction works correctly with user-defined types.
    using UserTuple = std::tuple<my_struct, int>;
    static_assert(std::is_same_v<std::tuple<my_struct>, subtuple_t<0, 1, UserTuple>>,
                  "User-defined type");

    /// @test CV-Qualified Types
    /// @brief Checks that const and volatile qualifiers are preserved in the subtuple.
    using CVTuple = std::tuple<int const, double volatile>;
    static_assert(std::is_same_v<std::tuple<int const, double volatile>, subtuple_t<0, 2, CVTuple>>,
                  "CV-qualified types");

    /// @test Reference Types
    /// @brief Verifies that reference types (lvalue and rvalue) are correctly handled in the
    /// subtuple.
    using RefTuple = std::tuple<int&, double&&>;
    static_assert(std::is_same_v<std::tuple<int&>, subtuple_t<0, 1, RefTuple>>, "Lvalue reference");
    static_assert(std::is_same_v<std::tuple<double&&>, subtuple_t<1, 1, RefTuple>>,
                  "Rvalue reference");

    /// @test make_subtuple Return Type
    /// @brief Confirms the return type of the make_subtuple function.
    static_assert(
      std::is_same_v<std::tuple<int>, decltype(make_subtuple<0, 1>(std::declval<MultiTuple>()))>,
      "make_subtuple return type");

    /// @test forward_as_subtuple Return Type
    /// @brief Confirms the return type of the forward_as_subtuple function.
    static_assert(
      std::is_same_v<std::tuple<int&&, double&&>,
                     decltype(forward_as_subtuple<0, 2>(std::declval<int>(), std::declval<double>(),
                                                        std::declval<char>()))>,
      "forward_as_subtuple return type");

    // Note: For invalid Offset and Count (e.g., Offset + Count > tuple size),
    // the static assert in the trait prevents compilation.
  }
}

#undef FWD
