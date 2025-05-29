#include <cstddef>
#include <utility>

namespace fho::detail
{
  template<std::size_t I>
  struct defaulted
  {
    constexpr defaulted() noexcept = default;

    template<typename T>
    constexpr
    operator T() const noexcept
    {
      return {};
    }
  };

  template<typename T, std::size_t N = 16>
  constexpr auto
  probe_arg_count() noexcept -> decltype(auto)
  {
    return []<std::size_t... Is>(this auto&& self, std::index_sequence<Is...> is)
    {
      static constexpr auto i = sizeof...(Is);
      if constexpr (i > N)
      {
        static_assert(i > N, "No matching call operator found");
        return -1;
      }
      else if constexpr (requires { std::declval<T>()(defaulted<Is>{}...); })
      {
        return sizeof...(Is);
      }
      else
      {
        return self(std::make_index_sequence<i + 1>{});
      }
    }(std::make_index_sequence<0>{});
  }

  /// @breif Number of arguments for callable `T`.
  /// @details Probes the function by checking `std::invocable` with an increasing count of
  /// arguments (max `N = 16`).
  /// @returns The number of aruments of the (first) found invocation.
  template<typename T>
  static constexpr auto arity = probe_arg_count<T>();
}

namespace fho::detail::test::invocation
{
  struct non_default
  {
    non_default()      = delete;
    non_default(auto&) = delete;

    auto operator()(int) -> int;
    auto operator()(int, float) -> int;
  };

  // void foo();
  constexpr auto
  foo(int, float) -> int
  {
    return 1;
  }

  constexpr auto x = foo(defaulted<0>{}, defaulted<0>{});

  static_assert(probe_arg_count<decltype(foo)>() == arity<decltype(foo)>);

  /// @brief TEST: Argument count
  static_assert(1 == arity<non_default>);
  static_assert(2 == arity<decltype(foo)>);

  static_assert(1 == arity<int (*)(int)>);
  static_assert(2 == arity<int (*)(int, float)>);
  static_assert(3 == arity<int (*)(int, float, double)>);

  static_assert(1 == arity<decltype([](int) -> int{ return 1; })>);
  static_assert(2 == arity<decltype([](int, float) -> int{ return 1; })>);
  static_assert(3 == arity<decltype([](int, float, double) -> int{ return 1; })>);
}
