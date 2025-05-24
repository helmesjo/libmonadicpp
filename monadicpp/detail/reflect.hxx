#include <string_view>

#if defined(__GNUC__) || defined(__clang__)
  #define FHO_PRETTY_FUNC __PRETTY_FUNCTION__
#elif defined(_MSC_VER)
  #define FHO_PRETTY_FUNC __FUNCSIG__
#else
  #define FHO_PRETTY_FUNC __func__
#endif

namespace fho::detail
{
  /// @brief Extracts the name of type `T` as a string view using compiler-specific macros.
  /// @tparam `T` The type to get the name of.
  /// @return `A` std::string_view containing the type name.
  /// @details
  /// For GCC/Clang, extracts the type name from `__PRETTY_FUNCTION__` by finding `"T = "` and
  /// taking the substring until `';'` or `']'`. For MSVC, extracts the template parameter from
  /// `__FUNCSIG__` by finding the matching `'<'` and `'>'` around the type name, handling nested
  /// templates via bracket counting. Returns `"Type not found"` if extraction fails.
  /// @example
  /// ```c++
  ///
  /// static_assert(type_str<int>().find("int") != std::string_view::npos);
  /// static_assert(type_str<std::string>().find("string") != std::string_view::npos);
  ///
  /// ```
  template<typename T>
  constexpr auto
  type_str() -> std::string_view
  {
    constexpr std::string_view pretty(FHO_PRETTY_FUNC);
#if defined(__GNUC__) || defined(__clang__)
    constexpr std::string_view prefix("T = ");
    constexpr auto             prefix_pos = pretty.find(prefix);
    if constexpr (prefix_pos != std::string_view::npos)
    {
      constexpr auto start         = prefix_pos + prefix.size();
      constexpr auto semicolon_pos = pretty.find(';', start);
      if constexpr (semicolon_pos != std::string_view::npos)
      {
        return pretty.substr(start, semicolon_pos - start);
      }
      constexpr auto end_pos = pretty.find(']', start);
      if constexpr (end_pos != std::string_view::npos)
      {
        return pretty.substr(start, end_pos - start);
      }
    }
#elif defined(_MSC_VER)
    constexpr auto param_start = pretty.find('(');
    if (param_start != std::string_view::npos)
    {
      constexpr size_t end_pos = [&]()
      {
        for (size_t i = param_start; i > 0; --i)
        {
          if (pretty[i - 1] == '>')
          {
            return i - 1;
          }
        }
        return std::string_view::npos;
      }();
      if (end_pos != std::string_view::npos)
      {
        constexpr size_t start_pos = [&]()
        {
          int    count = 1;
          size_t pos   = end_pos - 1;
          while (pos > 0)
          {
            if (pretty[pos] == '<')
            {
              --count;
            }
            else if (pretty[pos] == '>')
            {
              ++count;
            }
            if (count == 0)
            {
              return pos + 1;
            }
            --pos;
          }
          return std::string_view::npos;
        }();
        if (start_pos != std::string_view::npos)
        {
          return pretty.substr(start_pos, end_pos - start_pos);
        }
      }
    }
#else
  #error "Unsupported compiler"
#endif
    return "Type not found";
  }

  /// @brief Returns a string view representing the signature of the type of the given object.
  /// @tparam `T` The type of the object (deduced).
  /// @param `obj` The object whose type signature is to be retrieved (unused).
  /// @return A `std::string_view` containing the type signature.
  /// @details
  /// Delegates to `type_str<T>()` to extract the type name of the deduced type `T`.
  /// The parameter `obj` is unused, serving only to deduce `T`.
  /// @example
  /// ```c++
  ///
  /// void foo(int, float) {}
  /// static_assert(signature_sv(foo).find("void(int, float)") != std::string_view::npos);
  ///
  /// ```
  template<typename T>
  constexpr auto
  type_str([[maybe_unused]] T&&) -> std::string_view
  {
    return type_str<T>();
  }
}

namespace fho::detail::test::reflect
{
  struct hello
  {
    auto
    operator()(float, double) -> int
    {
      return 1;
    }

    [[nodiscard]] auto
    foo(float, double) -> int
    {
      return 1;
    }

    [[nodiscard]] auto
    bar(float, double) const -> int
    {
      return 1;
    }
  };

  constexpr void
  foo(int, float)
  {}

  constexpr void
  bar(float)
  {}

  constexpr auto npos = std::string_view::npos;

  /// @brief TEST: Free Functions
  static_assert(type_str(foo).find("void (&)(int, float)") != npos,
                "Free Function reference signature");
  static_assert(type_str(&foo).find("void (*)(int, float)") != npos,
                "Free Function pointer signature");

  /// @brief TEST: Member Functions
  static_assert(type_str(&hello::foo).find("hello::*)(float, double)") != npos,
                "Non-Const Member signature");
  static_assert(type_str(&hello::bar).find("hello::*)(float, double) const") != npos,
                "Const Member Function signature");

  /// @brief TEST: Primitives
  static_assert(type_str<int>().find("int") != npos, "Fundamental type");
  static_assert(type_str<std::string>().find("string") != npos, "Class type");
  static_assert(type_str<int const>().find("int") != npos, "CV-qualified type");
  static_assert(type_str<int>() != type_str<float>(), "Distinct types");

#if defined(__clang__)
  static_assert(type_str<int&>().find("int &") != npos, "Reference type");
  static_assert(type_str<int(float)>().find("int (float)") != npos, "Function type");
#elif defined(__GNUC__)
  static_assert(type_str<int&>().find("int&") != npos, "Reference type");
  static_assert(type_str<int(float)>().find("int(float)") != npos, "Function type");
#endif
}
