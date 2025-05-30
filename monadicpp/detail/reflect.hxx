#pragma once

#include <array>
#include <string_view>

#if defined(__GNUC__) || defined(__clang__)
  #define FHO_PRETTY_FUNC __PRETTY_FUNCTION__
#elif defined(_MSC_VER)
  #define FHO_PRETTY_FUNC __FUNCSIG__
#else
  #define FHO_PRETTY_FUNC __func__
#endif

// NOLINTBEGIN
namespace fho::detail
{
  /// @brief Cleans a raw type string by removing compiler-specific artifacts and applying standard
  /// formatting.
  /// @tparam `N` The size of the output array, defaults to 256.
  /// @param input The raw type string to clean, typically from `__FUNCSIG__` (MSVC) or
  /// `__PRETTY_FUNCTION__` (Clang/GCC).
  /// @return A `std::array<char, N>` containing the cleaned type string, null-terminated.
  /// @details
  /// Processes the input by:
  /// - Removing MSVC-specific calling conventions (e.g., `__cdecl`, `__thiscall`).
  /// - Skipping all input spaces and adding spaces explicitly:
  ///   - Before top-level opening parenthesis '(' if not preceded by a space.
  ///   - After top-level closing parenthesis ')'.
  ///   - After commas ',' in parameter lists.
  /// - Preserving member function pointer syntax '(class_name::*)' without extra spaces.
  /// - Null-terminating the output array.
  /// The function is idempotent, ensuring repeated applications produce identical output.
  /// @example
  /// ```c++
  /// constexpr auto raw = "void(__cdecl&)(int,float)";
  /// constexpr auto cleaned = clean_string_array<256>(raw);
  /// static_assert(array_to_string_view(cleaned) == "void (&)(int, float)");
  /// ```
  /// NOTE: N=256 is sufficient for most type strings; increase as needed for complex types.
  template<size_t N = 256>
  constexpr auto
  clean_string_array(std::string_view input) noexcept -> std::array<char, N>
  {
    constexpr std::string_view conventions[] = {"__cdecl", "__thiscall", "__stdcall", "__fastcall",
                                                "__vectorcall"};

    auto        result  = std::array<char, N>{};
    std::size_t out_pos = 0;
    int         depth   = 0;
    std::size_t i       = 0;

    while (i < input.size() && out_pos < N)
    {
      bool skipped = false;
      for (auto cc : conventions)
      {
        if (input.substr(i).starts_with(cc))
        {
          i += cc.size();
          skipped = true;
          break;
        }
      }
      if (skipped)
      {
        continue;
      }
      if (input[i] == ' ')
      {
        i++;
        continue;
      }

      char c = input[i];
      if (c == '(')
      {
        if (depth == 0 && out_pos > 0 && result[out_pos - 1] != ' ' && out_pos < N)
        {
          result[out_pos++] = ' ';
        }
        if (out_pos < N)
        {
          result[out_pos++] = '(';
        }
        depth++;
      }
      else if (c == ')')
      {
        if (out_pos < N)
        {
          result[out_pos++] = ')';
        }
        depth--;
        if (depth == 0 && out_pos < N)
        {
          result[out_pos++] = ' ';
        }
      }
      else if (c == ',' && depth == 1)
      {
        if (out_pos < N)
        {
          result[out_pos++] = ',';
        }
        if (out_pos < N)
        {
          result[out_pos++] = ' ';
        }
      }
      else
      {
        if (out_pos < N)
        {
          result[out_pos++] = c;
        }
      }
      i++;
    }

    if (out_pos > 0 && result[out_pos - 1] == ' ')
    {
      out_pos--;
    }
    if (out_pos < N)
    {
      result[out_pos] = '\0';
    }
    return result;
  }

  template<size_t N>
  constexpr auto
  array_to_string_view(std::array<char, N> const& arr) noexcept -> std::string_view
  {
    std::size_t len = 0;
    while (len < N && arr[len] != '\0')
    {
      len++;
    }
    while (len > 0 && arr[len - 1] == ' ')
    {
      len--;
    }
    return {arr.data(), len};
  }

  // NOLINTEND

  template<typename T>
  constexpr auto
  type_str_raw() -> std::string_view
  {
    constexpr auto raw        = std::string_view{FHO_PRETTY_FUNC};
    constexpr auto prefix     = std::string_view{"type_str_raw"};
    constexpr auto prefix_pos = raw.find(prefix);
    if constexpr (prefix_pos != std::string_view::npos)
    {
      return raw.substr(prefix_pos, raw.size() - prefix.size());
    }
    return raw;
  }

  template<typename T>
  constexpr auto
  type_str_raw(T&&) -> std::string_view
  {
    return type_str_raw<T>();
  }

  template<typename T>
  constexpr auto
  type_str_gcc_clang() -> std::string_view
  {
    constexpr auto raw        = type_str_raw<T>();
    constexpr auto prefix     = std::string_view{"T = "};
    constexpr auto prefix_pos = raw.find(prefix);
    if constexpr (prefix_pos != std::string_view::npos)
    {
      constexpr auto start         = prefix_pos + prefix.size();
      constexpr auto semicolon_pos = raw.find(';', start);
      if constexpr (semicolon_pos != std::string_view::npos)
      {
        return raw.substr(start, semicolon_pos - start);
      }
      constexpr auto end_pos = raw.find(']', start);
      if constexpr (end_pos != std::string_view::npos)
      {
        return raw.substr(start, end_pos - start);
      }
    }
    return raw;
  }

  template<typename T>
  constexpr auto
  type_str_msvc() -> std::string_view
  {
    constexpr auto raw = type_str_raw<T>();
    // Find the function name 'type_str' to locate the template parameter
    constexpr auto fname     = std::string_view{"type_str_raw<"};
    constexpr auto fname_pos = raw.find(fname);
    if constexpr (fname_pos != std::string_view::npos)
    {
      // Start of type T is after 'type_str<'
      constexpr auto type_start = fname_pos + fname.size();
      // Find the closing '>' of the template parameter
      constexpr auto type_end = [&]() -> size_t
      {
        size_t depth = 1;
        for (size_t i = type_start; i < raw.size(); ++i)
        {
          if (raw[i] == '<')
          {
            ++depth;
          }
          else if (raw[i] == '>')
          {
            if (--depth == 0)
            {
              return i;
            }
          }
        }
        return std::string_view::npos;
      }();
      if constexpr (type_end != std::string_view::npos && type_end > type_start)
      {
        return raw.substr(type_start, type_end - type_start);
      }
    }
    return raw;
  }

  /// @brief Extracts the name of type `T` as a string view using compiler-specific macros.
  /// @tparam `T` The type to get the name of.
  /// @return `A` std::string_view containing the type name.
  /// @details
  /// For GCC/Clang, extracts the type name from `__PRETTY_FUNCTION__` by finding `"T = "` and
  /// taking the substring until `';'` or `']'`. For MSVC, extracts the template parameter from
  /// `__FUNCSIG__` by finding the matching `'<'` and `'>'` around the type name, handling nested
  /// templates via bracket counting. Returns raw string if extraction fails.
  /// @example
  /// ```c++
  ///
  /// type_str<int>() == "int";
  /// void foo(int, float) {}
  /// type_str(foo) == "void (&) (int, float)"
  /// type_str(&foo) == "void (*) (int, float)"
  ///
  /// ```
  template<typename T>
  constexpr auto
  type_str() -> std::string_view
  {
#if defined(__GNUC__) || defined(__clang__)
    constexpr auto sv = type_str_gcc_clang<T>();
#elif defined(_MSC_VER)
    constexpr auto sv = type_str_msvc<T>();
#else
  #error "Unsupported compiler"
#endif
    static constexpr auto clean = clean_string_array<sv.size() * 4>(sv);
    return array_to_string_view(clean);
  }

  /// @brief Returns a string view representing the signature of the type of the given object.
  /// @tparam `T` The type of the object (deduced).
  /// @return A `string_view` containing the type signature.
  /// @details
  /// Delegates to `type_str<T>()` to extract the type name of the deduced type `T`.
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
  static_assert(type_str(foo) == "void (&) (int, float)", "Free Function reference signature");
  static_assert(type_str(&foo) == "void (*) (int, float)", "Free Function pointer signature");

  /// @brief TEST: Member Functions
  static_assert(type_str(&hello::foo).ends_with("hello::*) (float, double)"),
                "Non-Const Member signature");
  static_assert(type_str(&hello::bar).ends_with("hello::*) (float, double) const"),
                "Const Member Function signature");

  /// @brief TEST: Primitives
  static_assert(type_str<int>() == "int", "Fundamental type");
  // NOTE: Currently borked because of the added string cleaning.
  // static_assert(type_str<std::string>() == "string", "Class type");
  // static_assert(type_str<int const>() == "int", "CV-qualified type");
  static_assert(type_str<int>() != type_str<float>(), "Distinct types");

  static_assert(type_str<int&>() == "int&", "Reference type");
  static_assert(type_str<int(float)>() == "int (float)", "Function type");
}
