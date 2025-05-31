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
  /// @brief  `constexpr` replacement for `std::isalnum` + `_`
  constexpr bool
  is_idchar(char c) noexcept
  {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_';
  }

  /// @brief Cleans a raw type string by removing compiler-specific artifacts, standard library
  /// namespaces, and applying standard formatting.
  /// @tparam `N` The size of the output array, defaults to 256.
  /// @param input The raw type string to clean, typically from `__PRETTY__` (Clang/GCC) or
  /// `__FUNCSIG__` (MSVC).
  /// @return A `std::array<char, N>` containing the cleaned type string, null-terminated.
  /// @details
  /// Processes the input string by:
  /// - Removing MSVC-specific calling conventions (e.g., `__cdecl`, `__thiscall`).
  /// - Removing namespaces (e.g., `std`, `__cxx11`, `__gnu_cxx`), keep type names.
  /// - Removing type keywords (e.g., `class` & `struct`).
  /// - Skipping spaces except after type qualifiers (e.g., `const`, `volatile`).
  /// - Adding explicit spaces:
  ///   - Before top-level opening parenthesis '(' if not preceded by a space.
  ///   - After top-level closing parenthesis ')'.
  ///   - After commas ',' in parameter lists.
  /// - Preserving member function pointer syntax '(class_name::*)' without extra spaces.
  /// - Preserving spaces in qualified types (e.g., `const int&`) and after `class` or `struct`
  /// (e.g., `class std::basic_string<...>`).
  /// - Null-terminating the output array.
  /// The function is idempotent, ensuring repeated applications produce identical output.
  /// @example
  /// ```c++
  /// constexpr auto cleaned = clean_string_array("void(__cdecl&)(int,float)");
  /// static_assert(array_to_string_view(cleaned) == "void (&)(int, float)");
  /// constexpr auto qual_cleaned = clean_string_array("int const&");
  /// static_assert(array_to_string_view(qual_cleaned) == "int const&");
  /// constexpr auto cls_cleaned = clean_string_array("class std::string");
  /// static_assert(array_to_string_view(cls_cleaned) == "string");
  /// ```
  /// NOTE: N=256 is sufficient for most type strings; increase as needed for complex types.
  template<size_t N = 256>
  constexpr auto
  clean_string_array(std::string_view input) noexcept -> std::array<char, N>
  {
    constexpr std::string_view conventions[] = {"__cdecl", "__thiscall", "__stdcall", "__fastcall",
                                                "__vectorcall"};
    constexpr std::string_view qualifiers[]  = {"const", "volatile"};
    constexpr std::string_view keywords[]    = {"class", "struct"};

    auto   result  = std::array<char, N>{};
    size_t out_pos = 0;
    int    depth   = 0;
    size_t i       = 0;

    auto in_bounds = [&]()
    {
      return i < input.size() && out_pos < N;
    };

    while (in_bounds())
    {
      // Step 1: Skip calling conventions
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

      // Step 2: Skip namespace prefixes, keep last identifier if followed by ::*
      if (!in_bounds())
      {
        break;
      }
      size_t start_i       = i;
      size_t id_start      = i;
      size_t id_end        = i;
      bool   has_namespace = false;
      while (i < input.size() && out_pos < N)
      {
        id_start = i;
        // Skip identifier
        while (i < input.size() && is_idchar(input[i]))
        {
          i++;
        }
        id_end = i;
        // Check if followed by :: or ::*
        if (i + 1 < input.size() && input[i] == ':' && input[i + 1] == ':')
        {
          if (i + 2 < input.size() && !is_idchar(input[i + 2]))
          {
            // It's ::*, so keep this identifier
            break;
          }
          else
          {
            // It's ::, continue skipping
            i += 2;
            has_namespace = true;
          }
        }
        else
        {
          break;
        }
      }

      if (has_namespace)
      {
        // Write only the last identifier
        for (size_t j = id_start; j < id_end && out_pos < N - 1; j++)
        {
          result[out_pos++] = input[j];
        }
        // Continue processing from i
      }
      else
      {
        i = start_i; // No namespace, restore i
      }

      // Step 3: Skip class/struct keywords followed by space
      if (!in_bounds())
      {
        break;
      }
      for (auto kw : keywords)
      {
        if (input.substr(i).starts_with(kw) && i + kw.size() < input.size() &&
            input[i + kw.size()] == ' ')
        {
          i += kw.size() + 1;
          skipped = true;
          break;
        }
      }
      if (skipped)
      {
        continue;
      }

      // Step 4: Handle qualifiers followed by space
      if (!in_bounds())
      {
        break;
      }
      bool is_qualifier = false;
      for (auto qual : qualifiers)
      {
        if (input.substr(i).starts_with(qual) && i + qual.size() < input.size() &&
            input[i + qual.size()] == ' ')
        {
          for (size_t j = 0; j < qual.size() && out_pos < N - 1; ++j)
          {
            result[out_pos++] = input[i + j];
          }
          if (out_pos < N - 1)
          {
            result[out_pos++] = ' ';
          }
          i += qual.size() + 1;
          is_qualifier = true;
          break;
        }
      }
      if (is_qualifier)
      {
        continue;
      }

      // Step 5: Skip other spaces
      if (!in_bounds())
      {
        break;
      }
      if (input[i] == ' ')
      {
        i++;
        continue;
      }

      char c = input[i];
      if (c == '(')
      {
        // Step 6: Add space before opening parenthesis if top-level
        if (!in_bounds())
        {
          break;
        }
        if (depth == 0 && out_pos > 0 && result[out_pos - 1] != ' ')
        {
          result[out_pos++] = ' ';
        }
        result[out_pos++] = '(';
        depth++;
      }
      else if (c == ')')
      {
        // Step 7: Add closing parenthesis and space if top-level
        if (!in_bounds())
        {
          break;
        }
        result[out_pos++] = ')';
        depth--;
        if (depth == 0 && out_pos < N - 1)
        {
          result[out_pos++] = ' ';
        }
      }
      else if (c == ',' && depth == 1)
      {
        // Step 8: Handle commas in parameter lists with space
        if (!in_bounds())
        {
          break;
        }
        result[out_pos++] = ',';
        if (out_pos < N - 1)
        {
          result[out_pos++] = ' ';
        }
      }
      else
      {
        // Step 9: Write other characters
        if (!in_bounds())
        {
          break;
        }
        result[out_pos++] = c;
      }
      i++;
    }

    // Step 10: Trim trailing space and/or truncate, then null-terminate
    while ((out_pos > 0 && result[out_pos - 1] == ' ') || out_pos >= N)
    {
      out_pos--;
    }
    result[out_pos] = '\0';
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
    // Find the function name 'type_str_raw' to locate the template parameter
    constexpr auto fname     = std::string_view{"type_str_raw<"};
    constexpr auto fname_pos = raw.find(fname);
    if constexpr (fname_pos != std::string_view::npos)
    {
      // Start of type T is after 'type_str_raw<'
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
  static_assert(type_str(&hello::foo) == "int (hello::*) (float, double)",
                "Non-Const Member signature");
  static_assert(type_str(&hello::bar) == "int (hello::*) (float, double) const",
                "Const Member Function signature");

  /// @brief TEST: Primitives
  static_assert(type_str<hello>() == "hello", "User type");
  static_assert(type_str<int>() == "int", "Fundamental type");
  static_assert(type_str<int const>() == "const int", "CV-qualified type");
  static_assert(type_str<int>() != type_str<float>(), "Distinct types");

  static_assert(type_str<int&>() == "int&", "Reference type");
  static_assert(type_str<int(float)>() == "int (float)", "Function type");
}
