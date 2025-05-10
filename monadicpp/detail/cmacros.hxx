#pragma once

#define FHO_STRINGIZE(x) #x

#if defined(__clang__)
  #define FHO_OFF_ONE(warning) _Pragma(FHO_STRINGIZE(clang diagnostic ignored warning))
  /// @brief Pushes the current warning state while disabling one or more specified warnings.
  ///
  #define FHO_PUSH_OFF(...)    _Pragma("clang diagnostic push") FOR_EACH(FHO_OFF_ONE, __VA_ARGS__)
  /// @brief Restores the warning state to the one before the last FHO_PUSH_OFF.
  ///
  #define FHO_POP_OFF          _Pragma("clang diagnostic pop")
#elif defined(__GNUC__) || defined(__GNUG__)
  #define FHO_CONCAT_GCC_IGNORED(x) FHO_STRINGIZE(GCC diagnostic ignored x)
  #define FHO_OFF_ONE(warning)      _Pragma(FHO_CONCAT_GCC_IGNORED(warning))
  /// @brief Pushes the current warning state while disabling one or more specified warnings.
  ///
  #define FHO_PUSH_OFF(...)         _Pragma("GCC diagnostic push") FOR_EACH(FHO_OFF_ONE, __VA_ARGS__)
  /// @brief Restores the warning state to the one before the last FHO_PUSH_OFF.
  ///
  #define FHO_POP_OFF               _Pragma("GCC diagnostic pop")
#elif defined(_MSC_VER)
  #define FHO_OFF_ONE(warning) __pragma(warning(disable : warning))
  /// @brief Pushes the current warning state while disabling one or more specified warnings.
  ///
  #define FHO_PUSH_OFF(...)    __pragma(warning(push)) FOR_EACH(FHO_OFF_ONE, __VA_ARGS__)
  /// @brief Restores the warning state to the one before the last FHO_PUSH_OFF.
  ///
  #define FHO_POP_OFF          __pragma(warning(pop))
#else
  #define FHO_OFF_ONE(warning)
  #define FHO_PUSH_OFF(...)
  #define FHO_POP_OFF
#endif

// Helper macro to iterate over variadic arguments
#define FOR_EACH_1(what, x)                what(x)
#define FOR_EACH_2(what, x, y)             what(x) what(y)
#define FOR_EACH_3(what, x, y, z)          what(x) what(y) what(z)
#define FOR_EACH_4(what, x, y, z, w)       what(x) what(y) what(z) what(w)
#define FOR_EACH_N(_4, _3, _2, _1, N, ...) FOR_EACH_##N
#define FOR_EACH(what, ...)                FOR_EACH_N(__VA_ARGS__, 4, 3, 2, 1)(what, __VA_ARGS__)

// Helper macros for platform-specific warnings
#if defined(__clang__)
  #define FHO_W_UNDEF_INLINE "-Wundefined-inline"
  #define FHO_W_UNUSED       "-Wunused"
#elif defined(__GNUC__) || defined(__GNUG__)
  #define FHO_W_UNDEF_INLINE "-Wmissing-declarations"
  #define FHO_W_UNUSED       "-Wunused-variable", "-Wunused-function"
#elif defined(_MSC_VER)
  #define FHO_W_UNDEF_INLINE "5246" // MSVC undefined inline
  #define FHO_W_UNUSED       "4100" // MSVC unused parameter
#else
  #define FHO_W_UNDEF_INLINE ""
  #define FHO_W_UNUSED       ""
#endif
