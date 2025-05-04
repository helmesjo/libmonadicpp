#pragma once

#include <functional>

namespace fho::detail
{
  template<typename T>
  struct function_signature;

  /// @breif Specialization for function pointers.
  template<typename R, typename... Args>
  struct function_signature<R (*)(Args...)>
  {
    using type = R(Args...);
  };

  /// @breif  Specialization for `std::function`.
  template<typename R, typename... Args>
  struct function_signature<std::function<R(Args...)>>
  {
    using type = R(Args...);
  };

  /// @breif  Specialization for member functions (e.g., `operator()` in functors).
  template<typename Class, typename R, typename... Args>
  struct function_signature<R (Class::*)(Args...) const>
  {
    using type = R(Args...);
  };

  /// @breif  Primary template for callables (lambdas, functors).
  template<typename T>
  struct function_signature
  {
    /// @breif  Extract `operator()` signature.
    using type = typename function_signature<decltype(&T::operator())>::type;
  };

  /// @breif Type trait to extract a types function signature.
  template<typename T>
  using function_signature_t = typename function_signature<T>::type;
}
