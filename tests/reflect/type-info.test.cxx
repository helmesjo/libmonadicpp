#include <monadicpp/detail/reflect.hxx>

#include <format>
#include <iostream>

auto
main() -> int
{
  using namespace fho::detail;
  using namespace fho::detail::test::reflect;
  using namespace std;

  std::cout << '\n';
  std::cout << '\n';
  std::cout << std::format("    free func ref: {}\n", type_str(foo));
  std::cout << std::format("    free func ptr: {}\n", type_str(&foo));
  std::cout << std::format("      member func: {}\n", type_str(&hello::foo));
  std::cout << std::format("const member func: {}\n", type_str(&hello::bar));
  std::cout << std::format("  member operator: {}\n", type_str(&hello::operator()));
  std::cout << '\n';
}
