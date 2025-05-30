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
  std::cout << type_str<int&>() << "|\n";
  std::cout << type_str<int(float)>() << "|\n";
  std::cout << std::format("free func (raw)      : {}\n", type_str_raw(foo));
  std::cout << std::format("free func (ref)      : {}\n", type_str(foo));
  std::cout << std::format("free func (ptr)      : {}\n", type_str(&foo));
  std::cout << '\n';
  std::cout << std::format("member func (raw)    : {}\n", type_str_raw(&hello::foo));
  std::cout << std::format("member func          : {}\n", type_str(&hello::foo));
  std::cout << std::format("const member func    : {}\n", type_str(&hello::bar));
  std::cout << '\n';
  std::cout << std::format("member operator (raw): {}\n", type_str_raw(&hello::operator()));
  std::cout << std::format("member (operator)    : {}\n", type_str(&hello::operator()));
  std::cout << '\n';
}
