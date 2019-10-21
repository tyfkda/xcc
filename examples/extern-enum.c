// extern enum だと、定義してない enum 型でもアクセスしなければ通る
// アクセスする場合にも、後から定義すればよい。

extern enum Foo foo;

enum Foo {
  _,
};

int main() {
  foo = 123;
  return foo;
}

enum Foo foo;
