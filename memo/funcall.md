
## 関数呼び出し

### 戻り値が構造体の場合

gccでは、構造体が16バイト以下の場合、レジスタで返される (%rax, %rdx にパックされる)

16バイトより大きい場合、
  %rdi に戻り値の格納先アドレスを入れてコールする。
  結果がメモリに書き込まれる。



### 引数をメモリに割り付ける処理

alloc_variable_registers:
  * 引数に param_index をセットする


put_args_to_stack:
  * 関数の頭、引数をストアするコードを生成
  * ここでオフセットもセットしている


例

```c
typedef struct {
  int a;
} X;

int sub(int a, X x, int b, int c, int d, int e, int f, int g) {
  return x.a;
}

int main() {
  X foo;
  foo.a = 33;
  return sub(foo);
}
```


SP : | retadr | g  | foo   |


引数リストに対応するデータを作成する

{
  bool stack?
  offset
}

  * レジスタ渡しする引数の数
  * スタックに積む用のフレームサイズ


  sp -= frame_size
  calc params



###

引数として渡す場合、gccではスタック上に置かれるっぽい
（７個以上の場合と同様に）。

//
