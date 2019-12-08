
## 関数呼び出し

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

関数の戻り値として返される場合は、
gccでは第１引数に暗黙的に戻し先が渡される。


//
