# ll1_checker

与えられた文脈自由文法(CFG)が ll(1) であるかを判定するプログラムです。

## 使い方

### 実行可能プログラムとして

```rust
cargo run
```

でバイナリプログラムが起動し、プロンプト通りに CFG を入力すると、CFG が ll(1) かどうか判定します。

### クレートとして

`CFG` という構造体が使えます。

```rust
// CFG を生成
let g = CFG::new(...);

// non terminals が nullable かどうか計算したものを返す
let nullables = g.calculate_nullables();

// non terminals の first sets を計算したものを返す
let first_sets = g.calculate_first_sets();

// non terminals の follow sets を計算したものを返す
let follow_sets = g.calculate_follow_sets();

// productions の director sets を計算したものを返す
let director_sets = g.calculate_director_sets();

// ll(1) かどうか
let cfg_is_ll1 = g.is_ll1();
```
