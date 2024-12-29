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

`ECFG` という構造体も使えます(Extended Context Free Grammar)。

production の右辺にまたはを意味する `\\|`, 0個以上の繰り返しを意味する `\\{ \\}`, グルーピングを意味する `\\( \\)` という特殊文字を使えます。
`terminal` or `non_terminal` にこれらの特殊文字を含めることはできません。

```rust

let g = ECFG::new();

let efg_is_ell1 = g.is_ell1();

let is_nullable = g.calculate_nullable(&str);

let first_set = g.calculate_first_set(&str);

let follow_set = g.calculate_follow_set(&str);
```
