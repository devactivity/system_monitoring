# system_monitoring

Belajar bikin system monitoring dengan dasar pemrograman C, Haskell, Erlang, Rust

## Usage

```bash
cd c_utils/
gcc -shared -o libdiskio.so -fPIC disk_io.c

cd .. && cd erlang_collector
erlc -o ebin apps/erlang_collector/src/*.erl

cd .. && cd haskell_analyzer
cabal build

cd .. && cd rust_core
cargo build -r

cd .. && cd tui
cargo build -r
```
