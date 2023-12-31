ersvg
=====

ersvg is an Erlang wrapper around the Rust libary resvg. It uses the binary resvg.

Rust
-----

First we need to compile the binary from resvg

```
mkdir -p priv/bin
git clone https://github.com/RazrFalcon/resvg.git
cd resvg
cargo build --release

cp target/release/resvg ../priv/bin
```

You can (optionally) then remove the resvg directory.

Erlang
-----

    $ rebar3 compile
    $ rebar3 eunit

License
-----

This implementation, like the `resvg` project, is licensed under the [MPLv2.0](https://www.mozilla.org/en-US/MPL/).
