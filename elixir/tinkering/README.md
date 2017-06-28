# Tinkering

A place where we can just tinker around with some elixir code

## 28/06/17 GenServer introduction

First we wrote a trivial 'stack' implementation using raw `receive` and
tail recursion. See `stack_one.ex`.
Then we wrote an implementation of the same thing but making use of the
[GenServer](https://hexdocs.pm/elixir/GenServer.html) behaviour. See `stack_two.ex`

There's a [Getting Started Guide](https://elixir-lang.org/getting-started/mix-otp/genserver.html) for GenServer.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `tinkering` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:tinkering, "~> 0.1.0"}]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/tinkering](https://hexdocs.pm/tinkering).
