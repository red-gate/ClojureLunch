# Tinkering

A place where we can just tinker around with some elixir code

## 28/06/17 GenServer introduction

First we wrote a trivial 'stack' implementation using raw `receive` and
tail recursion. See `stack_one.ex`.
Then we wrote an implementation of the same thing but making use of the
[GenServer](https://hexdocs.pm/elixir/GenServer.html) behaviour. See `stack_two.ex`

There's a [Getting Started Guide](https://elixir-lang.org/getting-started/mix-otp/genserver.html) for GenServer.
