
# Elixir

[Elixir](https://elixir-lang.org) is a modern language which compiles down to BEAM bytecode to run on the Erlang VM.

## Installation

[Installation docs](https://elixir-lang.org/install.html)

### Choco

Can be installed using `chocolatey`

https://chocolatey.org/packages/Elixir

`choco install elixir`

This will also install `Erlang`

## Basics

### Docs

[Getting started guides](https://elixir-lang.org/getting-started/introduction.html)

[Main Docs](https://elixir-lang.org/docs.html)

### Repl : IEX

`iex` is the Repl.

### Build tool : mix

Elixir comes with a bundled build tool called `mix`

[Mix docs](https://hexdocs.pm/mix/Mix.html)

Use `mix new myproject` to create a new project

[Intro](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)

### Package manager : hex

[Hex](https://hex.pm/) is the Erlang package manager.

Install it with `mix local.hex`

## Phoenix, the web framework

[Phoenix](http://www.phoenixframework.org/) is a web framework built with Elixir

Installation, copied from [the docs](http://www.phoenixframework.org/docs/installation)

install hex `mix local.hex`

install phoenix `mix archive.install https://github.com/phoenixframework/archives/raw/master/phoenix_new.ez`

By default Phoenix projects expect to use [Brunch](http://brunch.io/) for asset management, but you can create
new projects with `--no-brunch` flag to avoid this.

Also they assume you'll want a database and use [Ecto](https://github.com/elixir-ecto/ecto) to provide this. Again `--no-ecto` flag on project creation skips this.

_note: ecto support for SQL Server is a bit poor, and in flux at the time of writing. Postgres or Mysql are better supported_

Simple, skeleton project: `mix phoenix.new web --no-brunch --no-ecto`
