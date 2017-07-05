# Tinkering

A place where we can just tinker around with some elixir code

## 28/06/17 GenServer introduction

First we wrote a trivial 'stack' implementation using raw `receive` and
tail recursion. See [stack_one.ex](lib/stack_one.ex).
Then we wrote an implementation of the same thing but making use of the
[GenServer](https://hexdocs.pm/elixir/GenServer.html) behaviour. See [stack_two.ex](lib/stack_two.ex)

There's a [Getting Started Guide](https://elixir-lang.org/getting-started/mix-otp/genserver.html) for GenServer.

## running stuff

`mix compile`

`mix test`

`iex -S mix` to load project into the Repl

`r StackOne` to recompile and reload `StackOne` module inside iex

## 05/07/17 Global registered process

We modified [stack_two.ex](lib/stack_two.ex) so that it now registers itself globally.

We then experimented with creating two Nodes and seeing how the stack behaved.

Create identified nodes by running iex with the `--sname` flag:

`iex --sname one -S mix`

Having two nodes running like this, you can link them by `Node.connect` in one of the repls
(_note that capitalisation is important, as we discovered to our cost!_).

`Node.connect :'two@DEV-SIMONH2'`

Behaviour is pretty much what you'd expect...

If the nodes are linked, there can only be one copy of the registered process running. Attempting to start a process in the other node fails with a 'process already running' message.

```
iex(two)> StackTwo.start_link
{:ok, #PID<0.93.0>}
iex(two)> Node.connect :'one@DEV-SIMONH2'
true

iex(one)> StackTwo.start_link
{:error, {:already_started, #PID<13750.93.0>}}
```

If the two nodes are disconnected, calls to the process result in a 'no such process' error on the node not hosting it. 

```
iex> Node.disconnect(:'two@DEV-SIMONH2')
true
iex> StackTwo.pop()
** (exit) exited in: GenServer.call({:global, :mystack}, :pop, 5000)
    ** (EXIT) no process: the process is not alive or there's no process currently associated with the given name, possi
bly because its application isn't started
    (elixir) lib/gen_server.ex:729: GenServer.call/3
```

If the nodes each have the process started before they are connected, upon connection one of those processes will be forcibly terminated. (We observed the process on the node initiating the connection being the one which was kept alive, but not sure about the underlying heuristic deciding which wins).

```
iex>
12:43:14.563 [info]  global: Name conflict terminating {:mystack, #PID<13619.107.0>}
```

You can view globally registered names using the `:global` erlang module:

```
iex> :global.registered_names
[:mystack]
```
