defmodule StackTwo do
 use GenServer

  #
  # A stack implementation using the GenServer behaviour
  #

# These are 'client' functions. Essentially just conveniences for our callers.

@doc "Create a Tinkering process, and register it with name :mystack"
def start_link do
  GenServer.start_link(__MODULE__, [], name: :mystack)
end

@doc "push a value onto the registered Tinkering stack"
def push(value) do
  GenServer.cast(:mystack, {:push, value})
end

@doc "pop a value from the registered Tinkering stack"
def pop() do
  GenServer.call(:mystack, :pop)
end

@doc "peek at the top value of the registered Tinkering stack"
def peek() do
  GenServer.call(:mystack, :peek)
end

# These are the actual 'GenServer functions' implementing our behaviour

@doc "handle a :push cast"
def handle_cast({:push, value}, stack) do
  {:noreply, [value|stack]}
end

@doc "handle a :pop call"
def handle_call(:pop, _from, [x | xs]) do
  {:reply, x, xs}
end

@doc "handle a :peek call"
def handle_call(:peek, _from, [x|xs]) do
  {:reply, x, [x|xs]}
end
end
