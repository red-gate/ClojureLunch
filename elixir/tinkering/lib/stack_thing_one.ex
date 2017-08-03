defmodule StackThingOne do
  use GenServer
  import StackTwo

  def start_link() do
    GenServer.start_link(__MODULE__, nil, name: {:global, :stackthingone})
  end

  def init(state) do
    {:ok, {&peek/0, state}}
  end

  def go() do
    GenServer.call({:global, :stackthingone}, :go)
  end

  def handle_call(:go, _from, {function, state}) do
    myFunc()
    newstate = function.()
    {:reply, newstate, {function, newstate}}
  end

  def myFunc() do
    IO.puts "ooold"
  end

end
