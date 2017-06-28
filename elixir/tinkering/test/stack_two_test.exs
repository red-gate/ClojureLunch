defmodule StackTwoTest do
  use ExUnit.Case
  doctest StackTwo

  # demonstrating how we could test the stack implementation
  # even though our convenience methods create a registered process

  test "All The Things" do

    # notice we specify StackTwo not __MODULE__ in the start_link call, here
    {:ok, pid} = GenServer.start_link(StackTwo, [], [])

    GenServer.cast(pid, {:push, :v1})

    v = GenServer.call(pid, :pop)
    assert v == :v1

    GenServer.cast(pid, {:push, :v2})
    GenServer.cast(pid, {:push, :v3})

    v = GenServer.call(pid, :peek)
    assert v == :v3
    v = GenServer.call(pid, :peek)
    assert v == :v3

    v = GenServer.call(pid, :pop)
    assert v == :v3

    v = GenServer.call(pid, :peek)
    assert v == :v2

  end

end
