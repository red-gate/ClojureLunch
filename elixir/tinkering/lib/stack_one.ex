defmodule StackOne do

  #
  # a stack implementation without using GenServer
  #

  def start_link do
    spawn fn -> mainloop([]) end
  end

  # push/2, pop/1 and peek/1 are convenience functions for our consumers

  def push(p, value) do
    send p, {:push, value}
  end

  # a send/receive pair like this is really fragile and not recommended!
  def pop(p) do
    send p, {:pop, self()}
    receive do
      v -> v
    after 1_000 -> nil
    end
  end

  def peek(p) do
    send p, {:peek, self()}
    receive do
      v -> v
    after 1_000 -> nil
    end
  end

  # the body of our server
  def mainloop(stack) do
    receive do
      {:push, value} ->
        mainloop([value|stack])
      {:pop, pid} ->
        [h|t] = stack
        send pid, h
        mainloop(t)
      {:peek, pid} ->
        [h|_] = stack
        send pid, h
        mainloop(stack)
      _ ->
        nil #do not loop, therefore die
    end
  end

end
