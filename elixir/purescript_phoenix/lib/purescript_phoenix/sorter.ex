defmodule Sorter do
    use GenServer

    # Example usage:
    # Sorter.start( Enum.map(1..1000000, fn _ -> Enum.random(1..1000000) end) )

    # clients should create us with Sorter.start([1,2,3,4,5])
    # ... except that there's no sane interface to report success back to them yet ;)
    def start(mylist) do
        {_, pid} = GenServer.start(__MODULE__, {Enum.with_index(mylist), self()})
        pid
    end
    defp start(mylist, overseer, op) do
      # start one of the internal 'worker' processes
      # state looks like { currentList, parent, status, operation }
      # status = :incomplete, :left or :right depending on whether we've
      # had any results reported back to us from our children
      # op = :left, :right or :report depending on what our parent is expecting of us
        {_, pid} = GenServer.start(__MODULE__, {mylist,self(),overseer,:incomplete,op})
        pid
    end

    def init({mylist,overseer}) when is_pid(overseer) and is_list(mylist) do
      # start a process which will report back to this one when it's done
      send(overseer, {:update, mylist})      
      start(mylist, overseer, :report)
      {:ok, overseer} # don't need any state
    end
    # We have no work to do, call our parent back immediately and die.
    # We cast the op we were given (:left or :right) and the list we hold
    def init({[], parent, _, _, op}=state) do
      GenServer.cast(parent, {op, []})
      {:stop, :normal, state}
    end
    def init({[a], parent, _, _, op}=state) do
      GenServer.cast(parent, {op, [a]})
      {:stop, :normal, state}
    end
    # We have some work to do -
    # Keep hold of the pivot (first element) as our currentList
    # Then create two children for left and right parts
    # Stay alive waiting to handle our children's casts
    def init({[h|t], parent, overseer, status, op}) do
      { head, _ } = h
      left = Enum.filter(t, fn {x, _} -> x <= head end)
      right = Enum.filter(t, fn {x, _} -> x > head end)
      start(left, overseer, :left)
      start(right, overseer, :right)
      {:ok, {[h], parent, overseer, status, op}}
    end

    # client interface
    # none, really - just call .start() :)

    # server implementation
    # If we've already received our 'right' response we can report back and die
    def handle_cast({:left, left}, {current, parent, overseer, :right, op}=s) do
      fullset = left ++ current      
      result = relabel(fullset)
      send(overseer, {:update, result})
      GenServer.cast(parent, {op, result})
      {:stop, :normal, s}
    end

    # We've not received our 'right' response so stay alive and wait for it
    def handle_cast({:left, left}, {current, parent, overseer,  _, op}) do
      {:noreply, {left ++ current, parent, overseer,  :left, op}}
    end
    # If we've already received our 'left' response we can report back and die
    def handle_cast({:right, right}, {current, parent, overseer, :left, op}=s) do
      fullset = current ++ right
      result = relabel(fullset)
      send(overseer, {:update, result})
      GenServer.cast(parent, {op, result})
      {:stop, :normal, s}
    end
    # We've not received our 'left' response so stay alive and wait for it
    def handle_cast({:right, right}, {current, parent, overseer, _, op}) do
      {:noreply, {current ++ right, parent, overseer, :right, op}}
    end
  
    def handle_cast({:report, lst}, parent) do
      IO.puts "All done sorting #{length lst} items\n#{Kernel.inspect lst}"

      send(parent, {:sortfinished, lst})
      {:stop, :normal, nil}
    end

    defp relabel(fullset) do
      indices = Enum.sort( Enum.map(fullset, fn {_,i} -> i end) )
      
      Enum.zip( Enum.map(fullset, fn {v,_} -> v end), indices)      
    end
end
