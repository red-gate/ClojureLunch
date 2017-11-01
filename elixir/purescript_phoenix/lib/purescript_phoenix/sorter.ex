defmodule Sorter do
    use GenServer

    # Example usage:
    # Sorter.start( Enum.map(1..1000000, fn _ -> Enum.random(1..1000000) end) )

    # clients should create us with Sorter.start([1,2,3,4,5])
    def start(mylist) do
        {_, pid} = GenServer.start(__MODULE__, {Enum.with_index(mylist), self()})
        pid
    end
    defp start(indexedList, overseer, op) do
      # start one of the internal 'worker' processes
      # state looks like { currentList, parent, overseer, status, operation }
      # status = :incomplete, :left or :right depending on whether we've
      # had any results reported back to us from our children
      # op = :left, :right or :report depending on what our parent is expecting of us
        {_, pid} = GenServer.start(__MODULE__, {indexedList, self(), overseer, :incomplete, op})
        pid
    end

    def init({indexedList,overseer}) when is_pid(overseer) and is_list(indexedList) do
      # start a process which will report back to this one when it's done
      send(overseer, {:update, indexedList})      
      start(indexedList, overseer, :report)
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
      
      _result = relabel(left ++ current)
               |> updateOverseer(overseer)
               |> notifyParent(parent, op)

      {:stop, :normal, s}
    end

    # We've not received our 'right' response so stay alive and wait for it
    def handle_cast({:left, left}, {current, parent, overseer,  _, op}) do
      
      result = relabel(left ++ current)
               |> updateOverseer(overseer)

      {:noreply, {result, parent, overseer,  :left, op}}
    end
    # If we've already received our 'left' response we can report back and die
    def handle_cast({:right, right}, {current, parent, overseer, :left, op}=s) do
      
      _result = relabel(current ++ right) 
               |> updateOverseer(overseer)
               |> notifyParent(parent, op)

      {:stop, :normal, s}
    end
    # We've not received our 'left' response so stay alive and wait for it
    def handle_cast({:right, right}, {current, parent, overseer, _, op}) do
      
      result = relabel(current ++ right) 
               |> updateOverseer(overseer)

      {:noreply, {result, parent, overseer, :right, op}}
    end
  
    def handle_cast({:report, indexedList}, parent) do
      IO.puts "All done sorting #{length indexedList} items\n#{Kernel.inspect values(indexedList)}"

      send(parent, {:sortfinished, values(indexedList)})
      {:stop, :normal, nil}
    end

    defp relabel(indexedList) do
      Enum.zip( values(indexedList), Enum.sort( indices(indexedList) ))      
    end

    defp updateOverseer(indexedList, overseer) do
      send(overseer, {:update, indexedList})
      indexedList
    end

    defp notifyParent(indexedList, parent, op) do
      GenServer.cast(parent, {op, indexedList})
      indexedList
    end

    defp values(indexedList) do
      Enum.map(indexedList, fn {v,_} -> v end)
    end

    defp indices(indexedList) do
      Enum.map(indexedList, fn {_,i} -> i end)
    end
end
