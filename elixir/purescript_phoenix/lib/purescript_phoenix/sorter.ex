defmodule Sorter do
    use GenServer

    # Example usage:
    # Sorter.start( Enum.map(1..1000000, fn _ -> Enum.random(1..1000000) end) )

    # clients should create us with Sorter.start([1,2,3,4,5])
    # ... except that there's no sane interface to report success back to them yet ;)
    def start(mylist) do
        {_, pid} = GenServer.start(__MODULE__, {mylist, self()})
        pid
    end
    defp start(mylist, op) do
      # start one of the internal 'worker' processes
      # state looks like { currentList, parent, status, operation }
      # status = :incomplete, :left or :right depending on whether we've
      # had any results reported back to us from our children
      # op = :left, :right or :report depending on what our parent is expecting of us
        {_, pid} = GenServer.start(__MODULE__, {mylist,self(),:incomplete,op})
        pid
    end

    def init({mylist,parent}) when is_pid(parent) and is_list(mylist) do
      # start a process which will report back to this one when it's done
      start(mylist, :report)
      {:ok, parent} # don't need any state
    end
    # We have no work to do, call our parent back immediately and die.
    # We cast the op we were given (:left or :right) and the list we hold
    def init({[], parent, _, op}=state) do
      GenServer.cast(parent, {op, []})
      {:stop, :normal, state}
    end
    def init({[a], parent, _, op}=state) do
      GenServer.cast(parent, {op, [a]})
      {:stop, :normal, state}
    end
    # We have some work to do -
    # Keep hold of the pivot (first element) as our currentList
    # Then create two children for left and right parts
    # Stay alive waiting to handle our children's casts
    def init({[h|t], parent, status, op}) do
      left = Enum.filter(t, fn x -> x<=h end)
      right = Enum.filter(t, fn x -> x>h end)
      start(left, :left)
      start(right, :right)
      {:ok, {[h], parent, status, op}}
    end

    # client interface
    # none, really - just call .start() :)

    # server implementation
    # If we've already received our 'right' response we can report back and die
    def handle_cast({:left, left}, {current, parent, :right, op}=s) do
      GenServer.cast(parent, {op, left ++ current})
      {:stop, :normal, s}
    end
    # We've not received our 'right' response so stay alive and wait for it
    def handle_cast({:left, left}, {current, parent, _, op}) do
      {:noreply, {left ++ current, parent, :left, op}}
    end
    # If we've already received our 'left' response we can report back and die
    def handle_cast({:right, right}, {current, parent, :left, op}=s) do
      GenServer.cast(parent, {op, current ++ right})
      {:stop, :normal, s}
    end
    # We've not received our 'left' response so stay alive and wait for it
    def handle_cast({:right, right}, {current, parent, _, op}) do
      {:noreply, {current ++ right, parent, :right, op}}
    end

    def handle_cast({:report, lst}, parent) do
      IO.puts "All done sorting #{length lst} items\n#{Kernel.inspect lst}"

      send (parent, {:sortfinished, list})
      {:stop, :normal, nil}
    end

end
