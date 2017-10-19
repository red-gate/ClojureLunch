defmodule Sorter do 
    use GenServer

    def start(mylist) do
        {:ok, pid} = GenServer.start({mylist,self()})
        pid
    end

    # client interface
    def doWork(pid) do
        GenServer.cast(pid,{:doyourwork})        
    end
    def workDone(pid, l) do
        GenServer.cast(pid,{:donework, l})        
    end

    # server implementation
    def handle_cast({:doyourwork}, {[], parent}=s) do
        Sorter.workDone(parent, [])
        GenServer.exit(self())
        {:noreply, nil}
    end    

    def handle_cast({:doyourwork}, {[a],parent}) do
        Sorter.workDone(parent, [a])
        GenServer.exit(self())
        {:noreply, nil}
    end

    def handle_cast({:doyourwork}, {l,parent}) do
        len = length(l)
        first = Enum.take(l, len/2)
        second = Enum.drop(l, len/2)
        firstSortedPid = Sorter.start(first)
        secondSortedPid = Sorter.start(second)
        Sorter.doWork(firstSortedPid)
        Sorter.doWork(secondSortedPid)
        {:noreply, {[], parent}}
    end

    def handle_cast({:workDone, childlist}, {[], parent}) do 
        {:noreply, {childlist, parent}}
    end
    def handle_cast({:workDone, childlist}, {oldlist, parent}) do 
        Sorter.workDone(parent, childlist ++ oldlist)
        Genserver.exit(self())
        {:noreply, {childlist, parent}}
    end

end