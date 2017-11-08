defmodule PurescriptPhoenix.SortChannel do 
use Phoenix.Channel

  def join("sorter:all", _message, socket) do
    {:ok, socket}
  end

  def handle_in("sort_list", payload, socket) do
    # todo Some sort start code
    Sorter.start(payload["value"])
    {:noreply, socket}
  end

  def handle_info({:update, list_update}, socket) do
    # broadcast list update (complete with indices)
    stream = Enum.map(list_update, fn {x,y} -> [x,y] end)
    # broadcast! socket, "list_update", %{value: Kernel.inspect(list_update)}
    broadcast! socket, "list_update", %{value: stream}
    {:noreply, socket}
  end
  def handle_info({:sortfinished, list}, socket) do
    # note that this is just the sorted list, stripped of indices
    # don't do anything, at the moment.
    {:noreply, socket}
  end

end