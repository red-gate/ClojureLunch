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
    broadcast! socket, "list_update", %{value: Kernel.inspect(list_update)}
    {:noreply, socket}
  end

end