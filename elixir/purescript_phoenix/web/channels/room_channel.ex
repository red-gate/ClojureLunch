defmodule PurescriptPhoenix.RoomChannel do 
use Phoenix.Channel

  def join("room:lobby", _message, socket) do
    {:ok, socket}
  end
  def handle_in("new_message", payload, socket) do
    broadcast! socket, "new_message", payload
    {:noreply, socket}
  end
  def handle_in("mouse_moved", payload, socket) do
    broadcast! socket, "mouse_moved", payload
    {:noreply, socket}
  end
#  def join("room:" <> _private_room_id, _params, _socket) do
#    {:error, %{reason: "unauthorized"}}
#  end

end