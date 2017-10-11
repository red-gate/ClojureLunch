defmodule PurescriptPhoenix.RoomChannel do 
use Phoenix.Channel

  def join("room:lobby", _message, socket) do
    {:ok, socket}
  end

  def join("room:" <> _, _message, socket) do
    {:ok, socket}
  end

  def handle_in("new_message", payload, socket) do
    
    token = case Map.has_key?(socket.assigns, :token) do
      true -> socket.assigns[:token]
      false -> Enum.random(0..1024)
    end
    socket = assign(socket, :token, token)
    load = Map.put(payload, :token, token)
    IO.puts "adding token #{Kernel.inspect load} to socket"
    broadcast! socket, "new_message" , load
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