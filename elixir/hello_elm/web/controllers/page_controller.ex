defmodule HelloElm.PageController do
  use HelloElm.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
