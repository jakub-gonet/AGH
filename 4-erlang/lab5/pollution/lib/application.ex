defmodule Pollution.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    :pollution_supervisor.start_link()
  end
end
