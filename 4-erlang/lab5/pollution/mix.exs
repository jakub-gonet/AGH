defmodule Pollution.MixProject do
  use Mix.Project

  def project do
    [
      app: :pollution,
      version: "0.1.0",
      elixir: "~> 1.11",
      erlc_paths: ["lib"],
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Pollution.Application, []}
    ]
  end

  defp deps do
    []
  end
end
