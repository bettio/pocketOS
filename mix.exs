defmodule PocketOS.MixProject do
  use Mix.Project

  def project do
    [
      app: :pocket_os,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      compilers: [:elixir_make] ++ Mix.compilers(),
      deps: deps(),
      atomvm: [
        start: Main,
        flash_offset: 0x250000
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:exatomvm, github: "AtomVM/exatomvm", runtime: false},
      {:elixir_make, "~> 0.4", runtime: false},
      {:avm_scene, github: "AtomVM/avm_scene"},
      {:aprotobuf, git: "https://github.com/atomvm/aprotobuf/"}
    ]
  end
end
