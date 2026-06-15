defmodule PocketOS.Config do
  @moduledoc """
  Layered config loader. For a domain it merges, lowest precedence first:

    1. schema defaults (`priv/config-schemas/<domain>.sxp`)
    2. base file `FS0:/<domain>.sxp`
    3. per-key `.d` overlay `Config:/<domain>.d/<key>` (NVS sub-namespace)

  then coerces/validates the result via `:config_schema`. Schemas and base files
  are S-expressions; the overlay is one NVS blob per key. `opts[:schema]` injects
  a schema map (the host-test seam, so a priv file need not be parsed under test).
  """
  @compile {:no_warn_undefined, :atomvm}
  @compile {:no_warn_undefined, :sexp_lexer}
  @compile {:no_warn_undefined, :sexp_parser}
  @build_env Mix.env()

  @spec load(atom(), keyword()) :: {:ok, map()}
  def load(domain, opts \\ []) do
    schema = opts[:schema] || load_schema(domain)
    raw = Map.merge(read_base(domain), read_overlay(domain, schema))
    {coerced, errors} = :config_schema.coerce(schema, raw)
    log_errors(domain, errors)
    {:ok, coerced}
  end

  @doc "The decoded schema map for a domain (priv data)."
  @spec schema(atom()) :: map()
  def schema(domain), do: load_schema(domain)

  @doc "Write one key's override into the NVS `.d` overlay."
  @spec set(atom(), atom(), map(), term()) :: :ok | {:error, term()}
  def set(domain, key, entry, value) do
    text = encode_value(Map.fetch!(entry, :type), value)
    path = overlay_path(domain, key, entry)

    with {:ok, file} <- PocketOS.File.open(path, [:write]),
         {:ok, _} <- PocketOS.File.write(file, text),
         :ok <- PocketOS.File.close(file) do
      :ok
    else
      other -> {:error, other}
    end
  end

  @doc "Clear a key's overlay override, falling back to base/default."
  @spec reset(atom(), atom(), map()) :: :ok | {:error, term()}
  def reset(domain, key, entry), do: PocketOS.File.delete(overlay_path(domain, key, entry))

  @doc "The overlay path a key is read from / written to."
  @spec overlay_path(atom(), atom(), map()) :: binary()
  def overlay_path(domain, key, entry), do: "Config:/#{domain}.d/#{overlay_seg(key, entry)}"

  @doc "Encode a typed value to its canonical overlay text (inverse of validate)."
  @spec encode_value(atom(), term()) :: binary()
  def encode_value(:int, v), do: Integer.to_string(v)
  def encode_value(:bool, true), do: "true"
  def encode_value(:bool, false), do: "false"
  def encode_value(:string, v) when is_binary(v), do: v
  def encode_value(:string, v), do: to_string(v)
  def encode_value(:enum, v), do: Atom.to_string(v)

  # --- schema (priv data, nested grammar) ---

  defp load_schema(domain) do
    case read_priv("config-schemas/#{domain}.sxp") do
      data when is_binary(data) -> data |> parse_sexp() |> decode_schema()
      _ -> %{}
    end
  end

  defp decode_schema(parsed) do
    for [key | attrs] <- parsed, into: %{}, do: {key, decode_entry(attrs)}
  end

  defp decode_entry(attrs) do
    for [attr | vals] <- attrs, into: %{}, do: {attr, decode_attr(attr, vals)}
  end

  defp decode_attr(:values, vals), do: vals
  defp decode_attr(_attr, [single]), do: single
  defp decode_attr(_attr, vals), do: vals

  # --- base .sxp (flat grammar) ---

  defp read_base(domain) do
    case read_fs("FS0:/#{domain}.sxp") do
      {:ok, data} -> data |> parse_sexp() |> decode_flat()
      :error -> %{}
    end
  end

  defp decode_flat(parsed) do
    for [k, v] <- parsed, into: %{}, do: {k, v}
  end

  # --- .d overlay (one NVS blob per key) ---

  defp read_overlay(domain, schema) do
    Enum.reduce(schema, %{}, fn {key, entry}, acc ->
      case read_fs("Config:/#{domain}.d/#{overlay_seg(key, entry)}") do
        {:ok, data} -> Map.put(acc, key, data)
        :error -> acc
      end
    end)
  end

  defp overlay_seg(_key, %{nvs: nvs}), do: to_string(nvs)
  defp overlay_seg(key, _entry), do: to_string(key)

  # --- io / parse shims (sexp + read_priv are AtomVM-only) ---

  defp read_priv(rel) do
    if @build_env == :test do
      case :file.read_file("priv/#{rel}") do
        {:ok, bin} -> bin
        _ -> nil
      end
    else
      :atomvm.read_priv(:pocket_os, rel)
    end
  end

  defp read_fs(path) do
    with {:ok, file} <- PocketOS.File.open(path, [:read]),
         {:ok, data} <- PocketOS.File.read(file, 8192) do
      _ = PocketOS.File.close(file)
      {:ok, data}
    else
      _ -> :error
    end
  end

  defp parse_sexp(data) do
    if @build_env == :test do
      PocketOS.Config.Sexp.parse(data)
    else
      data |> :sexp_lexer.string() |> :sexp_parser.parse()
    end
  end

  defp log_errors(_domain, []), do: :ok
  defp log_errors(domain, errors), do: IO.puts("[config] #{domain}: ignored #{inspect(errors)}")
end

defmodule PocketOS.Config.Sexp do
  @moduledoc false
  # Test-only reader replicating AtomVM's sexp_lexer/sexp_parser for the config
  # grammar: an outer (...) of entries; quoted "..." -> binary, bare token -> atom
  # or integer, nested (...) -> sub-list. Spaces only, decimal integers.

  def parse(bin) when is_binary(bin), do: parse(:erlang.binary_to_list(bin))

  def parse(chars) when is_list(chars) do
    [?( | rest] = skip_ws(chars)
    {items, _} = parse_items(rest, [])
    items
  end

  defp parse_items(chars, acc) do
    case skip_ws(chars) do
      [?) | rest] ->
        {Enum.reverse(acc), rest}

      [?( | rest] ->
        {sub, rest2} = parse_items(rest, [])
        parse_items(rest2, [sub | acc])

      other ->
        {tok, rest} = read_token(other)
        parse_items(rest, [tok | acc])
    end
  end

  defp skip_ws([c | t]) when c in [?\s, ?\t, ?\n, ?\r], do: skip_ws(t)
  defp skip_ws(chars), do: chars

  defp read_token([?" | t]), do: read_string(t, [])
  defp read_token(chars), do: read_bare(chars, [])

  defp read_string([?" | t], acc), do: {:erlang.list_to_binary(Enum.reverse(acc)), t}
  defp read_string([c | t], acc), do: read_string(t, [c | acc])

  defp read_bare([c | _] = chars, acc) when c in [?\s, ?\t, ?\n, ?\r, ?(, ?)],
    do: {bare_value(Enum.reverse(acc)), chars}

  defp read_bare([c | t], acc), do: read_bare(t, [c | acc])
  defp read_bare([], acc), do: {bare_value(Enum.reverse(acc)), []}

  defp bare_value(chars) do
    if integer_chars?(chars),
      do: :erlang.list_to_integer(chars),
      else: :erlang.list_to_atom(chars)
  end

  defp integer_chars?([?- | rest]), do: rest != [] and all_digits?(rest)
  defp integer_chars?(chars), do: chars != [] and all_digits?(chars)

  defp all_digits?([]), do: true
  defp all_digits?([c | rest]) when c >= ?0 and c <= ?9, do: all_digits?(rest)
  defp all_digits?(_), do: false
end
