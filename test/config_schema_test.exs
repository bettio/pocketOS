defmodule ConfigSchemaTest do
  use ExUnit.Case

  test "validate int accepts an integer and a decimal binary" do
    e = %{type: :int}
    assert :config_schema.validate(e, 42) == {:ok, 42}
    assert :config_schema.validate(e, "869525000") == {:ok, 869_525_000}
    assert {:error, _} = :config_schema.validate(e, "nope")
  end

  test "validate bool from atom and binary forms" do
    e = %{type: :bool}
    assert :config_schema.validate(e, true) == {:ok, true}
    assert :config_schema.validate(e, "true") == {:ok, true}
    assert :config_schema.validate(e, "false") == {:ok, false}
    assert :config_schema.validate(e, "1") == {:ok, true}
    assert :config_schema.validate(e, "0") == {:ok, false}
    assert {:error, _} = :config_schema.validate(e, "maybe")
  end

  test "validate string from binary and atom" do
    e = %{type: :string}
    assert :config_schema.validate(e, "hi") == {:ok, "hi"}
    assert :config_schema.validate(e, :hi) == {:ok, "hi"}
  end

  test "validate enum checks membership and coerces a binary" do
    e = %{type: :enum, values: [:meshtastic, :meshcore]}
    assert :config_schema.validate(e, :meshcore) == {:ok, :meshcore}
    assert :config_schema.validate(e, "meshtastic") == {:ok, :meshtastic}
    assert {:error, _} = :config_schema.validate(e, :other)
    assert {:error, _} = :config_schema.validate(e, "nonexistent_enum_value")
  end

  test "coerce layers defaults, overrides, and reports bad/unknown keys" do
    schema = %{
      preset: %{type: :enum, values: [:meshtastic, :meshcore], default: :meshtastic},
      tx_power: %{type: :int, default: 14},
      frequency: %{type: :int}
    }

    # absent -> default; present+valid -> coerced value; no-default+absent -> omitted
    assert {%{preset: :meshtastic, tx_power: 22}, []} =
             :config_schema.coerce(schema, %{tx_power: "22"})

    # present but invalid -> falls back to default, records the error
    {coerced, errs} = :config_schema.coerce(schema, %{tx_power: "abc"})
    assert coerced == %{preset: :meshtastic, tx_power: 14}
    assert {:tx_power, _} = List.keyfind(errs, :tx_power, 0)

    # unknown key reported, not propagated
    {coerced2, errs2} = :config_schema.coerce(schema, %{bogus: "x"})
    refute Map.has_key?(coerced2, :bogus)
    assert {:bogus, :unknown_key} in errs2
  end
end
