defmodule Ed25519X25519Test do
  # Pure tests for the Ed25519 -> X25519 shared-secret conversion. Runs on
  # host OTP/OpenSSL; the conversion arithmetic is written to also stay
  # within AtomVM's bounded integers (not exercised here).
  use ExUnit.Case, async: true
  import Bitwise

  @p (1 <<< 255) - 19

  defp caught(fun) do
    try do
      fun.()
      :no_error
    catch
      :error, reason -> reason
    end
  end

  test "both parties derive the same 32-byte shared secret" do
    {alice_pub, alice_priv} = :crypto.generate_key(:eddsa, :ed25519)
    {bob_pub, bob_priv} = :crypto.generate_key(:eddsa, :ed25519)

    alice = :ed25519_x25519.shared_secret(alice_priv, bob_pub)
    bob = :ed25519_x25519.shared_secret(bob_priv, alice_pub)

    assert alice == bob
    assert byte_size(alice) == 32
  end

  # Independent oracle: the converted public key must equal the x25519
  # public key OpenSSL derives from the converted (clamped) scalar.
  test "public-key conversion matches OpenSSL's x25519 derivation" do
    {ed_pub, ed_priv} = :crypto.generate_key(:eddsa, :ed25519)
    x_priv = :ed25519_x25519.ed_secret_to_x25519(ed_priv)
    {derived_pub, _} = :crypto.generate_key(:eddh, :x25519, x_priv)

    assert :ed25519_x25519.ed_pub_to_x25519(ed_pub) == derived_pub
  end

  # The Ed25519 base point (compressed 0x58 66..66) maps to x25519 u = 9.
  test "the Ed25519 base point maps to x25519 u=9" do
    basepoint = <<0x58>> <> :binary.copy(<<0x66>>, 31)
    assert :ed25519_x25519.ed_pub_to_x25519(basepoint) == <<9>> <> <<0::248>>
  end

  test "a 64-byte secret key converts like its 32-byte seed" do
    {pub, seed} = :crypto.generate_key(:eddsa, :ed25519)
    sk64 = seed <> pub
    assert :ed25519_x25519.ed_secret_to_x25519(seed) == :ed25519_x25519.ed_secret_to_x25519(sk64)
  end

  test "the scalar is clamped: low 3 bits clear, top bit clear, bit 6 set" do
    {_pub, seed} = :crypto.generate_key(:eddsa, :ed25519)
    <<first, _::30-bytes, last>> = :ed25519_x25519.ed_secret_to_x25519(seed)
    assert (first &&& 7) == 0
    assert (last &&& 0x80) == 0
    assert (last &&& 64) == 64
  end

  test "both conversions return 32 bytes" do
    {pub, seed} = :crypto.generate_key(:eddsa, :ed25519)
    assert byte_size(:ed25519_x25519.ed_secret_to_x25519(seed)) == 32
    assert byte_size(:ed25519_x25519.ed_pub_to_x25519(pub)) == 32
  end

  test "a bad private-key size is rejected" do
    assert caught(fn -> :ed25519_x25519.ed_secret_to_x25519(<<0::248>>) end) ==
             {:invalid_private_key_size, 31}

    assert caught(fn -> :ed25519_x25519.ed_secret_to_x25519(<<0::384>>) end) ==
             {:invalid_private_key_size, 48}
  end

  test "a bad public-key size is rejected" do
    assert caught(fn -> :ed25519_x25519.ed_pub_to_x25519(<<0::248>>) end) ==
             {:invalid_public_key_size, 31}

    assert caught(fn -> :ed25519_x25519.ed_pub_to_x25519(<<0::264>>) end) ==
             {:invalid_public_key_size, 33}
  end

  test "a non-canonical y >= p is rejected" do
    bad = <<@p::256-little>>

    assert caught(fn -> :ed25519_x25519.ed_pub_to_x25519(bad) end) ==
             {:public_key_y_out_of_range, @p}
  end

  test "a zero denominator (y = 1) is rejected" do
    y1 = <<1::256-little>>

    assert caught(fn -> :ed25519_x25519.ed_pub_to_x25519(y1) end) ==
             :public_key_denominator_zero
  end

  test "a low-order peer key (y = 0) is rejected" do
    {_pub, seed} = :crypto.generate_key(:eddsa, :ed25519)
    low_order = <<0::256>>

    assert caught(fn -> :ed25519_x25519.shared_secret(seed, low_order) end) ==
             :low_order_public_key
  end
end
