defmodule MeshcoreCallbacksTest do
  use ExUnit.Case

  test "stores decrypted texts in the meshcore_message table" do
    MeshcoreCallbacks.init()

    MeshcoreCallbacks.message_cb(%{type: :grp_txt, packet_hash: <<1>>, text: "testd: hello"})

    MeshcoreCallbacks.message_cb(%{
      type: :txt_msg,
      packet_hash: <<2>>,
      text: "hi",
      sender_name: "testd",
      src_hash: 0x5E
    })

    MeshcoreCallbacks.message_cb(%{
      type: :txt_msg,
      packet_hash: <<3>>,
      text: "anon",
      src_hash: 0x5E
    })

    assert :micronesia.dirty_read({:meshcore_message, <<1>>}) ==
             [{:meshcore_message, <<1>>, "testd: hello"}]

    assert :micronesia.dirty_read({:meshcore_message, <<2>>}) ==
             [{:meshcore_message, <<2>>, "testd: hi"}]

    assert :micronesia.dirty_read({:meshcore_message, <<3>>}) ==
             [{:meshcore_message, <<3>>, "#5E: anon"}]
  end

  test "frames without decrypted text are ignored" do
    MeshcoreCallbacks.init()

    MeshcoreCallbacks.message_cb(%{type: :advert, packet_hash: <<4>>, sig_ok: true})

    MeshcoreCallbacks.message_cb(%{
      type: :txt_msg,
      packet_hash: <<5>>,
      src_hash: 0x5E,
      decrypt_error: :no_contact
    })

    MeshcoreCallbacks.message_cb(%{type: :grp_txt, packet_hash: <<6>>, decrypt_error: :bad_mac})

    assert :micronesia.all(:meshcore_message) == []
  end
end
