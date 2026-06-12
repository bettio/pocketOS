defmodule MeshtasticCallbacksTest do
  use ExUnit.Case

  test "any decoded packet learns a placeholder node until a real node info arrives" do
    MeshtasticCallbacks.init()

    MeshtasticCallbacks.message_cb(%{
      message: %{portnum: :TEXT_MESSAGE_APP, payload: "ciao"},
      packet_id: 1,
      src: 0x43314684,
      rssi: -7,
      snr: 10
    })

    assert [{:meshtastic_node_info, 0x43314684, placeholder}] =
             :micronesia.dirty_read({:meshtastic_node_info, 0x43314684})

    assert placeholder.id == "!43314684"
    assert placeholder.short_name == "4684"
    assert placeholder.long_name == "Meshtastic 4684"

    # a real node info replaces the placeholder
    MeshtasticCallbacks.message_cb(%{
      message: %{
        portnum: :NODEINFO_APP,
        payload: %{id: "!43314684", short_name: "TD", long_name: "t-deck"}
      },
      src: 0x43314684
    })

    assert [{:meshtastic_node_info, 0x43314684, info}] =
             :micronesia.dirty_read({:meshtastic_node_info, 0x43314684})

    assert info.long_name == "t-deck"

    # later traffic does not downgrade it back to a placeholder
    MeshtasticCallbacks.message_cb(%{
      message: %{portnum: :TEXT_MESSAGE_APP, payload: "x"},
      packet_id: 2,
      src: 0x43314684,
      rssi: -8,
      snr: 9
    })

    assert [{:meshtastic_node_info, 0x43314684, %{long_name: "t-deck"}}] =
             :micronesia.dirty_read({:meshtastic_node_info, 0x43314684})
  end
end
