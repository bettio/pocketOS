alias PhotonUI.Widgets.Button
alias PhotonUI.Widgets.Container
alias PhotonUI.Widgets.IconListView
alias PhotonUI.Widgets.Rectangle
alias PhotonUI.Widgets.Text
alias PhotonUI.Widgets.TextInput
alias PhotonUI.Widgets.VerticalLayout
alias PhotonUI.UIServer

defmodule UI.ConfigEditor do
  @domains [:radi0cfg, :identity, :meshtastic, :meshcore, :network]

  @key_icon {:pocket_os, "icons/32/generic/new_mail.rgba"}
  @choice_icon {:pocket_os, "icons/32/generic/mail_doc.rgba"}
  @back_icon {:pocket_os, "icons/32/generic/go_back.rgba"}

  defp title_bar(title) do
    %Container{
      name: :title_bar,
      x: 0,
      y: 0,
      width: 320,
      height: 16,
      children: [
        %Rectangle{name: :title_label_bg, x: 0, y: 0, height: 16, width: 320, color: 0x000000},
        %Text{
          name: :title_label,
          x: 8,
          y: 0,
          height: 16,
          width: byte_size(title) * 8,
          text: title,
          bgcolor: 0x4792EC
        }
      ]
    }
  end

  defp list_ui do
    [
      %VerticalLayout{
        name: :vl,
        x: 0,
        y: 0,
        width: 320,
        height: 240,
        spacing: 1,
        children: [
          title_bar(" Config "),
          %IconListView{
            name: :grid,
            x: 0,
            y: 0,
            height: 220,
            width: 320,
            icon_size: 32,
            cell_height: 40
          }
        ]
      }
    ]
  end

  defp choice_ui(title) do
    [
      %VerticalLayout{
        name: :vl,
        x: 0,
        y: 0,
        width: 320,
        height: 240,
        spacing: 1,
        children: [
          title_bar(title),
          %IconListView{
            name: :choices,
            x: 0,
            y: 0,
            height: 220,
            width: 320,
            icon_size: 32,
            cell_height: 40
          }
        ]
      }
    ]
  end

  defp text_edit_ui(title) do
    [
      %VerticalLayout{
        name: :vl,
        x: 0,
        y: 0,
        width: 320,
        height: 240,
        spacing: 4,
        children: [
          title_bar(title),
          %Text{name: :value_label, text: "Value:", x: 0, y: 0, height: 16, width: 6 * 8},
          %TextInput{name: :value_input, x: 0, y: 0, height: 16, width: 320},
          %Button{name: :save, text: "Save", x: 0, y: 0, height: 32, width: 64},
          %Button{name: :reset, text: "Default", x: 0, y: 0, height: 32, width: 64},
          %Button{name: :back, text: "Back", x: 0, y: 0, height: 32, width: 64}
        ]
      }
    ]
  end

  def start_link(args, opts), do: UIServer.start_link(__MODULE__, args, opts)
  def start_monitor(args, opts), do: UIServer.start_monitor(__MODULE__, args, opts)

  def init(_opts) do
    {:ok, {list_ui(), %{}}, %{screen: :list, editing: nil}}
  end

  def handle_call(_msg, _from, state), do: {:reply, :error, state}
  def handle_cast(_msg, state), do: {:noreply, state}

  def handle_info(msg, _ui, state) do
    :erlang.display({:handle_info, msg})
    {:noreply, state}
  end

  def handle_event(:ui, :shown, ui, state) do
    {:noreply, set_list_model(ui), state}
  end

  def handle_event(:grid, {:clicked, _i, %{id: :exit}}, _ui, state) do
    {:stop, :normal, state}
  end

  def handle_event(:grid, {:clicked, _i, %{id: {:edit, domain, key}}}, ui, state) do
    entry = Map.get(PocketOS.Config.schema(domain), key)
    state = %{state | screen: :edit, editing: {domain, key, entry}}
    {:noreply, show_edit(ui, domain, key, entry), state}
  end

  def handle_event(:choices, {:clicked, _i, %{id: {:choose, v}}}, ui, state) do
    {domain, key, entry} = state.editing
    PocketOS.Config.set(domain, key, entry, v)
    back_to_list(ui, state)
  end

  def handle_event(:choices, {:clicked, _i, %{id: :reset}}, ui, state) do
    {domain, key, entry} = state.editing
    PocketOS.Config.reset(domain, key, entry)
    back_to_list(ui, state)
  end

  def handle_event(:choices, {:clicked, _i, %{id: :back}}, ui, state) do
    back_to_list(ui, state)
  end

  def handle_event(:save, :clicked, ui, state) do
    {domain, key, entry} = state.editing
    text = to_string(UIServer.get_property!(ui, :value_input, :text))

    case :config_schema.validate(entry, text) do
      {:ok, v} ->
        PocketOS.Config.set(domain, key, entry, v)
        back_to_list(ui, state)

      {:error, _} ->
        {:noreply, ui, state}
    end
  end

  def handle_event(:reset, :clicked, ui, state) do
    {domain, key, entry} = state.editing
    PocketOS.Config.reset(domain, key, entry)
    back_to_list(ui, state)
  end

  def handle_event(:back, :clicked, ui, state) do
    back_to_list(ui, state)
  end

  def handle_event(name, what, _ui, state) do
    :erlang.display({:handle_event, name, what})
    {:noreply, state}
  end

  defp show_edit(ui, _domain, key, %{type: t} = entry) when t == :enum or t == :bool do
    ui = UIServer.replace_ui(ui, choice_ui(" #{key} "))
    set_choice_model(ui, entry)
  end

  defp show_edit(ui, domain, key, _entry) do
    {:ok, values} = PocketOS.Config.load(domain)
    cur = edit_text(Map.get(values, key))
    ui = UIServer.replace_ui(ui, text_edit_ui(" #{key} "))

    UIServer.begin_widget_state_update(ui)
    |> UIServer.update_property!(:value_input, :text, cur)
    |> UIServer.apply_widget_state_update(ui)
  end

  defp set_list_model(ui) do
    UIServer.begin_widget_state_update(ui)
    |> UIServer.update_property!(:grid, :model, list_model())
    |> UIServer.apply_widget_state_update(ui)
  end

  defp set_choice_model(ui, entry) do
    UIServer.begin_widget_state_update(ui)
    |> UIServer.update_property!(:choices, :model, choice_model(entry))
    |> UIServer.apply_widget_state_update(ui)
  end

  defp back_to_list(ui, state) do
    ui = ui |> UIServer.replace_ui(list_ui()) |> set_list_model()
    {:noreply, ui, %{state | screen: :list, editing: nil}}
  end

  defp list_model do
    rows =
      Enum.flat_map(@domains, fn domain ->
        {:ok, values} = PocketOS.Config.load(domain)

        domain
        |> PocketOS.Config.schema()
        |> Map.keys()
        |> Enum.map(fn key ->
          %{
            id: {:edit, domain, key},
            text: "#{domain}.#{key}=#{display(Map.get(values, key))}",
            source: @key_icon
          }
        end)
      end)

    [%{id: :exit, text: "Exit", source: @back_icon} | rows]
  end

  defp choice_model(%{type: :bool}), do: choice_rows([true, false])
  defp choice_model(%{type: :enum, values: vals}), do: choice_rows(vals)

  defp choice_rows(values) do
    choices =
      Enum.map(values, fn v -> %{id: {:choose, v}, text: to_string(v), source: @choice_icon} end)

    choices ++
      [
        %{id: :reset, text: "(reset to default)", source: @choice_icon},
        %{id: :back, text: "Back", source: @back_icon}
      ]
  end

  defp display(nil), do: "(unset)"
  defp display(v), do: to_string(v)

  defp edit_text(nil), do: ""
  defp edit_text(v), do: to_string(v)
end
