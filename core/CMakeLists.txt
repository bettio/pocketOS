cmake_minimum_required (VERSION 2.6)
project (core)

include(../cmake/beam.cmake)

add_subdirectory(icons)

compile_erlang(display)
compile_erlang(input)
compile_erlang(scene)
compile_elixir(main.ex Elixir.Main.beam)
compile_elixir(icons_menu.ex Elixir.IconsMenu.beam)
compile_elixir(input_box.ex Elixir.InputBox.beam)
#compile_elixir(keyboard_parser.ex Elixir.KeyboardParser.beam)

pack_deps(deps.avm
    ${ATOMVM_INSTALL_PREFIX}/lib/AtomVM/ebin/estdlib.avm
    ${ATOMVM_INSTALL_PREFIX}/lib/AtomVM/ebin/eavmlib.avm
    ${ATOMVM_INSTALL_PREFIX}/lib/AtomVM/ebin/exavmlib.avm
)

pack_beam(pocketos.avm
    Elixir.Main.beam
    Elixir.IconsMenu.beam
    Elixir.InputBox.beam
#    Elixir.KeyboardParser.beam
    scene.beam
    display.beam
    input.beam
    icons/icons64.beam
    deps.avm
)

add_custom_target(core DEPENDS pocketos.avm)
add_dependencies(core icons)
