# 🌐 pocketOS

## 🎯 What is pocketOS?

**pocketOS** is not your typical operating system—it's an Elixir application that transforms ESP32-based handheld devices into powerful, hackable cyberdecks. Running on AtomVM — which brings the BEAM to resource-constrained devices — pocketOS delivers a complete handheld experience with functional, concurrent code on hardware that fits in your pocket.

## ✨ Features

- **🎨 Native Elixir UI** - Build UIs without leaving the BEAM
- **💻 ALisp Terminal** - Interactive LISP environment for on-device hacking
- **📡 Meshtastic Protocol** - Send messages over LoRa without infrastructure
- **🗺️ GPS & Mapping** - Track your location and visualize your world
- **🔧 Hackable by Design** - Extend, modify, and make it yours

## 🛠️ Supported Hardware

### Tested & Verified
- **[LILYGO T-Deck](https://lilygo.cc/products/t-deck)**
- **[LILYGO T-LoRa-Pager](https://lilygo.cc/products/t-lora-pager)**
- **Linux** - With atomgl plugin for development and testing

## 📋 Requirements

- **AtomVM** >= v0.6.6
- **atomgl** component ([github.com/atomvm/atomgl](https://github.com/atomvm/atomgl))
- Elixir development environment
- ESP-IDF (for flashing to hardware)

## 🚀 Quick Start

### 1. Clone the Repository
```bash
git clone https://github.com/yourusername/pocketOS.git
cd pocketOS
```

### 2. Build the Application
```bash
mix deps.get
mix compile
```

### 3. Package for AtomVM
```bash
mix atomvm.packbeam
```

## 🤝 Contributing

We welcome contributions from the community! Whether you're into:
- 🎨 UI/UX improvements
- 🔧 Hardware support
- 📡 Protocol implementations
- 📚 Documentation
- 🐛 Bug fixes

## 📜 License

pocketOS is open source software licensed under the [Apache 2.0 License](LICENSE).

---

<p align="center">
  <em>Built with ❤️ and Elixir by hackers, for hackers</em>
</p>
