# AtomVM SSD1306 Driver

This is an [AtomVM](https://atomvm.net) driver module for SSD1306-based monochrome OLED displays, using I2C communication. It provides basic drawing capabilities like clearing the screen, rendering text, and drawing bitmaps.

The driver is implemented as a `gen_server` in Erlang and is intended to run on embedded platforms supported by AtomVM, such as the ESP32.

---

## Example Usage

See [`ssd1306_demo.erl`](./ssd1306_demo.erl) for a complete usage example.

```erlang
Opts = [
    {scl, 9},
    {sda, 10},
    {clock_speed_hz, 400000},
    {peripheral, "i2c0"},
    {address, 16#3C},
    {height, 32},
    flip_horizontal
],
{ok, Pid} = ssd1306:start_link(Opts),

ssd1306:set_inversion(Pid, false),
ssd1306:set_text(Pid, left, top, <<"Hello World!">>),
ssd1306:set_text(Pid, left, bottom, <<"Bye!">>).
```

---

## Build Instructions

To build and flash the driver and demo to an ESP32:

### 1. Compile the code

```bash
rebar3 compile
```

### 2. Package the BEAM files

```bash
rebar3 atomvm packbeam
```

### 3. Flash to the device

```bash
rebar3 atomvm esp32_flash --port /dev/ttyUSB0
```

Replace `/dev/ttyUSB0` with the actual port used by your device.

---

## Requirements

- AtomVM toolchain (`rebar3` with `atomvm` plugin)
- Supported target (e.g., ESP32 with I2C)
- SSD1306 OLED display (e.g., 128x64 or 128x32) using I2C
- Optional font module that exports `glyph/1` for text rendering

---
