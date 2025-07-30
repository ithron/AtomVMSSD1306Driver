#!/usr/bin/env bash

set -euo pipefail

# --- Defaults ---
BAUD=115200
PORT=""
RESET_ESP32=false

print_usage() {
    echo "Usage: $0 [--baud <rate>] [--reset-esp32] [PORT]"
    echo "  --baud <rate>      Set baud rate (default: 115200)"
    echo "  --reset-esp32      Reset ESP32 device before monitoring"
    echo "  PORT               Optional serial port (e.g., /dev/ttyUSB0)"
}

# --- Parse arguments ---
while [[ $# -gt 0 ]]; do
    case "$1" in
        --baud)
            BAUD="$2"
            shift 2
            ;;
        --reset-esp32)
            RESET_ESP32=true
            shift
            ;;
        -h|--help)
            print_usage
            exit 0
            ;;
        *)
            if [[ -z "$PORT" ]]; then
                PORT="$1"
            else
                echo "❌ Unexpected argument: $1"
                print_usage
                exit 1
            fi
            shift
            ;;
    esac
done

# --- Auto-detect port if not specified ---
if [[ -z "$PORT" ]]; then
    PORT=$(ls /dev/tty.usb* /dev/ttyUSB* 2>/dev/null | head -n 1 || true)
    if [[ -z "$PORT" ]]; then
        echo "❌ No serial port found. Please connect your device or specify the port."
        print_usage
        exit 1
    fi
    echo "🔌 Auto-detected port: $PORT"
else
    echo "🔌 Using specified port: $PORT"
fi

# --- Reset ESP32 if requested ---
if [[ "$RESET_ESP32" = true ]]; then
    echo "🔁 Sending ESP32 reset sequence to $PORT..."
    python3 - <<EOF
import serial
import time

try:
    s = serial.Serial("$PORT")
    s.setDTR(False)
    s.setRTS(True)
    time.sleep(0.1)
    s.setDTR(False)
    s.setRTS(False)
    s.close()
    print("✅ Reset pulse sent.")
except Exception as e:
    print(f"❌ Failed to reset ESP32: {e}")
    exit(1)
EOF
fi

# --- Start monitoring ---
echo "📟 Starting monitor on $PORT at ${BAUD} baud..."
echo "🔚 Press Ctrl+A then Ctrl+X to exit picocom."
picocom --baud "$BAUD" "$PORT"
