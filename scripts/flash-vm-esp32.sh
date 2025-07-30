#!/usr/bin/env bash

set -euo pipefail

# --- Default values ---
DEFAULT_VERSION="v0.6.6"
USER_CHIP=""
USER_VERSION=""
POSITIONAL_ARGS=()

print_usage() {
    echo "Usage: $0 [--chip <chip>] [--version <vX.Y.Z>] [PORT]"
    echo "  --chip <chip>     Override chip type (e.g., esp32c3, esp32)"
    echo "  --version <ver>   Override AtomVM version (default: $DEFAULT_VERSION)"
    echo "  PORT              Optional serial port (e.g., /dev/ttyUSB0)"
}

# --- Parse arguments ---
while [[ $# -gt 0 ]]; do
    case "$1" in
        --chip)
            USER_CHIP="$2"
            shift 2
            ;;
        --version)
            USER_VERSION="$2"
            shift 2
            ;;
        -h|--help)
            print_usage
            exit 0
            ;;
        *)
            POSITIONAL_ARGS+=("$1")
            shift
            ;;
    esac
done

PORT="${POSITIONAL_ARGS[0]:-}"

# --- Auto-detect port if not provided ---
if [[ -z "$PORT" ]]; then
    PORT=$(ls /dev/tty.usb* /dev/ttyUSB* 2>/dev/null | head -n 1 || true)
    if [[ -z "$PORT" ]]; then
        echo "❌ No serial port found. Please connect your device or specify the port explicitly."
        exit 1
    fi
    echo "🔌 Auto-detected port: $PORT"
else
    echo "🔌 Using specified port: $PORT"
fi

# --- Detect chip unless overridden ---
DETECTED_CHIP=""
if [[ -z "$USER_CHIP" ]]; then
    echo "🔎 Detecting chip on $PORT ..."
    CHIP_INFO=$(esptool.py --port "$PORT" chip_id 2>&1 || true)

    if echo "$CHIP_INFO" | grep -q "ESP32-C3"; then
        DETECTED_CHIP="esp32c3"
    elif echo "$CHIP_INFO" | grep -q "ESP32-S3"; then
        DETECTED_CHIP="esp32s3"
    elif echo "$CHIP_INFO" | grep -q "ESP32"; then
        DETECTED_CHIP="esp32"
    else
        echo "❌ Could not detect chip type from output:"
        echo "$CHIP_INFO"
        exit 1
    fi
    echo "🔍 Detected chip: $DETECTED_CHIP"
fi

# --- Final config ---
CHIP="${USER_CHIP:-$DETECTED_CHIP}"
VERSION="${USER_VERSION:-$DEFAULT_VERSION}"
IMAGE_NAME="AtomVM-${CHIP}-${VERSION}.img"
CHECKSUM_NAME="${IMAGE_NAME}.sha256"
BASE_URL="https://github.com/atomvm/AtomVM/releases/download/${VERSION}"

# --- Create temp dir ---
TMP_DIR=$(mktemp -d)
cleanup() {
    echo "🧹 Cleaning up..."
    rm -rf "$TMP_DIR"
}
trap cleanup EXIT
cd "$TMP_DIR"

# --- Download image and checksum ---
echo "⬇️  Downloading files to temporary directory: $TMP_DIR"
echo "📦 Downloading $IMAGE_NAME..."
curl -sSL -O "${BASE_URL}/${IMAGE_NAME}"

echo "📦 Downloading $CHECKSUM_NAME..."
curl -sSL -O "${BASE_URL}/${CHECKSUM_NAME}"

# --- Verify checksum ---
echo "🔐 Verifying checksum..."
if command -v sha256sum >/dev/null 2>&1; then
    sha256sum -c "$CHECKSUM_NAME"
elif command -v shasum >/dev/null 2>&1; then
    shasum -a 256 -c "$CHECKSUM_NAME"
else
    echo "❌ Neither sha256sum nor shasum is available."
    exit 1
fi

echo "✅ Checksum valid."

# --- Flash image ---
echo "⚡ Flashing $IMAGE_NAME to $PORT ..."
esptool.py --chip "$CHIP" --port "$PORT" --baud 460800 write_flash 0x0 "$IMAGE_NAME"

echo "🎉 Flashing complete."
