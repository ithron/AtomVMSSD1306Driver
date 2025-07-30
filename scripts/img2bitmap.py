from PIL import Image
import sys
import os

def bitmap_to_erlang_binaries(path):
    image = Image.open(path).convert('1')  # Convert to 1-bit image (white=255, black=0)
    width, height = image.size
    pixels = image.load()

    all_bytes = []
    for x in range(width):
        for page in range(0, height, 8):
            byte = 0
            for bit in range(8):
                y = page + bit
                if y < height:
                    bit_val = 1 if pixels[x, y] > 127 else 0  # white = 1, black = 0
                    byte |= (bit_val << bit)
            all_bytes.append(f"16#{byte:02X}")
    binary_string = f"<<{', '.join(all_bytes)}>>"
    return binary_string, (width, height)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python bitmap_to_erlang.py <image_path>")
        sys.exit(1)

    image_path = sys.argv[1]

    if not os.path.isfile(image_path):
        print(f"Error: File not found: {image_path}")
        sys.exit(1)

    binary, size = bitmap_to_erlang_binaries(image_path)
    print(f"{size[0]} x {size[1]}")
    print(binary)
