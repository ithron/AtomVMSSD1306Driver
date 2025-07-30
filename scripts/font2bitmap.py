import sys
import argparse
from PIL import Image, ImageFont, ImageDraw

def generate_preview_image(font):
    # Render all glyphs and determine widths
    glyphs = [render_char(c, font) for c in CHARS]
    max_w = max(len(g) for g in glyphs)

    CHARS_PER_ROW = 16
    scale = 10
    cell_w = max_w * scale
    cell_h = FONT_SIZE * scale
    num_chars = len(glyphs)
    rows = (num_chars + CHARS_PER_ROW - 1) // CHARS_PER_ROW
    img_w = CHARS_PER_ROW * cell_w
    img_h = rows * cell_h

    # Create white RGB image
    img = Image.new('RGB', (img_w, img_h), 'white')
    draw = ImageDraw.Draw(img)

    # Draw each glyph
    for idx, data in enumerate(glyphs):
        c = CHARS[idx]
        col = idx % CHARS_PER_ROW
        row = idx // CHARS_PER_ROW
        x0 = col * cell_w
        y0 = row * cell_h
        # Cell border
        draw.rectangle([x0, y0, x0 + cell_w - 1, y0 + cell_h - 1], outline='black')
        # Glyph pixels
        for x, byte in enumerate(data):
            for y in range(FONT_SIZE):
                if (byte >> y) & 1:
                    x1 = x0 + x * scale
                    y1 = y0 + y * scale
                    draw.rectangle([x1, y1, x1 + scale - 1, y1 + scale - 1], fill='black')

    # Save preview
    img.save('preview.png')

FONT_SIZE = 8
GLYPH_WIDTH = 5  # Fixed width for all glyphs
CHARS = ''.join(chr(i) for i in range(32, 127))  # Printable ASCII


def render_char(c, font):
    bbox = font.getbbox(c)
    w = bbox[2] - bbox[0]
    image = Image.new('1', (w, FONT_SIZE), 1)
    draw = ImageDraw.Draw(image)
    # Draw glyph at its left bearing offset
    draw.text((-bbox[0], 0), c, font=font, fill=0)

    glyph_data = []

    for x in range(w):
        byte = 0
        for y in range(FONT_SIZE):
            if image.getpixel((x, y)) == 0:
                byte |= (1 << y)
        glyph_data.append(byte)

    return glyph_data


def main():
    parser = argparse.ArgumentParser(description="Convert TTF font to Erlang module with glyph/1 function")
    parser.add_argument("font", help="Path to TTF font file")
    parser.add_argument('--preview', action='store_true', help='Generate preview.png with all glyphs')
    parser.add_argument("-o", "--output", help="Output file (default: stdout)", default=None)
    args = parser.parse_args()

    try:
        font = ImageFont.truetype(args.font, FONT_SIZE)
        if args.preview:
            generate_preview_image(font)
            print("Saved preview.png")
            return
    except Exception as e:
        print(f"Error loading font: {e}")
        sys.exit(1)

    output_lines = []
    output_lines.append("-module(font).")
    output_lines.append("-export([glyph/1]).\n")
    output_lines.append("-spec glyph(binary()) -> binary() | undefined.\n")
    output_lines.append("% Auto-generated font glyphs")

    for c in CHARS:
        try:
            data = render_char(c, font)
            hex_bytes = ', '.join(f'16#{byte:02X}' for byte in data)
            escaped = c.replace("\\", "\\\\").replace('"', '\\"')
            output_lines.append(f'glyph(<<"{escaped}">>) -> <<{hex_bytes}>>;')
        except Exception as e:
            output_lines.append(f"% Skipped '{c}': {e}")

    output_lines.append("glyph(_) -> undefined.")

    if args.output:
        with open(args.output, "w") as f:
            f.write("\n".join(output_lines))
    else:
        print("\n".join(output_lines))

if __name__ == "__main__":
    main()