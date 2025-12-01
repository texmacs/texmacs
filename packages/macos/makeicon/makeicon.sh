#!/bin/bash

INPUT="icon.png"
ASSET_DIR="Media.xcassets"
ICON_SET="$ASSET_DIR/AppIcon.appiconset"

rm -rf "$ASSET_DIR"
mkdir -p "$ICON_SET"

magick icon.png -resize 16x16   "$ICON_SET/icon_16x16.png"
magick icon.png -resize 32x32   "$ICON_SET/icon_16x16@2x.png"
magick icon.png -resize 32x32   "$ICON_SET/icon_32x32.png"
magick icon.png -resize 64x64   "$ICON_SET/icon_32x32@2x.png"
magick icon.png -resize 128x128 "$ICON_SET/icon_128x128.png"
magick icon.png -resize 256x256 "$ICON_SET/icon_128x128@2x.png"
magick icon.png -resize 256x256 "$ICON_SET/icon_256x256.png"
magick icon.png -resize 512x512 "$ICON_SET/icon_256x256@2x.png"
magick icon.png -resize 512x512 "$ICON_SET/icon_512x512.png"
magick icon.png -resize 1024x1024 "$ICON_SET/icon_512x512@2x.png"

cat > "$ICON_SET/Contents.json" <<EOF
{
  "images" : [
    { "size" : "16x16", "idiom" : "mac", "filename" : "icon_16x16.png", "scale" : "1x" },
    { "size" : "16x16", "idiom" : "mac", "filename" : "icon_16x16@2x.png", "scale" : "2x" },
    { "size" : "32x32", "idiom" : "mac", "filename" : "icon_32x32.png", "scale" : "1x" },
    { "size" : "32x32", "idiom" : "mac", "filename" : "icon_32x32@2x.png", "scale" : "2x" },
    { "size" : "128x128", "idiom" : "mac", "filename" : "icon_128x128.png", "scale" : "1x" },
    { "size" : "128x128", "idiom" : "mac", "filename" : "icon_128x128@2x.png", "scale" : "2x" },
    { "size" : "256x256", "idiom" : "mac", "filename" : "icon_256x256.png", "scale" : "1x" },
    { "size" : "256x256", "idiom" : "mac", "filename" : "icon_256x256@2x.png", "scale" : "2x" },
    { "size" : "512x512", "idiom" : "mac", "filename" : "icon_512x512.png", "scale" : "1x" },
    { "size" : "512x512", "idiom" : "mac", "filename" : "icon_512x512@2x.png", "scale" : "2x" }
  ],
  "info" : { "version" : 1, "author" : "xcode" }
}
EOF

xcrun actool "$ASSET_DIR" \
    --compile . \
    --platform macosx \
    --minimum-deployment-target 11.0 \
    --app-icon AppIcon \
    --output-partial-info-plist partial.plist
