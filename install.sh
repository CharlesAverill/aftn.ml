#!/usr/bin/env bash
set -euo pipefail

# Step 1: Install OCaml deps
echo "[*] Installing dependencies..."
opam install . --deps-only -y

# Step 2: Build and install with dune
echo "[*] Building and installing..."
make
dune install

# Step 3: Locate installed binary
BIN_PATH="$(command -v AFTN || true)"
if [[ -z "$BIN_PATH" ]]; then
    echo "[!] Could not find AFTN in PATH after install."
    exit 1
fi
echo "[*] Found binary at: $BIN_PATH"

# Step 4: Fill in Exec= in .desktop file
DESKTOP_SRC="game_data/AFTN.desktop"
DESKTOP_TMP="/tmp/AFTN.desktop"
sed "s|Exec=FILL_IN_EXEC|Exec=$BIN_PATH|" "$DESKTOP_SRC" > "$DESKTOP_TMP"

# Step 5: Copy to system applications directory
echo "[*] Installing .desktop file..."
sudo install -Dm644 "$DESKTOP_TMP" /usr/share/applications/AFTN.desktop

# Step 6: Copy icon to system icons
sudo install -Dm644 game_data/icon.png /usr/share/icons/hicolor/512x512/apps/aftn.png
sudo gtk-update-icon-cache /usr/share/icons/hicolor

echo "[*] Done! You can now search for 'AFTN' in your desktop menu."

