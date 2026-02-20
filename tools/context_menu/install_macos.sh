#!/usr/bin/env bash
# install_macos.sh
# Installs blackvideo-player + creates a macOS Quick Action (right-click service)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_EXE="$SCRIPT_DIR/../../build/blackvideo-player"
INSTALL_DIR="/usr/local/bin"
SERVICES_DIR="$HOME/Library/Services"
SERVICE_NAME="Open with BlackVideo Player.workflow"

echo "============================================================"
echo " BlackVideo Player — macOS Install + Right-click Service"
echo "============================================================"

if [[ ! -f "$BUILD_EXE" ]]; then
    echo "ERROR: blackvideo-player not found at $BUILD_EXE"
    echo "Build it first:  ./scripts/build.sh"
    exit 1
fi

# ── Install binary ───────────────────────────────────────────────────────────
echo "Installing binary to $INSTALL_DIR ..."
sudo cp "$BUILD_EXE" "$INSTALL_DIR/blackvideo-player"
sudo chmod 755 "$INSTALL_DIR/blackvideo-player"

# ── Create Automator Quick Action (workflow) ─────────────────────────────────
echo "Creating Quick Action service ..."
mkdir -p "$SERVICES_DIR/$SERVICE_NAME/Contents"

cat > "$SERVICES_DIR/$SERVICE_NAME/Contents/Info.plist" << 'EOF'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>NSServices</key>
    <array>
        <dict>
            <key>NSMenuItem</key>
            <dict>
                <key>default</key>
                <string>Open with BlackVideo Player</string>
            </dict>
            <key>NSMessage</key>
            <string>runWorkflowAsService</string>
            <key>NSSendFileTypes</key>
            <array>
                <string>public.movie</string>
                <string>public.mpeg-4</string>
                <string>com.apple.quicktime-movie</string>
                <string>public.avi</string>
                <string>org.webmproject.webm</string>
            </array>
        </dict>
    </array>
</dict>
</plist>
EOF

cat > "$SERVICES_DIR/$SERVICE_NAME/Contents/document.wflow" << 'WFLOW'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>AMApplicationBuild</key>
    <string>523</string>
    <key>AMApplicationVersion</key>
    <string>2.10</string>
    <key>AMDocumentVersion</key>
    <string>2</string>
    <key>actions</key>
    <array>
        <dict>
            <key>action</key>
            <dict>
                <key>AMAccepts</key>
                <dict>
                    <key>Container</key>
                    <string>List</string>
                    <key>Optional</key>
                    <true/>
                    <key>Types</key>
                    <array><string>com.apple.cocoa.path</string></array>
                </dict>
                <key>AMActionVersion</key>
                <string>2.0.3</string>
                <key>AMApplication</key>
                <array><string>Automator</string></array>
                <key>AMParameterProperties</key>
                <dict>
                    <key>COMMAND_STRING</key>
                    <dict/>
                    <key>shell</key>
                    <dict/>
                    <key>source</key>
                    <dict/>
                </dict>
                <key>AMProvides</key>
                <dict>
                    <key>Container</key>
                    <string>List</string>
                    <key>Types</key>
                    <array><string>com.apple.cocoa.path</string></array>
                </dict>
                <key>ActionBundlePath</key>
                <string>/System/Library/Automator/Run Shell Script.action</string>
                <key>ActionName</key>
                <string>Run Shell Script</string>
                <key>ActionParameters</key>
                <dict>
                    <key>COMMAND_STRING</key>
                    <string>for f in "$@"; do
    /usr/local/bin/blackvideo-player "$f" &amp;
done</string>
                    <key>shell</key>
                    <string>/bin/bash</string>
                    <key>source</key>
                    <string>0</string>
                </dict>
                <key>BundleIdentifier</key>
                <string>com.apple.RunShellScript</string>
                <key>CFBundleVersion</key>
                <string>2.0.3</string>
                <key>CanShowSelectedItemsWhenRun</key>
                <false/>
                <key>CanShowWhenRun</key>
                <true/>
                <key>Class Name</key>
                <string>RunShellScriptAction</string>
                <key>InputUUID</key>
                <string>98D3AC81-B5C1-4B57-B8C7-93D04B64D7D3</string>
                <key>Keywords</key>
                <array>
                    <string>Shell</string>
                    <string>Script</string>
                    <string>Command</string>
                    <string>Run</string>
                    <string>Unix</string>
                </array>
                <key>OutputUUID</key>
                <string>4C2E73E1-0F7A-4A33-9D0E-3FEB46C4B2BB</string>
                <key>UUID</key>
                <string>82A7B0B8-57D8-48A7-881A-E8A2FFCF0DB8</string>
                <key>UnlocalizedApplications</key>
                <array><string>Automator</string></array>
                <key>arguments</key>
                <dict>
                    <key>0</key>
                    <dict>
                        <key>default value</key>
                        <integer>0</integer>
                        <key>name</key>
                        <string>shell</string>
                        <key>required</key>
                        <string>0</string>
                        <key>type</key>
                        <string>0</string>
                        <key>uuid</key>
                        <string>0</string>
                    </dict>
                </dict>
                <key>isViewVisible</key>
                <true/>
                <key>location</key>
                <string>309.000000:253.000000</string>
                <key>nibPath</key>
                <string>/System/Library/Automator/Run Shell Script.action/Contents/Resources/English.lproj/main.nib</string>
            </dict>
            <key>isViewVisible</key>
            <true/>
        </dict>
    </array>
    <key>connectors</key>
    <dict/>
    <key>workflowMetaData</key>
    <dict>
        <key>serviceInputTypeIdentifier</key>
        <string>com.apple.Automator.fileSystemObject.movie</string>
        <key>serviceOutputTypeIdentifier</key>
        <string>com.apple.Automator.nothing</string>
        <key>serviceProcessesInput</key>
        <integer>0</integer>
        <key>workflowTypeIdentifier</key>
        <string>com.apple.Automator.servicesMenu</string>
    </dict>
</dict>
</plist>
WFLOW

# Reload services
/System/Library/CoreServices/pbs -flush 2>/dev/null || true

echo ""
echo "Done!"
echo "Right-click any video file → Services → 'Open with BlackVideo Player'"
echo ""
echo "If the service doesn't appear, go to:"
echo "  System Preferences → Keyboard → Shortcuts → Services"
echo "  and enable 'Open with BlackVideo Player'"
