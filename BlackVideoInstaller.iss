[Setup]
AppName=BlackVideo
AppVersion=1.1.0
DefaultDirName={pf}\BlackVideo
DefaultGroupName=BlackVideo
OutputBaseFilename=BlackVideo-Setup
UninstallDisplayIcon={app}\blackvideo-player.exe

[Files]
Source: "build\\blackvideo-player.exe"; DestDir: "{app}"; Flags: ignoreversion
; Remove the next line if no DLLs exist in build folder at compile time
Source: "build\\*.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "LICENSE.txt"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\BlackVideo"; Filename: "{app}\blackvideo-player.exe"
Name: "{commondesktop}\BlackVideo"; Filename: "{app}\blackvideo-player.exe"
