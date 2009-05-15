; -- TeXmacs.iss --
; Inno Setup configuration file for the compilation of the 
; Windows TeXmacs installer

[Setup]
AppName=TeXmacs
AppVerName=TeXmacs version 1.0.7.2
DefaultDirName={pf}\TeXmacs
DefaultGroupName=TeXmacs
UninstallDisplayIcon={app}\TeXmacs.ico
OutputDir=..\..\..\distr
OutputBaseFilename=texmacs-installer
; SourceDir=../..

WizardImageFile=TeXmacs-large.bmp
WizardImageStretch=no
WizardSmallImageFile=TeXmacs-small.bmp

;PrivilegesRequired=none

[Files]
Source: ..\..\..\distr\TeXmacs-Windows\*; DestDir: {app}; Flags: recursesubdirs createallsubdirs
Source: TeXmacs.ico; DestDir: {app}

[Icons]
Name: "{group}\TeXmacs"; Filename: "{app}\bin\texmacs.exe"; IconFilename: "{app}\TeXmacs.ico"
Name: "{group}\Uninstall TeXmacs"; Filename: "{uninstallexe}"
Name: "{commondesktop}\TeXmacs"; Filename: "{app}\bin\texmacs.exe"; IconFilename: "{app}\TeXmacs.ico"
