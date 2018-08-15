unit untCmdMac;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, untCfg, untCompile, process;

procedure doCompileMac(pt: String; mainFile: string);
procedure doCompileMacUI(mainFile: string; uiType: string);
procedure doCreateMacBundle(mainFile: string);

implementation

const
  ROOT = '/usr/local/codetyphon';
  TYPHON = ROOT + '/typhon';
  FPC32 = ROOT + '/fpc/fpc32/bin/i386-darwin/fpc';
  FPC64 = ROOT + '/fpc/fpc64/bin/x86_64-darwin/fpc';
  MACSDK = '/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib';

  FPC_ROOT = '/usr/local/lib/fpc/3.1.1';
  FPC_A64 = FPC_ROOT + '/ppcrossa64';
  FPC_ARM = FPC_ROOT + '/ppcrossarm';
  FPC_X64 = FPC_ROOT + '/ppcx64';
  FPC_X86 = FPC_ROOT + '/ppc386';

procedure compileIOSSub(cmd: string; target: string; lib: string; mainFile: string; surfix: string);
var
  plist: TStringList;
begin
  plist := getCfg(mainFile, lib);
  plist.Add('-T' + target);
  plist.Add('-Cn');
  plist.Add('-dIPHONEALL');
  compile(cmd, plist, mainFile);
  plist.Free;
  combine('link.res', mainFile, surfix);
end;

procedure compileIOS(mainFile: string);
begin
  compileIOSSub(FPC_A64, 'darwin', 'lib/aarch64-ios', mainFile, 'A64');
  compileIOSSub(FPC_ARM, 'darwin', 'lib/arm-ios', mainFile, 'ARM');
  compileIOSSub(FPC_X64, 'iphonesim', 'lib/x86_64-ios', mainFile, 'X64');
  compileIOSSub(FPC_X86, 'iphonesim', 'lib/i386-ios', mainFile, 'X86');
  combineAll(mainFile, ['A64', 'ARM', 'X64', 'X86']);
end;

procedure compileMac(mainFile: string);
const
  LIB = 'lib/x86_64-darwin';
var
  cmd: string = FPC64;
  plist: TStringList;
begin
  plist := getCfg(mainFile, LIB, True);
  plist.Add('-Fu' + TYPHON + '/packager/units/x86_64-darwin');
  plist.Add('-Fl' + MACSDK);
  compile(cmd, plist, mainFile);
  plist.Free;
end;

procedure doCompileMac(pt: String; mainFile: string);
begin
  if (pt = 'M') then begin
    compileMac(mainFile);
  end else if (pt = 'I') then begin
    compileIOS(mainFile);
  end;
end;

procedure doCompileMacUI(mainFile: string; uiType: string);
var
  LIB: string;
  cmd: string;
  plist: TStringList;
begin
  if (uiType = 'O') then begin
    LIB := 'lib/i386-darwin';
    cmd := FPC32;
    plist := getCfg(mainFile, LIB, True);
    plist.Add('-k-framework');
    plist.Add('-kCarbon');
    plist.Add('-k-framework');
    plist.Add('-kOpenGL');
    plist.Add('-k-dylib_file');
    plist.Add('-k/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib');
    plist.Add('-Fu' + TYPHON + '/components/pl_ORCA/lib/i386-darwin/carbon');
    plist.Add('-Fu' + TYPHON + '/components/BaseControls/lib/i386-darwin/carbon');
    plist.Add('-Fu' + TYPHON + '/lcl/units/i386-darwin/carbon');
    plist.Add('-Fu' + TYPHON + '/lcl/units/i386-darwin');
    plist.Add('-Fu' + TYPHON + '/components/BaseUtils/lib/i386-darwin');
    plist.Add('-Fu' + TYPHON + '/packager/units/i386-darwin');
    plist.Add('-dLCL');
    plist.Add('-dLCLcarbon');
  end else begin
    LIB := 'lib/x86_64-darwin';
    cmd := FPC64;
    plist := getCfg(mainFile, LIB, True);
    plist.Add('-k-framework ');
    plist.Add('-kCocoa');
    plist.Add('-Fu' + TYPHON + '/lcl/units/x86_64-darwin/cocoa');
    plist.Add('-Fu' + TYPHON + '/lcl/units/x86_64-darwin');
    plist.Add('-Fu' + TYPHON + '/components/BaseUtils/lib/x86_64-darwin');
    plist.Add('-Fu' + TYPHON + '/packager/units/x86_64-darwin');
    plist.Add('-dLCL');
    plist.Add('-dLCLcocoa');
  end;
  compile(cmd, plist, mainFile);
  plist.Free;
end;

procedure doCreateMacBundle(mainFile: string);
var
  proj: string;
  path: string;
  outstr: string;

  procedure makePkgInfo(filepath: string);
  begin
    with TStringList.Create do begin
      Text:= 'APPL????'#10;
      SaveToFile(filepath);
      Free;
    end;
  end;

  procedure makeInfoPlist(filepath: string; projname: string);
  begin
    with TStringList.Create do begin
      Clear;
      Add('<?xml version="1.0" encoding="UTF-8"?>');
      Add('<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">');
      Add('<plist version="1.0">');
      Add('<dict>');
      Add('    <key>CFBundleDevelopmentRegion</key>');
      Add('    <string>English</string>');
      Add('    <key>CFBundleExecutable</key>');
      Add(Format('    <string>%s</string>', [projname]));
      Add('    <key>CFBundleName</key>');
      Add(Format('    <string>%s</string>', [projname]));
      Add('    <key>CFBundleIdentifier</key>');
      Add(Format('    <string>com.rarnu.%s</string>', [projname]));
      Add('    <key>CFBundleInfoDictionaryVersion</key>');
      Add('    <string>6.0</string>');
      Add('    <key>CFBundlePackageType</key>');
      Add('    <string>APPL</string>');
      Add('    <key>CFBundleSignature</key>');
      Add('    <string>smal</string>');
      Add('    <key>CFBundleShortVersionString</key>');
      Add('    <string>1.0</string>');
      Add('    <key>CFBundleVersion</key>');
      Add('    <string>1</string>');
      Add('    <key>CSResourcesFileMapped</key>');
      Add('    <true/>');
      Add('    <key>CFBundleDocumentTypes</key>');
      Add('    <array>');
      Add('        <dict>');
      Add('            <key>CFBundleTypeRole</key>');
      Add('            <string>Viewer</string>');
      Add('            <key>CFBundleTypeExtensions</key>');
      Add('            <array>');
      Add('                <string>*</string>');
      Add('            </array>');
      Add('            <key>CFBundleTypeOSTypes</key>');
      Add('            <array>');
      Add('                <string>fold</string>');
      Add('                <string>disk</string>');
      Add('                <string>****</string>');
      Add('            </array>');
      Add('        </dict>');
      Add('    </array>');
      Add('    <key>CFBundleIconFile</key>');
      Add('    <string>icon.icns</string>');
      Add('    <key>NSHighResolutionCapable</key>');
      Add('    <true/>');
      Add('</dict>');
      Add('</plist>');
      SaveToFile(filepath);
      Free;
    end;
  end;

begin
  path := GetCurrentDir + '/';
  // create mac bundle
  proj:= ExtractFileName(mainFile);
  proj:= proj.Substring(0, proj.LastIndexOf('.'));
  ForceDirectories(path + proj + '.app/Contents/MacOS');
  ForceDirectories(path + proj + '.app/Contents/Resources');
  makePkgInfo(path + proj + '.app/Contents/PkgInfo');
  makeInfoPlist(path + proj + '.app/Contents/Info.plist', proj);
  RunCommand('cp', [path + 'icon.icns', path + proj + '.app/Contents/Resources/icon.icns'], outstr, [poWaitOnExit, poUsePipes, poStderrToOutPut]);
  RunCommand('mv', [path + proj, path + proj + '.app/Contents/MacOS/' + proj], outstr, [poWaitOnExit, poUsePipes, poStderrToOutPut]);
end;

end.

