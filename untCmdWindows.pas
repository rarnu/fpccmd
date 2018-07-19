unit untCmdWindows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, untCfg, untCompile, untAndroid;

procedure doCompileWindows(pt: String; mainFile: string);
procedure doCompileWindowsUI(mainFile: string; uiType: string);

implementation

const
  ROOT = 'C:\codetyphon';
  TYPHON = ROOT + '\typhon';
  TYPHON_BINLIB = ROOT + '\binLibraries';
  FPC64 = ROOT + '\fpc\fpc64\bin\x86_64-win64\fpc.exe';
  FPC32 = ROOT + '\fpc\fpc32\bin\i386-win32\fpc.exe';

procedure compileAndroid(pt: string; mainFile: string);
var
  cmd: string = FPC32;
  lib: string = 'lib/';
  plist: TStringList;
  ndk: string;
begin
  if (pt = 'AA') then lib += 'arm'
  else if (pt = 'AI') then lib += 'i386'
  else if (pt = 'AM') then lib += 'mips';
  lib += '-android';
  plist := getCfg(mainFile, LIB);
  plist.Add('-Tandroid');
  if (pt = 'AA') then plist.Add('-Parm')
  else if (pt = 'AI') then plist.Add('-Pi386')
  else if (pt = 'AM') then plist.Add('-Pmipsel');
  ndk := findLastAndroidNDK(pt, TYPHON_BINLIB);
  plist.Add('-Fl' + TYPHON_BINLIB + '\' + ndk);
  compile(cmd, plist, mainFile);
  plist.Free;
end;

procedure compileWindows(mainFile: string);
const
  LIB = 'lib\x86_64-win64';
var
  cmd: string = FPC64;
  plist: TStringList;
begin
  plist := getCfg(mainFile, LIB);
  plist.Add('-Fu' + TYPHON + '\packager\units/x86_64-win64');
  compile(cmd, plist, mainFile);
  plist.Free;
end;

procedure doCompileWindows(pt: String; mainFile: string);
begin
  if (pt = 'W') then begin
    compileWindows(mainFile);
  end else begin
    compileAndroid(pt, mainFile);
  end;
end;

procedure doCompileWindowsUI(mainFile: string; uiType: string);
const
  LIB = 'lib\x86_64-win64';
var
  cmd: string = FPC64;
  plist: TStringList;
begin
  plist := getCfg(mainFile, LIB);
  plist.Add('-WG');
  plist.Add('-Fu' + TYPHON + '\packager\units\x86_64-win64');
  plist.Add('-Fu' + TYPHON + '\lcl\units\x86_64-win64\win32');
  plist.Add('-Fu' + TYPHON + '\lcl\units\x86_64-win64');
  plist.Add('-Fu' + TYPHON + '\components\BaseUtils\lib/x86_64-win64');
  plist.Add('-Fu' + TYPHON + '\components\BaseControls\lib/x86_64-win64/win32');
  if (uiType = 'O') then begin
    plist.Add('-Fu' + TYPHON + '\components\pl_ORCA\lib\x86_64-win64/win32');
    plist.Add('-Fu' + TYPHON + '\components\pl_Win_DirectX11\lib\x86_64-win64\win32');
    plist.Add('-Fu' + TYPHON + '\components\pl_Win_GDI\lib\x86_64-win64\win32');
    plist.Add('-Fu' + TYPHON + '\components\pl_Win_DirectX\lib\x86_64-win64\win32');
  end;
  plist.Add('-dLCL');
  plist.Add('-dLCLwin32');
  compile(cmd, plist, mainFile);
  plist.Free;
end;

end.

