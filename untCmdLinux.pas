unit untCmdLinux;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, untCfg, untCompile, untAndroid;

procedure doCompileLinux(pt: String; mainFile: string);
procedure doCompileLinuxUI(mainFile: string; uiType: string);

implementation

const
  ROOT = '/usr/local/codetyphon';
  TYPHON = ROOT + '/typhon';
  TYPHON_BINLIB = ROOT + '/binLibraries';
  FPC = ROOT + '/fpc/fpc64/bin/x86_64-linux/fpc';

procedure compileLinux(mainFile: string);
const
  LIB = 'lib/x86_64-linux';
var
  cmd: string = FPC;
  plist: TStringList;
begin
  plist := getCfg(mainFile, LIB, True);
  plist.Add('-Fu' + TYPHON + '/packager/units/x86_64-linux');
  compile(cmd, plist, mainFile);
  plist.Free;
end;

procedure compileAndroid(pt: string; mainFile: string);
var
  cmd: string = FPC;
  lib: string = 'lib/';
  plist: TStringList;
  ndk: string;
begin
  if (pt = 'AA') then lib += 'arm'
  else if (pt = 'AI') then lib += 'i386'
  else if (pt = 'AM') then lib += 'mips'
  else if (pt = 'A64') then lib := 'aarch64';
  lib += '-android';
  plist := getCfg(mainFile, LIB);
  plist.Add('-Tandroid');
  if (pt = 'AA') then plist.Add('-Parm')
  else if (pt = 'AI') then plist.Add('-Pi386')
  else if (pt = 'AM') then plist.Add('-Pmipsel')
  else if (pt = 'A64') then plist.Add('-Paarch64');
  ndk := findLastAndroidNDK(pt, TYPHON_BINLIB);
  plist.Add('-Fl' + TYPHON_BINLIB + '/' + ndk);
  compile(cmd, plist, mainFile);
  plist.Free;
end;

procedure doCompileLinux(pt: String; mainFile: string);
begin
  if (pt = 'L') then begin
    compileLinux(mainFile);
  end else begin
    compileAndroid(pt, mainFile);
  end;
end;

procedure doCompileLinuxUI(mainFile: string; uiType: string);
const
  LIB = 'lib/x86_64-linux';
var
  cmd: string = FPC;
  plist: TStringList;
begin
  plist := getCfg(mainFile, LIB, True);
  plist.Add('-Fu' + TYPHON + '/packager/units/x86_64-linux');
  plist.Add('-Fu' + TYPHON + '/lcl/units/x86_64-linux/gtk2');
  plist.Add('-Fu' + TYPHON + '/lcl/units/x86_64-linux');
  plist.Add('-Fu' + TYPHON + '/components/BaseUtils/lib/x86_64-linux');
  plist.Add('-Fu' + TYPHON + '/components/BaseControls/lib/x86_64-linux/gtk2');
  if (uiType = 'O') then begin
    plist.Add('-Fu' + TYPHON + '/components/pl_ORCA/lib/x86_64-linux/gtk2');
  end;
  plist.Add('-dLCL');
  plist.Add('-dLCLgtk2');
  compile(cmd, plist, mainFile);
  plist.Free;
end;

end.

