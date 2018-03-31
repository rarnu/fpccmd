unit untCmdMac;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, untCfg, untCompile;

procedure doCompileMac(pt: String; mainFile: string);

implementation

const
  ROOT = '/usr/local/codetyphon';
  TYPHON = ROOT + '/typhon';
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
  plist := getCfg(mainFile, LIB);
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

end.

