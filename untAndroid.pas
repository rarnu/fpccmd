unit untAndroid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function findLastAndroidNDK(pt: string; binPath: string): string;

implementation

const
   SPLIT = {$IFDEF WINDOWS}'\'{$ELSE}'/'{$ENDIF};

function extractApiVersion(fn: string): Integer;
var
  tmp: string;
begin
  tmp := fn.Substring(fn.IndexOf('-api') + 4);
  tmp := tmp.Substring(0, tmp.IndexOf('-'));
  WriteLn(tmp);
  Exit(StrToIntDef(tmp, 0));
end;

function findLastAndroidNDK(pt: string; binPath: string): string;
var
  src: TSearchRec;
  current: string = '';
  currentVersion: Integer = 0;
  v: Integer;
  lib: string = 'arm';
begin
  if (pt = 'AI') then lib := 'i386'
  else if (pt = 'AM') then lib := 'mips';
  if (FindFirst(binPath +  SPLIT + 'android-*', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name = '.') or (src.Name = '..') then Continue;
      v := extractApiVersion(src.Name);
      if (v > currentVersion) then begin
        if (string(src.Name).EndsWith(lib)) then begin
          currentVersion:= v;
          current:= src.Name;
        end;
      end;
    until FindNext(src) <> 0;
    FindClose(src);
  end;
  Exit(current);
end;

end.

