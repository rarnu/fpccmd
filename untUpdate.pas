unit untUpdate;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}windows, {$ENDIF}Classes, SysUtils, untCompile {$IFDEF WINDOWS}, fphttpclient{$ENDIF};

procedure doUpdate(source: string; filename: string);

implementation

{$IFDEF WINDOWS}
procedure downloadUpdater(source: string);
var
  localFile: string;
begin
  localFile:= ExtractFilePath(ParamStr(1)) + 'updater.exe';
  if (FileExists(localFile)) then begin
    DeleteFile(localFile);
  end;
  with TFPHTTPClient.Create(nil) do begin
    try
      Get(source + 'updater.exe', localFile);
    except
      on E: Exception do begin
        WriteLn('Error => ' + E.Message);
      end;
    end;
    Free;
  end;
end;

procedure runUpdater();
var
  updaterPath: string;
begin
  updaterPath:= ExtractFilePath(ParamStr(1)) + 'updater.exe';
  ShellExecute(0, 'open', PChar(updaterPath), nil, nil, 0);
end;

{$ENDIF}

procedure doUpdate(source: string; filename: string);
var
  originPath: string;
  params: TStringList;
begin
  originPath:= ParamStr(1);
  {$IFDEF WINDOWS}
  downloadUpdater(source);
  runUpdater();
  {$ELSE}
  params := TStringList.Create;
  params.Add('-f');
  params.Add(filename);
  params.Add(originPath);
  params.Free;
  innerRunCmd('/bin/cp', params);
  {$ENDIF}
end;

end.

