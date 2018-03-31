unit untIOSEnvironment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt, registry, fphttpclient, untCompile {$IFNDEF WINDOWS}, BaseUnix{$ENDIF};

procedure setSource(src: string);
function getSource(): string;

{$IFDEF DARWIN}
procedure installIOSEnvironment();
{$ENDIF}

implementation

type
  { TDownloadProcess }

  TDownloadProcess = class
  public
    procedure onData(Sender: TObject; Const ContentLength, CurrentPos : Int64);
  end;

function isIOSEnvInstalled(): Boolean;
const
  FPCPATH = '/usr/local/lib/fpc/3.1.1';
begin
  Exit(DirectoryExists(FPCPATH));
end;

procedure downloadFpc();
var
  src: string;
  url: string;
  localFileName: string;
  de: TDownloadProcess;
begin
  src := getSource();
  url := src + 'fpc_ios.tar.gz';
  localFileName:= GetEnvironmentVariable('HOME') + '/.config/fpccmd/';
  ForceDirectories(localFileName);
  localFileName += 'fpc_ios.tar.gz';
  if (FileExists(localFileName)) then begin
    Exit;
  end;
  de := TDownloadProcess.Create;
  with TFPHTTPClient.Create(nil) do begin
    OnDataReceived:= @de.onData;
    try
      Get(url, localFileName);
    except
      on e: Exception do begin
        WriteLn();
        WriteLn('Download error => ' + e.Message);
      end;
    end;
    Free;
  end;
  de.Free;
  WriteLn('');
end;

procedure setSource(src: string);
begin
  if (src = '') then begin
    src := 'http://120.27.9.223/repo/';
  end;
  if (not src.EndsWith('/')) then begin
    src += '/';
  end;
  with TRegistry.Create do begin
    RootKey:= HKEY_CURRENT_USER;
    OpenKey('fpccmd', True);
    WriteString('source', src);
    CloseKey;
    Free;
  end;
end;

function getSource(): string;
begin
  Result := '';
  with TRegistry.Create do begin
    RootKey:= HKEY_CURRENT_USER;
    OpenKey('fpccmd', True);
    try
      result := ReadString('source');
    except
    end;
    CloseKey;
    Free;
  end;
  if (Result = '') then begin
    Result := 'http://120.27.9.223/repo/';
  end;
end;

{$IFDEF DARWIN}
procedure installIOSEnvironment();
const
  LIBFPC = '/usr/local/lib/fpc';
var
  confirm: Char;
  localFileName: string;
  params: TStringList;
  uid: LongWord;
begin
  uid := FpGetuid;
  if (uid <> 0) then begin
    WriteLn('Install of iOS environment requires sudo.');
    Exit;
  end;
  if (isIOSEnvInstalled()) then begin
    Write('pascal for iOS compiling is installed, reinstall it? [y/N]');
    confirm:= ReadKey;
    if (confirm <> 'Y') and (confirm <> 'y') then begin
      WriteLn('');
      Exit;
    end;
  end;
  WriteLn('');

  localFileName:= GetEnvironmentVariable('HOME') + '/.config/fpccmd/';
  ForceDirectories(localFileName);
  SetCurrentDir(localFileName);
  downloadFpc();

  if (DirectoryExists(localFileName + 'fpc')) then begin
    params := TStringList.Create;
    params.Add('-fr');
    params.Add(localFileName + 'fpc');
    innerRunCmd('/bin/rm', params);
    params.Free;
  end;

  WriteLn('extracting ...');
  params := TStringList.Create;
  params.Add('zxf');
  params.Add(localFileName + 'fpc_ios.tar.gz');
  innerRunCmd('/usr/bin/tar', params);
  params.Free;

  WriteLn('installing...');
  if (DirectoryExists(LIBFPC)) then begin
    params := TStringList.Create;
    params.Add('-fr');
    params.Add(LIBFPC);
    innerRunCmd('/bin/rm', params);
    params.Free;
  end;

  params := TStringList.Create;
  params.Add('fpc');
  params.Add('/usr/local/lib/');
  innerRunCmd('/bin/mv', params);
  params.Free;

  params := TStringList.Create;
  params.Add('-R');
  params.Add('root:admin');
  params.Add(LIBFPC);
  innerRunCmd('/usr/sbin/chown', params);
  params.Free;

end;

{$ENDIF}

{ TDownloadProcess }

procedure TDownloadProcess.onData(Sender: TObject; const ContentLength,
  CurrentPos: Int64);
begin
  Write(Format(#13'Downloading: %d / %d', [CurrentPos, ContentLength]));
end;

end.

