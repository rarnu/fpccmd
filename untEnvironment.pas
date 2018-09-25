unit untEnvironment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crt, registry, fphttpclient, fpjson, jsonparser, jsonscanner, untVersion, untUpdate {$IFNDEF WINDOWS}, BaseUnix{$ENDIF};

procedure setSource(src: string);
function getSource(): string;
function getVersion(): string;
procedure updateSelf();

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

function getVersion(): string;
begin
  Exit(Format('%s[%d]', [VERSION_NAME, VERSION_CODE]));
end;

procedure updateSelf();
const
  PLATFORM = {$IFDEF WINDOWS}'W'{$ELSE}{$IFDEF DARWIN}'M'{$ELSE}'L'{$ENDIF}{$ENDIF};
  DOWNLOAD_FILE = PLATFORM + 'DOWNLOAD';
var
  jsonstr: string;
  url: string;
  json: TJSONObject = nil;
  parser: TJSONParser = nil;
  lastversion: Integer = 0;
  localFileName: string;
  de: TDownloadProcess;
  downloadFilename: string = '';
  {$IFNDEF WINDOWS}
  uid: LongWord;
  {$ENDIF}
begin
  {$IFNDEF WINDOWS}
  uid := FpGetuid;
  if (uid <> 0) then begin
    WriteLn('Install of iOS environment requires sudo.');
    Exit;
  end;
  {$ENDIF}
  url := getSource();
  with TFPHTTPClient.Create(nil) do begin
    try
      jsonstr := Get(url + 'version.json');
    except
    end;
    Free;
  end;
  try
    try
      parser := TJSONParser.Create(jsonstr, [joUTF8]);
      json := TJSONObject(parser.Parse);
      lastversion:= json.Integers[PLATFORM];
      downloadFilename := json.Strings[DOWNLOAD_FILE];
    except
    end;
  finally
    if (parser <> nil) then parser.Free;
    if (json <> nil) then json.Free;
  end;
  if (lastversion > VERSION_CODE) and (downloadFilename <> '') then begin
    {$IFDEF WINDOWS}
    localFileName:= ExtractFilePath(ParamStr(1));
    {$ELSE}
    localFileName:= GetEnvironmentVariable('HOME') + '/.config/fpccmd/';
    {$ENDIF}
    ForceDirectories(localFileName);
    SetCurrentDir(localFileName);
    localFileName += downloadFilename;
    if (FileExists(localFileName)) then DeleteFile(localFileName);
    de := TDownloadProcess.Create;
    with TFPHTTPClient.Create(nil) do begin
      OnDataReceived:= @de.onData;
      try
        Get(url + downloadFilename, localFileName {$IFDEF WINDOWS} + '.new'{$ENDIF});
      except
        on e: Exception do begin
          WriteLn();
          WriteLn('Download error => ' + e.Message);
        end;
      end;
      Free;
    end;
    de.Free;
    doUpdate(url, localFileName);
  end else begin
    WriteLn('You are already installed the last version.');
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

