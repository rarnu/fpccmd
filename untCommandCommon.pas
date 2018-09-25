unit untCommandCommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

procedure commandsBefore(mainFile: string; platform: string; uitype: string);
procedure commandsAfter(mainFile: string; platform: string; uitype: string);

implementation

procedure internalRunLine(proj: string; platform: string; uitype: string; cmd: string);
var
  prefix: string;
  run: Boolean = True;
  outstr: string;
begin
  // check platform
  if (cmd.StartsWith('${ALL}')) then begin
    cmd := cmd.Replace('${ALL}', '');
  end else begin
    prefix := Format('${%s}', [platform]);
    if (cmd.StartsWith(prefix)) then begin
      cmd := cmd.Replace(prefix, '');
    end else begin
      run:= False;
    end;
  end;
  if (not run) then Exit;
  // check uitype
  if (cmd.StartsWith('${UNA}') and (uitype = '')) then begin
    cmd := cmd.Replace('${UNA}', '');
  end else if (cmd.StartsWith('${UALL}') and (uitype <> '')) then begin
    cmd := cmd.Replace('${UALL}', '');
  end else begin
    prefix:= Format('${U%s}', [uitype]);
    if (cmd.StartsWith(prefix)) then begin
      cmd := cmd.Replace(prefix, '');
    end else begin
      run := False;
    end;
  end;
  if (not run) then Exit;

  cmd := cmd.Trim;
  // translate macro
  cmd := cmd.Replace('${PATH}', GetCurrentDir);
  cmd := cmd.Replace('${PROJ}', proj);
  {$IFDEF WINDOWS}
  // translate command for windows
  // cp, mv, rm, mkdir
  cmd := cmd.Replace('/', '\');
  if (cmd.StartsWith('cp ')) then begin
    cmd := cmd.Replace('cp ', 'XCOPY /S /Y ');
  end;
  if (cmd.StartsWith('mv ')) then begin
    cmd := cmd.Replace('mv ', 'MOVE ');
  end;
  if (cmd.StartsWith('rm ')) then begin
    cmd := cmd.Replace('rm ', 'DEL /S /Q ');
  end;
  if (cmd.StartsWith('mkdir ')) then begin
    cmd := cmd.Replace('mkdir ', 'MD ');
  end;
  {$ELSE}
  if (cmd.StartsWith('cp ')) then begin
    cmd := cmd.Replace('cp ', 'cp -fr ');
  end;
  if (cmd.StartsWith('rm ')) then begin
    cmd := cmd.Replace('rm ', 'rm -fr ');
  end;
  if (cmd.StartsWith('mkdir ')) then begin
    cmd := cmd.Replace('mkdir ', 'mkdir -p ');
  end;
  {$ENDIF}
  {$Warnings off}
  RunCommandInDir(GetCurrentDir, cmd, outstr);
  {$Warnings on}
end;

procedure internalCommands(mainFile: string; platform: string; uitype:string; cmdFile: string);
var
  sl: TStringList;
  s: string;
  proj: string;
begin
  proj := ExtractFileName(mainFile);
  proj:= proj.Substring(0, proj.LastIndexOf('.'));
  sl := TStringList.Create;
  sl.LoadFromFile(cmdFile);
  for s  in sl do begin
    if (s.Trim <> '') then begin
      internalRunLine(proj, platform, uitype, s.Trim);
    end;
  end;
  sl.Free;
end;

procedure commandsBefore(mainFile: string; platform: string; uitype: string);
var
  filepath: string;
begin
  filepath:= GetCurrentDir + DirectorySeparator + 'fpccmd.before';
  if (FileExists(filepath)) then begin
    internalCommands(mainFile, platform, uitype, filepath);
  end;
end;

procedure commandsAfter(mainFile: string; platform: string; uitype: string);
var
  filepath: string;
begin
  filepath:= GetCurrentDir + DirectorySeparator + 'fpccmd.after';
  if (FileExists(filepath)) then begin
    internalCommands(mainFile, platform, uitype, filepath);
  end;
end;

end.

