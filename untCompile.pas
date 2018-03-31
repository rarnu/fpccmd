unit untCompile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;


procedure compile(cmd: string; params: TStringList; mainFile: string);
procedure combine(linkFile: string; mainFile: string; surfix: string);
procedure combineAll(mainFile: string; surfixes: TStringArray);

implementation

const
  BUFSIZE = 1024;

procedure innerRunCmd(cmd: string; params: TStringList);
var
  p: TProcess;
  byteRead: LongInt;
  buf: array[0..BUFSIZE - 1] of Byte;
  outstr: string;
begin
  p := TProcess.Create(nil);
  p.Executable:= cmd;
  p.Parameters.Assign(params);
  p.Options:= [poUsePipes];
  p.Execute;
  while (True) do begin
    byteRead:= p.Output.Read(buf, BUFSIZE);
    if (byteRead = 0) then Break;
    SetLength(outstr, byteRead);
    Move(buf[0], outstr[1], byteRead);
    Write(outstr);
  end;
  p.Free;
end;

procedure compile(cmd: string; params: TStringList; mainFile: string);
begin
  params.Add(mainFile);
  innerRunCmd(cmd, params);
end;

procedure combine(linkFile: string; mainFile: string; surfix: string);
var
  params: TStringList;
  libName: string;
  s: string;
  linkList: TStringList;
begin
  libName:= ExtractFileName(mainFile);
  libName:= mainFile.Substring(0, mainFile.LastIndexOf('.'));
  libName:= Format('lib%s_%s.a', [libName, surfix]);

  params := TStringList.Create;
  params.Add('-q');
  params.Add(libName);
  linkList := TStringList.Create;
  linkList.LoadFromFile(linkFile);
  for s in linkList do begin
    if (s.EndsWith('.o')) then begin
      params.Add(s);
    end;
  end;
  linkList.Free;
  innerRunCmd('/usr/bin/ar', params);

  params.Clear;
  params.Add(libName);
  innerRunCmd('/usr/bin/ranlib', params);

  params.Free;
end;

procedure combineAll(mainFile: string; surfixes: TStringArray);
var
  libName: string;
  params: TStringList;
  s: string;
begin
  libName:= ExtractFileName(mainFile);
  libName:= mainFile.Substring(0, mainFile.LastIndexOf('.'));
  libName:= 'lib' + libName;

  params := TStringList.Create;
  params.Add('-create');
  for s in surfixes do begin
    params.Add(Format('%s_%s.a', [libName, s]));
  end;
  params.Add('-output');
  params.Add(libName + '.a');
  innerRunCmd('/usr/bin/lipo', params);
  params.Free;

  for s in surfixes do begin
    DeleteFile(Format('%s_%s.a', [libName, s]));
  end;
end;



end.



