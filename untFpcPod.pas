unit untFpcPod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, XMLRead, XMLWrite, DOM;

procedure moduleInitDep();
procedure moduleInitSpec();
procedure moduleGet(AName: string);

implementation

uses
  untCompile;

procedure moduleInitDep();
begin
  // init dep
  with TStringList.Create do begin
    Add('# add source url of the module');
    Add('# and then add module name and version tag');
    Add('#');
    Add('# for example:');
    Add('# source=https://github.com/rarnu/');
    Add('# myct,1.0');
    Add('#');
    Add('# and then fpccmd will pull "myct" with tag "1.0" to your project and change the project search path.');
    Add('# source and module can be appeared many times if you want to use the different sources of modules.');
    Add('');
    SaveToFile(GetCurrentDir + DirectorySeparator + 'fpcdep.spec');
    Free;
  end;
end;

procedure moduleInitSpec();
begin
  // init spec
  with TStringList.Create do begin
    Add('# url for clone module code, like "url=https://github.com/rarnu/myct.git"');
    Add('url=');
    Add('# version code of module, must be a digit');
    Add('version_code=');
    Add('# git tag of module, fpccmd will checkout the code with the tag');
    Add('version_tag=');
    Add('# module author name');
    Add('author=');
    Add('# module name for manage in project');
    Add('module_name=');
    Add('# unit search path for the module, split by ";"');
    Add('unit_dirs=');
    Add('# include search path for the module, split by ";"');
    Add('include_dirs=');
    Add('# library preference path for the module, split by ";"');
    Add('library_dirs=');
    SaveToFile(GetCurrentDir + DirectorySeparator + 'fpcmodule.spec');
    Free;
  end;
end;

function findProjectFile(): string;
var
  src: TSearchRec;
begin
  Result := '';
  if (FindFirst(GetCurrentDir + DirectorySeparator + '*.ctpr', faAnyFile, src) = 0) then begin
    Result := GetCurrentDir + DirectorySeparator + src.Name;
    FindClose(src);
  end;
end;

procedure mergeGitIgnore(AModuleName: string);
var
  p: string;
  sl: TStringList;
  changed: Boolean = False;
begin
  p := GetCurrentDir + DirectorySeparator + '.gitignore';
  sl := TStringList.Create;
  if (FileExists(p)) then begin
    sl.LoadFromFile(p);
  end;
  if (sl.IndexOf(AModuleName + '/') = -1) then begin
    sl.Add(AModuleName + '/');
    changed:= True;
  end;
  if (changed) then begin
    sl.SaveToFile(p);
  end;
  sl.Free;
end;

procedure modifyProjectFile(AModuleName: string);
var
  proj: string;
  dom: TXMLDocument;
  nBase: TDOMNode;
  nReqBase: TDOMNode;
  nInc, nUnit, nLib, nReq: TDOMNode;

  aUnit, aInc, aLib, aReq: TStringArray;

  function trimArray(arr: TStringArray): TStringArray;
  var
    i: Integer;
    len: Integer = 0;
  begin
    SetLength(Result, 0);
    for i := 0 to Length(arr) - 1 do begin
      if (arr[i].Trim <> '') then begin
        Inc(len);
        SetLength(Result, len);
        Result[len - 1] := arr[i].Trim;
      end;
    end;
  end;

  procedure loadSpec(AModuleName: string; var arrUnit, arrInc, arrLib, arrReq: TStringArray);
  var
    path: string;
    tmp: string;
  begin
    path := GetCurrentDir + DirectorySeparator + AModuleName + DirectorySeparator + 'fpcmodule.spec';
    if (not FileExists(path)) then begin
      SetLength(arrUnit, 0);
      SetLength(arrInc, 0);
      SetLength(arrLib, 0);
    end else begin
      with TStringList.Create do begin
        LoadFromFile(path);
        tmp := Values['unit_dirs'];
        arrUnit := trimArray(tmp.Split(';'));
        tmp := Values['include_dirs'];
        arrInc := trimArray(tmp.Split(';'));
        tmp := Values['library_dirs'];
        arrLib := trimArray(tmp.Split(';'));
        tmp := Values['required_package'];
        arrReq := trimArray(tmp.Split(';'));
        Free;
      end;
    end;
  end;

  function createNodeValue(m: string; a: TStringArray): widestring;
  var
    ret: string = '';
    s: string;
  begin
    for s in a do begin
      ret += m + '/' + s + ';';
    end;
    ret := ret.Trim([';']);
    Exit(widestring(ret));
  end;

  function mergeNodeValue(n: TDOMNode; m: string; a: TStringArray): widestring;
  var
    tmp: string;
    s: string;
    i: Integer;
    ret: string = '';
  begin
    tmp := string(n.Attributes.GetNamedItem('Value').NodeValue);
    with TStringList.Create do begin
      Delimiter:= ';';
      DelimitedText:= tmp;
      for s in a do begin
        if (IndexOf(m + '/' + s) = -1) then Add(m + '/' + s);
      end;
      for i := 0 to Count - 1 do begin
        ret += Strings[i] + ';';
      end;
      ret := ret.Trim([';']);
      Free;
    end;
    Exit(widestring(ret));
  end;

  procedure mergeReq(x: TXMLDocument; n: TDOMNode; a: TStringArray);
  var
    currentList: TStringList;
    i: Integer;
    item: TDOMNode;
    pkg: TDOMNode;
  begin
    currentList := TStringList.Create;
    for i := 0 to n.ChildNodes.Count - 1 do begin
      currentList.Add(string(n.ChildNodes[i].FirstChild.Attributes.GetNamedItem('Value').NodeValue));
    end;
    // rmeove all node
    for i := n.ChildNodes.Count - 1 downto 0 do begin
      n.DetachChild(n.ChildNodes[i]);
    end;

    for i := 0 to Length(a) - 1 do begin
      if (currentList.IndexOf(a[i]) = -1) then begin
        currentList.Add(a[i]);
      end;
    end;

    TDOMElement(n).SetAttribute('Count', widestring(currentList.Count.ToString));

    for i:= 0 to currentList.Count - 1 do begin
      item := x.CreateElement(widestring(Format('Item%d', [i + 1])));
      pkg := x.CreateElement('PackageName');
      TDOMElement(pkg).SetAttribute('Value', widestring(currentList[i]));
      item.AppendChild(pkg);
      n.AppendChild(item);
    end;
    currentList.Free;
  end;

begin
  proj:= findProjectFile();
  if (proj = '') then begin
    WriteLn('Project file not found, stopped.');
    Exit;
  end;
  WriteLn('Start modify project ' + proj);

  // modify project file
  ReadXMLFile(dom, proj);
  nBase := dom.DocumentElement.FindNode('CompilerOptions').FindNode('SearchPaths');
  nInc := nBase.FindNode('IncludeFiles');
  nUnit := nBase.FindNode('OtherUnitFiles');
  nLib := nBase.FindNode('Libraries');

  loadSpec(AModuleName, aUnit, aInc, aLib, aReq);
  if (nInc = nil) then begin
    nInc := dom.CreateElement('IncludeFiles');
    TDOMElement(nInc).SetAttribute('Value', createNodeValue(AModuleName, aInc));
    nBase.AppendChild(nInc);
  end else begin
    // merge inc
    TDOMElement(nInc).SetAttribute('Value', mergeNodeValue(nInc, AModuleName, aInc));
  end;
  if (nUnit = nil) then begin
    nUnit := dom.CreateElement('OtherUnitFiles');
    TDOMElement(nUnit).SetAttribute('Value', createNodeValue(AModuleName, aUnit));
    nBase.AppendChild(nUnit);
  end else begin
    // metge unit
    TDOMElement(nUnit).SetAttribute('Value', mergeNodeValue(nUnit, AModuleName, aUnit));
  end;
  if (nLib = nil) then begin
    nLib := dom.CreateElement('Libraries');
    TDOMElement(nLib).SetAttribute('Value', createNodeValue(AModuleName, aLib));
    nBase.AppendChild(nLib);
  end else begin
    // merge lib
    TDOMElement(nLib).SetAttribute('Value', mergeNodeValue(nLib, AModuleName, aLib));
  end;

  // modify required packages
  nReqBase := dom.DocumentElement.FindNode('ProjectOptions');
  nReq := nReqBase.FindNode('RequiredPackages');
  if (nReq = nil) then begin
    nReq := dom.CreateElement('RequiredPackages');
    mergeReq(dom, nReq, aReq);
    nBase.AppendChild(nReq);
  end else begin
    mergeReq(dom, nReq, aReq);
  end;

  WriteXMLFile(dom, proj);
  dom.Free;
  WriteLn('Modified project');
end;

procedure moduleGet(AName: string);
var
  path: string;
  sl: TStringList;
  i: Integer;
  str: string;
  src: string = 'https://github.com/rarnu/';
  sarr: TStringArray;
  params: TStringList;
  gitPath: string;
begin
  // get
  path := GetCurrentDir + DirectorySeparator + 'fpcdep.spec';
  if (not FileExists(path)) then begin
    WriteLn('fpcdep.spec not found, please execute initdep.');
    Exit;
  end;
  sl := TStringList.Create;
  sl.LoadFromFile(path);
  for i := 0 to sl.Count - 1 do begin
    str := sl[i].Trim;
    if (str = '') or (str.StartsWith('#')) then begin
      Continue;
    end;
    if (str.StartsWith('source=')) then begin
      src := str.Replace('source=', '');
      if (not src.EndsWith(DirectorySeparator)) then begin
        src += DirectorySeparator;
      end;
      Continue;
    end;
    if (str.Contains(',')) then begin
      sarr := str.Split(',');
      if (AName <> '') and (sarr[0].Trim <> AName) then Continue;
      WriteLn(Format('Cleaning module %s', [sarr[0]]));
      params := TStringList.Create;
      params.Add('-f');
      params.Add('-r');
      params.Add(GetCurrentDir + DirectorySeparator + sarr[0].Trim);
      innerRunCmd('rm', params);
      params.Free;
      WriteLn(Format('Cloning module %s with tag %s', [sarr[0], sarr[1]]));
      params := TStringList.Create;
      params.Add('clone');
      params.Add('--branch');
      params.Add(sarr[1].Trim);
      params.Add(src + sarr[0].Trim);
      innerRunCmd('git', params);
      params.Free;
      gitPath:= GetCurrentDir + DirectorySeparator + sarr[0].Trim + DirectorySeparator + '.git';
      params := TStringList.Create;
      params.Add('-f');
      params.Add('-r');
      params.Add(gitPath);
      innerRunCmd('rm', params);
      params.Free;
      modifyProjectFile(sarr[0]);
      mergeGitIgnore(sarr[0]);
    end else begin
      WriteLn(Format('no tag for %s, skipped.', [str]));
    end;
  end;
  sl.Free;
end;

end.

