unit logger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Log(AStr: string);

implementation

var
  LStrList: TStringList;

procedure Log(AStr: string);
begin
  LStrList.Add(AStr);
end;

initialization

  LStrList := TStringList.Create;

finalization

  LStrList.SaveToFile('log.log');
  FreeAndNil(LStrList);

end.
