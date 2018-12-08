unit jsonproblemparser;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  jsonparser,
  baustellenproblem;

type

  { TJSONBaustellenProblemParser }

  TJSONBaustellenProblemParser = class
  private
    class function ParseFeldSize(AJSON: TJSONObject): TFeld;
    class procedure ParseAnlagen(AJSONAnlagen: TJSONArray; AFeld: TFeld);
    class function ParseAgenten(AJSONAgenten: TJSONArray): TBaufirma;
  public
    class function LoadProblem(AFileName: string): TBaustellenProblem;
  end;

implementation

{ TJSONBaustellenProblemParser }

class function TJSONBaustellenProblemParser.ParseFeldSize(AJSON: TJSONObject): TFeld;
var
  m, n: TJSONIntegerNumber;
begin
  m := AJSON['m'] as TJSONIntegerNumber;
  n := AJSON['n'] as TJSONIntegerNumber;
  Result := TFeld.Create(m.AsInt64, n.AsInt64);
  Result.SetNulls;
end;

class procedure TJSONBaustellenProblemParser.ParseAnlagen(AJSONAnlagen: TJSONArray;
  AFeld: TFeld);
var
  variable: integer;
  LTyp: TJSONString;
  LAnlage: TJSONObject;
  i, j: TJSONIntegerNumber;
begin
  for variable := 0 to AJSONAnlagen.Count - 1 do
  begin
    LAnlage := AJSONAnlagen[variable] as TJSONObject;
    LTyp := LAnlage['typ'] as TJSONString;
    i := LAnlage['i'] as TJSONIntegerNumber;
    j := LAnlage['j'] as TJSONIntegerNumber;
    if LTyp.AsString = 'atBahnhof' then
      AFeld.Cells[i.AsInt64, j.AsInt64] := atBahnhof;
    if LTyp.AsString = 'atImbiss' then
      AFeld.Cells[i.AsInt64, j.AsInt64] := atImbiss;
    if LTyp.AsString = 'atHaus' then
      AFeld.Cells[i.AsInt64, j.AsInt64] := atHaus;
  end;
  AFeld.FinishInit;
end;

class function TJSONBaustellenProblemParser.ParseAgenten(
  AJSONAgenten: TJSONArray): TBaufirma;
var
  variable: integer;
  LAgent: TJSONString;
  LAgentInterface: IAgent;
begin
  Result := TBaufirma.Create;
  for variable := 0 to AJSONAgenten.Count - 1 do
  begin
    LAgent := AJSONAgenten[variable] as TJSONString;
    if LAgent.AsString = 'Handwerker' then
      LAgentInterface := THandwerker.Create;
    if LAgent.AsString = 'Landschaftsarchitekt' then
      LAgentInterface := TLandschaftsarchitekt.Create;
    if LAgent.AsString = 'Verkehrsspezialist' then
      LAgentInterface := TVerkehrsspezialist.Create;
    if LAgent.AsString = 'Werkstudent' then
      LAgentInterface := TWerkstudent.Create;
    if LAgent.AsString = 'Elektriker' then
      LAgentInterface := TElektriker.Create;
    Result.Add(LAgentInterface);
  end;
end;

class function TJSONBaustellenProblemParser.LoadProblem(AFileName:
  string): TBaustellenProblem;
var
  LFile: TStringList;
  LJSON: TJSONObject;
  LFeld: TFeld;
  LBaufirma: TBaufirma;
begin
  LFile := TStringList.Create;
  LFile.LoadFromFile(AFileName);
  LJSON := GetJSON(LFile.Text) as TJSONObject;
  LFeld := ParseFeldSize(LJSON);
  ParseAnlagen(LJSON['anlagen'] as TJSONArray, LFeld);
  LBaufirma := ParseAgenten(LJSON['agenten'] as TJSONArray);
  Result := TBaustellenProblem.Create(LFeld, LBaufirma);
  FreeAndNil(LFile);
end;

end.




