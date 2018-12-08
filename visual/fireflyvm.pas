unit fireflyvm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  baustellenproblem,
  jsonproblemparser;

type

  TListGrid = specialize  TFPGObjectList<TStringList>;

  { INotifiable }

  INotifiable = interface
    procedure Notify(AMsg: string);
  end;

  { TFireflyViewModel }

  TFireflyViewModel = class
  private
    FProblem: TBaustellenProblem;
    FView: INotifiable;
  public
    constructor Create(AFileName: string; AView: INotifiable);
    function GetGrid: TListGrid;
    function GetM: integer;
    function GetN: integer;
    destructor Destroy; override;
  end;

const
  MSG_PLEASE_UPDATE_GUI = 'MSG_PLEASE_UPDATE_GUI';

implementation

{ TFireflyViewModel }

constructor TFireflyViewModel.Create(AFileName: string; AView: INotifiable);
begin
  FProblem := TJSONBaustellenProblemParser.LoadProblem(AFileName);
  FView := AView;
end;

function TFireflyViewModel.GetGrid: TListGrid;
var
  LRow: TStringList;
  i, j: integer;
begin
  Result := TListGrid.Create;
  for i := 0 to FProblem.F.m - 1 do
  begin
    LRow := TStringList.Create;
    for j := 0 to FProblem.F.n - 1 do
    begin
      if FProblem.F.Cells[i, j] = atBahnhof then
        LRow.Add('Bahnhof');
      if FProblem.F.Cells[i, j] = atImbiss then
        LRow.Add('Imbiss');
      if FProblem.F.Cells[i, j] = at0 then
        LRow.Add('');
      if FProblem.F.Cells[i, j] = atHaus then
        LRow.Add('Haus');
    end;
    Result.Add(LRow);
  end;
end;

function TFireflyViewModel.GetM: integer;
begin
  Result := FProblem.F.m;
end;

function TFireflyViewModel.GetN: integer;
begin
  Result := FProblem.F.n;
end;

destructor TFireflyViewModel.Destroy;
begin
  FreeAndNil(FProblem);
  FView := nil;
  inherited Destroy;
end;

end.


