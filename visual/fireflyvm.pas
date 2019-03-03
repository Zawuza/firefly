unit fireflyvm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  baustellenproblem,
  jsonproblemparser,
  fireflyalgorithm,
  paretocalc;

type

  TListGrid = specialize  TFPGObjectList<TStringList>;

  { INotifiable }

  INotifiable = interface
    procedure Notify(AMsg: string);
  end;

  { TCoordinates }

  TCoordinates = class
    i, j: integer;
  end;

  TCoordinatesList = specialize TFPGObjectList<TCoordinates>;

  { TFireflyViewModel }

  TFireflyViewModel = class
  private
    FProblem: TBaustellenProblem;
    FView: INotifiable;
    FFirefly: TFireflyAlgorithm;
  public
    constructor Create(AFileName: string; AView: INotifiable);
    function GetGrid: TListGrid;
    function GetM: integer;
    function GetN: integer;
    function GetBestSolutionI: integer;
    function GetBestSolutionJ: integer;
    procedure Step;
    function CalcParetoOptimum: TCoordinatesList;
    destructor Destroy; override;
  end;

const
  MSG_PLEASE_UPDATE_GUI = 'MSG_PLEASE_UPDATE_GUI';

implementation

{ TFireflyViewModel }

constructor TFireflyViewModel.Create(AFileName: string; AView: INotifiable);
begin
  FProblem := TJSONBaustellenProblemParser.LoadProblem(AFileName);
  FFirefly := TFireflyAlgorithm.Create(FProblem);
  FView := AView;
end;

function TFireflyViewModel.GetGrid: TListGrid;
var
  LRow: TStringList;
  i, j: integer;
  LAgentPosition: TAgentPosition;
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
  for i := 0 to FFirefly.Positionen.Count - 1 do
  begin
    LAgentPosition := FFirefly.Positionen[i];
    Result[LAgentPosition.I][LAgentPosition.J] := FProblem.B[i].GetLabel;
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

function TFireflyViewModel.GetBestSolutionI: integer;
begin
  Result := FFirefly.BestSolution.I;
end;

function TFireflyViewModel.GetBestSolutionJ: integer;
begin
  Result := FFirefly.BestSolution.J;
end;

procedure TFireflyViewModel.Step;
begin
  FFirefly.Step;
  FView.Notify(MSG_PLEASE_UPDATE_GUI);
end;

function TFireflyViewModel.CalcParetoOptimum: TCoordinatesList;
var
  LCoord: TCoordinates;
  LParetoFront: TParetoFront;
  k: integer;
begin
  Result := TCoordinatesList.Create(True);
  LParetoFront := TParetoCalc.CalculateParetoFront(FProblem);
  for k := 0 to LParetoFront.Count - 1 do
  begin
    LCoord := TCoordinates.Create;
    LCoord.i := LParetoFront[k].i;
    LCoord.j := LParetoFront[k].j;
    Result.Add(LCoord);
  end;
  FreeAndNil(LParetoFront);
end;

destructor TFireflyViewModel.Destroy;
begin
  FreeAndNil(FProblem);
  FView := nil;
  inherited Destroy;
end;

end.

