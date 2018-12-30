unit fireflyalgorithm;

{$mode objfpc}{$H+}

interface

uses
  Math,
  Classes,
  SysUtils,
  fgl,
  baustellenproblem,
  logger;

type

  { TAgentPosition }

  TAgentPosition = class
  public
    I, J: integer;
  end;

  TPositionList = specialize TFPGList<TAgentPosition>;

  TNeighbours = (nLeft = 0, nTop = 1, nRight = 2, nBottom = 3);

  { TFireflyAlgorithm }

  TFireflyAlgorithm = class
  private
    FProblem: TBaustellenProblem;
    FPositionen: TPositionList;
    procedure MoveAToB(AIndexOfA: integer; AIndexOfB: integer);
    procedure MoveRandomly(AIndex: integer);
    function OneHasParetoDominatedSolutionOverTwo(AAgentIndex1,
      AAGentIndex2: integer): boolean;
    procedure MoveAllTwoStepsCloser;
    function CheckBounds(i, j: integer): boolean;
    function ManhattanDistanz(i1, j1, i2, j2: integer): integer;
  public
    constructor Create(AProblem: TBaustellenProblem); overload;
    constructor Create(AProblem: TBaustellenProblem; APositionen: TPositionList);
    property Positionen: TPositionList read FPositionen;
    procedure Step;
    destructor Destroy; override;
  end;

implementation

{ TFireflyAlgorithm }

procedure TFireflyAlgorithm.MoveAToB(AIndexOfA: integer; AIndexOfB: integer);
var
  LPositionOfA, LPositionOfB: TAgentPosition;
  LBestMove: TNeighbours;
  LMinDistance: integer;
  LDistance: integer;
begin
  LMinDistance := High(LMinDistance);
  LPositionOfA := FPositionen[AIndexOfA];
  LPositionOfB := FPositionen[AIndexOfB];
  //left
  if CheckBounds(LPositionOfA.I, LPositionOfA.J - 1) then
  begin
    LDistance := ManhattanDistanz(LPositionOfA.I, LPositionOfA.J -
      1, LPositionOfB.I, LPositionOfB.J);
    if LDistance < LMinDistance then
    begin
      LMinDistance := LDistance;
      LBestMove := nLeft;
    end;
  end;
  //top
  if CheckBounds(LPositionOfA.I - 1, LPositionOfA.J) then
  begin
    LDistance := ManhattanDistanz(LPositionOfA.I - 1, LPositionOfA.J,
      LPositionOfB.I, LPositionOfB.J);
    if LDistance < LMinDistance then
    begin
      LMinDistance := LDistance;
      LBestMove := nTop;
    end;
  end;
  //right
  if CheckBounds(LPositionOfA.I, LPositionOfA.J + 1) then
  begin
    LDistance := ManhattanDistanz(LPositionOfA.I, LPositionOfA.J +
      1, LPositionOfB.I, LPositionOfB.J);
    if LDistance < LMinDistance then
    begin
      LMinDistance := LDistance;
      LBestMove := nRight;
    end;
  end;
  //bottom
  if CheckBounds(LPositionOfA.I + 1, LPositionOfA.J) then
  begin
    LDistance := ManhattanDistanz(LPositionOfA.I + 1, LPositionOfA.J,
      LPositionOfB.I, LPositionOfB.J);
    if LDistance < LMinDistance then
    begin
      LMinDistance := LDistance;
      LBestMove := nBottom;
    end;
  end;

  //Movement
  case LBestMove of
    nLeft:
    begin
      LPositionOfA.I := LPositionOfA.I;
      LPositionOfA.J := LPositionOfA.J - 1;
    end;
    nTop:
    begin
      LPositionOfA.I := LPositionOfA.I - 1;
      LPositionOfA.J := LPositionOfA.J;
    end;
    nRight:
    begin
      LPositionOfA.I := LPositionOfA.I;
      LPositionOfA.J := LPositionOfA.J + 1;
    end;
    nBottom:
    begin
      LPositionOfA.I := LPositionOfA.I + 1;
      LPositionOfA.J := LPositionOfA.J;
    end;
  end;
end;

procedure TFireflyAlgorithm.MoveRandomly(AIndex: integer);
var
  LFinalDirection: 0..3;
  LDirections: array[0..3] of TNeighbours;
  LValues: array[0..3] of double;
  LPosition: TAgentPosition;
  LAgent: IAgent;
  i, j: integer;
  LValue: double;
  LDirection: TNeighbours;
  LRandomGennumber: 0..7;
  LIndex: integer;
begin
  //FPositionen[AIndex].I := 9;
  //FPositionen[AIndex].J := 15;

  LDirections[0] := nLeft;
  LDirections[1] := nTop;
  LDirections[2] := nRight;
  LDirections[3] := nBottom;

  LAgent := FProblem.B[AIndex];
  LPosition := FPositionen[AIndex];
  if CheckBounds(LPosition.I, LPosition.J - 1) then
    LValues[0] := LAgent.t(FProblem.F, LPosition.I, LPosition.J - 1)
  else
    LValues[0] := Infinity;
  if CheckBounds(LPosition.I - 1, LPosition.J) then
    LValues[1] := LAgent.t(FProblem.F, LPosition.I - 1, LPosition.J)
  else
    LValues[1] := Infinity;
  if CheckBounds(LPosition.I, LPosition.J + 1) then
    LValues[2] := LAgent.t(FProblem.F, LPosition.I, LPosition.J + 1)
  else
    LValues[2] := Infinity;
  if CheckBounds(LPosition.I + 1, LPosition.J) then
    LValues[3] := LAgent.t(FProblem.F, LPosition.I + 1, LPosition.J)
  else
    LValues[3] := Infinity;

  for i := 0 to 2 do
    for j := 0 to 2 do
      if LValues[j + 1] < LValues[j] then
      begin
        LValue := LValues[j + 1];
        LValues[j + 1] := LValues[j];
        LValues[j] := LValue;

        LDirection := LDirections[j + 1];
        LDirections[j + 1] := LDirections[j];
        LDirections[j] := LDirection;
      end;

  repeat
    LRandomGennumber := Random(8);
    case LRandomGennumber of
      0..2: LIndex := 0;
      3..4: LIndex := 1;
      5..6: LIndex := 2;
      7: LIndex := 3;
    end;
    LFinalDirection:= Integer(LDirections[LIndex]);
  until LValues[LIndex] < Infinity;

  case LFinalDirection of
    0:
    begin
      LPosition.I := LPosition.I;
      LPosition.J := LPosition.J - 1;
    end;
    1:
    begin
      LPosition.I := LPosition.I - 1;
      LPosition.J := LPosition.J;
    end;
    2:
    begin
      LPosition.I := LPosition.I;
      LPosition.J := LPosition.J + 1;
    end;
    3:
    begin
      LPosition.I := LPosition.I + 1;
      LPosition.J := LPosition.J;
    end;
  end;
end;

function TFireflyAlgorithm.OneHasParetoDominatedSolutionOverTwo(
  AAgentIndex1, AAGentIndex2: integer): boolean;
var
  i: integer;
  LAgent: IAgent;
  LParetoDominatedValue, LParetoDominatingValue: double;
begin
  Result := True;
  for i := 0 to FPositionen.Count - 1 do
  begin
    LAgent := FProblem.B[i];
    LParetoDominatingValue := LAgent.t(FProblem.F, FPositionen[AAgentIndex1].I,
      FPositionen[AAgentIndex1].J);
    LParetoDominatedValue := LAgent.t(FProblem.F, FPositionen[AAGentIndex2].I,
      FPositionen[AAGentIndex2].J);
    if LParetoDominatingValue >= LParetoDominatedValue then
    begin
      Result := False;
      exit;
    end;
  end;
end;

procedure TFireflyAlgorithm.MoveAllTwoStepsCloser;
var
  i, j: integer;
begin
  for i := 0 to FPositionen.Count - 1 do
    for j := 0 to FPositionen.Count - 1 do
    begin
      MoveAToB(i, j);
      MoveAToB(i, j);
    end;
end;

function TFireflyAlgorithm.CheckBounds(i, j: integer): boolean;
begin
  Result := (i >= 0) and (i < FProblem.F.m) and (j >= 0) and (j < FProblem.F.n);
end;

function TFireflyAlgorithm.ManhattanDistanz(i1, j1, i2, j2: integer): integer;
begin
  Result := abs(i1 - i2) + abs(j1 - j2);
end;

constructor TFireflyAlgorithm.Create(AProblem: TBaustellenProblem);
var
  LBaufirma: TBaufirma;
  i: integer;
  LPosition: TAgentPosition;
begin
  FProblem := AProblem;
  LBaufirma := FProblem.B;
  FPositionen := TPositionList.Create;
  //Place agent randomly
  Randomize;
  for i := 0 to LBaufirma.Count - 1 do
  begin
    LPosition := TAgentPosition.Create;
    repeat
      LPosition.I := Random(FProblem.F.m);
      LPosition.J := Random(FProblem.F.n);
    until FProblem.F.Cells[LPosition.I, LPosition.J] = at0;
    FPositionen.Add(LPosition);
  end;
end;

constructor TFireflyAlgorithm.Create(AProblem: TBaustellenProblem;
  APositionen: TPositionList);
begin
  FProblem := AProblem;
  FPositionen := APositionen;
end;

procedure TFireflyAlgorithm.Step;
var
  i, j: integer;
  LSomeoneMovedUsingLight: boolean;
begin
  LSomeoneMovedUsingLight := False;
  for i := 0 to FPositionen.Count - 1 do
    for j := 0 to FPositionen.Count - 1 do
      if OneHasParetoDominatedSolutionOverTwo(j, i) then
      begin
        MoveAToB(i, j);
        LSomeoneMovedUsingLight := True;
        Log(i.ToString() + ' firefly moved to ' + FPositionen[i].I.ToString() +
          ',' + FPositionen[i].J.ToString() + ' using light from ' + j.ToString());
      end
      else
      begin
        MoveRandomly(i);
        Log(i.ToString() + ' firefly moved to ' + FPositionen[i].I.ToString() +
          ',' + FPositionen[i].J.ToString() + ' randomly');
      end;
  if not LSomeoneMovedUsingLight then
    MoveAllTwoStepsCloser;
  for i := 0 to FPositionen.Count - 1 do
    if (FPositionen[i].I >= FProblem.F.m) or (FPositionen[i].J >= FProblem.F.n) or
      (FPositionen[i].I < 0) or (FPositionen[i].J < 0) then
      Sleep(0);
end;

destructor TFireflyAlgorithm.Destroy;
begin
  FProblem := nil;
end;

end.
