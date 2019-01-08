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
    FStage2: integer;
    FProblem: TBaustellenProblem;
    FPositionen: TPositionList;
    function MoveAToB(AIndexOfA: integer; AIndexOfB: integer;
      AStopOnMidDist: boolean = False): boolean;
    procedure MoveRandomly(AIndex: integer);
    function OneHasParetoDominatedSolutionOverTwo(AAgentIndex1,
      AAGentIndex2: integer): boolean;
    procedure MoveAllCloser;
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

function TFireflyAlgorithm.MoveAToB(AIndexOfA: integer; AIndexOfB: integer;
  AStopOnMidDist: boolean = False): boolean;
var
  LPositionOfA, LPositionOfB: TAgentPosition;
  LBestMove: TNeighbours;
  LMinDistance: integer;
  LDistance, LStartDistance: integer;
begin
  Result := false;
  LPositionOfA := FPositionen[AIndexOfA];
  LPositionOfB := FPositionen[AIndexOfB];

  LStartDistance := ManhattanDistanz(LPositionOfA.I, LPositionOfA.J,
    LPositionOfB.I, LPositionOfB.J);
  if (LStartDistance = 0) and AStopOnMidDist then
    exit;

  LMinDistance := High(LMinDistance);
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
    if LDistance = LMinDistance then
      if Round(Random) = 1 then
        LBestMove := nTop;
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
    if LDistance = LMinDistance then
      if Round(Random) = 1 then
        LBestMove := nRight;
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
    if LDistance = LMinDistance then
      if Round(Random) = 1 then
        LBestMove := nBottom;
  end;

  if LMinDistance > LStartDistance then
    exit;

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
  Result := true;
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
  LMyValue: double;
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

  for i := 0 to 3 do
    Log('Direction: ' + integer(LDirections[i]).ToString() + ', Value: ' +
      LValues[i].ToString());


  LFinalDirection := integer(LDirections[0]);
  LMyValue := LAgent.t(FProblem.F, LPosition.I, LPosition.J);
  if LValues[0] >= LMyValue then
  begin
    Log('NOTHING BETTER, my value is ' + LMyValue.ToString);
    exit;
  end;

  Log('Selected direction: ' + IntToStr(LFinalDirection));

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

procedure TFireflyAlgorithm.MoveAllCloser;
var
  i, j: integer;
begin
  for i := 0 to FPositionen.Count - 1 do
    for j := 0 to FPositionen.Count - 1 do
    begin
      MoveAToB(i, j, True);
    end;
end;

function TFireflyAlgorithm.CheckBounds(i, j: integer): boolean;
var
  z: integer;
begin
  Result := (i >= 0) and (i < FProblem.F.m) and (j >= 0) and
    (j < FProblem.F.n) and (FProblem.F.Cells[i, j] = at0);
  if Result then
    for z := 0 to FPositionen.Count - 1 do
      Result := Result and not ((FPositionen[z].i = i) and (FPositionen[z].j = j));
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
    until CheckBounds(LPosition.I, LPosition.J);
    FPositionen.Add(LPosition);
  end;
  FStage2 := 0;
end;

constructor TFireflyAlgorithm.Create(AProblem: TBaustellenProblem;
  APositionen: TPositionList);
begin
  FProblem := AProblem;
  FPositionen := APositionen;
  FStage2 := 0;
end;

procedure TFireflyAlgorithm.Step;
var
  i, j: integer;
  LSomeoneMovedUsingLight: boolean;
begin
  Randomize;
  LSomeoneMovedUsingLight := False;
  for i := 0 to FPositionen.Count - 1 do
    for j := i + 1 to FPositionen.Count - 1 do
      if OneHasParetoDominatedSolutionOverTwo(j, i) then
      begin
        LSomeoneMovedUsingLight := MoveAToB(i, j);
        Log(i.ToString() + ' firefly moved to ' + FPositionen[i].I.ToString() +
          ',' + FPositionen[i].J.ToString() + ' using light from ' + j.ToString());
      end
      else
      if OneHasParetoDominatedSolutionOverTwo(i, j) then
      begin
        LSomeoneMovedUsingLight := MoveAToB(j, i);
        Log(j.ToString() + ' firefly moved to ' + FPositionen[j].I.ToString() +
          ',' + FPositionen[j].J.ToString() + ' using light from ' + i.ToString());
      end;
  if not LSomeoneMovedUsingLight then
    for i := 0 to FPositionen.Count - 1 do
      MoveRandomly(i);
end;

destructor TFireflyAlgorithm.Destroy;
begin
  FProblem := nil;
end;

end.
