unit fireflyalgorithm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl,
  baustellenproblem;

type

  { TAgentPosition }

  TAgentPosition = class
  public
    Index: integer;
    I, J: integer;
  end;

  TPositionList = specialize TFPGList<TAgentPosition>;

  { TFireflyAlgorithm }

  TFireflyAlgorithm = class
  private
    FProblem: TBaustellenProblem;
    FPositionen: TPositionList;
    procedure MoveAToB(AIndexOfA: integer; AIndexOfB: integer);
    procedure MoveRandomly(AIndex: integer);
  public
    constructor Create(AProblem: TBaustellenProblem);
    property Positionen: TPositionList read FPositionen;
    procedure Step;
    destructor Destroy; override;
  end;

implementation

{ TFireflyAlgorithm }

procedure TFireflyAlgorithm.MoveAToB(AIndexOfA: integer; AIndexOfB: integer);
begin
  //TODO
end;

procedure TFireflyAlgorithm.MoveRandomly(AIndex: integer);
begin
  //TODO
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
    LPosition.Index := i;
    repeat
      LPosition.I := Random(FProblem.F.m);
      LPosition.J := Random(FProblem.F.n);
    until FProblem.F.Cells[LPosition.I, LPosition.J] = at0;
    FPositionen.Add(LPosition);
  end;
end;

procedure TFireflyAlgorithm.Step;
var
  i, j: integer;
begin
  for i := 0 to FPositionen.Count - 1 do
    for j := 0 to FPositionen.Count - 1 do
      if HasParetoDominatedSolution(j, i) then
        MoveAToB(i, j)
      else
        MoveRandomly(i);
end;

destructor TFireflyAlgorithm.Destroy;
begin
  FProblem := nil;
end;

end.


