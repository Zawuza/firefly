unit fireflyalgorithmtestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, fireflyalgorithm,
  baustellenproblem, jsonproblemparser;

type

  TFireflyAlgorithmTestCase = class(TTestCase)
  published
    procedure BasicMovementTest;
  end;

implementation

procedure TFireflyAlgorithmTestCase.BasicMovementTest;
var
  LFirefly: TFireflyAlgorithm;
  LProblem: TBaustellenProblem;
  LPositionen: TPositionList;
begin
  LProblem := TJSONBaustellenProblemParser.LoadProblem('.\testdata\basicmovement.json');
  LPositionen := TPositionList.Create;
  LPositionen.Add(TAgentPosition.Create);
  LPositionen[0].I := 0;
  LPositionen[0].J := 0;
  LFirefly := TFireflyAlgorithm.Create(LProblem,LPositionen);
  AssertTrue(LFirefly.Positionen[0].I = 0);
  AssertTrue(LFirefly.Positionen[0].J = 0);
  LFirefly.Step;
  AssertTrue(((LFirefly.Positionen[0].I = 0) and (LFirefly.Positionen[0].J = 1)) or
    (LFirefly.Positionen[0].I = 1) and (LFirefly.Positionen[0].J = 0));
end;



initialization

  RegisterTest(TFireflyAlgorithmTestCase);
end.
