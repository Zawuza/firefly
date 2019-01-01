unit paretocalctestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, paretocalc,
  baustellenproblem, jsonproblemparser;

type

  TParetoCalcTestCase = class(TTestCase)
  published
    procedure ParetoTest;
  end;

implementation

procedure TParetoCalcTestCase.ParetoTest;
var
  LParetoFront: TParetoFront;
  LProblem: TBaustellenProblem;
begin
  LProblem := TJSONBaustellenProblemParser.LoadProblem('.\testdata\normpareto.json');
  LParetoFront := TParetoCalc.CalculateParetoFront(LProblem);
  AssertTrue(LParetoFront.Count = 5);
  AssertTrue(LParetoFront[0].i = 0);
  AssertTrue(LParetoFront[0].j = 0);
  AssertTrue(LParetoFront[1].i = 0);
  AssertTrue(LParetoFront[1].j = 2);
  AssertTrue(LParetoFront[2].i = 1);
  AssertTrue(LParetoFront[2].j = 1);
  AssertTrue(LParetoFront[3].i = 2);
  AssertTrue(LParetoFront[3].j = 0);
  AssertTrue(LParetoFront[4].i = 2);
  AssertTrue(LParetoFront[4].j = 2);
  FreeAndNil(LParetoFront);
  FreeAndNil(LProblem)
end;



initialization

  RegisterTest(TParetoCalcTestCase);
end.
