unit jsonproblemparsertestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testutils,
  testregistry,
  baustellenproblem,
  jsonproblemparser;

type

  { TJSONProblemParserTestCase }

  TJSONProblemParserTestCase = class(TTestCase)
  published
    procedure ReadPredefinedProblem;
  end;

implementation

procedure TJSONProblemParserTestCase.ReadPredefinedProblem;
var
  LBaustellenProblem: TBaustellenProblem;
begin
  LBaustellenProblem := TJSONBaustellenProblemParser.LoadProblem(
    '.\testdata\problem.json');

  AssertTrue(LBaustellenProblem.F.m = 4);
  AssertTrue(LBaustellenProblem.F.n = 3);

  AssertTrue(LBaustellenProblem.F.Cells[0, 0] = atBahnhof);
  AssertTrue(LBaustellenProblem.F.Cells[0, 1] = atImbiss);
  AssertTrue(LBaustellenProblem.F.Cells[0, 2] = at0);
  AssertTrue(LBaustellenProblem.F.Cells[1, 0] = at0);
  AssertTrue(LBaustellenProblem.F.Cells[1, 1] = atHaus);
  AssertTrue(LBaustellenProblem.F.Cells[1, 2] = at0);
  AssertTrue(LBaustellenProblem.F.Cells[2, 0] = at0);
  AssertTrue(LBaustellenProblem.F.Cells[2, 1] = at0);
  AssertTrue(LBaustellenProblem.F.Cells[2, 2] = at0);
  AssertTrue(LBaustellenProblem.F.Cells[3, 0] = at0);
  AssertTrue(LBaustellenProblem.F.Cells[3, 1] = at0);
  AssertTrue(LBaustellenProblem.F.Cells[3, 2] = at0);

  AssertTrue(LBaustellenProblem.B.Count = 7);
  AssertTrue(LBaustellenProblem.B[0] is THandwerker);
  AssertTrue(LBaustellenProblem.B[1] is TLandschaftsarchitekt);
  AssertTrue(LBaustellenProblem.B[2] is TVerkehrsspezialist);
  AssertTrue(LBaustellenProblem.B[3] is TWerkstudent);
  AssertTrue(LBaustellenProblem.B[4] is TElektriker);
  AssertTrue(LBaustellenProblem.B[5] is TElektriker);
  AssertTrue(LBaustellenProblem.B[6] is TWerkstudent);
end;



initialization

  RegisterTest(TJSONProblemParserTestCase);
end.
