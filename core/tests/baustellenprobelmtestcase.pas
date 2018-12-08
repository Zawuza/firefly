unit baustellenprobelmtestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  fpcunit,
  testutils,
  testregistry,

  baustellenproblem;

type

  { TBaustellenProblemTestCase }

  TBaustellenProblemTestCase= class(TTestCase)
  published
    procedure FeldTest;
    procedure DistanceTest;
    procedure HandwerkerTest;
    procedure VerkehrsspezialistTest;
    procedure LandschaftsarchitektTest;
    procedure ElektrikerTest;
    procedure WerkstudentTest;
  end;

implementation

procedure TBaustellenProblemTestCase.FeldTest;
var LFeld: TFeld;
begin
  LFeld := TFeld.Create(2,3);
  LFeld.SetNulls;
  LFeld.Cells[0,0] := atBahnhof;
  LFeld.Cells[1,2] := atHaus;
  LFeld.Cells[0,2] := atImbiss;
  LFeld.FinishInit;
  LFeld.Cells[0,0] := atImbiss;
  AssertTrue(LFeld.Cells[0,0] = atBahnhof);
  AssertTrue(LFeld.Cells[0,1] = at0);
  AssertTrue(LFeld.Cells[0,2] = atImbiss);
  AssertTrue(LFeld.Cells[1,0] = at0);
  AssertTrue(LFeld.Cells[1,1] = at0);
  AssertTrue(LFeld.Cells[1,2] = atHaus);
  FreeAndNil(LFeld);
end;

procedure TBaustellenProblemTestCase.DistanceTest;
var LFeld: TFeld;
begin
  LFeld := TFeld.Create(4,4);
  LFeld.SetNulls;
  LFeld.Cells[0,3] := atImbiss;
  LFeld.Cells[3,0] := atBahnhof;
  LFeld.FinishInit;
  AssertTrue(TDistance.Manhattan(LFeld,2,1,atImbiss) = 4);
  AssertTrue(TDistance.Manhattan(LFeld,2,1,atBahnhof) = 2);
  FreeAndNil(LFeld);
end;

procedure TBaustellenProblemTestCase.HandwerkerTest;
var LFeld: TFeld;
    LHandwerker: IAgent;
begin
  LFeld := TFeld.Create(3,3);
  LFeld.SetNulls;
  LFeld.Cells[0,2] := atImbiss;
  LHandwerker := THandwerker.Create;
  AssertEquals(3.0,LHandwerker.t(LFeld,1,0));
  FreeAndNil(LFeld);
end;

procedure TBaustellenProblemTestCase.VerkehrsspezialistTest;
var LFeld: TFeld;
    LVerkehrsspezialist: IAgent;
begin
  LFeld := TFeld.Create(3,3);
  LFeld.SetNulls;
  LFeld.Cells[0,2] := atBahnhof;
  LVerkehrsspezialist := TVerkehrsspezialist.Create;
  AssertEquals(4.0,LVerkehrsspezialist.t(LFeld,2,0));
  FreeAndNil(LFeld);
end;

procedure TBaustellenProblemTestCase.LandschaftsarchitektTest;
var
   LFeld: TFeld;
   LLandschaftsarchitekt: IAgent;
begin
  LFeld := TFeld.Create(3,3);
  LFeld.SetNulls;
  LFeld.Cells[0,3] := atBahnhof;
  LFeld.Cells[2,1] := atImbiss;
  LLandschaftsarchitekt := TLandschaftsarchitekt.Create;
  AssertEquals(-1.0,LLandschaftsarchitekt.t(LFeld,2,0));
  FreeAndNil(LFeld);
end;

procedure TBaustellenProblemTestCase.ElektrikerTest;
var
   LFeld: TFeld;
   LElektriker: IAgent;
begin
  LFeld := TFeld.Create(3,3);
  LFeld.SetNulls;
  LFeld.Cells[2,1] := atHaus;
  LElektriker := TElektriker.Create;
  AssertEquals(1.0,LElektriker.t(LFeld,2,0));
  FreeAndNil(LFeld);
end;

procedure TBaustellenProblemTestCase.WerkstudentTest;
var
   LFeld: TFeld;
   LWerkstudent: IAgent;
begin
  LFeld := TFeld.Create(3,3);
  LFeld.SetNulls;
  LWerkstudent := TWerkstudent.Create;
  AssertEquals(-2.0,LWerkstudent.t(LFeld,2,0));
  FreeAndNil(LFeld);
end;



initialization

  RegisterTest(TBaustellenProblemTestCase);
end.

