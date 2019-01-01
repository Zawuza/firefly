unit paretocalc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, baustellenproblem, fgl;

type
  TParetoCoord = class
    i, j: integer;
  end;

  TParetoFront = specialize TFPGObjectList<TParetoCoord>;

  { TParetoCalc }

  TParetoCalc = class
    class function CalculateParetoFront(AProblem: TBaustellenProblem): TParetoFront;
  end;

implementation

{ TParetoCalc }

class function TParetoCalc.CalculateParetoFront(AProblem: TBaustellenProblem):
TParetoFront;
var
  i1, j1, i2, j2, k: integer;
  LAgent: IAgent;
  LIsDominated: boolean;
  LPCoord: TParetoCoord;
  LParetoNonDominatedValue, LParetoDominationCandidate: double;
  LDominationsCount: integer;
begin
  Result := TParetoFront.Create(True);
  for i1 := 0 to AProblem.F.m - 1 do
    for j1 := 0 to AProblem.F.n - 1 do
    begin
      if AProblem.F.Cells[i1, j1] <> at0 then
        continue;
      LIsDominated := False;
      for i2 := 0 to AProblem.F.m - 1 do
        for j2 := 0 to AProblem.F.n - 1 do
        begin
          if AProblem.F.Cells[i2, j2] <> at0 then
            continue;
          LDominationsCount := 0;
          for k := 0 to AProblem.B.Count - 1 do
          begin
            LAgent := AProblem.B[k];
            LParetoNonDominatedValue := LAgent.t(AProblem.F, i1, j1);
            LParetoDominationCandidate := LAgent.t(AProblem.F, i2, j2);
            if LParetoNonDominatedValue > LParetoDominationCandidate then
            begin
              Inc(LDominationsCount);
            end;
          end;
          LIsDominated := (LDominationsCount = AProblem.B.Count) or LIsDominated;
        end;
      if not LIsDominated then
      begin
        LPCoord := TParetoCoord.Create;
        LPCoord.i := i1;
        LPCoord.j := j1;
        Result.Add(LPCoord);
      end;
    end;
end;

end.
