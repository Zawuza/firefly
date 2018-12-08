unit baustellenproblem;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
  fgl;

type
  TAnlageTyp = (atImbiss, atBahnhof, atHaus, at0, {Nicht existierende Zelle}atNotExists);

  { TFeld }

  TFeld = class
  private
    Fm, Fn: word;
    FCells: array of array of TAnlageTyp;
    FInitFinished: boolean;
    function GetCells(i, j: word): TAnlageTyp;
    procedure SetCells(i, j: word; AValue: TAnlageTyp);
  public
    constructor Create(m, n: word);
    property Cells[i, j: word]: TAnlageTyp read GetCells write SetCells;
    procedure SetNulls;
    procedure FinishInit;
    property m: word read Fm;
    property n: word read Fn;
  end;

  { IAgent }

  IAgent = interface
    function t(F: TFeld; i, j: integer): double;
  end;

  TBaufirma = TFPGList<IAgent>;

  { TDistance }

  TDistance = class
    class function Manhattan(AFeld: TFeld; i, j: integer; AAnlage: TAnlageTyp): integer;
  end;

  { THandwerker }

  THandwerker = class(TInterfacedObject, IAgent)
    function t(F: TFeld; i, j: integer): double;
  end;

  { TVerkehrsspezialist }

  TVerkehrsspezialist = class(TInterfacedObject, IAgent)
    function t(F: TFeld; i, j: integer): double;
  end;

  { TLandschaftsarchitekt }

  TLandschaftsarchitekt = class(TInterfacedObject, IAgent)
    function t(F: TFeld; i, j: integer): double;
  end;

  { TElektriker }

  TElektriker = class(TInterfacedObject, IAgent)
    function t(F: TFeld; i, j: integer): double;
  end;

  { TWerkstudent }

  TWerkstudent = class(TInterfacedObject, IAgent)
    function t(F: TFeld; i, j: integer): double;
  end;

  { TBaustellenProblem }

  TBaustellenProblem = class
  private
    FB: TBaufirma;
    FF: TFeld;
  public
    constructor Create(F: TFeld; B: TBaufirma);
    property F: TFeld read FF;
    property B: TBaufirma read FB;
  end;

implementation

{ TBaustellenProblem }

constructor TBaustellenProblem.Create(F: TFeld; B: TBaufirma);
begin
  FF := F;
  FB := B;
end;

{ TWerkstudent }

function TWerkstudent.t(F: TFeld; i, j: integer): double;
begin
  Result := -(i + j);
end;

{ TElektriker }

function TElektriker.t(F: TFeld; i, j: integer): double;
begin
  Result := TDistance.Manhattan(F, i, j, atHaus);
end;

{ TLandschaftsarchitekt }

function TLandschaftsarchitekt.t(F: TFeld; i, j: integer): double;
var
  LImbissDistanz, LBahnhofDistanz, LHausDistanz: integer;
begin
  LImbissDistanz := TDistance.Manhattan(F, i, j, atImbiss);
  LBahnhofDistanz := TDistance.Manhattan(F, i, j, atBahnhof);
  LHausDistanz := TDistance.Manhattan(F, i, j, atHaus);
  Result := MinIntValue([LImbissDistanz, LBahnhofDistanz, LHausDistanz]);
  Result := -1 * Result;
end;

{ TVerkehrsspezialist }

function TVerkehrsspezialist.t(F: TFeld; i, j: integer): double;
begin
  Result := TDistance.Manhattan(F, i, j, atBahnhof);
end;

{ THandwerker }

function THandwerker.t(F: TFeld; i, j: integer): double;
begin
  Result := TDistance.Manhattan(F, i, j, atImbiss);
end;

{ TDistance }

class function TDistance.Manhattan(AFeld: TFeld; i, j: integer;
  AAnlage: TAnlageTyp): integer;
var
  k, m: integer;
  LDistance: integer;
begin
  Result := AFeld.m + AFeld.n;
  for k := 0 to AFeld.m - 1 do
    for m := 0 to AFeld.n - 1 do
      if AFeld.Cells[k, m] = AAnlage then
      begin
        LDistance := Abs(k - i) + Abs(m - j);
        if Result > LDistance then
          Result := LDistance;
      end;
end;

{ TFeld }

function TFeld.GetCells(i, j: word): TAnlageTyp;
begin
  if (i < Fm) and (j < Fn) then
    Result := FCells[i][j]
  else
    Result := atNotExists;
end;

procedure TFeld.SetCells(i, j: word; AValue: TAnlageTyp);
begin
  if not FInitFinished then
    FCells[i][j] := AValue;
end;

constructor TFeld.Create(m, n: word);
var
  i: integer;
begin
  FInitFinished := False;
  Fm := m;
  Fn := n;
  SetLength(FCells, m);
  for i := 0 to Length(FCells) - 1 do
    SetLength(FCells[i], n);
end;

procedure TFeld.SetNulls;
var
  i, j: integer;
begin
  for i := 0 to Fm - 1 do
    for j := 0 to Fn - 1 do
      FCells[i][j] := at0;
end;

procedure TFeld.FinishInit;
begin
  FInitFinished := True;
end;

end.
