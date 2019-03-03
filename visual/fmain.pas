unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Grids,
  fireflyvm,
  Types;

type

  { TFireflyView }

  TFireflyView = class(TForm, INotifiable)
    Button1: TButton;
    Button2: TButton;
    DrawGrid1: TDrawGrid;
    Panel1: TPanel;
    SchrittText: TStaticText;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FVM: TFireflyViewModel;
    FGrid: TListGrid;
    FSchritteCount: integer;
    FParetoOptimals: TCoordinatesList;
    procedure IncSchritteCount;
    function IsPareto(i, j: integer): boolean;
    function IsSolution(i, j: integer): boolean;
  public
    procedure Notify(AMsg: string);
  end;

const
  MSG_RESIZE = 'MSG_RESIZE';

var
  FireflyView: TFireflyView;

implementation

{$R *.lfm}

{ TFireflyView }

procedure TFireflyView.FormResize(Sender: TObject);
begin
  Notify(MSG_RESIZE);
end;

procedure TFireflyView.DrawGrid1DrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  TextStyle: TTextStyle;
begin
  //Fill cell
  if (UpperCase(FGrid[aRow][aCol]) = FGrid[aRow][aCol]) or (FGrid[aRow][aCol] = '') then
    if IsPareto(aRow, aCol) then
      if IsSolution(aRow, aCol) then
        DrawGrid1.Canvas.Brush.Color := clOlive
      else
        DrawGrid1.Canvas.Brush.Color := clWhite
    else
      if IsSolution(aRow, aCol) then
        DrawGrid1.Canvas.Brush.Color := clYellow
      else
        DrawGrid1.Canvas.Brush.Color := cl3DLight
  else
    DrawGrid1.Canvas.Brush.Color := clSkyBlue;
  DrawGrid1.Canvas.FillRect(aRect);
  DrawGrid1.Canvas.Rectangle(aRect);

  //Draw Text
  TextStyle := DrawGrid1.Canvas.TextStyle;
  TextStyle.Alignment := taCenter;
  TextStyle.Layout := tlCenter;
  DrawGrid1.Canvas.TextStyle := TextStyle;
  DrawGrid1.Canvas.TextRect(aRect, aRect.Left,
    aRect.Top, FGrid[aRow][aCol]);
end;

procedure TFireflyView.Button1Click(Sender: TObject);
begin
  FVM.Step;
  IncSchritteCount;
end;

procedure TFireflyView.Button2Click(Sender: TObject);
begin
  FParetoOptimals := FVM.CalcParetoOptimum;
  Self.Notify(MSG_PLEASE_UPDATE_GUI);
end;

procedure TFireflyView.FormCreate(Sender: TObject);
begin
  FVM := TFireflyViewModel.Create(ParamStr(1), Self);
  FSchritteCount := 0;
end;

procedure TFireflyView.FormShow(Sender: TObject);
begin
  Self.Notify(MSG_PLEASE_UPDATE_GUI);
end;

procedure TFireflyView.IncSchritteCount;
begin
  FSchritteCount := FSchritteCount + 1;
  SchrittText.Caption := 'Schritte: ' + FSchritteCount.ToString();
end;

function TFireflyView.IsPareto(i, j: integer): boolean;
var
  k: integer;
  LCoord: TCoordinates;
begin
  if not Assigned(FParetoOptimals) then
    exit(False);
  Result := False;
  for k := 0 to FParetoOptimals.Count - 1 do
  begin
    LCoord := FParetoOptimals[k];
    if (LCoord.i = i) and (LCoord.j = j) then
      exit(True);
  end;
end;

function TFireflyView.IsSolution(i, j: integer): boolean;
begin
  Result := False;
  if (i = FVM.GetBestSolutionI) and (j = FVM.GetBestSolutionJ) then
    Result := True;
end;

procedure TFireflyView.Notify(AMsg: string);
var
  LRowHeight: integer;
begin
  //Get size of grid
  DrawGrid1.ColCount := FVM.GetN;
  DrawGrid1.RowCount := FVM.GetM;

  //Resize grid
  DrawGrid1.Height := Self.Height - Panel1.Height - 50;
  DrawGrid1.Width := DrawGrid1.Height;
  DrawGrid1.Top := 25;
  DrawGrid1.Left := (Self.Width - DrawGrid1.Width) div 2;

  //Resize cells
  LRowHeight := DrawGrid1.Height div DrawGrid1.RowCount;
  DrawGrid1.DefaultRowHeight := LRowHeight;

  if AMsg = MSG_RESIZE then
    exit;

  //Add content
  FGrid := FVM.GetGrid;
  DrawGrid1.Repaint;
end;

end.
