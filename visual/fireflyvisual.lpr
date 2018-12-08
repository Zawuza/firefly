program fireflyvisual;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  fMain,
  fireflyvm,
  { you can add units after this }
  SysUtils;

{$R *.res}

begin
  if FileExists(ParamStr(1)) then
  begin
    RequireDerivedFormResource := True;
    Application.Initialize;
    Application.CreateForm(TFireflyView, FireflyView);
    Application.Run;
  end;
end.

