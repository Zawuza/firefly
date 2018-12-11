program fireflytests;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  GuiTestRunner,
  baustellenproblem,
  jsonproblemparser,
  jsonproblemparsertestcase,
  baustellenprobelmtestcase, fireflyalgorithm;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
