program fireflytests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, baustellenproblem, jsonproblemparser,
  jsonproblemparsertestcase, baustellenprobelmtestcase, fireflyalgorithm,
  fireflyalgorithmtestcase, paretocalctestcase, paretocalc, logger;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
