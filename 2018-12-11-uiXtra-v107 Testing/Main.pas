unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, uniGUITypes, uniGUIAbstractClasses,
  uniGUIClasses, uniGUIRegClasses, uniGUIForm, uniMemo, uniGUIBaseClasses,
  uniButton, uniJToast, uniSweetAlert, uniPanel, uniHTMLFrame, uniTyped,
  uniLabel, uniEdit, uniEditValidate, uniBubblyBg;

type
  TMainForm = class(TUniForm)
    UniSweetAlert1: TUniSweetAlert;
    UniJToast1: TUniJToast;
    UniButton1: TUniButton;
    UniButton2: TUniButton;
    UniButton3: TUniButton;
    UniMemo1: TUniMemo;
    UniTyped1: TUniTyped;
    UniButton4: TUniButton;
    UniEditValidate1: TUniEditValidate;
    UniLabel1: TUniLabel;
    UniBubbleBg1: TUniBubbleBg;
    procedure UniButton1Click(Sender: TObject);
    procedure UniButton3Click(Sender: TObject);
    procedure UniButton2Click(Sender: TObject);
    procedure UniFormShow(Sender: TObject);
    procedure UniSweetAlert1Success(Sender: TObject);
    procedure UniSweetAlert1Dismiss(Sender: TObject);
    procedure UniJToast1AfterHidden(Sender: TObject);
    procedure UniJToast1AfterShown(Sender: TObject);
    procedure UniJToast1BeforeHide(Sender: TObject);
    procedure UniJToast1BeforeShow(Sender: TObject);
    procedure UniButton4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uniGUIVars, MainModule, uniGUIApplication;

function MainForm: TMainForm;
begin
  Result := TMainForm(UniMainModule.GetFormInstance(TMainForm));
end;

procedure TMainForm.UniButton1Click(Sender: TObject);
begin
 UniSweetAlert1.Success('Success','Working..');
end;

procedure TMainForm.UniButton2Click(Sender: TObject);
begin
 UniSweetAlert1.Question('Did it work','Please select Yes or No', 'Yes','No');
end;

procedure TMainForm.UniButton3Click(Sender: TObject);
begin
 UniJToast1.text := 'A progressive toast' ;
 UniJToast1.loaderBg := '#FF0000';
 UniJToast1.pos := TPosition.mid_center;
 UniJToast1.hideAfter := 10000;
 UniJToast1.ShowMessage;
end;

procedure TMainForm.UniButton4Click(Sender: TObject);
var s: TStrings;
begin
  s := TStringList.Create;
  s.Add('Welcome to the jungle');
  s.Add('Restarting now...');
  UniTyped1.text := s;
  UniTyped1.PlayTyped;
  s.Free;
end;

procedure TMainForm.UniFormShow(Sender: TObject);
begin
 UniMemo1.Lines.Clear;
end;

procedure TMainForm.UniJToast1AfterHidden(Sender: TObject);
begin
 UniMemo1.Lines.Add('Toast AfterHidden');
end;

procedure TMainForm.UniJToast1AfterShown(Sender: TObject);
begin
 UniMemo1.Lines.Add('Toast OnAfterShown');
end;

procedure TMainForm.UniJToast1BeforeHide(Sender: TObject);
begin
 UniMemo1.Lines.Add('Toast BeforeHide');
end;

procedure TMainForm.UniJToast1BeforeShow(Sender: TObject);
begin
 UniMemo1.Lines.Add('Toast BeforeShow');
end;

procedure TMainForm.UniSweetAlert1Dismiss(Sender: TObject);
begin
 UniMemo1.Lines.Add('UniSweetAlert Cancel');
end;

procedure TMainForm.UniSweetAlert1Success(Sender: TObject);
begin
 UniMemo1.Lines.Add('UniSweetAlert Yes/Confirm');
end;

initialization
  RegisterAppFormClass(TMainForm);

end.
