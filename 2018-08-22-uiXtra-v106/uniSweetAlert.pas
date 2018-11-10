unit uniSweetAlert;
{------------------------------------------------------------------------------}
{                                                                              }
{ uniSweetAlert                                                                }
{ =============                                                                }
{                                                                              }
{ Description:  This class contains a wrapper for the SweetAlert2 javascript   }
{               alert library.                                                 }
{                                                                              }
{               Github:  https://limonte.github.io/sweetalert2/                }
{               cdnjs:   https://cdnjs.com/libraries/limonte-sweetalert2       }
{                                                                              }
{ Installation: Under the files folder copy the following files:               }
{               sweetalert2/sweetalert2.min.css                                }
{               sweetalert2/sweetalert2.min.js                                 }
{                                                                              }
{ This source code is copyrighted material.                                    }
{                                                                              }
{ Copyright (c) CastleSoft Pty Ltd. 2017-2018. All rights reserved.            }
{                                                                              }
{ MIT License                                                                  }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to     }
{ deal in the Software without restriction, including without limitation the   }
{ rights to use, copy, modify, merge, publish, distribute, sublicense, and/or  }
{ sell copies of the Software, and to permit persons to whom the Software is   }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,}
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN    }
{ THE SOFTWARE.                                                                }
{                                                                              }
{ Version       Date          Description              Modified by             }
{ ---------------------------------------------------------------------------- }
{  1.0.0        29-Jul-2017   Initial Release          Andrew Tierney          }
{  1.0.1        03-Aug-2017   Fixed Z-Index,           Andrew Tierney          }
{                             input type and                                   }
{                             buttonsStyling                                   }
{  1.0.2        02-Jun-2018   Tweak for Tokyo          Andrew Tierney          }
{  1.0.5        31-Jul-2018   Minor tweaks             Andrew Tierney          }
{  1.0.6        22-Aug-2018   Changed License to MIT   Andrew Tierney          }
{                                                                              }
{------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, uniGUIBaseClasses, uniGUIClasses, uniGUITypes, uniGUIJSUtils, uniGUIApplication,RTTI;

type TAlertType = (success, error, warning, info, question);
type TDismissType = ( cancel, overlay, close, timer );
type TInputType = ( none, text, email, password, number, tel, range, textarea, select, radio, checkbox, fileinput, url);

type
  TUniSweetAlert = class(TUniComponent)
  private
    fTitle: string;
    fText: string;
    fAlertType: TAlertType;
    fallowOutsideClick: Boolean;
    ftimer: integer;
    fshowCancelButton: Boolean;
    ftitleText: string;
    fHtml: string;
    fTarget: string;
    fInputType: TInputType;
    fWidth: string;
    fPadding: integer;
    fBackground: string;
    fcustomClass: string;
    fAnimation: Boolean;
    fallowEscapeKey: Boolean;
    fallowEnterKey: Boolean;
    fshowConfirmButton: Boolean;
    fconfirmButtonText: string;
    fcancelButtonText: string;
    fconfirmButtonColor: string;
    fcancelButtonColor: string;
    fconfirmButtonClass: string;
    fcancelButtonClass: string;
    fbuttonsStyling: Boolean;
    freverseButtons: Boolean;
    ffocusCancel: Boolean;
    fshowCloseButton: Boolean;
    fshowLoaderOnConfirm: Boolean;

    { ClientSide events raised on the server via AJAX }
    FOnSuccess : TNotifyEvent;
    FOnDismiss : TNotifyEvent;

    procedure H_OnSuccess(Sender: TObject);
    procedure H_OnDismiss(Sender: TObject);

    function GetOnSuccess: TNotifyEvent;
    procedure SetOnSuccess(const Value: TNotifyEvent);
    function GetOnDismiss: TNotifyEvent;
    procedure SetOnDismiss(const Value: TNotifyEvent);

    { Build JSON and Ajax strings for uniSweetAlert2 }
    function BuildJsonParams(): string;
    function BuildAjaxRequest(callback: string; params: string = ''): string;
    function GetInputType(i: TInputType): string;
    function GetAlertType(a: TAlertType): string;
  protected
    function VCLComponentClassName: string; override;
    procedure WebCreate; override;
    procedure JSEventHandler(AEventName: string; AParams: TUniStrings); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property title: string read fTitle write fTitle;
    property titleText: string read ftitleText write ftitleText;
    property text: string read fText write fText;
    property html: string read fHtml write fHtml;
    property alertType: TAlertType read fAlertType write fAlertType;
    property target: string read fTarget write fTarget;
    property inputType: TInputType read fInputType write fInputType;
    property width: string read fWidth write fWidth;
    property padding: integer read fPadding write fPadding;
    property background: string read fBackground write fBackground;
    property customClass: string read fcustomClass write fcustomClass;
    property timer: integer read ftimer write ftimer;
    property animation: Boolean read fAnimation write fAnimation;
    property allowOutsideClick: Boolean read fallowOutsideClick write fallowOutsideClick;
    property allowEscapeKey: Boolean read fallowEscapeKey write fallowEscapeKey;
    property allowEnterKey: Boolean read fallowEnterKey write fallowEnterKey;
    property showConfirmButton: Boolean read fshowConfirmButton write fshowConfirmButton;
    property showCancelButton: Boolean read fshowCancelButton write fshowCancelButton;
    property confirmButtonText: string read fconfirmButtonText write fconfirmButtonText;
    property cancelButtonText: string read fcancelButtonText write fcancelButtonText;
    property confirmButtonColor: string read fconfirmButtonColor write fconfirmButtonColor;
    property cancelButtonColor: string read fcancelButtonColor write fcancelButtonColor;
    property confirmButtonClass: string read fconfirmButtonClass write fconfirmButtonClass;
    property cancelButtonClass: string read fcancelButtonClass write fcancelButtonClass;
    property buttonsStyling: Boolean read fbuttonsStyling write fbuttonsStyling;
    property reverseButtons: Boolean read freverseButtons write freverseButtons;
    property focusCancel: Boolean read ffocusCancel write ffocusCancel;
    property showCloseButton: Boolean read fshowCloseButton write fshowCloseButton;
    property showLoaderOnConfirm: Boolean read fshowLoaderOnConfirm write fshowLoaderOnConfirm;

    { ClientSide events raised on the server via AJAX }
    property OnSuccess: TNotifyEvent read GetOnSuccess write SetOnSuccess;
    property OnDismiss: TNotifyEvent read GetOnDismiss write SetOnDismiss;

    procedure Clear;
    { SweetAlert Methods - ShowMessage, Success, Info, Warning, etc }
    procedure ShowMessage;
    procedure Error(heading:string; msg: string = ''; confirmText: string = ''; timeMilliseconds: integer = 0);
    procedure Info(heading:string; msg: string = ''; confirmText: string = '';timeMilliseconds: integer = 0);
    procedure Success(heading:string; msg: string = ''; confirmText: string = ''; timeMilliseconds: integer = 0);
    procedure Warning(heading:string; msg: string = ''; confirmText: string = ''; timeMilliseconds: integer = 0);
    procedure Question(heading:string; msg: string = ''; confirmText: string = ''; cancelText: string = ''; timeMilliseconds: integer = 0);
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('uniGUI Extensions', [TUniSweetAlert]);
end;

{ TUniSweetAlert }

function TUniSweetAlert.BuildAjaxRequest(callback, params: string): string;
var ajax : string;
    jss  : string;
begin
    jss := JSName;
    ajax := 'ajaxRequest(' + jss + ',"'+ callback + '",[' + params + ']);';
    result := ajax;
end;

function TUniSweetAlert.BuildJsonParams: string;
var bstr: string;
begin
   bstr := '{';
   bstr := bstr + 'title:' + '"' + title + '",';
   bstr := bstr + 'titleText:' + '"' + titleText + '",';
   bstr := bstr + 'text:' + '"' + text + '",';
   bstr := bstr + 'html:' + '"' + html + '",';
   bstr := bstr + 'type:' + '"' + GetAlertType(alertType) + '",';
   bstr := bstr + 'target:' + '"' + target + '",';

   if (inputType <> TInputType.none) then
       bstr := bstr + 'input:' + '"' + GetInputType(inputType) + '",';

   bstr := bstr + 'width:' + '"' + width + '",';
   bstr := bstr + 'padding:' + IntToStr(padding) + ',';
   bstr := bstr + 'background:' + '"' + background + '",';
   bstr := bstr + 'customClass:' + '"' + customClass + '",';
   bstr := bstr + 'timer:' + IntToStr(timer) + ',';
   bstr := bstr + 'animation:' + BoolToStr(animation) + ',';
   bstr := bstr + 'allowOutsideClick:' + BoolToStr(allowOutsideClick) + ',';
   bstr := bstr + 'allowEscapeKey:' + BoolToStr(allowEscapeKey) + ',';
   bstr := bstr + 'allowEnterKey:' + BoolToStr(allowEnterKey) + ',';
   bstr := bstr + 'showConfirmButton:' + BoolToStr(showConfirmButton) + ',';
   bstr := bstr + 'showCancelButton:' + BoolToStr(showCancelButton) + ',';
   bstr := bstr + 'confirmButtonText:' + '"' + confirmButtonText + '",';
   bstr := bstr + 'cancelButtonText:' + '"' + cancelButtonText + '",';
   bstr := bstr + 'confirmButtonColor:' + '"' + confirmButtonColor + '",';
   bstr := bstr + 'cancelButtonColor:' + '"' + cancelButtonColor + '",';
   bstr := bstr + 'confirmButtonClass:' + '"' + confirmButtonClass + '",';
   bstr := bstr + 'cancelButtonClass:' + '"' + cancelButtonClass + '",';
   bstr := bstr + 'allowOutsideClick:' + BoolToStr(allowOutsideClick) + ',';
   bstr := bstr + 'buttonsStyling:' + BoolToStr(buttonsStyling) + ',';
   bstr := bstr + 'reverseButtons:' + BoolToStr(reverseButtons) + ',';
   bstr := bstr + 'focusCancel:' + BoolToStr(focusCancel) + ',';
   bstr := bstr + 'showCloseButton:' + BoolToStr(showCloseButton) + ',';
   bstr := bstr + 'showLoaderOnConfirm:' + BoolToStr(showLoaderOnConfirm) + ',';
   bstr := bstr + '}';
   result := bstr;
end;

procedure TUniSweetAlert.Clear;
begin
    title := '';
    titleText := '';
    text := '';
    html := '';
    alertType := TAlertType.success;
    target := 'body';
    inputType := TInputType.none;
    width := '500px';
    padding := 20;
    background := '#fff';
    customClass := '';
    timer := 0;
    animation := true;
    allowOutsideClick := false;
    allowEscapeKey := true;
    allowEnterKey := true;
    showConfirmButton := true;
    showCancelButton := false;
    confirmButtonText :='OK';
    cancelButtonText :='Cancel';
    confirmButtonColor := '#3085d6';
    cancelButtonColor := '#aaa';
    confirmButtonClass := '';
    cancelButtonClass := '';
    buttonsStyling := true;
    reverseButtons := false;
    focusCancel := false;
    showCloseButton := false;
    showLoaderOnConfirm := false;
end;

constructor TUniSweetAlert.Create(AOwner: TComponent);
begin
  inherited;
  // Initialise values here..
  Clear;
end;

function TUniSweetAlert.GetAlertType(a: TAlertType): string;
begin
   result := TRttiEnumerationType.GetName(a);
end;

function TUniSweetAlert.GetInputType(i: TInputType): string;
var res: string;
begin
  if (i = TInputType.fileinput) then
      res := 'file'
  else if (i = TInputType.none) then
       res := ''
  else
      res := TRttiEnumerationType.GetName(i);

  result := res;
end;

function TUniSweetAlert.GetOnDismiss: TNotifyEvent;
begin
   result := FOnDismiss;
end;

function TUniSweetAlert.GetOnSuccess: TNotifyEvent;
begin
   result := FOnSuccess;
end;

procedure TUniSweetAlert.H_OnDismiss(Sender: TObject);
begin
  if Assigned(FOnDismiss) then
     FOnDismiss(Sender);
end;

procedure TUniSweetAlert.H_OnSuccess(Sender: TObject);
begin
  if Assigned(FOnSuccess) then
     FOnSuccess(Sender);
end;

procedure TUniSweetAlert.JSEventHandler(AEventName: string; AParams: TUniStrings);
begin
  if AEventName = 'success' then
  begin
      H_OnSuccess(self);
  end;

  if AEventName = 'dismiss' then
  begin
      H_OnDismiss(self);
  end;

end;

procedure TUniSweetAlert.SetOnDismiss(const Value: TNotifyEvent);
begin
  FOnDismiss := Value;
end;

procedure TUniSweetAlert.SetOnSuccess(const Value: TNotifyEvent);
begin
  FOnSuccess := Value;
end;

procedure TUniSweetAlert.ShowMessage;
var ajaxCallbackStr : string;
    dismissCallbackStr : string;
    jsonSweetAlert: string;
begin
   jsonSweetAlert := BuildJsonParams;
   ajaxCallbackStr := BuildAjaxRequest('success');
   dismissCallbackStr := BuildAjaxRequest('dismiss','dismiss');
   UniSession.AddJS('swal(' + jsonSweetAlert + ').then(function(){' + ajaxCallbackStr + ' return false; },function (dismiss) { '+ dismissCallbackStr + '});');
end;


function TUniSweetAlert.VCLComponentClassName: string;
begin
    result := 'TUniSweetAlert';
end;

procedure TUniSweetAlert.Error(heading:string; msg: string; confirmText: string; timeMilliseconds: integer);
begin
   title := heading;
   text := msg;
   showCancelButton := false;
   timer := timeMilliseconds;
   alertType := TAlertType.error;
   ShowMessage;
end;

procedure TUniSweetAlert.Info(heading:string; msg: string; confirmText: string;timeMilliseconds: integer);
begin
   title := heading;
   text := msg;
   showCancelButton := false;
   timer := timeMilliseconds;
   alertType := TAlertType.info;
   ShowMessage;
end;

procedure TUniSweetAlert.Question(heading:string; msg: string; confirmText: string; cancelText: string;timeMilliseconds: integer);
begin
   title := heading;
   text := msg;
   showCancelButton := true;
   timer := timeMilliseconds;

   if (confirmText <> '') then
      confirmButtonText := confirmText;

   if (cancelText <> '') then
      cancelButtonText := cancelText;

   alertType := TAlertType.question;
   ShowMessage;
end;

procedure TUniSweetAlert.Success(heading:string; msg: string; confirmText: string; timeMilliseconds: integer);
begin
   title := heading;
   text := msg;
   showCancelButton := false;
   timer := timeMilliseconds;
   alertType := TAlertType.success;
   ShowMessage;
end;

procedure TUniSweetAlert.Warning(heading:string; msg: string; confirmText: string; timeMilliseconds: integer);
begin
   title := heading;
   text := msg;
   showCancelButton := false;
   timer := timeMilliseconds;
   alertType := TAlertType.warning;
   ShowMessage;
end;

procedure TUniSweetAlert.WebCreate;
begin
  inherited;
  JSComponent := TJSObject.JSCreate('Object');
end;

initialization

 UniAddCSSLibrary('sweetalert2/sweetalert2.min.css',False,[upoFolderFiles,upoPlatformBoth]);
 UniAddJSLibrary('sweetalert2/sweetalert2.min.js',False,[upoFolderFiles,upoPlatformBoth]);
 UniAddCSSLibrary('sweetalert2/unisweetalert2.css',False,[upoFolderFiles,upoPlatformBoth]);


end.
