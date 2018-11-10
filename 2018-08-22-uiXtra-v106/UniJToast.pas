unit uniJToast;
{------------------------------------------------------------------------------}
{                                                                              }
{ uniJToast                                                                    }
{ =============                                                                }
{                                                                              }
{ Description:  This class contains a wrapper for the JToast javascript        }
{               library.                                                       }
{                                                                              }
{               Github:  https://github.com/kamranahmedse/jquery-toast-plugin  }
{                                                                              }
{ Installation: Under the files folder copy the following files:               }
{               jtoast/jquery.toast.min.css                                    }
{               jtoast/jquery.toast.min.js                                     }
{                                                                              }
{ This source code is copyrighted material.                                    }
{                                                                              }
{ Copyright (c) CastleSoft Pty Ltd. 2017-2018. All rights reserved.            }
{                                                                              {
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
{ Version       Date          Description            Modified by               }
{ ---------------------------------------------------------------------------- }
{  1.0.0        29-Jul-2017   Initial Release          Andrew Tierney          }
{  1.0.1        03-Aug-2017   Fixed Z-Index,           Andrew Tierney          }
{                             added custom/position,                           }
{                             textcolor                                        }
{  1.0.2        02-Jun-2018   Tweak for Tokyo          Andrew Tierney          }
{  1.0.5        31-Jul-2018   Minor tweaks             Andrew Tierney          }
{  1.0.6        22-Aug-2018   License changed to MIT   Andrew Tierney          }
{                                                                              }
{------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, uniGUIBaseClasses, uniGUIClasses, uniGUITypes, uniGUIJSUtils, uniGUIApplication, RTTI;

type TIconType = (none, success, error, warning, info);
type TPosition = (bottom_left,bottom_right,bottom_center,top_left,top_right,top_center,mid_center,custom);
type TTextAlign    = (left,right,center);
type TTransition = (fade, slide, plain);

type
  TUniJToast = class(TUniComponent)
  private
    { Private declarations }

    { ClientSide events raised on the server via AJAX }
    FOnBeforeShow : TNotifyEvent;
    FOnAfterShown : TNotifyEvent;
    FOnBeforeHide : TNotifyEvent;
    FOnAfterHidden : TNotifyEvent;
    fText: string;
    fHeading: string;
    fIconType: TIconType;
    fShowHideTransition: TTransition;
    fAllowToastClose: Boolean;
    fHideAfter: integer;
    fStack: Boolean;
    fStackSize: integer;
    fPos: TPosition;
    fTextAlign: TTextAlign;
    fLoader: Boolean;
    fLoaderBg: string;
    fbgColor: string;
    ftextColor: string;
    fleft: Integer;
    ftop: Integer;

    procedure H_OnBeforeShow(Sender: TObject);
    procedure H_OnAfterShown(Sender: TObject);
    procedure H_OnBeforeHide(Sender: TObject);
    procedure H_OnAfterHidden(Sender: TObject);

    function GetOnBeforeShow: TNotifyEvent;
    function GetOnAfterShown: TNotifyEvent;
    function GetOnBeforeHide: TNotifyEvent;
    function GetOnAfterHidden: TNotifyEvent;

    procedure SetOnBeforeShow(const Value: TNotifyEvent);
    procedure SetOnAfterShown(const Value: TNotifyEvent);
    procedure SetOnBeforeHide(const Value: TNotifyEvent);
    procedure SetOnAfterHidden(const Value: TNotifyEvent);

    function BuildJsonParams(): string;
    function GetIconType(i: TIconType): string;
    function GetTransition(i: TTransition): string;
    function GetPosition(i: TPosition): string;
    function GetTextAlign(i: TTextAlign): string;


  protected
    { Protected declarations }
    function VCLComponentClassName: string; override;
    procedure WebCreate; override;
    procedure JSEventHandler(AEventName: string; AParams: TUniStrings); override;
  public
    { Public declarations }
  published
    property text: string read fText write fText;
    property heading: string read fHeading write fHeading;
    property icon: TIconType read fIconType write fIconType;
    property showHideTransition: TTransition read fShowHideTransition write fShowHideTransition;
    property allowToastClose: Boolean read fAllowToastClose write fAllowToastClose;
    property hideAfter: integer read fHideAfter write fHideAfter;
    property stack: Boolean read fStack write fStack;
    property stackSize: integer read fStackSize write fStackSize;
    property pos: TPosition read fPos write fPos;
    property textAlign: TTextAlign read fTextAlign write fTextAlign;
    property loader: Boolean read fLoader write fLoader;
    property loaderBg: string read fLoaderBg write fLoaderBg;

    property bgColor: string read fbgColor write fbgColor;
    property textColor: string read ftextColor write ftextColor;

    property left: Integer read fleft write fleft;
    property top: Integer read ftop write ftop;

    { ClientSide events raised on the server via AJAX }
    property OnBeforeShow:  TNotifyEvent read GetOnBeforeShow write SetOnBeforeShow;
    property OnAfterShown:  TNotifyEvent read GetOnAfterShown write SetOnAfterShown;
    property onBeforeHide:  TNotifyEvent read GetOnBeforeHide write SetOnBeforeHide;
    property OnAfterHidden: TNotifyEvent read GetOnAfterHidden write SetOnAfterHidden;

    procedure Clear;
    procedure ShowMessage;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('uniGUI Extensions', [TUniJToast]);
end;

{ TUniJToast }

function TUniJToast.BuildJsonParams: string;
var bstr: string;
begin
   bstr := '{';
   bstr := bstr + 'text:' + '"' + text + '",';
   bstr := bstr + 'heading:' + '"' + heading + '",';
   bstr := bstr + 'icon:'    + '"' + GetIconType(icon) + '",';
   bstr := bstr + 'showHideTransition:' + '"' + GetTransition(showHideTransition) + '",';
   bstr := bstr + 'allowToastClose:' + BoolToStr(allowToastClose) + ',';

   if hideAfter > 0 then
      bstr := bstr + 'hideAfter:' + IntToStr(hideAfter) + ',';

   if (stack) then
      bstr := bstr + 'stack:' + IntToStr(stackSize) +','
   else
      bstr := bstr + 'stack:' + BoolToStr(stack) + ',';

   bstr := bstr + 'position:' + '"' + GetPosition(pos) + '",';
   bstr := bstr + 'textAlign:' + '"' + GetTextAlign(textAlign)  + '",';
   bstr := bstr + 'loader:' + BoolToStr(loader) +',';
   bstr := bstr + 'loaderBg:' + '"' + loaderBg + '",';
   bstr := bstr + 'bgColor:' + '"' + bgColor +'",';
   bstr := bstr + 'textColor:' + '"' + textColor + '",';

   // Add Callbacks here
   bstr := bstr + 'beforeShow: function() { ajaxRequest('+JSName+',"beforeShow",[]); },';
   bstr := bstr + 'afterShown: function() { ajaxRequest('+JSName+',"afterShown",[]); },';
   bstr := bstr + 'beforeHide: function() { ajaxRequest('+JSName+',"beforeHide",[]); },';
   bstr := bstr + 'afterHidden: function() { ajaxRequest('+JSName+',"afterHidden",[]); }';
   bstr := bstr + '}';
   result := bstr;
end;

procedure TUniJToast.Clear;
begin
  // Clear Properties here..
  text    := '';
  heading := '';
  icon    := TIconType.none;
  showHideTransition := TTransition.plain;
  allowToastClose := true;
  hideAfter := 0;
  stack     := true;
  stackSize := 5;
  pos       := TPosition.mid_center;
  textAlign := TTextAlign.center;
  loader    := true;
  loaderBg  := '#9EC600';
  bgColor   := '#444';
  textColor := '#eee';
  left      := 0;
  top       := 0;
end;

function TUniJToast.GetIconType(i: TIconType): string;
var res: string;
begin
  if (i = TIconType.none) then
      res := ''
  else
      res := TRttiEnumerationType.GetName(i);

  result := res;
end;

function TUniJToast.GetOnAfterHidden: TNotifyEvent;
begin
  result := FOnAfterHidden;
end;

function TUniJToast.GetOnAfterShown: TNotifyEvent;
begin
  result := FOnAfterShown;
end;

function TUniJToast.GetOnBeforeHide: TNotifyEvent;
begin
  result := FOnBeforeHide;
end;

function TUniJToast.GetOnBeforeShow: TNotifyEvent;
begin
  result := FOnBeforeShow;
end;

function TUniJToast.GetPosition(i: TPosition): string;
var enumStr: string;
    fixedName: string;
begin
     enumStr := TRttiEnumerationType.GetName(i);

     if (enumStr='custom') then
     begin
        fixedName := 'left: ' + IntToStr(left) + ',';
        fixedName := 'top: ' + IntToStr(top);
     end
     else
     begin
         fixedName := stringreplace(enumStr,'_','-',[rfReplaceAll,rfIgnoreCase]);
     end;
     result := fixedName;
end;

function TUniJToast.GetTextAlign(i: TTextAlign): string;
begin
     result := TRttiEnumerationType.GetName(i);
end;

function TUniJToast.GetTransition(i: TTransition): string;
begin
     result := TRttiEnumerationType.GetName(i);
end;

procedure TUniJToast.H_OnAfterHidden(Sender: TObject);
begin
    if Assigned(FOnAfterHidden) then
       FOnAfterHidden(Sender);
end;

procedure TUniJToast.H_OnAfterShown(Sender: TObject);
begin
    if Assigned(FOnAfterShown) then
       FOnAfterShown(Sender);
end;

procedure TUniJToast.H_OnBeforeHide(Sender: TObject);
begin
    if Assigned(FOnBeforeHide) then
       FOnBeforeHide(Sender);
end;

procedure TUniJToast.H_OnBeforeShow(Sender: TObject);
begin
    if Assigned(FOnBeforeShow) then
       FOnBeforeShow(Sender);
end;

procedure TUniJToast.JSEventHandler(AEventName: string; AParams: TUniStrings);
begin
  inherited;

  if AEventName = 'beforeShow' then
  begin
      H_OnBeforeShow(self);
  end;

  if AEventName = 'afterShown' then
  begin
      H_OnAfterShown(self);
  end;

  if AEventName = 'beforeHide' then
  begin
      H_OnBeforeHide(self);
  end;

  if AEventName = 'afterHidden' then
  begin
      H_OnAfterHidden(self);
  end;

end;

procedure TUniJToast.SetOnAfterHidden(const Value: TNotifyEvent);
begin
    FOnAfterHidden := Value;
end;

procedure TUniJToast.SetOnAfterShown(const Value: TNotifyEvent);
begin
   FOnAfterShown := Value;
end;

procedure TUniJToast.SetOnBeforeHide(const Value: TNotifyEvent);
begin
  FOnBeforeHide := Value;
end;

procedure TUniJToast.SetOnBeforeShow(const Value: TNotifyEvent);
begin
  FOnBeforeShow := Value;
end;

procedure TUniJToast.ShowMessage;
var jsonSweetAlert: string;
begin
   jsonSweetAlert := BuildJsonParams;
   UniSession.AddJS('$.toast(' + jsonSweetAlert + ');');
end;

function TUniJToast.VCLComponentClassName: string;
begin
  result := 'TUniJToast';
end;

procedure TUniJToast.WebCreate;
begin
  inherited;
  JSComponent := TJSObject.JSCreate('Object');
end;

initialization

 UniAddCSSLibrary('jtoast/jquery.toast.min.css',False,[upoFolderFiles,upoPlatformBoth]);
 UniAddJSLibrary('jtoast/jquery.toast.min.js',False,[upoFolderFiles,upoPlatformBoth]);
 UniAddCSSLibrary('jtoast/unijtoast.css',False,[upoFolderFiles,upoPlatformBoth]);

end.
