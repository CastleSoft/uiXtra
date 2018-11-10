unit uniTyped;
{------------------------------------------------------------------------------}
{                                                                              }
{ uniTyped                                                                     }
{ =============                                                                }
{                                                                              }
{ Description:  This class contains a wrapper for the iTyped javascript        }
{               library.                                                       }
{                                                                              }
{               Github:  https://github.com/luisvinicius167/ityped             }
{                                                                              }
{ Installation: Under the files folder copy the following files:               }
{               ityped/ityped.min.js                                           }
{                                                                              }
{ This source code is copyrighted material.                                    }
{                                                                              }
{ Copyright (c) CastleSoft Pty Ltd. 2017-2018. All rights reserved.            }
{                                                                              }
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
{ Version       Date          Description            Modified by               }
{ ---------------------------------------------------------------------------- }
{  1.0.0        04-Aug-2017   Initial Release          Andrew Tierney          }
{  1.0.2        02-Jun-2018   Fix Memory Leak JSON     Andrew Tierney          }
{  1.0.5        31-Jul-2018   Minor tweaks             Andrew Tierney          }
{  1.0.6        22-Aug-2018   Changed license to MIT   Andrew Tierney          }
{                                                                              }
{------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, uniGUIBaseClasses,
  uniGUIClasses, uniGUITypes, uniGUIJSUtils, uniGUIApplication, uniPanel, uniHTMLFrame, System.Json;

type
  TUniTyped = class(TUniCustomHTMLFrame)
  private
    { Private declarations }
    FOnFinished : TNotifyEvent;
    fText: TStrings;
    fTypeSpeed: integer;
    fBackSpeed: integer;
    fStartDelay: integer;
    fBackDelay: integer;
    fLoop: Boolean;
    fShowCursor: Boolean;
    fCursorChar: char;

    procedure H_OnFinished(Sender: TObject);
    function GetOnFinished: TNotifyEvent;
    procedure SetOnFinished(const Value: TNotifyEvent);

    function BuildHTML(): string;
    procedure SetText(Value: TStrings);
    function StringToJSON(const d: TStrings): TJSONArray;

  protected
    { Protected declarations }
    procedure WebCreate; override;
    procedure JSEventHandler(AEventName: string; AParams: TUniStrings); override;
  public
    { Public declarations }
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure PlayTyped;
  published
    { Published declarations }
    property text: TStrings read fText write SetText;
    property typeSpeed: integer read fTypeSpeed write fTypeSpeed;
    property backSpeed: integer read fBackSpeed write fBackSpeed;
    property startDelay: integer read fStartDelay write fStartDelay;
    property backDelay: integer read fBackDelay write fBackDelay;
    property loop: Boolean read fLoop write fLoop;
    property showCursor: Boolean read fShowCursor write fShowCursor;
    property cursorChar: char read fCursorChar write fCursorChar;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('uniGUI Extensions', [TUniTyped]);
end;

{ TUniTyped }

function TUniTyped.BuildHTML: string;
var bstr: string;
    js : TJSONArray;
begin
  js := StringToJSON(text);         // Keep a hook to the created TJSONArray
  bstr := '';
  bstr := bstr + '<div>';
  bstr := bstr + '<span id="'+JSName+'" class="ityped"><span>';
  bstr := bstr + '</div>';
  bstr := bstr + '<script type="text/javascript">';
  bstr := bstr + 'ityped.init("#' + JSName + '",{';
  bstr := bstr + 'strings: ' + js.ToString + ',';
  bstr := bstr + 'typeSpeed: ' + IntToStr(typeSpeed) + ',';
  bstr := bstr + 'backSpeed: ' + IntToStr(backSpeed) + ',';
  bstr := bstr + 'startDelay: ' + IntToStr(startDelay) + ',';
  bstr := bstr + 'backDelay: ' + IntToStr(backDelay) + ',';
  bstr := bstr + 'loop: ' + BoolToStr(loop) + ',';
  bstr := bstr + 'showCursor: ' + BoolToStr(showCursor) + ',';
  bstr := bstr + 'cursorChar: ' + '"' + cursorChar + '",';
  bstr := bstr + 'onFinished: function() { ajaxRequest('+JSName+',"onFinished",[]); },';
  bstr := bstr + '});';
  bstr := bstr + '</script>';
  result := bstr;
  js.Destroy;                      // Destroy and NIL the TJSONArray
  js := nil;
end;


procedure TUniTyped.Clear;
begin
   fText.Clear;
   typeSpeed := 55;
   backSpeed := 30;
   startDelay := 500;
   backDelay := 500;
   loop := true;
   showCursor := true;
   cursorChar := '|';
end;

constructor TUniTyped.Create(owner: TComponent);
begin
  inherited;
  fText := TStringList.Create;
  clear;
end;

destructor TUniTyped.Destroy;
begin
  fText.Free;
  inherited;
end;

function TUniTyped.GetOnFinished: TNotifyEvent;
begin
 result := FOnFinished;
end;


procedure TUniTyped.H_OnFinished(Sender: TObject);
begin
    if Assigned(FOnFinished) then
       FOnFinished(Sender);
end;

procedure TUniTyped.PlayTyped;
begin
  HTML.Text := BuildHTML;
end;

procedure TUniTyped.JSEventHandler(AEventName: string; AParams: TUniStrings);
begin
  inherited;

  if AEventName = 'onFinished' then
  begin
     H_OnFinished(self);
  end;

end;

procedure TUniTyped.SetOnFinished(const Value: TNotifyEvent);
begin
   FOnFinished := Value;
end;

procedure TUniTyped.SetText(Value: TStrings);
begin
  fText.Assign(Value);
end;

function TUniTyped.StringToJSON(const d: TStrings): TJSONArray;
var
  I: Integer;
begin
  Result := TJSONArray.Create;
  for I := 0 to d.Count - 1 do
    Result.Add(d[I]);
end;

procedure TUniTyped.WebCreate;
begin
  inherited;
  // Optional Webcreate extras
end;

initialization

 UniAddJSLibrary('ityped/ityped.min.js',False,[upoFolderFiles,upoPlatformBoth]);
 UniAddCSSLibrary('ityped/unityped.css',False,[upoFolderFiles,upoPlatformBoth]);


end.
