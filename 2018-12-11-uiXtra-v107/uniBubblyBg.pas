unit uniBubblyBg;
{------------------------------------------------------------------------------}
{                                                                              }
{ uniBubblyBg                                                                  }
{ =============                                                                }
{                                                                              }
{ Description:  This class contains a wrapper for the Bubbly-Bg javascript     }
{               library.                                                       }
{                                                                              }
{               Github:  https://github.com/tipsy/bubbly-bg                    }
{                        https://github.com/marcj/css-element-queries          }
{                                                                              }
{ Installation: Under the files folder copy the following files:               }
{               bubbly-bg/bubbly-bg.js                                         }
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
{ Version       Date          Description             Modified by              }
{ ---------------------------------------------------------------------------- }
{  1.0.0        30-Jul-2018   Initial Release         Andrew Tierney           }
{  1.0.5        31-Jul-2018   Minor tweaks            Andrew Tierney           }
{  1.0.6        22-Aug-2018   License changed to MIT  Andrew Tierney           }
{                                                                              }
{------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, uniGUIBaseClasses,
  uniGUIClasses, uniGUITypes, uniGUIJSUtils, uniGUIApplication, uniPanel, uniHTMLFrame, System.Json;

type
  TUniBubbleBg = class(TUniCustomHTMLFrame)
  private
    { Private declarations }
    fAnimate: Boolean;
    fBlur: integer;
    fBubbleFunc: string;
    fBubbles: integer;
    fColorStart: string;
    fColorStop: string;
    fCompose: string;
    fShadowColor: string;
    fAngleFunc: string;
    fVelocityFunc: string;
    fRadiusFunc: string;

  protected
    { Protected declarations }
    procedure WebCreate; override;
    procedure Defaults;
  public
    { Public declarations }
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    function BuildHTML(): string;
    procedure ForceResize();
  published
    { Published declarations }
    property Align;
    property Anchors;
    property Animate:Boolean read fAnimate write fAnimate;
    property Blur:integer read fBlur write fBlur;
    property BubbleFunc:string read fBubbleFunc write fBubbleFunc;
    property Bubbles:integer read fBubbles write fBubbles;
    property ColorStart:string read fColorStart write fColorStart;
    property ColorStop:string read fColorStop write fColorStop;
    property Compose:string read fCompose write fCompose;
    property ShadowColor:string read fShadowColor write fShadowColor;
    property AngleFunc:string read fAngleFunc write fAngleFunc;
    property VelocityFunc:string read fVelocityFunc write fVelocityFunc;
    property RadiusFunc:string read fRadiusFunc write fRadiusFunc;
    property OnResize;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('uniGUI Extensions', [TUniBubbleBg]);
end;

{ TUniBubbleBg }

function TUniBubbleBg.BuildHTML: string;
var bstr: string;
begin
  bstr := '';
  bstr := bstr + '<canvas id="'+JSName+'"></canvas>';
  bstr := bstr + '<script type="text/javascript">';

  bstr := bstr + 'function resizeCanv(){';
  bstr := bstr + '    var canvas = $( "canvas#'+JSName+'" )[0];';
  bstr := bstr + '    canvas.width = canvas.parentElement.clientWidth;';
  bstr := bstr + '    canvas.height = canvas.parentElement.clientHeight;';
  bstr := bstr + '}';

  bstr := bstr + 'function bindPar(){';
  bstr := bstr + '        var canvas = $( "canvas#'+JSName+'" )[0];';
  bstr := bstr + '        var pid = canvas.parentElement.id;';
  bstr := bstr + '        var element = document.getElementById(pid);';
  bstr := bstr + '        new ResizeSensor(element, function() {';
  bstr := bstr + '             console.log("Changed to w,h: " + element.clientWidth + " " + element.clientHeight);';
  bstr := bstr + '             canvas.width = element.clientWidth;';
  bstr := bstr + '             canvas.height = element.clientHeight-1;';
  bstr := bstr + '             doBubbly();';
  bstr := bstr + '        });';
  bstr := bstr + '}';

  bstr := bstr + 'function doBubbly(){';
  bstr := bstr + '   bubbly({';
  bstr := bstr + '   animate: ' + LowerCase(BoolToStr(Animate,true)) + ',';
  bstr := bstr + '   blur: ' + IntToStr(Blur) + ',';
  bstr := bstr + '   bubbleFunc: ' + BubbleFunc + ',';
  bstr := bstr + '   bubbles: ' + IntToStr(Bubbles) + ',';
  bstr := bstr + '   canvas: document.querySelector("#'+JSNAME+'"),'; // DRAW on DIV uniGUI
  bstr := bstr + '   colorStart: "' + ColorStart + '",';
  bstr := bstr + '   colorStop: "'  + ColorStop  + '",';
  bstr := bstr + '   compose: "' + Compose + '",';
  bstr := bstr + '   shadowColor: "' + ShadowColor + '",';
  bstr := bstr + '   angleFunc: ' + AngleFunc + ',';
  bstr := bstr + '   velocityFunc: ' + VelocityFunc + ',';
  bstr := bstr + '   radiusFunc: ' + RadiusFunc;
  bstr := bstr + '   });';
  bstr := bstr + '}';

  bstr := bstr + 'bindPar();';
  bstr := bstr + 'resizeCanv();';
  bstr := bstr + 'doBubbly();';

  bstr := bstr + '</script>';
  result := bstr;
end;


procedure TUniBubbleBg.Defaults;
begin
    Animate := true;
    Blur := 4;
    BubbleFunc := '() => `hsla(0, 0%, 100%, ${Math.random() * 0.1})`';
    Bubbles := 100;
    ColorStart := '#2AE';
    ColorStop := '#17B';
    Compose   := 'lighter';
    ShadowColor:='#fff';
    AngleFunc := '() => Math.random() * Math.PI * 2';
    VelocityFunc := '() => 0.1 + Math.random() * 0.5';
    RadiusFunc := '() => 4 + Math.random() * 25';
end;

constructor TUniBubbleBg.Create(owner: TComponent);
begin
  inherited;
  Defaults;
  ForceResize;
end;

destructor TUniBubbleBg.Destroy;
begin
  inherited;
end;

procedure TUniBubbleBg.ForceResize;
begin
      HTML.Text := BuildHTML;
end;

procedure TUniBubbleBg.WebCreate;
begin
  inherited;
  // Optional Webcreate extras
end;

initialization

 UniAddJSLibrary('bubbly-bg/bubbly-bg.js',False,[upoFolderFiles,upoPlatformBoth]);
 UniAddJSLibrary('bubbly-bg/ResizeSensor.js',False,[upoFolderFiles,upoPlatformBoth]);
 UniAddJSLibrary('bubbly-bg/ElementQueries.js',False,[upoFolderFiles,upoPlatformBoth]);



end.
