unit uniEditValidate;
{------------------------------------------------------------------------------}
{                                                                              }
{ uniEditValidate                                                              }
{ ===============                                                              }
{                                                                              }
{ Description:  This class extends the uniEdit giving Validation via           }
{               a red indicator icon..                                         }
{                                                                              }
{ This source code is copyrighted material.                                    }
{                                                                              }
{ Copyright (c) CastleSoft Pty Ltd. 2018. All rights reserved.                 }
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
{  1.0.0        02-Jun-2018   Initial Release          Andrew Tierney          }
{  1.0.3        05-Jun-2018   Cleanup Code             Andrew Tierney          }
{  1.0.5        31-Jul-2018   Minor tweaks             Andrew Tierney          }
{  1.0.6        22-Aug-2018   Changed License to MIT   Andrew Tierney          }
{                                                                              }
{------------------------------------------------------------------------------}
interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, uniGUIBaseClasses,
  uniGUIClasses, uniEdit;

type
    TValidateType = (vNone, vEmail, vAlphaNumeric, vAlpha, vTime, vIpaddress, vUrl, vInteger);

type
  TUniEditValidate = class(TUniEdit)
  private
    { Private declarations }
    FValidateType      : TValidateType;
    // Optionally - Add custom ValidateText in future release
    //FValidateText      : String;
    function BuildCMD: string;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
  published
    { Published declarations }
    property Validate      : TValidateType read FValidateType write FValidateType;
    // Optionally - Add custom ValidateText in future release
    //property ValidateText  : String read FValidateText write FValidateText;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('uniGUI Extensions', [TUniEditValidate]);
end;

{ TUniEditValidate }

constructor TUniEditValidate.Create(AOwner: TComponent);
begin
  inherited;
end;

function TUniEditValidate.BuildCMD: string;
var s: String;
begin
    case Validate of
      vNone:
        begin
          s := '';
        end;
      vEmail:
        begin
           s := 'beforeinit=function beforeInit(sender, config)' +
           '{ ' +
           '  Ext.apply(sender,{allowBlank:false,vtype:''email'',msgTarget : ''side''});   ' +
           '}';
        end;
      vAlphaNumeric:
        begin
          s := 'beforeinit=function beforeInit(sender, config)' +
               '{ ' +
               '    Ext.apply(Ext.form.field.VTypes,' +
               '    { '+
               '        AlphaNum:  function(v) ' +
               '        { ' +
               '          return /^[a-zA-Z0-9_]+$/i.test(v);'+
               '        }, ' +
               '        AlphaNumText: ''Must be an alphanumeric word'', ' +
               '        AlphaNumMask: /[a-z0-9]/i ' +
               '    });' +
               '    Ext.apply(sender,' +
               '    {' +
               '       vtype: ''AlphaNum'', ' +
               '       msgTarget : ''side'' ' +
               '    }); ' +
               '}';
        end;
      vAlpha:
        begin
          s := 'beforeinit=function beforeInit(sender, config)' +
               '{ ' +
               '    Ext.apply(Ext.form.field.VTypes,' +
               '    { '+
               '        AlphaNum:  function(v) ' +
               '        { ' +
               '          return /^[a-zA-Z]+$/i.test(v);'+
               '        }, ' +
               '        AlphaNumText: ''Must be an word containing letters only'', ' +
               '        AlphaNumMask: /[a-z]/i ' +
               '    });' +
               '    Ext.apply(sender,' +
               '    {' +
               '       vtype: ''AlphaNum'', ' +
               '       msgTarget : ''side'' ' +
               '    }); ' +
               '}';
        end;
      vTime:
        begin
           s := 'beforeinit=function beforeInit(sender, config)' +
                '{' +
                   'var timeTest = /^([1-9]|1[0-9]):([0-5][0-9])(\s[a|p]m)$/i;' +
                   'Ext.apply(Ext.form.field.VTypes,'+
                   '{' +
                   '   time: function(val, field)' +
                   '         {'  +
                   '           return timeTest.test(val); ' +
                   '         },' +
                   '   timeText: ''Not a valid time.  Must be in the format "12:34 PM".'', ' +
                   '   timeMask: /[\d\s:amp]/i' +
                   '});' +
                   'Ext.apply(sender,' +
                   '{' +
                   '   name: ''departureTime'', ' +
                   '   vtype: ''time'', ' +
                   '   msgTarget : ''side'' ' +
                   '}); ' +
                '}';
        end;
      vIpaddress:
        begin
           s := 'beforeinit=function beforeInit(sender, config)' +
                '{' +
                    'Ext.apply(Ext.form.field.VTypes, ' +
                    '{' +
                    '  IPAddress:  function(v) {' +
                    '  return /^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$/.test(v);' +
                    '  },' +
                    '  IPAddressText: ''Must be a numeric IP address'', ' +
                    '  IPAddressMask: /[\d\.]/i '+
                    '});' +
                    'Ext.apply(sender,' +
                    '{' +
                        'name: ''IPAddress'','+
                        'vtype: ''IPAddress'','+
                        'msgTarget : ''side'' ' +
                    '});'+
                '}';
        end;
      vUrl:
        begin
           s := 'beforeinit=function beforeInit(sender, config)' +
                '{' +
                    'var url = /(((^https?)|(^ftp)):\/\/([\-\w]+\.)+\w{2,3}(\/[%\-\w]+(\.\w{2,})?)*(([\w\-\.\?\\\/+@&#;`~=%!]*)(\.\w{2,})?)*\/?)/i;' +
                    'Ext.apply(Ext.form.field.VTypes,' +
                    '{ ' +
                        'url:  function(v)' +
                        '{'+
                            'return url.test(v);' +
                        '},' +
                        'urlText: ''Must be a valid URL (ie. http, https or ftp)'' '+
                    '});'+
                    'Ext.apply(sender,' +
                    '{' +
                              'vtype: ''url'','+
                              'msgTarget : ''side'' '+
                    '});' +
                '}';
        end;
      vInteger:
        begin
          s := 'beforeinit=function beforeInit(sender, config)' +
               '{ ' +
               '    Ext.apply(Ext.form.field.VTypes,' +
               '    { '+
               '        AlphaNum:  function(v) ' +
               '        { ' +
               '          return /^[0-9_]+$/i.test(v);'+
               '        }, ' +
               '        AlphaNumText: ''Must be an number'', ' +
               '        AlphaNumMask: /[0-9]/i ' +
               '    });' +
               '    Ext.apply(sender,' +
               '    {' +
               '       vtype: ''AlphaNum'', ' +
               '       msgTarget : ''side'' ' +
               '    }); ' +
               '}';
        end
      else s:= '';
    end;
    result := s;
end;

procedure TUniEditValidate.Loaded;
var s: string;
begin
  inherited;
  s := BuildCMD;
  ClientEvents.UniEvents.Clear;
  ClientEvents.UniEvents.Add(s);
end;

end.
