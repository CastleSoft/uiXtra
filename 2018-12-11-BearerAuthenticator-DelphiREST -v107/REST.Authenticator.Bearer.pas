{------------------------------------------------------------------------------}
{                                                                              }
{             Delphi REST Client Framework                                     }
{             Custom Bearer/Token Authenticator                                }
{                                                                              }
{ REST.Authenticator.Bearer                                                    }
{ =========================                                                    }
{                                                                              }
{ Description:  Custom Bearer/Token Authenticator for REST                     }
{                                                                              }
{ This source code is coprighted material.                                     }
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
{                                                                              }
{ Version       Date          Description               Modified by            }
{ ---------------------------------------------------------------------------- }
{  1.0.3        12-Jun-2016   Initial Release           Andrew Tierney         }
{  1.0.6        22-Aug-2018   License changed to MIT    Andrew Tierney         }
{------------------------------------------------------------------------------}
unit REST.Authenticator.Bearer;

interface

uses
  System.Classes,
  Data.Bind.ObjectScope, Data.Bind.Components,
  REST.Types,
  REST.Client,
  REST.Consts,
  REST.BindSource;

type
  TSubBearerAuthenticationBindSource = class;

  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 or pidiOSSimulator or pidiOSDevice or pidiOSDevice64 or pidAndroid)]
  TBearerAuthenticator = class(TCustomAuthenticator)
  private
    FToken: string;
    FBindSource: TSubBearerAuthenticationBindSource;
    procedure SetToken(const AValue: string);
  protected
    procedure DoAuthenticate(ARequest: TCustomRESTRequest); override;
    function CreateBindSource: TBaseObjectBindSource; override;
  public
    constructor Create(const AToken: string); reintroduce; overload;
  published
    property Token: string read FToken write SetToken;
    property BindSource: TSubBearerAuthenticationBindSource read FBindSource;
  end;

  TSubBearerAuthenticationBindSource = class
    (TRESTAuthenticatorBindSource<TBearerAuthenticator>)
  protected
    function CreateAdapterT
      : TRESTAuthenticatorAdapter<TBearerAuthenticator>; override;
  end;

  TBearerAuthenticatorAdapter = class(TRESTAuthenticatorAdapter<TBearerAuthenticator>)
  protected
    procedure AddFields; override;
  end;

procedure Register;

implementation

constructor TBearerAuthenticator.Create(const AToken: string);
begin
  Create(NIL);
  FToken := AToken;
end;

procedure TBearerAuthenticator.SetToken(const AValue: string);
begin
  if (AValue <> FToken) then
  begin
    FToken := AValue;
    PropertyValueChanged;
  end;
end;

function TBearerAuthenticator.CreateBindSource: TBaseObjectBindSource;
begin
  FBindSource := TSubBearerAuthenticationBindSource.Create(Self);
  FBindSource.Name := 'BindSource'; { Do not localize }
  FBindSource.SetSubComponent(True);
  FBindSource.Authenticator := Self;

  result := FBindSource;
end;

procedure TBearerAuthenticator.DoAuthenticate(ARequest: TCustomRESTRequest);
begin
  inherited;

  ARequest.Params.BeginUpdate;
  try
    ARequest.AddAuthParameter('Authorization', 'Bearer ' + FToken,
      TRESTRequestParameterKind.pkHTTPHEADER,
      [TRESTRequestParameterOption.poDoNotEncode]);
  finally
    ARequest.Params.EndUpdate;
  end;
end;

{ TSubBearerAuthenticationBindSource }

function TSubBearerAuthenticationBindSource.CreateAdapterT: TRESTAuthenticatorAdapter<TBearerAuthenticator>;
begin
  result := TBearerAuthenticatorAdapter.Create(Self);
end;

{ TBearAuthenticatorAdapter }

procedure TBearerAuthenticatorAdapter.AddFields;

const
  sToken = 'Token';
var
  LGetMemberObject: IGetMemberObject;
begin
  CheckInactive;
  ClearFields;
  if Authenticator <> nil then
  begin
    LGetMemberObject := TBindSourceAdapterGetMemberObject.Create(Self);

    CreateReadWriteField<string>(sToken, LGetMemberObject, TScopeMemberType.mtText,
      function: string
      begin
        result := Authenticator.Token;
      end,
      procedure(AValue: string)
      begin
        Authenticator.Token := AValue;
      end);

  end;

end;

procedure Register;
begin
  RegisterComponents('REST Client', [TBearerAuthenticator]);
end;

end.
