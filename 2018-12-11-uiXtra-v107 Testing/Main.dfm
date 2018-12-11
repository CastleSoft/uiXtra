object MainForm: TMainForm
  Left = 0
  Top = 0
  ClientHeight = 488
  ClientWidth = 634
  Caption = 'MainForm'
  OnShow = UniFormShow
  OldCreateOrder = False
  MonitoredKeys.Keys = <>
  DesignSize = (
    634
    488)
  PixelsPerInch = 96
  TextHeight = 13
  object UniButton1: TUniButton
    Left = 8
    Top = 24
    Width = 75
    Height = 25
    Hint = ''
    Caption = 'Success'
    TabOrder = 0
    OnClick = UniButton1Click
  end
  object UniButton2: TUniButton
    Left = 112
    Top = 24
    Width = 75
    Height = 25
    Hint = ''
    Caption = 'Question'
    TabOrder = 1
    OnClick = UniButton2Click
  end
  object UniButton3: TUniButton
    Left = 232
    Top = 24
    Width = 75
    Height = 25
    Hint = ''
    Caption = 'Toast'
    TabOrder = 2
    OnClick = UniButton3Click
  end
  object UniMemo1: TUniMemo
    Left = 8
    Top = 88
    Width = 619
    Height = 203
    Hint = ''
    Lines.Strings = (
      'UniMemo1')
    TabOrder = 3
  end
  object UniTyped1: TUniTyped
    Left = 8
    Top = 297
    Width = 619
    Height = 80
    Hint = ''
    text.Strings = (
      'Welcome'
      'This is a test'
      'Restarting soon..')
    typeSpeed = 55
    backSpeed = 30
    startDelay = 500
    backDelay = 500
    loop = True
    showCursor = True
    cursorChar = '|'
  end
  object UniButton4: TUniButton
    Left = 344
    Top = 24
    Width = 75
    Height = 25
    Hint = ''
    Caption = 'Play Typed'
    TabOrder = 5
    OnClick = UniButton4Click
  end
  object UniEditValidate1: TUniEditValidate
    Left = 440
    Top = 51
    Width = 187
    Hint = ''
    Text = 'UniEditValidate1'
    TabOrder = 6
    ClientEvents.UniEvents.Strings = (
      
        'beforeinit=function beforeInit(sender, config){   Ext.apply(send' +
        'er,{allowBlank:false,vtype:'#39'email'#39',msgTarget : '#39'side'#39'});   }')
    Validate = vEmail
  end
  object UniLabel1: TUniLabel
    Left = 440
    Top = 24
    Width = 147
    Height = 13
    Hint = ''
    Caption = 'Please Enter an email address:'
    TabOrder = 7
  end
  object UniBubbleBg1: TUniBubbleBg
    Left = 8
    Top = 385
    Width = 618
    Height = 95
    Hint = ''
    Anchors = [akLeft, akTop, akRight, akBottom]
    Animate = True
    Blur = 4
    BubbleFunc = '() => `hsla(0, 0%, 100%, ${Math.random() * 0.1})`'
    Bubbles = 100
    ColorStart = '#2AE'
    ColorStop = '#17B'
    Compose = 'lighter'
    ShadowColor = '#fff'
    AngleFunc = '() => Math.random() * Math.PI * 2'
    VelocityFunc = '() => 0.1 + Math.random() * 0.5'
    RadiusFunc = '() => 4 + Math.random() * 25'
  end
  object UniSweetAlert1: TUniSweetAlert
    alertType = success
    target = 'body'
    inputType = none
    width = '500px'
    padding = 20
    background = '#fff'
    timer = 0
    animation = True
    allowOutsideClick = False
    allowEscapeKey = True
    allowEnterKey = True
    showConfirmButton = True
    showCancelButton = False
    confirmButtonText = 'OK'
    cancelButtonText = 'Cancel'
    confirmButtonColor = '#3085d6'
    cancelButtonColor = '#aaa'
    buttonsStyling = True
    reverseButtons = False
    focusCancel = False
    showCloseButton = False
    showLoaderOnConfirm = False
    OnSuccess = UniSweetAlert1Success
    OnDismiss = UniSweetAlert1Dismiss
    Left = 368
    Top = 56
  end
  object UniJToast1: TUniJToast
    icon = none
    showHideTransition = fade
    allowToastClose = False
    hideAfter = 0
    stack = False
    stackSize = 0
    pos = bottom_left
    textAlign = left
    loader = False
    left = 288
    top = 56
    OnBeforeShow = UniJToast1BeforeShow
    OnAfterShown = UniJToast1AfterShown
    onBeforeHide = UniJToast1BeforeHide
    OnAfterHidden = UniJToast1AfterHidden
  end
end
