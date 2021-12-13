object fmCtrLock: TfmCtrLock
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Controller Lock Times'
  ClientHeight = 223
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    556
    223)
  PixelsPerInch = 96
  TextHeight = 13
  object grLockOut: TGroupBox
    Left = 279
    Top = 3
    Width = 273
    Height = 174
    Caption = 'Lock Times (out)'
    TabOrder = 0
    object Label1: TLabel
      Left = 66
      Top = 86
      Width = 113
      Height = 13
      Alignment = taRightJustify
      Caption = 'Control open time (ms):'
      ParentShowHint = False
      ShowHint = True
    end
    object Label2: TLabel
      Left = 66
      Top = 59
      Width = 113
      Height = 13
      Alignment = taRightJustify
      Caption = 'Control close time (ms):'
      ParentShowHint = False
      ShowHint = True
    end
    object Label3: TLabel
      Left = 102
      Top = 32
      Width = 77
      Height = 13
      Alignment = taRightJustify
      Caption = 'Open time (ms):'
      ParentShowHint = False
      ShowHint = True
    end
    object edtLockMaxTimeOut: TSpinEdit
      Left = 185
      Top = 83
      Width = 56
      Height = 22
      Increment = 100
      MaxValue = 25500
      MinValue = 0
      TabOrder = 0
      Value = 1000
    end
    object edtLockLetTimeOut: TSpinEdit
      Left = 185
      Top = 56
      Width = 56
      Height = 22
      Increment = 100
      MaxValue = 25500
      MinValue = 0
      TabOrder = 1
      Value = 1000
    end
    object edtLockOpenTimeOut: TSpinEdit
      Left = 185
      Top = 29
      Width = 56
      Height = 22
      Increment = 100
      MaxValue = 25500
      MinValue = 0
      TabOrder = 2
      Value = 3000
    end
    object btnLockTimes2Defaullt: TButton
      Left = 13
      Top = 135
      Width = 125
      Height = 25
      Caption = 'Defaullt'
      TabOrder = 3
      OnClick = btnLockTimes2DefaulltClick
    end
  end
  object grLockTimes: TGroupBox
    Left = 3
    Top = 3
    Width = 270
    Height = 174
    Caption = 'Lock Times'
    TabOrder = 1
    object labLockMaxTime: TLabel
      Left = 66
      Top = 86
      Width = 113
      Height = 13
      Alignment = taRightJustify
      Caption = 'Control open time (ms):'
      ParentShowHint = False
      ShowHint = True
    end
    object labLockLetTime: TLabel
      Left = 66
      Top = 59
      Width = 113
      Height = 13
      Alignment = taRightJustify
      Caption = 'Control close time (ms):'
      ParentShowHint = False
      ShowHint = True
    end
    object labLockOpenTime: TLabel
      Left = 102
      Top = 32
      Width = 77
      Height = 13
      Alignment = taRightJustify
      Caption = 'Open time (ms):'
      ParentShowHint = False
      ShowHint = True
    end
    object edtLockMaxTime: TSpinEdit
      Left = 185
      Top = 83
      Width = 56
      Height = 22
      Increment = 100
      MaxValue = 25500
      MinValue = 0
      TabOrder = 0
      Value = 1000
    end
    object edtLockLetTime: TSpinEdit
      Left = 185
      Top = 56
      Width = 56
      Height = 22
      Increment = 100
      MaxValue = 25500
      MinValue = 0
      TabOrder = 1
      Value = 1000
    end
    object edtLockOpenTime: TSpinEdit
      Left = 185
      Top = 29
      Width = 56
      Height = 22
      Increment = 100
      MaxValue = 25500
      MinValue = 0
      TabOrder = 2
      Value = 3000
    end
    object btnLockTimesDefaullt: TButton
      Left = 15
      Top = 135
      Width = 125
      Height = 25
      Caption = 'Defaullt'
      TabOrder = 3
      OnClick = btnLockTimesDefaulltClick
    end
  end
  object btnOk: TButton
    Left = 392
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 473
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
