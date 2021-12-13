object fmCtrSchedule: TfmCtrSchedule
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Controller Schedule'
  ClientHeight = 270
  ClientWidth = 606
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    606
    270)
  PixelsPerInch = 96
  TextHeight = 13
  object grTimeZonesOut: TGroupBox
    Left = 303
    Top = 75
    Width = 298
    Height = 192
    Caption = 'Time zones (out)'
    TabOrder = 0
    object lbTzsOut: TListBox
      Left = 10
      Top = 17
      Width = 282
      Height = 136
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier New'
      Font.Style = []
      ItemHeight = 17
      Items.Strings = (
        '1 _ _ _ _ _ _  __:__-__:__'
        '2 _ _ _ _ _ _  __:__-__:__'
        '3 _ _ _ _ _ _  __:__-__:__'
        '4 _ _ _ _ _ _  __:__-__:__'
        '5 _ _ _ _ _ _  __:__-__:__'
        '6 _ _ _ _ _ _  __:__-__:__'
        '7 _ _ _ _ _ _  __:__-__:__')
      ParentFont = False
      TabOrder = 0
      OnDblClick = lbTzsOutDblClick
    end
    object btnTzOutDefault: TButton
      Left = 10
      Top = 159
      Width = 95
      Height = 25
      Caption = 'Defaullt'
      TabOrder = 1
      OnClick = btnTzOutDefaultClick
    end
    object btnChangeTZOut: TButton
      Left = 159
      Top = 159
      Width = 125
      Height = 25
      Caption = 'Change Time Zone...'
      TabOrder = 2
      OnClick = btnChangeTZOutClick
    end
  end
  object grTimeZones: TGroupBox
    Left = 0
    Top = 75
    Width = 297
    Height = 192
    Caption = 'Time zones'
    TabOrder = 1
    object btnTzDefault: TButton
      Left = 10
      Top = 159
      Width = 95
      Height = 25
      Caption = 'Defaullt'
      TabOrder = 0
      OnClick = btnTzDefaultClick
    end
    object lbTzs: TListBox
      Left = 10
      Top = 17
      Width = 282
      Height = 136
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier New'
      Font.Style = []
      ItemHeight = 17
      Items.Strings = (
        '1 _ _ _ _ _ _  __:__-__:__'
        '2 _ _ _ _ _ _  __:__-__:__'
        '3 _ _ _ _ _ _  __:__-__:__'
        '4 _ _ _ _ _ _  __:__-__:__'
        '5 _ _ _ _ _ _  __:__-__:__'
        '6 _ _ _ _ _ _  __:__-__:__'
        '7 _ _ _ _ _ _  __:__-__:__')
      ParentFont = False
      TabOrder = 1
      OnDblClick = lbTzsDblClick
    end
    object btnChangeTZ: TButton
      Left = 159
      Top = 159
      Width = 125
      Height = 25
      Caption = 'Change Time Zone...'
      TabOrder = 2
      OnClick = btnChangeTZClick
    end
  end
  object grCtrClock: TGroupBox
    Left = 0
    Top = 0
    Width = 601
    Height = 75
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Controller clock'
    TabOrder = 2
    DesignSize = (
      601
      75)
    object labCtrCurrTime: TLabel
      Left = 33
      Top = 23
      Width = 120
      Height = 13
      Alignment = taRightJustify
      Caption = 'Controller date and time:'
    end
    object labSysCurrTime: TLabel
      Left = 45
      Top = 46
      Width = 108
      Height = 13
      Alignment = taRightJustify
      Caption = 'System date and time:'
    end
    object labCurCtrTime: TLabel
      Left = 159
      Top = 20
      Width = 164
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = '--:--:--'
      Color = clWhite
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object labCurSysTime: TLabel
      Left = 159
      Top = 43
      Width = 164
      Height = 17
      Alignment = taCenter
      AutoSize = False
      Caption = '03.01.2006 12:48:57'
      Color = clWhite
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object btnTimeSync: TButton
      Left = 464
      Top = 16
      Width = 131
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Synchronize'
      TabOrder = 0
      OnClick = btnTimeSyncClick
    end
    object chkClockAutoSync: TCheckBox
      Left = 464
      Top = 44
      Width = 131
      Height = 17
      Anchors = [akTop, akRight]
      Caption = 'Autosyncronize (>=5s)'
      TabOrder = 1
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 300
    OnTimer = Timer1Timer
    Left = 424
    Top = 8
  end
end
