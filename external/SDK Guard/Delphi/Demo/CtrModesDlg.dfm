object fmCtrModes: TfmCtrModes
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Controller Modes'
  ClientHeight = 209
  ClientWidth = 405
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
    405
    209)
  PixelsPerInch = 96
  TextHeight = 13
  object labCurrMode: TLabel
    Left = 32
    Top = 16
    Width = 70
    Height = 13
    Alignment = taRightJustify
    Caption = 'Current mode:'
  end
  object cbCurrMode: TComboBox
    Left = 108
    Top = 13
    Width = 111
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 0
    Text = 'Normal'
    OnChange = cbCurrModeChange
    Items.Strings = (
      'Normal'
      'Block'
      'Free'
      'Wait')
  end
  object grTimeZones: TGroupBox
    Left = 3
    Top = 98
    Width = 394
    Height = 105
    Caption = 'Time zones'
    TabOrder = 1
    object btnTzDefault: TButton
      Left = 10
      Top = 71
      Width = 95
      Height = 25
      Caption = 'Defaullt'
      TabOrder = 0
      OnClick = btnTzDefaultClick
    end
    object lbTzs: TListBox
      Left = 10
      Top = 17
      Width = 375
      Height = 48
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Courier New'
      Font.Style = []
      ItemHeight = 17
      Items.Strings = (
        '1 _ _ _ _ _ _  __:__-__:__'
        '2 _ _ _ _ _ _  __:__-__:__')
      ParentFont = False
      TabOrder = 1
    end
    object btnChangeTZ: TButton
      Left = 260
      Top = 71
      Width = 125
      Height = 25
      Caption = 'Change Time Zone...'
      TabOrder = 2
      OnClick = btnChangeTZClick
    end
  end
  object btnRefresh: TButton
    Left = 225
    Top = 11
    Width = 75
    Height = 25
    Caption = 'Refresh'
    TabOrder = 2
    OnClick = btnRefreshClick
  end
  object lbStateFlags: TListBox
    Left = 3
    Top = 42
    Width = 394
    Height = 50
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    Items.Strings = (
      '[v] time zone'
      '[v] Remote command'
      '[v] Card')
    TabOrder = 3
    ExplicitWidth = 297
  end
end
