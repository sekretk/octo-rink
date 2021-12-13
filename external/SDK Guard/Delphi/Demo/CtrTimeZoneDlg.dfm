object fmCtrTimeZone: TfmCtrTimeZone
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Controller Time Zone'
  ClientHeight = 171
  ClientWidth = 340
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
    340
    171)
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton
    Left = 257
    Top = 138
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    ExplicitLeft = 162
    ExplicitTop = 147
  end
  object btnOk: TButton
    Left = 176
    Top = 138
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
    ExplicitLeft = 81
    ExplicitTop = 147
  end
  object GroupBox1: TGroupBox
    Left = 119
    Top = 0
    Width = 210
    Height = 65
    Caption = 'Time'
    TabOrder = 2
    object Label1: TLabel
      Left = 23
      Top = 27
      Width = 26
      Height = 16
      Alignment = taRightJustify
      Caption = 'from'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 124
      Top = 27
      Width = 11
      Height = 16
      Alignment = taRightJustify
      Caption = 'to'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object dtpFrom: TDateTimePicker
      Left = 55
      Top = 24
      Width = 58
      Height = 21
      Date = 40784.640990115740000000
      Format = 'HH:mm'
      Time = 40784.640990115740000000
      Kind = dtkTime
      TabOrder = 0
    end
    object dtpTo: TDateTimePicker
      Left = 141
      Top = 24
      Width = 58
      Height = 21
      Date = 40784.640990115740000000
      Format = 'HH:mm'
      Time = 40784.640990115740000000
      Kind = dtkTime
      TabOrder = 1
    end
  end
  object grDaysOfWeek: TGroupBox
    Left = -1
    Top = 0
    Width = 114
    Height = 137
    Caption = 'Days of week'
    TabOrder = 3
    object lbDows: TCheckListBox
      Left = 11
      Top = 20
      Width = 94
      Height = 109
      ItemHeight = 13
      Items.Strings = (
        'Monday'
        'Tuesday'
        'Wednesday'
        'Thursday'
        'Friday'
        'Saturday'
        'Sunday')
      TabOrder = 0
    end
  end
  object grCtrMode: TGroupBox
    Left = 119
    Top = 71
    Width = 210
    Height = 58
    Caption = 'Mode'
    TabOrder = 4
    object cbMode: TComboBox
      Left = 16
      Top = 21
      Width = 177
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Normal'
      Items.Strings = (
        'Normal'
        'Block'
        'Free'
        'Wait')
    end
  end
end
