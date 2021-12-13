object fmCtrKey: TfmCtrKey
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Key'
  ClientHeight = 309
  ClientWidth = 424
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
    424
    309)
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton
    Left = 341
    Top = 276
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    ExplicitTop = 259
  end
  object btnOk: TButton
    Left = 260
    Top = 276
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
    ExplicitTop = 259
  end
  object rgKeyType: TRadioGroup
    AlignWithMargins = True
    Left = 150
    Top = 0
    Width = 80
    Height = 80
    Caption = 'Key type'
    Items.Strings = (
      'Normal'
      'Blocking'
      'Master')
    TabOrder = 2
  end
  object grAccess: TGroupBox
    Left = 0
    Top = 86
    Width = 423
    Height = 185
    Caption = 'Access'
    TabOrder = 3
    DesignSize = (
      423
      185)
    object roAccessEver: TRadioButton
      Left = 12
      Top = 16
      Width = 404
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'ever'
      TabOrder = 0
    end
    object roAccessNever: TRadioButton
      Left = 12
      Top = 32
      Width = 404
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'never'
      TabOrder = 1
    end
    object roAccessSchedule: TRadioButton
      Left = 12
      Top = 48
      Width = 404
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'on schedule'
      TabOrder = 2
    end
    object cbTZs: TCheckListBox
      Left = 24
      Top = 71
      Width = 396
      Height = 106
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ItemHeight = 14
      Items.Strings = (
        '1 - In: ------- 09:00-18:00  Out: ------- 09:00-18:00'
        '2 - '
        '3 - '
        '4 - '
        '5 - '
        '6 - '
        '7 - ')
      ParentFont = False
      TabOrder = 3
    end
  end
  object grKeyNumber: TGroupBox
    Left = 0
    Top = 0
    Width = 145
    Height = 80
    Caption = 'Key number'
    TabOrder = 4
    DesignSize = (
      145
      80)
    object edtNum: TEdit
      Left = 12
      Top = 24
      Width = 121
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = '11 22 33 AA BB CC'
      OnChange = edtNumChange
    end
    object edtNum2: TEdit
      Left = 12
      Top = 51
      Width = 121
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = '11 22 33 AA BB CC'
      OnChange = edtNum2Change
    end
  end
  object chkShortNum: TCheckBox
    Left = 238
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Short number'
    TabOrder = 5
  end
  object chkFunctional: TCheckBox
    Left = 238
    Top = 26
    Width = 97
    Height = 17
    Caption = 'Functional'
    TabOrder = 6
  end
  object chkDual: TCheckBox
    Left = 238
    Top = 43
    Width = 97
    Height = 17
    Caption = 'Dual'
    TabOrder = 7
    OnClick = chkDualClick
  end
  object edtCode1: TEdit
    Left = 252
    Top = 64
    Width = 69
    Height = 21
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 6
    TabOrder = 8
    Text = '000000'
    OnChange = edtCode1Change
  end
  object edtCode2: TEdit
    Left = 327
    Top = 64
    Width = 69
    Height = 21
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 6
    TabOrder = 9
    Text = '000000'
    OnChange = edtCode2Change
  end
end
