object fmConverter: TfmConverter
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Converter'
  ClientHeight = 372
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    624
    372)
  PixelsPerInch = 96
  TextHeight = 13
  object labCvtVer: TLabel
    Left = 213
    Top = 11
    Width = 14
    Height = 13
    Caption = '?.?'
  end
  object Label3: TLabel
    Left = 168
    Top = 11
    Width = 39
    Height = 13
    Alignment = taRightJustify
    Caption = 'Version:'
  end
  object labCvtSn: TLabel
    Left = 61
    Top = 30
    Width = 5
    Height = 13
    Caption = '?'
  end
  object Label1: TLabel
    Left = 39
    Top = 30
    Width = 16
    Height = 13
    Alignment = taRightJustify
    Caption = 'Sn:'
  end
  object Label4: TLabel
    Left = 177
    Top = 30
    Width = 30
    Height = 13
    Alignment = taRightJustify
    Caption = 'Mode:'
  end
  object labCvtMode: TLabel
    Left = 213
    Top = 30
    Width = 5
    Height = 13
    Caption = '?'
  end
  object Label5: TLabel
    Left = 23
    Top = 11
    Width = 32
    Height = 13
    Alignment = taRightJustify
    Caption = 'Model:'
  end
  object labCvtType: TLabel
    Left = 61
    Top = 11
    Width = 5
    Height = 13
    Caption = '?'
  end
  object labStatus: TLabel
    Left = 20
    Top = 57
    Width = 35
    Height = 13
    Alignment = taRightJustify
    Caption = 'Status:'
  end
  object labStatusValue: TLabel
    Left = 61
    Top = 57
    Width = 15
    Height = 13
    Caption = '???'
  end
  object edtInfoLines: TMemo
    Left = 8
    Top = 88
    Width = 340
    Height = 90
    Lines.Strings = (
      '?')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object grCtrs: TGroupBox
    Left = 0
    Top = 184
    Width = 624
    Height = 188
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Controllers'
    TabOrder = 1
    DesignSize = (
      624
      188)
    object lvCtrs: TListView
      Left = 3
      Top = 16
      Width = 484
      Height = 169
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Address'
          Width = 55
        end
        item
          Caption = 'Model'
          Width = 85
        end
        item
          Caption = 'Sn'
          Width = 45
        end
        item
          Caption = 'Version'
        end
        item
          Caption = 'Banks'
        end
        item
          Caption = 'Keys'
        end
        item
          Caption = 'Events'
        end
        item
          Caption = 'Key Mode'
          Width = 90
        end>
      ColumnClick = False
      HideSelection = False
      OwnerData = True
      RowSelect = True
      PopupMenu = PopupMenu1
      TabOrder = 0
      ViewStyle = vsReport
      OnData = lvCtrsData
      OnDblClick = lvCtrsDblClick
      OnEdited = lvCtrsEdited
      ExplicitHeight = 119
    end
    object btnCtrOpen: TButton
      Left = 493
      Top = 55
      Width = 123
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Open...'
      TabOrder = 1
      OnClick = btnCtrOpenClick
    end
    object btnUpdateFW: TButton
      Left = 493
      Top = 16
      Width = 123
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Update Firmware...'
      TabOrder = 2
      OnClick = btnUpdateFWClick
    end
  end
  object grLicense: TGroupBox
    Left = 351
    Top = 16
    Width = 272
    Height = 162
    Caption = 'License'
    TabOrder = 2
    object Label6: TLabel
      Left = 43
      Top = 22
      Width = 102
      Height = 13
      Alignment = taRightJustify
      Caption = 'Used license number:'
    end
    object labLicNum: TLabel
      Left = 151
      Top = 22
      Width = 5
      Height = 13
      Caption = '?'
    end
    object Label7: TLabel
      Left = 15
      Top = 41
      Width = 130
      Height = 13
      Alignment = taRightJustify
      Caption = 'Limit number of controllers:'
    end
    object labLicCtrs: TLabel
      Left = 151
      Top = 41
      Width = 5
      Height = 13
      Caption = '?'
    end
    object Label8: TLabel
      Left = 43
      Top = 60
      Width = 102
      Height = 13
      Alignment = taRightJustify
      Caption = 'Limit number of keys:'
    end
    object labLicKeys: TLabel
      Left = 151
      Top = 60
      Width = 5
      Height = 13
      Caption = '?'
    end
    object Label9: TLabel
      Left = 68
      Top = 79
      Width = 77
      Height = 13
      Alignment = taRightJustify
      Caption = 'Expiration date:'
    end
    object labLicDate: TLabel
      Left = 151
      Top = 79
      Width = 5
      Height = 13
      Caption = '?'
    end
    object Label10: TLabel
      Left = 102
      Top = 98
      Width = 43
      Height = 13
      Alignment = taRightJustify
      Caption = 'Counter:'
    end
    object labLicCounter: TLabel
      Left = 151
      Top = 98
      Width = 5
      Height = 13
      Caption = '?'
    end
    object btnClearAllLic: TButton
      Left = 12
      Top = 126
      Width = 123
      Height = 25
      Caption = 'Clear all licenses'
      TabOrder = 0
      OnClick = btnClearAllLicClick
    end
    object btnSetLicense: TButton
      Left = 141
      Top = 126
      Width = 123
      Height = 25
      Caption = 'Set license...'
      TabOrder = 1
      OnClick = btnSetLicenseClick
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 216
    Top = 216
    object miRefresh: TMenuItem
      Caption = 'Refresh'
      OnClick = miRefreshClick
    end
    object Devicetypes1: TMenuItem
      Caption = 'Device types'
      object miDevTypesGATE: TMenuItem
        Caption = 'GATE-controllers'
        RadioItem = True
        OnClick = miDevTypesGATEClick
      end
      object miDevTypesEurolock: TMenuItem
        Caption = 'Eurolock EHT net'
        RadioItem = True
        OnClick = miDevTypesEurolockClick
      end
      object miDevTypesAll: TMenuItem
        Caption = 'All'
        RadioItem = True
        OnClick = miDevTypesAllClick
      end
    end
  end
end
