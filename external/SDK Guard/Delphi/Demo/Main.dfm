object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Demo SDK Guard'
  ClientHeight = 308
  ClientWidth = 613
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCanResize = FormCanResize
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    613
    308)
  PixelsPerInch = 96
  TextHeight = 13
  object labPortName: TLabel
    Left = 202
    Top = 245
    Width = 53
    Height = 13
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    Caption = 'Port name:'
  end
  object labActCode: TLabel
    Left = 185
    Top = 267
    Width = 72
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Activate Code:'
  end
  object labCvtSn: TLabel
    Left = 379
    Top = 267
    Width = 20
    Height = 13
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    Caption = 'S/n:'
  end
  object lvCvts: TListView
    Left = 0
    Top = 0
    Width = 613
    Height = 234
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Port'
        Width = 200
      end
      item
        Caption = 'Model'
        Width = 85
      end
      item
        Caption = 'Sn'
      end
      item
        Caption = 'Version'
        Width = 60
      end
      item
        Caption = 'Mode'
        Width = 85
      end>
    ColumnClick = False
    HideSelection = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
    OnCustomDrawItem = lvCvtsCustomDrawItem
    OnData = lvCvtsData
    OnDblClick = lvCvtsDblClick
    OnSelectItem = lvCvtsSelectItem
  end
  object btnOpen: TButton
    Left = 530
    Top = 240
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Open...'
    TabOrder = 1
    OnClick = btnOpenClick
  end
  object btnUpdateFW: TButton
    Left = 8
    Top = 240
    Width = 121
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Update Firmware...'
    TabOrder = 2
    OnClick = btnUpdateFWClick
  end
  object cbSpeed: TComboBox
    Left = 451
    Top = 240
    Width = 73
    Height = 21
    Style = csDropDownList
    Anchors = [akRight, akBottom]
    ItemIndex = 0
    TabOrder = 3
    Text = '19200'
    Items.Strings = (
      '19200'
      '57600')
  end
  object edtPort: TEdit
    Left = 261
    Top = 240
    Width = 110
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 4
  end
  object cbMode: TComboBox
    Left = 377
    Top = 240
    Width = 68
    Height = 21
    Style = csDropDownList
    Anchors = [akRight, akBottom]
    ItemIndex = 0
    TabOrder = 5
    Text = 'Server'
    OnChange = cbModeChange
    Items.Strings = (
      'Server'
      'Client'
      'Proxy')
  end
  object cbCvtSn: TComboBox
    Left = 405
    Top = 263
    Width = 68
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 6
    Text = '0'
  end
  object btnSnRefresh: TButton
    Left = 473
    Top = 263
    Width = 51
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'refresh'
    TabOrder = 7
    OnClick = btnSnRefreshClick
  end
  object cbActCode: TComboBox
    Left = 261
    Top = 263
    Width = 110
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 8
  end
  object chkZ397: TCheckBox
    Left = 530
    Top = 271
    Width = 61
    Height = 17
    Anchors = [akRight, akBottom]
    Caption = 'Z-397'
    TabOrder = 9
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 289
    Width = 613
    Height = 19
    Panels = <>
    SimplePanel = True
    ExplicitLeft = 312
    ExplicitTop = 288
    ExplicitWidth = 0
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 80
    Top = 112
    object miRefresh: TMenuItem
      Caption = 'Refresh'
      OnClick = miRefreshClick
    end
    object miDevTypes: TMenuItem
      Caption = 'Device types'
      object miDevTypesUsb: TMenuItem
        Caption = 'Usb'
        RadioItem = True
        OnClick = miDevTypesUsbClick
      end
      object miDevTypesIP: TMenuItem
        Caption = 'IP'
        RadioItem = True
        OnClick = miDevTypesIPClick
      end
      object miDevTypesAll: TMenuItem
        Caption = 'All'
        RadioItem = True
        OnClick = miDevTypesAllClick
      end
    end
    object miShowUnkDev: TMenuItem
      Caption = 'Show Unknown Devices'
      Checked = True
      OnClick = miShowUnkDevClick
    end
    object miSearchBackground: TMenuItem
      Caption = 'Search in background'
      Checked = True
      OnClick = miSearchBackgroundClick
    end
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Firmware files (*.rom)|*.rom'
    Left = 160
    Top = 128
  end
end
