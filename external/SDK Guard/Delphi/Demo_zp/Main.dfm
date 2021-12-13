object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Demo ZPort'
  ClientHeight = 353
  ClientWidth = 545
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
    545
    353)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 338
    Top = 313
    Width = 34
    Height = 13
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    Caption = 'Speed:'
  end
  object labPort: TLabel
    Left = 136
    Top = 312
    Width = 24
    Height = 13
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    Caption = 'Port:'
  end
  object lvPorts: TListView
    Left = 0
    Top = 0
    Width = 546
    Height = 303
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'Port'
      end
      item
        Caption = 'Busy'
      end
      item
        Caption = 'Device'
        Width = 90
      end
      item
        Caption = 'S/n'
        Width = 40
      end
      item
        Caption = 'Version'
        Width = 60
      end>
    ColumnClick = False
    HideSelection = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
    OnData = lvPortsData
    OnDblClick = lvPortsDblClick
    OnSelectItem = lvPortsSelectItem
  end
  object btnRescan: TButton
    Left = 8
    Top = 308
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Rescan'
    TabOrder = 1
    OnClick = btnRescanClick
  end
  object btnOpen: TButton
    Left = 462
    Top = 309
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Open...'
    TabOrder = 2
    OnClick = btnOpenClick
  end
  object cbBaud: TComboBox
    Left = 378
    Top = 310
    Width = 78
    Height = 21
    Anchors = [akRight, akBottom]
    ItemIndex = 0
    TabOrder = 3
    Text = '9600'
    Items.Strings = (
      '9600'
      '19200'
      '57600'
      '230400')
  end
  object cbPort: TComboBox
    Left = 166
    Top = 309
    Width = 155
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 4
    OnSelect = cbPortSelect
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 334
    Width = 545
    Height = 19
    Panels = <>
    SimplePanel = True
    ExplicitLeft = 136
    ExplicitTop = 336
    ExplicitWidth = 0
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 80
    Top = 112
    object miPortOpen: TMenuItem
      Caption = 'Open...'
      OnClick = miPortOpenClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miRefresh: TMenuItem
      Caption = 'Refresh'
      OnClick = miRefreshClick
    end
    object miDevTypes: TMenuItem
      Caption = 'Device Types'
      object miDevTypesNone: TMenuItem
        Caption = 'None'
        RadioItem = True
        OnClick = miDevTypesNoneClick
      end
      object miDevTypesSerial: TMenuItem
        Caption = 'Serial'
        RadioItem = True
        OnClick = miDevTypesSerialClick
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
end
