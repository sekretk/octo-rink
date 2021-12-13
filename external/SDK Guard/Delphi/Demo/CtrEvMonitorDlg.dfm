object fmCtrEvMonitor: TfmCtrEvMonitor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Controller Event Monitor'
  ClientHeight = 505
  ClientWidth = 583
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  DesignSize = (
    583
    505)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 365
    Top = 446
    Width = 83
    Height = 13
    Alignment = taRightJustify
    Anchors = [akRight, akBottom]
    Caption = 'Last key number:'
    ExplicitTop = 419
  end
  object lvEvents: TListView
    Left = 0
    Top = 0
    Width = 584
    Height = 435
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Date & Time'
        Width = 120
      end
      item
        AutoSize = True
        Caption = 'Event'
      end
      item
        Caption = 'Direction'
        Width = 52
      end
      item
        AutoSize = True
        Caption = 'Parameters'
      end>
    ColumnClick = False
    OwnerData = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = PopupMenu1
    TabOrder = 0
    ViewStyle = vsReport
    OnData = lvEventsData
    ExplicitHeight = 408
  end
  object edtLastKeyNum: TEdit
    Left = 454
    Top = 443
    Width = 121
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 7
    ExplicitTop = 416
  end
  object btnSchedule: TButton
    Left = 89
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Schedule...'
    TabOrder = 3
    OnClick = btnScheduleClick
  end
  object btnClear: TButton
    Left = 8
    Top = 441
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 1
    OnClick = btnClearClick
    ExplicitTop = 414
  end
  object btnModes: TButton
    Left = 251
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Modes...'
    TabOrder = 5
    OnClick = btnModesClick
  end
  object btnLock: TButton
    Left = 8
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Lock..'
    TabOrder = 2
    OnClick = btnLockClick
  end
  object btnKeys: TButton
    Left = 170
    Top = 472
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Keys..'
    TabOrder = 4
    OnClick = btnKeysClick
  end
  object btnElectro: TButton
    Left = 332
    Top = 472
    Width = 93
    Height = 25
    Anchors = [akLeft, akRight]
    Caption = 'ElectroControl...'
    TabOrder = 6
    OnClick = btnElectroClick
  end
  object btnOpenIn: TButton
    Left = 137
    Top = 441
    Width = 88
    Height = 25
    Anchors = [akLeft, akRight]
    Caption = 'Open Door (In)'
    TabOrder = 8
    OnClick = btnOpenInClick
  end
  object btnOpenOut: TButton
    Left = 231
    Top = 441
    Width = 95
    Height = 25
    Anchors = [akLeft, akRight]
    Caption = 'Open Door (Out)'
    TabOrder = 9
    OnClick = btnOpenOutClick
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 328
    Top = 160
    object Numberformat1: TMenuItem
      Caption = 'Number format'
      object miNumFmtAuto: TMenuItem
        Caption = 'Auto'
        RadioItem = True
        OnClick = miNumFmtAutoClick
      end
      object miNumFmtEm: TMenuItem
        Caption = 'Em-Marine'
        RadioItem = True
        OnClick = miNumFmtEmClick
      end
      object miNumFmtDS: TMenuItem
        Caption = 'Dallas'
        RadioItem = True
        OnClick = miNumFmtDSClick
      end
    end
  end
end
