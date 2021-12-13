object fmCtrEvents: TfmCtrEvents
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Controller Events'
  ClientHeight = 367
  ClientWidth = 619
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
  OnShow = FormShow
  DesignSize = (
    619
    367)
  PixelsPerInch = 96
  TextHeight = 13
  object lvEvents: TListView
    Left = 0
    Top = 0
    Width = 622
    Height = 328
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Index'
        Width = 40
      end
      item
        Caption = 'Date & Time'
        Width = 120
      end
      item
        Caption = 'Event'
        Width = 130
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
    OnCustomDrawItem = lvEventsCustomDrawItem
    OnData = lvEventsData
  end
  object btnReadNew: TButton
    Left = 97
    Top = 334
    Width = 83
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Read new'
    TabOrder = 1
    OnClick = btnReadNewClick
  end
  object btnReadAll: TButton
    Left = 8
    Top = 334
    Width = 83
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Read all'
    TabOrder = 2
    OnClick = btnReadAllClick
  end
  object btnDump: TButton
    Left = 232
    Top = 334
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Dump...'
    TabOrder = 3
    OnClick = btnDumpClick
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
