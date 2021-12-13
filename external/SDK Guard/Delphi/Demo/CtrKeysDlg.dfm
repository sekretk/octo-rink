object fmCtrKeys: TfmCtrKeys
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Controller Keys'
  ClientHeight = 352
  ClientWidth = 749
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grKeys: TGroupBox
    Left = 0
    Top = 0
    Width = 370
    Height = 351
    Caption = 'Keys'
    TabOrder = 0
    DesignSize = (
      370
      351)
    object lvKeys: TListView
      Left = 3
      Top = 16
      Width = 364
      Height = 295
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = '#'
        end
        item
          AutoSize = True
          Caption = 'Key Number'
        end
        item
          Caption = 'Type'
          Width = 75
        end
        item
          Caption = 'Access'
          Width = 100
        end>
      ColumnClick = False
      HideSelection = False
      MultiSelect = True
      OwnerData = True
      ReadOnly = True
      RowSelect = True
      PopupMenu = PopupMenu1
      TabOrder = 0
      ViewStyle = vsReport
      OnData = lvKeysData
      OnDblClick = lvKeysDblClick
    end
    object btnClearAll: TButton
      Left = 80
      Top = 317
      Width = 65
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Clear all'
      TabOrder = 1
      OnClick = btnClearAllClick
    end
    object btnReadAll: TButton
      Left = 9
      Top = 317
      Width = 65
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Read all'
      TabOrder = 2
      OnClick = btnReadAllClick
    end
    object btnRemove: TButton
      Left = 293
      Top = 317
      Width = 65
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Remove'
      TabOrder = 3
      OnClick = btnRemoveClick
    end
    object btnChange: TButton
      Left = 222
      Top = 317
      Width = 65
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Change...'
      TabOrder = 4
      OnClick = btnChangeClick
    end
    object btnAdd: TButton
      Left = 151
      Top = 317
      Width = 65
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Add...'
      TabOrder = 5
      OnClick = btnAddClick
    end
  end
  object grKeysOut: TGroupBox
    Left = 376
    Top = 0
    Width = 370
    Height = 351
    Caption = 'Keys (out)'
    TabOrder = 1
    DesignSize = (
      370
      351)
    object lvKeysOut: TListView
      Left = 3
      Top = 16
      Width = 364
      Height = 295
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = '#'
        end
        item
          AutoSize = True
          Caption = 'Key Number'
        end
        item
          Caption = 'Type'
          Width = 75
        end
        item
          Caption = 'Access'
          Width = 100
        end>
      ColumnClick = False
      HideSelection = False
      MultiSelect = True
      OwnerData = True
      ReadOnly = True
      RowSelect = True
      PopupMenu = PopupMenu2
      TabOrder = 0
      ViewStyle = vsReport
      OnData = lvKeysOutData
      OnDblClick = lvKeysOutDblClick
    end
    object btnRemoveOut: TButton
      Left = 292
      Top = 317
      Width = 65
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Remove'
      TabOrder = 1
      OnClick = btnRemoveOutClick
    end
    object btnChangeOut: TButton
      Left = 221
      Top = 317
      Width = 65
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Change...'
      TabOrder = 2
      OnClick = btnChangeOutClick
    end
    object btnReadAllOut: TButton
      Left = 9
      Top = 317
      Width = 65
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Read all'
      TabOrder = 3
      OnClick = btnReadAllOutClick
    end
    object btnClearAllOut: TButton
      Left = 80
      Top = 317
      Width = 65
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Clear all'
      TabOrder = 4
      OnClick = btnClearAllOutClick
    end
    object btnAddOut: TButton
      Left = 151
      Top = 317
      Width = 65
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Add...'
      TabOrder = 5
      OnClick = btnAddOutClick
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 208
    Top = 120
    object miReadAll1: TMenuItem
      Caption = 'Read All'
      OnClick = miReadAll1Click
    end
    object miClearAll1: TMenuItem
      Caption = 'Clear All'
      OnClick = miClearAll1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miCopyToBankOut: TMenuItem
      Caption = 'Copy to bank "out"'
      OnClick = miCopyToBankOutClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
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
    object N4: TMenuItem
      Caption = '-'
    end
    object miImportCSV1: TMenuItem
      Caption = 'Import from CSV...'
      OnClick = miImportCSV1Click
    end
    object miImportExcel1: TMenuItem
      Caption = 'Import from Excel...'
      OnClick = miImportExcel1Click
    end
  end
  object OpenDialogCSV1: TOpenDialog
    Filter = 'CSV-files (*.csv)|*.csv'
    Left = 280
    Top = 112
  end
  object PopupMenu2: TPopupMenu
    Left = 448
    Top = 120
    object miReadAll2: TMenuItem
      Caption = 'ReadAll'
      OnClick = miReadAll2Click
    end
    object miClearAll2: TMenuItem
      Caption = 'Clear All'
      OnClick = miClearAll2Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miImportCSV2: TMenuItem
      Caption = 'Import from CSV...'
      OnClick = miImportCSV2Click
    end
    object miImportExcel2: TMenuItem
      Caption = 'Import from Excel...'
      OnClick = miImportExcel2Click
    end
  end
  object OpenDialogXls1: TOpenDialog
    Filter = 'Excel files (*.xls;*.xlsx)|*.xls;*.xlsx|All files|*.*'
    Left = 280
    Top = 168
  end
end
