object fmController: TfmController
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Controller'
  ClientHeight = 362
  ClientWidth = 478
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    478
    362)
  PixelsPerInch = 96
  TextHeight = 13
  object labVersion: TLabel
    Left = 214
    Top = 16
    Width = 14
    Height = 13
    Caption = '?.?'
  end
  object Label3: TLabel
    Left = 169
    Top = 16
    Width = 39
    Height = 13
    Alignment = taRightJustify
    Caption = 'Version:'
  end
  object labSn: TLabel
    Left = 62
    Top = 35
    Width = 5
    Height = 13
    Caption = '?'
  end
  object Label1: TLabel
    Left = 40
    Top = 35
    Width = 16
    Height = 13
    Alignment = taRightJustify
    Caption = 'Sn:'
  end
  object Label4: TLabel
    Left = 157
    Top = 35
    Width = 51
    Height = 13
    Alignment = taRightJustify
    Caption = 'Key mode:'
  end
  object labKeyMode: TLabel
    Left = 214
    Top = 35
    Width = 5
    Height = 13
    Caption = '?'
  end
  object Label5: TLabel
    Left = 24
    Top = 16
    Width = 32
    Height = 13
    Alignment = taRightJustify
    Caption = 'Model:'
  end
  object labType: TLabel
    Left = 62
    Top = 16
    Width = 5
    Height = 13
    Caption = '?'
  end
  object Label2: TLabel
    Left = 8
    Top = 54
    Width = 48
    Height = 13
    Alignment = taRightJustify
    Caption = 'Info lines:'
  end
  object labMaxEvents: TLabel
    Left = 405
    Top = 35
    Width = 5
    Height = 13
    Caption = '?'
  end
  object Label7: TLabel
    Left = 339
    Top = 35
    Width = 60
    Height = 13
    Alignment = taRightJustify
    Caption = 'Max Events:'
  end
  object Label8: TLabel
    Left = 349
    Top = 16
    Width = 50
    Height = 13
    Alignment = taRightJustify
    Caption = 'Max Keys:'
  end
  object labMaxKeys: TLabel
    Left = 405
    Top = 16
    Width = 5
    Height = 13
    Caption = '?'
  end
  object edtInfoLines: TMemo
    Left = 8
    Top = 73
    Width = 462
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      '?')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object grCtrConfig: TGroupBox
    Left = 13
    Top = 176
    Width = 132
    Height = 177
    Caption = 'Configuration'
    TabOrder = 1
    object btnLock: TButton
      Left = 16
      Top = 24
      Width = 100
      Height = 25
      Caption = 'Lock..'
      TabOrder = 0
      OnClick = btnLockClick
    end
    object btnSchedule: TButton
      Left = 16
      Top = 63
      Width = 100
      Height = 25
      Caption = 'Schedule..'
      TabOrder = 1
      OnClick = btnScheduleClick
    end
    object btnKeys: TButton
      Left = 16
      Top = 102
      Width = 100
      Height = 25
      Caption = 'Keys..'
      TabOrder = 2
      OnClick = btnKeysClick
    end
    object btnCfgDump: TButton
      Left = 16
      Top = 141
      Width = 100
      Height = 25
      Caption = 'Dump...'
      TabOrder = 3
      OnClick = btnCfgDumpClick
    end
  end
  object grCtrControl: TGroupBox
    Left = 157
    Top = 176
    Width = 142
    Height = 177
    Caption = 'Control'
    TabOrder = 2
    object btnRemoteOpenDoor: TButton
      Left = 16
      Top = 24
      Width = 100
      Height = 25
      Caption = 'Open Door'
      TabOrder = 0
      OnClick = btnRemoteOpenDoorClick
    end
    object btnRemoteOpenDoorOut: TButton
      Left = 16
      Top = 63
      Width = 100
      Height = 25
      Caption = 'Open Door (out)'
      TabOrder = 1
      OnClick = btnRemoteOpenDoorOutClick
    end
    object btnDisconnectLocks: TButton
      Left = 16
      Top = 102
      Width = 100
      Height = 25
      Caption = 'Close Lock'
      TabOrder = 2
      OnClick = btnDisconnectLocksClick
    end
    object chkEmergUnlock: TCheckBox
      Left = 16
      Top = 144
      Width = 123
      Height = 17
      Caption = 'Emergency Unlocking'
      TabOrder = 3
      OnClick = chkEmergUnlockClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 331
    Top = 176
    Width = 130
    Height = 97
    Caption = 'Events'
    TabOrder = 3
    object btnEvents: TButton
      Left = 16
      Top = 24
      Width = 100
      Height = 25
      Caption = 'Events..'
      TabOrder = 0
      OnClick = btnEventsClick
    end
    object btnMonitor: TButton
      Left = 16
      Top = 63
      Width = 100
      Height = 25
      Caption = 'Monitor..'
      TabOrder = 1
      OnClick = btnMonitorClick
    end
  end
  object btnElectro: TButton
    Left = 349
    Top = 285
    Width = 100
    Height = 25
    Caption = 'ElectroControl...'
    TabOrder = 4
    OnClick = btnElectroClick
  end
  object btnModes: TButton
    Left = 349
    Top = 316
    Width = 100
    Height = 25
    Caption = 'Modes...'
    TabOrder = 5
    OnClick = btnModesClick
  end
end
