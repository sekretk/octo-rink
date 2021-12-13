object fmCtrElectro: TfmCtrElectro
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'ElectroControl'
  ClientHeight = 400
  ClientWidth = 667
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
    667
    400)
  PixelsPerInch = 96
  TextHeight = 13
  object grElConfig: TGroupBox
    Left = 3
    Top = 127
    Width = 664
    Height = 266
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Configuration'
    TabOrder = 0
    DesignSize = (
      664
      266)
    object labElSchedVal: TLabel
      Left = 56
      Top = 240
      Width = 182
      Height = 14
      Caption = '10:00-18:00  m t w t f - -'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      OnDblClick = labElSchedValDblClick
    end
    object grElCard: TGroupBox
      Left = 11
      Top = 50
      Width = 639
      Height = 79
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Management Card'
      TabOrder = 1
      DesignSize = (
        639
        79)
      object chkElCardNoBlock: TCheckBox
        Left = 218
        Top = 22
        Width = 407
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Do not block the function of opening the door key'
        TabOrder = 1
      end
      object rgElCardReader: TRadioGroup
        Left = 16
        Top = 21
        Width = 185
        Height = 51
        Caption = 'Command device'
        ItemIndex = 0
        Items.Strings = (
          'Matrix II Net'
          'External card reader')
        TabOrder = 0
      end
    end
    object chkElEnable: TCheckBox
      Left = 12
      Top = 22
      Width = 638
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Activate the power management'
      TabOrder = 0
      OnClick = chkElEnableClick
    end
    object chkElSchedule: TCheckBox
      Left = 27
      Top = 217
      Width = 478
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use schedule time zone #7'
      TabOrder = 5
      OnClick = chkElScheduleClick
    end
    object chkElInvInput: TCheckBox
      Left = 27
      Top = 194
      Width = 478
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Powering Zero'
      TabOrder = 4
    end
    object chkElPwrOffForExit: TCheckBox
      Left = 182
      Top = 143
      Width = 323
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Sensor doors off immediately when you exit the room'
      TabOrder = 3
    end
    object grElPwrDelay: TGroupBox
      Left = 14
      Top = 135
      Width = 158
      Height = 50
      Caption = 'Delay on shutdown'
      TabOrder = 2
      object labElPwrDelay: TLabel
        Left = 92
        Top = 20
        Width = 16
        Height = 13
        Alignment = taRightJustify
        Caption = 'sec'
      end
      object edtElPwrDelay: TSpinEdit
        Left = 24
        Top = 17
        Width = 57
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
    end
    object btnRead: TButton
      Left = 575
      Top = 144
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Read'
      TabOrder = 6
      OnClick = btnReadClick
    end
    object btnWrite: TButton
      Left = 575
      Top = 175
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Write'
      TabOrder = 7
      OnClick = btnWriteClick
    end
  end
  object grElState: TGroupBox
    Left = 3
    Top = 3
    Width = 664
    Height = 118
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Current state'
    TabOrder = 1
    DesignSize = (
      664
      118)
    object panElState: TPanel
      Left = 14
      Top = 22
      Width = 555
      Height = 27
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsSingle
      Caption = 'Power Off (12:00)'
      ParentBackground = False
      ParentColor = True
      TabOrder = 0
    end
    object lbElStatusEx: TListBox
      Left = 16
      Top = 55
      Width = 553
      Height = 50
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      Items.Strings = (
        '[v] time zone'
        '[v] Remote command'
        '[v] The key is brought to the reader (processing delays ...)')
      TabOrder = 1
    end
    object btnElRefresh: TButton
      Left = 575
      Top = 22
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Refresh'
      TabOrder = 2
      OnClick = btnElRefreshClick
    end
    object btnPowerOn: TButton
      Left = 575
      Top = 55
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Power On'
      TabOrder = 3
      OnClick = btnPowerOnClick
    end
    object btnPowerOff: TButton
      Left = 575
      Top = 86
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Power Off'
      TabOrder = 4
      OnClick = btnPowerOffClick
    end
  end
end
