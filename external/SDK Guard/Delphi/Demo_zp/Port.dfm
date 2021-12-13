object fmPort: TfmPort
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Port'
  ClientHeight = 331
  ClientWidth = 502
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    502
    331)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 502
    Height = 292
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnSend: TButton
    Left = 419
    Top = 298
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Send'
    TabOrder = 2
    OnClick = btnSendClick
  end
  object btnClear: TButton
    Left = 8
    Top = 298
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear'
    TabOrder = 1
    OnClick = btnClearClick
  end
  object cbCmd: TComboBox
    Left = 103
    Top = 300
    Width = 310
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
    Text = 'i'
    Items.Strings = (
      'i'
      'r'
      'get XX'
      'put XX X0 X1 X2 X3'
      't57 00'
      't57 05 XX XX XX'
      't57 01 XX X0 X1 X2 X3 XX XX')
  end
end
