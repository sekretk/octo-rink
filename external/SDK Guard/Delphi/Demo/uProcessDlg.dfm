object fmProcess: TfmProcess
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Processing'
  ClientHeight = 83
  ClientWidth = 485
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  DesignSize = (
    485
    83)
  PixelsPerInch = 96
  TextHeight = 13
  object labDescription: TLabel
    Left = 8
    Top = 8
    Width = 63
    Height = 13
    Caption = 'Processing...'
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 27
    Width = 469
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 208
    Top = 50
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
end
