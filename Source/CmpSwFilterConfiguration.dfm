object frmCmpSwFilterConfiguration: TfrmCmpSwFilterConfiguration
  Left = 317
  Top = 192
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  BorderWidth = 8
  Caption = 'Filter'
  ClientHeight = 477
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object gbMain: TGroupBox
    Left = 0
    Top = 0
    Width = 371
    Height = 442
    Align = alClient
    TabOrder = 0
    DesignSize = (
      371
      442)
    object lblName: TLabel
      Left = 12
      Top = 20
      Width = 31
      Height = 13
      Caption = '&Name:'
    end
    object lblFilter: TLabel
      Left = 12
      Top = 48
      Width = 44
      Height = 13
      Caption = '&Matches:'
    end
    object lblHelp: TLabel
      Left = 12
      Top = 260
      Width = 25
      Height = 13
      Caption = 'Help:'
    end
    object lblHelpText: TLabel
      Left = 72
      Top = 260
      Width = 285
      Height = 41
      AutoSize = False
      Caption = 
        'Enter one match per line in the '#39'Matches'#39' box. If any of the lin' +
        'es match a component class name, the filter will apply to that c' +
        'omponent.'
      WordWrap = True
    end
    object lblHelpText2: TLabel
      Left = 72
      Top = 312
      Width = 249
      Height = 45
      AutoSize = False
      Caption = 
        'You may use wildcards to specify a match. For example: '#39'TId*'#39' to' +
        ' match all Indy components. See Delphi'#39's TMask documentation for' +
        ' more information.'
      WordWrap = True
    end
    object lblHelpText3: TLabel
      Left = 72
      Top = 368
      Width = 285
      Height = 57
      AutoSize = False
      Caption = 
        #39'Match descendant classes'#39' only applies to match lines which do ' +
        'not include wildcards. For example: '#39'TIdBaseComponent'#39' with '#39'Mat' +
        'ch descendant classes'#39' will match all Indy components.'
      WordWrap = True
    end
    object bvlHelp: TBevel
      Left = 12
      Top = 248
      Width = 349
      Height = 13
      Shape = bsTopLine
    end
    object chkIncludeDescendants: TCheckBox
      Left = 72
      Top = 196
      Width = 161
      Height = 17
      Caption = 'Match &descendant classes'
      TabOrder = 2
    end
    object edtName: TEdit
      Left = 72
      Top = 16
      Width = 289
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object mmoFilter: TMemo
      Left = 72
      Top = 44
      Width = 289
      Height = 145
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
    end
    object chkVisible: TCheckBox
      Left = 72
      Top = 220
      Width = 129
      Height = 17
      Caption = 'Show as &checkbox'
      TabOrder = 3
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 442
    Width = 371
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      371
      35)
    object btnOk: TButton
      Left = 215
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 296
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
