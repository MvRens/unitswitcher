object frmCmpSwConfiguration: TfrmCmpSwConfiguration
  Left = 279
  Top = 170
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'ComponentSwitcher Configuration'
  ClientHeight = 423
  ClientWidth = 328
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    328
    423)
  PixelsPerInch = 96
  TextHeight = 13
  object pcConfiguration: TPageControl
    Left = 4
    Top = 4
    Width = 320
    Height = 382
    ActivePage = tsGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = 'Filters'
      DesignSize = (
        312
        354)
      object btnDefault: TButton
        Left = 4
        Top = 323
        Width = 109
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Reset to &default'
        TabOrder = 0
        OnClick = btnDefaultClick
      end
      object chkAllowEmptyResults: TCheckBox
        Left = 4
        Top = 292
        Width = 273
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Allow e&mpty results'
        TabOrder = 1
      end
      object lbFilters: TListBox
        Left = 4
        Top = 4
        Width = 301
        Height = 233
        Style = lbVirtual
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 16
        TabOrder = 2
        OnClick = lbFiltersClick
        OnData = lbFiltersData
        OnDataObject = lbFiltersDataObject
      end
      object btnAdd: TButton
        Left = 4
        Top = 243
        Width = 97
        Height = 25
        Action = actAdd
        Anchors = [akLeft, akBottom]
        TabOrder = 3
      end
      object btnEdit: TButton
        Left = 108
        Top = 243
        Width = 97
        Height = 25
        Action = actEdit
        Anchors = [akLeft, akBottom]
        TabOrder = 4
      end
      object btnRemove: TButton
        Left = 212
        Top = 243
        Width = 93
        Height = 25
        Action = actRemove
        Anchors = [akLeft, akBottom]
        TabOrder = 5
      end
    end
    object tsAbout: TTabSheet
      Caption = 'About...'
      ImageIndex = 1
      DesignSize = (
        312
        354)
      object imgAbout: TImage
        Left = 8
        Top = 8
        Width = 32
        Height = 32
        Picture.Data = {
          055449636F6E0000010001002020100001000400E80200001600000028000000
          2000000040000000010004000000000000020000000000000000000000000000
          0000000000000000000080000080000000808000800000008000800080800000
          80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
          FFFFFF0000000000000000000000000000000000000000000000000000000000
          00000000000000F7777777777777777777777700000000F8F8F8F8F8F8F8F8F8
          F8F8F700000000FF8F8F8F8F8F8F8F8F8F8F8700000000F8F8F8F8F8F8F8F8F8
          F8F8F700000000FF8F8F8F8F8F8F8F8F8F8F8700000000F8F000000000000000
          0008F700000000FF8F8F8F8F8F8F8F8F8F8F8700000000F8F8F8F8F8F8F8F8F8
          F8F8F700000000FF8F8F8F8F8F8F8F8F8F8F8700000000F8F000000000000000
          0008F700000000FF8F8F8F8F8F8F8F8F8F8F87000C0000F99999999999999999
          999997000CC000F99999999999999999999997000CCE00F99FFFFFFFFFFFFFFF
          FFF997000CE000F99999999999999999999997000E0000F99999999999999999
          99999700000000FF8F8F8F8F8F8F8F8F8F8F8700000000F8F000000000000000
          0008F700000000FF8F8F8F8F8F8F8F8F8F8F8700000000F8F8F8F8F8F8F8F8F8
          F8F8F700000000FF8F8F8F8F8F8F8F8F8F8F8700000000F8F0000000000008F8
          77777700000000FF8F8F8F8F8F8F8F8000000000000000F8F8F8F8F8F8F8F8F0
          FFFFF000000000FF8F8F8F8F8F8F8F80FFFF0000000000F8F0000000000008F0
          FFF00000000000FF8F8F8F8F8F8F8F80FF000000000000F8F8F8F8F8F8F8F8F0
          F0000000000000FFFFFFFFFFFFFFFFF000000000000000000000000000000000
          00000000FFFFFFFFF8000001F8000001F8000001F8000001F8000001F8000001
          F8000001F8000001F8000001F800000178000001380000011800000108000001
          0000000108000001180000013800000178000001F8000001F8000001F8000001
          F8000001F8000001F8000003F8000007F800000FF800001FF800003FF800007F
          F80000FF}
      end
      object TLabel
        Left = 56
        Top = 8
        Width = 130
        Height = 16
        Caption = 'ComponentSwitcher'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblVersion: TLabel
        Left = 56
        Top = 23
        Width = 54
        Height = 13
        Caption = 'Version 0.1'
      end
      object TLabel
        Left = 160
        Top = 23
        Width = 145
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Copyright '#169' 2007 X'#178'Software'
      end
      object TLabel
        Left = 56
        Top = 90
        Width = 241
        Height = 41
        AutoSize = False
        Caption = 
          'ComponentSwitcher is released as open-source under the zlib/libp' +
          'ng OSI-approved license. See license.txt for details.'
        WordWrap = True
      end
      object lblBugReport: TLabel
        Left = 56
        Top = 328
        Width = 75
        Height = 13
        Cursor = crHandPoint
        Anchors = [akLeft, akBottom]
        Caption = 'Report a bug...'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = lblBugReportClick
      end
    end
  end
  object btnCancel: TButton
    Left = 249
    Top = 392
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 168
    Top = 392
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object dlgColor: TColorDialog
    Options = [cdFullOpen]
    Left = 24
    Top = 364
  end
  object alMain: TActionList
    Left = 96
    Top = 364
    object actAdd: TAction
      Caption = '&Add...'
      OnExecute = actAddExecute
    end
    object actEdit: TAction
      Caption = '&Edit...'
      Enabled = False
      OnExecute = actEditExecute
    end
    object actRemove: TAction
      Caption = '&Remove'
      Enabled = False
      OnExecute = actRemoveExecute
    end
  end
end
