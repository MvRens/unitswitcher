object frmUnSwConfiguration: TfrmUnSwConfiguration
  Left = 279
  Top = 170
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'UnitSwitcher Configuration'
  ClientHeight = 250
  ClientWidth = 303
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    303
    250)
  PixelsPerInch = 96
  TextHeight = 13
  object pcConfiguration: TPageControl
    Left = 4
    Top = 4
    Width = 295
    Height = 209
    ActivePage = tsGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsGeneral: TTabSheet
      Caption = 'General'
      object chkCustomColor: TCheckBox
        Left = 8
        Top = 8
        Width = 249
        Height = 17
        Caption = 'Use custom text &colors to indicate the unit type:'
        Checked = True
        State = cbChecked
        TabOrder = 0
        OnClick = chkCustomColorClick
      end
      object pnlCustomColor: TPanel
        Left = 8
        Top = 31
        Width = 273
        Height = 98
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Color = clWindow
        TabOrder = 1
        object TImage
          Left = 8
          Top = 8
          Width = 16
          Height = 16
          Picture.Data = {
            055449636F6E0000010001001010100001000400280100001600000028000000
            1000000020000000010004000000000080000000000000000000000000000000
            0000000000000000000080000080000000808000800000008000800080800000
            80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
            FFFFFF0000000000000000000000F777777777700000FF8F8F8F8F700000F8F8
            F8F8F8700000FF0000000F700000F8F8F8F8F8700000F999999999700400F9FF
            FFFFF9700000F999999999700000F8F8F8F8F8700000FF0000008F700000F8F8
            F8F8F7700000FF00008F00000000F8F8F8F80F000000FFFFFFFF000000000000
            00000000E0000000E0000000E0000000E0000000E00000006000000020000000
            000000002000000060000000E0000000E0000000E0000000E0010000E0030000
            E0070000}
        end
        object TImage
          Left = 8
          Top = 68
          Width = 16
          Height = 16
          Picture.Data = {
            055449636F6E0000010001001010100001000400280100001600000028000000
            1000000020000000010004000000000080000000000000000000000000000000
            0000000000000000000080000080000000808000800000008000800080800000
            80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
            FFFFFF0000000000000000000F777777777770000FF8F8F8F8F870000F8F8F80
            000000000FF8F8F0F77777700F8F8F80FF8F8F700FF8F8F0F8F8F8700F8F8F80
            FFFFFFF00FF8F8F0000000000F8F8F80CCCCC0800FF8F8F0000000000F8F8F8F
            8F8F70000FFFFFFFFFFFF00000000000000000000CCCCCC08080800000000000
            0000000000030000000300000003000000000000000000000000000000000000
            0000000000000000000000000000000000030000000300000003000000030000
            00030000}
        end
        object TImage
          Left = 8
          Top = 48
          Width = 16
          Height = 16
          Picture.Data = {
            055449636F6E0000010001001010100001000400280100001600000028000000
            1000000020000000010004000000000080000000000000000000000000000000
            0000000000000000000080000080000000808000800000008000800080800000
            80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
            FFFFFF0000040070000000000444407888800000040400777077000004000000
            700000000000000000000000700000000000000077000F777777777000000F8F
            8F8F8F7000000FF89998F87000000F8F998F8F7000000FF89899F87000000F8F
            8F8F8F7000000FFFFFFFFFF0000000000000000000000CCCC080808000000000
            00000000EC0F0000840F0000AC0F0000BF3F0000FFFF00001000000010000000
            B0000000F0000000F0000000F0000000F0000000F0000000F0000000F0000000
            F0000000}
        end
        object TImage
          Left = 8
          Top = 28
          Width = 16
          Height = 16
          Picture.Data = {
            055449636F6E0000010001001010100001000400280100001600000028000000
            1000000020000000010004000000000080000000000000000000000000000000
            0000000000000000000080000080000000808000800000008000800080800000
            80808000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
            FFFFFF000000000000000000000000000000000000000000000000000F777777
            777777700FF8F8F8F8F8F8700F8F8F8F8F8F8F700FF8F8F8F8F8F8700F8F8F8F
            8F8F8F700FF8F8F8F8F8F8700F8F8F8F8F8F8F700FFFFFFFFFFFFFF000000000
            000000000CCCCCCCC08080800000000000000000000000000000000000000000
            00000000FFFF0000FFFF00000000000000000000000000000000000000000000
            00000000000000000000000000000000000000000000000000000000FFFF0000
            FFFF0000}
        end
        object lblUnitColor: TLabel
          Tag = 1
          Left = 32
          Top = 10
          Width = 19
          Height = 13
          Cursor = crHandPoint
          Caption = 'Unit'
          OnClick = PickColor
        end
        object lblFormColor: TLabel
          Tag = 2
          Left = 32
          Top = 30
          Width = 24
          Height = 13
          Cursor = crHandPoint
          Caption = 'Form'
          OnClick = PickColor
        end
        object lblDataModuleColor: TLabel
          Tag = 3
          Left = 32
          Top = 50
          Width = 60
          Height = 13
          Cursor = crHandPoint
          Caption = 'Data Module'
          OnClick = PickColor
        end
        object lblProjectColor: TLabel
          Tag = 4
          Left = 32
          Top = 70
          Width = 70
          Height = 13
          Cursor = crHandPoint
          Caption = 'Project Source'
          OnClick = PickColor
        end
        object btnUnitColor: TButton
          Tag = 1
          Left = 240
          Top = 8
          Width = 23
          Height = 18
          Caption = '...'
          TabOrder = 0
          OnClick = PickColor
        end
        object btnFormColor: TButton
          Tag = 2
          Left = 240
          Top = 28
          Width = 23
          Height = 18
          Caption = '...'
          TabOrder = 1
          OnClick = PickColor
        end
        object btnDataModuleColor: TButton
          Tag = 3
          Left = 240
          Top = 48
          Width = 23
          Height = 18
          Caption = '...'
          TabOrder = 2
          OnClick = PickColor
        end
        object btnProjectColor: TButton
          Tag = 4
          Left = 240
          Top = 68
          Width = 23
          Height = 18
          Caption = '...'
          TabOrder = 3
          OnClick = PickColor
        end
      end
      object btnDefault: TButton
        Left = 8
        Top = 135
        Width = 109
        Height = 25
        Caption = 'Reset to &default'
        TabOrder = 2
        OnClick = btnDefaultClick
      end
    end
    object tsAbout: TTabSheet
      Caption = 'About...'
      ImageIndex = 1
      DesignSize = (
        287
        181)
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
        Width = 82
        Height = 16
        Caption = 'UnitSwitcher'
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
        Caption = 'Version 0.3'
      end
      object TLabel
        Left = 135
        Top = 23
        Width = 145
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Copyright '#169' 2006 X'#178'Software'
      end
      object TLabel
        Left = 56
        Top = 90
        Width = 225
        Height = 41
        Anchors = [akLeft, akRight, akBottom]
        AutoSize = False
        Caption = 
          'UnitSwitcher is released as open-source under the zlib/libpng OS' +
          'I-approved license. See license.txt for details.'
        WordWrap = True
      end
      object TLabel
        Left = 56
        Top = 52
        Width = 225
        Height = 29
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'Many thanks to Richard Lichtendahl for the idea, feedback and be' +
          'ta testing.'
        WordWrap = True
      end
      object lblBugReport: TLabel
        Left = 56
        Top = 156
        Width = 75
        Height = 13
        Cursor = crHandPoint
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
    Left = 224
    Top = 219
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOk: TButton
    Left = 143
    Top = 219
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
    Left = 260
    Top = 160
  end
end
