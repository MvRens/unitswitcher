object frmUnSwShortcuts: TfrmUnSwShortcuts
  Left = 188
  Top = 81
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'UnitSwitcher Shortcut keys'
  ClientHeight = 470
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblHeader: TLabel
    Left = 8
    Top = 8
    Width = 105
    Height = 13
    Caption = 'Navigating the list:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object TLabel
    Left = 28
    Top = 28
    Width = 110
    Height = 13
    Caption = 'Up / Down arrow keys:'
  end
  object TLabel
    Left = 168
    Top = 28
    Width = 145
    Height = 13
    Caption = 'Select the previous / next unit'
  end
  object TLabel
    Left = 28
    Top = 44
    Width = 110
    Height = 13
    Caption = 'Page-Up / Page-Down:'
  end
  object TLabel
    Left = 168
    Top = 44
    Width = 122
    Height = 13
    Caption = 'Jump through the unit list'
  end
  object TLabel
    Left = 28
    Top = 60
    Width = 101
    Height = 13
    Caption = 'Ctrl-Home / Ctrl-End:'
  end
  object TLabel
    Left = 168
    Top = 60
    Width = 118
    Height = 13
    Caption = 'Select the first / last unit'
  end
  object TLabel
    Left = 8
    Top = 89
    Width = 70
    Height = 13
    Caption = 'Multi-select:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object TLabel
    Left = 28
    Top = 108
    Width = 285
    Height = 25
    AutoSize = False
    Caption = 
      'Hold Shift while using the navigation keys to select more than o' +
      'ne unit, or hold Ctrl while clicking with the mouse.'
    WordWrap = True
  end
  object TLabel
    Left = 28
    Top = 144
    Width = 32
    Height = 13
    Caption = 'Ctrl-A:'
  end
  object TLabel
    Left = 168
    Top = 145
    Width = 152
    Height = 13
    Caption = 'Select all units in the current list'
  end
  object TLabel
    Left = 28
    Top = 160
    Width = 29
    Height = 13
    Caption = 'Ctrl-I:'
  end
  object TLabel
    Left = 168
    Top = 160
    Width = 94
    Height = 13
    Caption = 'Invert the selection'
  end
  object TLabel
    Left = 8
    Top = 188
    Width = 62
    Height = 13
    Caption = 'Sub-filters:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object TLabel
    Left = 28
    Top = 208
    Width = 43
    Height = 13
    Caption = 'Ctrl-Tab:'
  end
  object TLabel
    Left = 168
    Top = 208
    Width = 169
    Height = 13
    Caption = 'Start a new filter on the current list'
  end
  object TLabel
    Left = 28
    Top = 224
    Width = 75
    Height = 13
    Caption = 'Ctrl-Backspace:'
  end
  object TLabel
    Left = 168
    Top = 224
    Width = 118
    Height = 13
    Caption = 'Revert the last sub-filter'
  end
  object TLabel
    Left = 8
    Top = 316
    Width = 35
    Height = 13
    Caption = 'Other:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object TLabel
    Left = 28
    Top = 336
    Width = 49
    Height = 13
    Caption = 'Ctrl-Alt-N:'
  end
  object TLabel
    Left = 168
    Top = 336
    Width = 99
    Height = 13
    Caption = 'Sort the list by name'
  end
  object TLabel
    Left = 28
    Top = 352
    Width = 48
    Height = 13
    Caption = 'Ctrl-Alt-T:'
  end
  object TLabel
    Left = 168
    Top = 352
    Width = 95
    Height = 13
    Caption = 'Sort the list by type'
  end
  object TLabel
    Left = 28
    Top = 367
    Width = 48
    Height = 13
    Caption = 'Ctrl-Alt-F:'
  end
  object TLabel
    Left = 168
    Top = 368
    Width = 109
    Height = 13
    Caption = 'Open containing folder'
  end
  object TLabel
    Left = 28
    Top = 384
    Width = 68
    Height = 13
    Caption = 'Ctrl-Alt-Enter:'
  end
  object TLabel
    Left = 168
    Top = 384
    Width = 95
    Height = 13
    Caption = 'Open file properties'
  end
  object TLabel
    Left = 8
    Top = 252
    Width = 119
    Height = 13
    Caption = 'Most-Recently-Used:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object TLabel
    Left = 28
    Top = 288
    Width = 38
    Height = 13
    Caption = 'Ctrl-Up:'
  end
  object TLabel
    Left = 168
    Top = 288
    Width = 93
    Height = 13
    Caption = 'Use later MRU filter'
  end
  object TLabel
    Left = 28
    Top = 272
    Width = 52
    Height = 13
    Caption = 'Ctrl-Down:'
  end
  object TLabel
    Left = 168
    Top = 272
    Width = 101
    Height = 13
    Caption = 'Use earlier MRU filter'
  end
  object TLabel
    Left = 28
    Top = 400
    Width = 77
    Height = 13
    Caption = 'Ctrl-Shift-Enter:'
  end
  object TLabel
    Left = 168
    Top = 400
    Width = 156
    Height = 13
    Caption = 'Open file properties for the .dfm'
  end
  object btnOk: TButton
    Left = 139
    Top = 437
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
end
