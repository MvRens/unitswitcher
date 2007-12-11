object frmBaseSwDialog: TfrmBaseSwDialog
  Left = 284
  Top = 120
  Width = 320
  Height = 425
  BorderIcons = [biSystemMenu]
  Caption = 'UnitSwitcher'
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 290
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010100001000400280100001600000028000000100000002000
    0000010004000000000080000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000F777777777700000FF8F8F8F8F700000F8F8F8F8F8700000
    FF0000000F700000F8F8F8F8F8700000F999999999700400F9FFFFFFF9700000
    F999999999700000F8F8F8F8F8700000FF0000008F700000F8F8F8F8F7700000
    FF00008F00000000F8F8F8F80F000000FFFFFFFF00000000000000000000E000
    0000E0000000E0000000E0000000E00000006000000020000000000000002000
    000060000000E0000000E0000000E0000000E0010000E0030000E0070000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object sbStatus: TStatusBar
    Left = 0
    Top = 379
    Width = 312
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 312
    Height = 343
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object pnlSearch: TPanel
      Left = 4
      Top = 24
      Width = 304
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        304
        25)
      object cmbSearch: TComboBox
        Left = 0
        Top = 0
        Width = 304
        Height = 21
        AutoComplete = False
        Anchors = [akLeft, akTop, akRight]
        DropDownCount = 10
        ItemHeight = 13
        TabOrder = 0
        OnChange = cmbSearchChange
        OnKeyDown = cmbSearchKeyDown
        OnKeyPress = cmbSearchKeyPress
      end
    end
    object lstItems: TListBox
      Left = 4
      Top = 49
      Width = 304
      Height = 290
      Style = lbVirtualOwnerDraw
      Align = alClient
      ItemHeight = 20
      MultiSelect = True
      PopupMenu = pmnItems
      TabOrder = 1
      OnClick = lstItemsClick
      OnData = lstItemsData
      OnDblClick = lstItemsDblClick
      OnDrawItem = lstItemsDrawItem
      OnMouseDown = lstItemsMouseDown
    end
    object pnlSubFilters: TPanel
      Left = 4
      Top = 4
      Width = 304
      Height = 20
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      Visible = False
      DesignSize = (
        304
        20)
      object lblSubFilters: TLabel
        Left = 0
        Top = 1
        Width = 305
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'frm '#187' Dialog '#187
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 343
    Width = 312
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      312
      36)
    object btnCancel: TButton
      Left = 233
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 152
      Top = 5
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
    end
  end
  object ilsTypes: TImageList
    Left = 28
    Top = 228
  end
  object alMain: TActionList
    Left = 84
    Top = 228
    object actSelectAll: TAction
      Caption = 'Select &All'
      ShortCut = 16449
      OnExecute = actSelectAllExecute
    end
    object actSelectInvert: TAction
      Caption = '&Invert Selection'
      ShortCut = 16457
      OnExecute = actSelectInvertExecute
    end
    object actMRUPrior: TAction
      Caption = 'actMRUPrior'
      ShortCut = 16422
      OnExecute = actMRUPriorExecute
    end
    object actMRUNext: TAction
      Caption = 'actMRUNext'
      ShortCut = 16424
      OnExecute = actMRUNextExecute
    end
  end
  object pmnItems: TPopupMenu
    Left = 140
    Top = 228
    object pmnItemsSelectAll: TMenuItem
      Action = actSelectAll
    end
    object pmnItemsSelectInvert: TMenuItem
      Action = actSelectInvert
    end
  end
end
