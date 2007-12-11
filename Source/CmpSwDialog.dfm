inherited frmCmpSwDialog: TfrmCmpSwDialog
  Width = 358
  Height = 557
  Caption = 'ComponentSwitcher'
  PixelsPerInch = 96
  TextHeight = 13
  inherited sbStatus: TStatusBar
    Top = 511
    Width = 350
  end
  inherited pnlMain: TPanel
    Width = 350
    Height = 402
    inherited pnlSearch: TPanel
      Width = 342
      inherited cmbSearch: TComboBox
        Width = 342
      end
    end
    inherited lstItems: TListBox
      Width = 342
      Height = 349
    end
    inherited pnlSubFilters: TPanel
      Width = 342
      inherited lblSubFilters: TLabel
        Width = 343
      end
    end
  end
  inherited pnlButtons: TPanel
    Top = 475
    Width = 350
    inherited btnCancel: TButton
      Left = 271
    end
    inherited btnOK: TButton
      Left = 190
    end
  end
  object pnlFilters: TPanel [3]
    Left = 0
    Top = 402
    Width = 350
    Height = 73
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 3
    object gbFilters: TGroupBox
      Left = 4
      Top = 4
      Width = 342
      Height = 65
      Align = alClient
      Caption = ' Filter '
      TabOrder = 0
      DesignSize = (
        342
        65)
      object btnMoreFilters: TButton
        Left = 304
        Top = 32
        Width = 31
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '>>'
        TabOrder = 0
        OnClick = btnMoreFiltersClick
      end
    end
  end
  inherited alMain: TActionList
    object actFilterSelected: TAction
      Caption = '&Filter selected class(es)'
      Enabled = False
    end
  end
  inherited pmnItems: TPopupMenu
    object pmnItemsFilters: TMenuItem [0]
      Caption = 'Filters'
    end
    object pmnItemsFilterSelected: TMenuItem [1]
      Action = actFilterSelected
    end
    object pmnItemsSep1: TMenuItem [2]
      Caption = '-'
    end
  end
  object pmnMoreFilters: TPopupMenu
    Left = 192
    Top = 228
  end
end
