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
      TabOrder = 1
      inherited cmbSearch: TComboBox
        Width = 342
      end
    end
    inherited lstItems: TListBox
      Width = 342
      Height = 349
      TabOrder = 2
    end
    inherited pnlSubFilters: TPanel
      Width = 342
      TabOrder = 0
      inherited lblSubFilters: TLabel
        Width = 343
      end
    end
  end
  inherited pnlButtons: TPanel
    Top = 475
    Width = 350
    TabOrder = 2
    inherited btnCancel: TButton
      Left = 271
      TabOrder = 2
    end
    inherited btnOK: TButton
      Left = 190
      TabOrder = 1
    end
    object btnConfiguration: TButton
      Left = 4
      Top = 5
      Width = 85
      Height = 25
      Caption = '&Configuration'
      TabOrder = 0
      OnClick = btnConfigurationClick
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
    TabOrder = 1
    object gbFilters: TGroupBox
      Left = 4
      Top = 4
      Width = 342
      Height = 65
      Align = alClient
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
    object actSortByName: TAction
      Caption = 'Sort by &Name'
      GroupIndex = 1
      ShortCut = 49230
      OnExecute = SortExecute
    end
    object actSortByType: TAction
      Caption = 'Sort by &Type'
      GroupIndex = 1
      ShortCut = 49236
      OnExecute = SortExecute
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
    object pmnItemsSortByName: TMenuItem [3]
      Action = actSortByName
    end
    object pmnItemsSortByType: TMenuItem [4]
      Action = actSortByType
    end
    object pmnItemsSep2: TMenuItem [5]
      Caption = '-'
    end
  end
  object pmnMoreFilters: TPopupMenu
    Left = 192
    Top = 228
  end
end
