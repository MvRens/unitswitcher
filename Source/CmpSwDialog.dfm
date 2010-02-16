inherited frmCmpSwDialog: TfrmCmpSwDialog
  Caption = 'ComponentSwitcher'
  ClientHeight = 521
  ClientWidth = 342
  ExplicitWidth = 358
  ExplicitHeight = 557
  PixelsPerInch = 96
  TextHeight = 13
  inherited sbStatus: TStatusBar
    Top = 502
    Width = 342
    ExplicitTop = 502
    ExplicitWidth = 342
  end
  inherited pnlMain: TPanel
    Width = 342
    Height = 393
    ExplicitWidth = 342
    ExplicitHeight = 393
    inherited pnlSearch: TPanel
      Width = 334
      TabOrder = 1
      ExplicitWidth = 334
      inherited cmbSearch: TComboBox
        Width = 342
        ExplicitWidth = 342
      end
    end
    inherited lstItems: TListBox
      Width = 334
      Height = 340
      TabOrder = 2
      ExplicitWidth = 334
      ExplicitHeight = 340
    end
    inherited pnlSubFilters: TPanel
      Width = 334
      TabOrder = 0
      ExplicitWidth = 334
      inherited lblSubFilters: TLabel
        Width = 343
        ExplicitWidth = 343
      end
    end
  end
  inherited pnlButtons: TPanel
    Top = 466
    Width = 342
    TabOrder = 2
    ExplicitTop = 466
    ExplicitWidth = 342
    inherited btnCancel: TButton
      Left = 263
      TabOrder = 2
      ExplicitLeft = 263
    end
    inherited btnOK: TButton
      Left = 182
      TabOrder = 1
      ExplicitLeft = 182
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
    Top = 393
    Width = 342
    Height = 73
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object gbFilters: TGroupBox
      Left = 4
      Top = 4
      Width = 334
      Height = 65
      Align = alClient
      TabOrder = 0
      DesignSize = (
        334
        65)
      object btnMoreFilters: TButton
        Left = 296
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
