object FormPopclient: TFormPopclient
  Left = 231
  Top = 113
  Caption = 'MailBox'
  ClientHeight = 462
  ClientWidth = 510
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OnResize = FormResize
  DesignSize = (
    510
    462)
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 16
    Width = 393
    Height = 26
    Caption = 
      'This is a demo of building a simple email client applications us' +
      'ing  POP and SMTP.  Click the '#39'Login'#39' button to connect to a POP' +
      ' Server.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object ListBoxMessage: TListBox
    Left = 8
    Top = 248
    Width = 494
    Height = 206
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 6
  end
  object ListViewMailbox: TListView
    Left = 8
    Top = 81
    Width = 494
    Height = 151
    Anchors = [akLeft, akTop, akRight]
    Columns = <
      item
        Caption = 'Number'
      end
      item
        Caption = 'From'
        Width = 150
      end
      item
        Caption = 'Subject'
        Width = 150
      end
      item
        Caption = 'Date'
        Width = 150
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 5
    ViewStyle = vsReport
    OnClick = ListViewMailboxClick
  end
  object ButtonLoginLogout: TButton
    Left = 8
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Login'
    Default = True
    TabOrder = 0
    OnClick = ButtonLoginLogoutClick
  end
  object ButtonRetrieve: TButton
    Left = 104
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Retrieve'
    Enabled = False
    TabOrder = 1
    OnClick = ButtonRetrieveClick
  end
  object ButtonCompose: TButton
    Left = 184
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Compose'
    Enabled = False
    TabOrder = 2
    OnClick = ButtonComposeClick
  end
  object ButtonReply: TButton
    Left = 264
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Reply'
    Enabled = False
    TabOrder = 3
    OnClick = ButtonReplyClick
  end
  object ButtonDelete: TButton
    Left = 344
    Top = 52
    Width = 75
    Height = 23
    Caption = 'Delete'
    Enabled = False
    TabOrder = 4
    OnClick = ButtonDeleteClick
  end
  object SaveDialog1: TSaveDialog
    Left = 404
    Top = 8
  end
  object ipsSMTP1: TipsSMTP
    SSLCertStore = 'MY'
    OnEndTransfer = ipsSMTP1EndTransfer
    OnPITrail = ipsSMTP1PITrail
    OnSSLServerAuthentication = ipsSMTP1SSLServerAuthentication
    OnStartTransfer = ipsSMTP1StartTransfer
    Left = 472
    Top = 8
  end
  object ipsPOP1: TipsPOP
    SSLCertStore = 'MY'
    OnHeader = ipsPOP1Header
    OnSSLServerAuthentication = ipsPOP1SSLServerAuthentication
    Left = 440
    Top = 8
  end
end


