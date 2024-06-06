object FormHtmlmailer: TFormHtmlmailer
  Left = 200
  Top = 130
  BorderStyle = bsSingle
  Caption = 'HTMLMailer Demo'
  ClientHeight = 426
  ClientWidth = 515
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clDefault
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  TextHeight = 13
  object lblHeading: TLabel
    Left = 10
    Top = 8
    Width = 487
    Height = 26
    Caption = 
      'The HTMLMailer component can be used to send emails with html pa' +
      'rts as well as file attachments.  It will handle all necessary e' +
      'ncoding automatically.'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object lblMailServer: TLabel
    Left = 10
    Top = 50
    Width = 53
    Height = 13
    Caption = 'Mail Server'
  end
  object lblSendTo: TLabel
    Left = 12
    Top = 105
    Width = 39
    Height = 13
    Caption = 'Send To'
  end
  object lblFrom: TLabel
    Left = 268
    Top = 105
    Width = 24
    Height = 13
    Caption = 'From'
  end
  object lblSubject: TLabel
    Left = 12
    Top = 132
    Width = 36
    Height = 13
    Caption = 'Subject'
  end
  object lblHTML: TLabel
    Left = 8
    Top = 156
    Width = 96
    Height = 13
    Caption = 'HTML Message Text'
  end
  object lblAttachments: TLabel
    Left = 8
    Top = 362
    Width = 61
    Height = 13
    Caption = 'Attachments'
  end
  object Label1: TLabel
    Left = 266
    Top = 50
    Width = 20
    Height = 13
    Caption = 'Port'
  end
  object Label2: TLabel
    Left = 10
    Top = 77
    Width = 48
    Height = 13
    Caption = 'Username'
  end
  object Label3: TLabel
    Left = 266
    Top = 78
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object txtMailServer: TEdit
    Left = 72
    Top = 47
    Width = 188
    Height = 21
    TabOrder = 0
  end
  object txtSendTo: TEdit
    Left = 72
    Top = 102
    Width = 190
    Height = 21
    TabOrder = 4
  end
  object txtFrom: TEdit
    Left = 298
    Top = 102
    Width = 209
    Height = 21
    TabOrder = 5
  end
  object txtSubject: TEdit
    Left = 72
    Top = 129
    Width = 435
    Height = 21
    TabOrder = 6
    Text = 'HTMLMailer Demo Test Message'
  end
  object txtHTML: TMemo
    Left = 8
    Top = 175
    Width = 497
    Height = 181
    Lines.Strings = (
      
        'This message was sent with the /n software HTMLMailer Control!  ' +
        '<a '
      
        'href="http://www.nsoftware.com">www.nsoftware.com</a> is our web' +
        'site address.  Or, you can '
      
        'email us at <a href="mailto:support@nsoftware.com">support@nsoft' +
        'ware.com</a>.  Thank you '
      
        'for using our products!<p>You can include your HTML links and im' +
        'ages, just be sure that the paths '
      
        'are relative to the directory where you are running the program.' +
        '  ')
    TabOrder = 7
  end
  object lstAttachments: TListBox
    Left = 75
    Top = 362
    Width = 286
    Height = 56
    ItemHeight = 13
    TabOrder = 8
  end
  object btnAdd: TButton
    Left = 367
    Top = 362
    Width = 66
    Height = 25
    Caption = 'Add'
    TabOrder = 9
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 367
    Top = 393
    Width = 66
    Height = 25
    Caption = 'Remove'
    TabOrder = 10
    OnClick = btnRemoveClick
  end
  object btnSend: TButton
    Left = 439
    Top = 393
    Width = 66
    Height = 25
    Caption = 'Send'
    TabOrder = 11
    OnClick = btnSendClick
  end
  object tPort: TEdit
    Left = 296
    Top = 48
    Width = 209
    Height = 21
    TabOrder = 1
    Text = '587'
  end
  object tPassword: TEdit
    Left = 318
    Top = 75
    Width = 189
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object tUser: TEdit
    Left = 72
    Top = 74
    Width = 188
    Height = 21
    TabOrder = 2
  end
  object ipsHTMLMailer1: TipsHTMLMailer
    SSLAcceptServerCertStore = 'MY'
    SSLCertStore = 'MY'
    OnSSLServerAuthentication = ipsHTMLMailer1SSLServerAuthentication
    Left = 8
    Top = 379
  end
  object OpenDialog1: TOpenDialog
    Left = 40
    Top = 379
  end
end


