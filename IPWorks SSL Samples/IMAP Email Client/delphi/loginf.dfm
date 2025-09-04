object FormLogin: TFormLogin
  Left = 389
  Top = 269
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 218
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 24
    Width = 63
    Height = 13
    Caption = 'IMAP Server:'
  end
  object Label2: TLabel
    Left = 12
    Top = 88
    Width = 28
    Height = 13
    Caption = 'User: '
  end
  object Label3: TLabel
    Left = 12
    Top = 120
    Width = 52
    Height = 13
    Caption = 'Password: '
  end
  object Label4: TLabel
    Left = 12
    Top = 152
    Width = 70
    Height = 13
    Caption = 'SMTP Server: '
  end
  object Label5: TLabel
    Left = 12
    Top = 56
    Width = 48
    Height = 13
    Caption = 'IMAP Port'
  end
  object EditUser: TEdit
    Left = 87
    Top = 85
    Width = 175
    Height = 21
    TabOrder = 2
  end
  object EditIMAPServer: TEdit
    Left = 87
    Top = 21
    Width = 175
    Height = 21
    TabOrder = 0
  end
  object EditSMTPServer: TEdit
    Left = 87
    Top = 149
    Width = 175
    Height = 21
    TabOrder = 4
  end
  object Button1: TButton
    Left = 87
    Top = 189
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object Button2: TButton
    Left = 187
    Top = 189
    Width = 75
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object EditPassword: TEdit
    Left = 87
    Top = 117
    Width = 175
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object EditIMAPPort: TEdit
    Left = 87
    Top = 53
    Width = 174
    Height = 21
    TabOrder = 1
  end
end
