object FormLogin: TFormLogin
  Left = 389
  Top = 269
  BorderStyle = bsSingle
  Caption = 'Login'
  ClientHeight = 194
  ClientWidth = 272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 59
    Height = 13
    Caption = 'POP Server:'
  end
  object Label2: TLabel
    Left = 8
    Top = 71
    Width = 28
    Height = 13
    Caption = 'User: '
  end
  object Label3: TLabel
    Left = 8
    Top = 103
    Width = 52
    Height = 13
    Caption = 'Password: '
  end
  object Label4: TLabel
    Left = 8
    Top = 135
    Width = 70
    Height = 13
    Caption = 'SMTP Server: '
  end
  object Label5: TLabel
    Left = 8
    Top = 44
    Width = 47
    Height = 13
    Caption = 'POP Port:'
  end
  object EditUser: TEdit
    Left = 88
    Top = 67
    Width = 175
    Height = 21
    TabOrder = 2
  end
  object EditPassword: TEdit
    Left = 88
    Top = 99
    Width = 175
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
  end
  object EditPOPServer: TEdit
    Left = 88
    Top = 12
    Width = 175
    Height = 21
    TabOrder = 0
  end
  object EditSMTPServer: TEdit
    Left = 88
    Top = 131
    Width = 175
    Height = 21
    TabOrder = 4
  end
  object ButtonOK: TButton
    Left = 108
    Top = 163
    Width = 75
    Height = 23
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object ButtonCancel: TButton
    Left = 192
    Top = 163
    Width = 71
    Height = 23
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object EditPOPPort: TEdit
    Left = 89
    Top = 40
    Width = 175
    Height = 21
    TabOrder = 1
  end
end
