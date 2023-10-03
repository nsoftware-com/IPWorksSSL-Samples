(*
 * IPWorks SSL 2022 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks SSL in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksssl
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit popclientf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, ipspop, ipscore, ipstypes, ipssmtp;

type

  TFormPopclient = class(TForm)
    ListBoxMessage: TListBox;
    ListViewMailbox: TListView;
    SaveDialog1: TSaveDialog;
    ButtonLoginLogout: TButton;
    ButtonRetrieve: TButton;
    ButtonCompose: TButton;
    ButtonReply: TButton;
    ButtonDelete: TButton;
    Label2: TLabel;
    ipsSMTP1: TipsSMTP;
    ipsPOP1: TipsPOP;
    procedure ipsPOP1Header(Sender: TObject; const Field, Value: String);
    procedure ButtonLoginLogoutClick(Sender: TObject);
    procedure ButtonComposeClick(Sender: TObject);
    procedure ButtonReplyClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonRetrieveClick(Sender: TObject);
    procedure ListViewMailboxClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ipsSMTP1PITrail(Sender: TObject; Direction: Integer;
      const Message: String);
    procedure ipsSMTP1EndTransfer(Sender: TObject; Direction: Integer);
    procedure ipsSMTP1StartTransfer(Sender: TObject; Direction: Integer);
    procedure ipsPOP1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
    procedure ipsSMTP1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);

  private
    { Private declarations }
    msg_from: string;
    msg_date: string;
    msg_subject: string;
    lines: integer;

  public
    { Public declarations }
    constructor Create( Owner: TComponent ); override;

  end;

var
  FormPopclient: TFormPopclient;

implementation

uses progressf, loginf, composef;

{$R *.DFM}

constructor TFormPopclient.Create( Owner: TComponent );
begin
   inherited Create(Owner);
   lines := 0;
   msg_from := '';
   msg_date := '';
   msg_subject := '';
end;




procedure TFormPopclient.ipsPOP1Header(Sender: TObject; const Field,
  Value: String);
begin
   if Field = 'From' then
   begin
   	msg_from := Value;
   end

   else if Field = 'Date' then
   begin
   	msg_date := Value;
   end

   else if Field = 'Subject' then
   begin
      msg_subject := Value;
   end
end;

procedure TFormPopclient.ipsPOP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormPopclient.ButtonLoginLogoutClick(Sender: TObject);
var
   k: integer;
begin
   // Login
   Screen.Cursor := crHourGlass;
   if ButtonLoginLogout.Caption = 'Login' then
   begin
      if FormLogin.ShowModal() = mrOk then
      begin
         ListViewMailbox.Items.Clear();
         ipsPOP1.Disconnect();

         ipsPOP1.MailServer := FormLogin.EditPOPServer.Text;
         ipsPOP1.User := FormLogin.EditUser.Text;
         ipsPOP1.Password := FormLogin.EditPassword.Text;
         ipsSMTP1.MailServer := FormLogin.EditSMTPServer.Text;
         try
            ipsPOP1.Connect;
         except on E: EipsPOP do
            ShowMessage(E.Message);
         end;

         ButtonLoginLogout.Caption := 'Logout';
         ButtonRetrieve.Enabled := true;
         ButtonCompose.Enabled := true;

         // Get message headers.
         ipsPOP1.MaxLines := 1;
         for k := 1 to ipsPOP1.MessageCount do
         begin
            msg_from := '';
            msg_date := '';
            msg_subject := '';
            ipsPOP1.MessageNumber := k;
            try
               ipsPOP1.Retrieve;
            except on E: EipsPOP do
               ShowMessage(E.Message);
            end;
            ListViewMailbox.Items.Add();
            ListViewMailbox.Items.Item[k - 1].Caption := IntToStr(ipsPOP1.MessageNumber);
            ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_from);
            ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_subject);
            ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_date);
         end;
         ipsPOP1.MaxLines := 0;
      end;
   end
   else
   begin
      try
         ipsPOP1.Disconnect();
      except on E: EipsPOP do
         ShowMessage(E.Message);
      end;
      ButtonLoginLogout.Caption := 'Login';
      ButtonCompose.Enabled := false;
      ButtonRetrieve.Enabled := false;
      ButtonReply.Enabled := false;
      ButtonDelete.Enabled := false;
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.ButtonComposeClick(Sender: TObject);
var
   i: integer;
begin
// Compose
   Screen.Cursor := crHourGlass;
   if FormCompose.ShowModal() = mrOk then
   begin
      ipsSMTP1.ResetHeaders();
      ipsSMTP1.From := FormCompose.EditFrom.Text;
      ipsSMTP1.SendTo := FormCompose.EditTo.Text;
      ipsSMTP1.Cc := FormCompose.EditCc.Text;
      ipsSMTP1.Subject := FormCompose.EditSubject.Text;
      ipsSMTP1.MessageText := FormCompose.MemoMessage.Text;

      try
         ipsSMTP1.Connect();
         ipsSMTP1.Send();
         ipsSMTP1.Disconnect();
      except on E: EipsSMTP do
         ShowMessage(E.Message);
      end;
      FormCompose.EditTo.Text := '';
      FormCompose.EditCc.Text := '';
      FormCompose.EditSubject.Text := '';
      FormCompose.MemoMessage.Lines.Clear();
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.ButtonReplyClick(Sender: TObject);
var
   j: integer;
begin
// Reply
   Screen.Cursor := crHourGlass;
   // Only reply if a message is selected.
   if ListViewMailbox.SelCount > 0 then
   begin
      FormCompose.EditTo.Text := msg_from;
      FormCompose.EditSubject.Text := 'Re: ' + msg_subject;
      for j := 0 to ListBoxMessage.Items.Count - 1 do
         FormCompose.MemoMessage.Lines.Add( '> ' + ListBoxMessage.Items.Strings[j] );

      if FormCompose.ShowModal() = mrOk then
      begin
         ipsSMTP1.ResetHeaders();
         ipsSMTP1.From := FormCompose.EditFrom.Text;
         ipsSMTP1.SendTo := FormCompose.EditTo.Text;
         ipsSMTP1.Cc := FormCompose.EditCc.Text ;
         ipsSMTP1.Subject := FormCompose.EditSubject.Text;
         ipsSMTP1.MessageText := FormCompose.MemoMessage.Text;

         try
            ipsSMTP1.Connect;
            ipsSMTP1.Send;
            ipsSMTP1.Disconnect;
         except on E: EipsSMTP do
            ShowMessage(E.Message);
         end;
         FormCompose.EditTo.Text := '';
         FormCompose.EditCc.Text := '';
         FormCompose.EditSubject.Text := '';
         FormCompose.MemoMessage.Lines.Clear();
      end;
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.ButtonDeleteClick(Sender: TObject);
var
   k: integer;
begin
// Delete a message.
   Screen.Cursor := crHourGlass;
   // Only delete if a message is selected.
   if ListViewMailbox.SelCount > 0 then
   begin
      ipsPOP1.MessageNumber := StrToInt(ListViewMailbox.Selected.Caption);
      try
         ipsPOP1.Delete();
      except on E: EipsPOP do
         ShowMessage(E.Message);
      end;
      // Now for convenience, refresh the header list for remaining messages.
      if ButtonLoginLogout.Caption = 'Logout' then
      begin
         ListViewMailbox.Items.Clear();
         try
            ipsPOP1.Disconnect();
            ipsPOP1.Connect();
         except on E: EipsPOP do
            ShowMessage(E.Message);
         end;
         ipsPOP1.MaxLines := 1;
         for k := 1 to ipsPOP1.MessageCount do
         begin
            msg_from := '';
            msg_date := '';
            msg_subject := '';
            ipsPOP1.MessageNumber := k;
            try
               ipsPOP1.Retrieve();
            except on E: EipsPOP do
               ShowMessage(E.Message);
            end;
            ListViewMailbox.Items.Add();
            ListViewMailbox.Items.Item[k - 1].Caption := IntToStr( ipsPOP1.MessageNumber );
            ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_from);
            ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_subject);
            ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_date);
         end;
         ipsPOP1.MaxLines := 0;
      end;
   end
   else
   begin
      ShowMessage('No message selected.');
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.ButtonRetrieveClick(Sender: TObject);
var
   k: integer;
begin
// Retrieve
   Screen.Cursor := crHourGlass;
   if ButtonLoginLogout.Caption = 'Logout' then
   begin
      ListViewMailbox.Items.Clear();
      try
         ipsPOP1.Disconnect();
         ipsPOP1.Connect();
      except on E: EipsPOP do
         ShowMessage(E.Message);
      end;
      ipsPOP1.MaxLines := 1;

      for k := 1 to ipsPOP1.MessageCount do
      begin
         msg_from := '';
         msg_date := '';
         msg_subject := '';
         ipsPOP1.MessageNumber := k;
         try
            ipsPOP1.Retrieve();
         except on E: EipsPOP do
            ShowMessage(E.Message);
         end;
         ListViewMailbox.Items.Add();
         ListViewMailbox.Items.Item[k - 1].Caption := IntToStr( ipsPOP1.MessageNumber );
         ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_from);
         ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_subject);
         ListViewMailbox.Items.Item[k - 1].SubItems.Add(msg_date);
      end;
      ipsPOP1.MaxLines := 0;
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.ListViewMailboxClick(Sender: TObject);
var
headerIndex: Integer;
begin
// A message header was clicked.  Display the message text.
   Screen.Cursor := crHourGlass;
   ListBoxMessage.Items.Clear();
   if ListViewMailbox.SelCount > 0 then
   begin
      ipsPOP1.MessageNumber := StrToInt(ListViewMailbox.Selected.Caption);
      try
        ipsPOP1.Retrieve();
        ListBoxMessage.Items.Text := ipsPOP1.MessageText;
      except on E: EipsPOP do
         ShowMessage(E.Message);
      end;
      ButtonReply.Enabled := true;
      ButtonDelete.Enabled := true;
   end;
   Screen.Cursor := crDefault;
end;


procedure TFormPopclient.FormResize(Sender: TObject);
begin
   ListViewMailbox.Columns.Items[1].Width := (ListViewMailbox.Width - 50) div 3;
   ListViewMailbox.Columns.Items[2].Width := ListViewMailbox.Columns.Items[1].Width;
   ListViewMailbox.Columns.Items[3].Width := ListViewMailbox.Columns.Items[1].Width;
end;

procedure TFormPopclient.ipsSMTP1EndTransfer(Sender: TObject; Direction: Integer);
begin
   FormProgress.Hide();
end;

procedure TFormPopclient.ipsSMTP1PITrail(Sender: TObject; Direction: Integer;
  const Message: String);
begin
   FormProgress.ListBoxPITrail.Items.Add(Message);
end;

procedure TFormPopclient.ipsSMTP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormPopclient.ipsSMTP1StartTransfer(Sender: TObject;
  Direction: Integer);
begin
   FormProgress.Show();
end;
end.


