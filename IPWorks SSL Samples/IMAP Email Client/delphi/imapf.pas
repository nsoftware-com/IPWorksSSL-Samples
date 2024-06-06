(*
 * IPWorks SSL 2024 Delphi Edition - Sample Project
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
unit imapf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ipshtmlmailer, ipscore, ipstypes, ipsimap, StdCtrls, ExtCtrls, ComCtrls;

type
  GetMode = ( HEADERS_, TEXT_ );
  TFormImap = class(TForm)
    ListViewMessages: TListView;
    ListBoxMessage: TListBox;
    TreeViewMailboxes: TTreeView;
    ButtonCompose: TButton;
    ButtonLogin: TButton;
    Label5: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    ipsIMAP1: TipsIMAP;
    ipsHTMLMailer1: TipsHTMLMailer;
    procedure ButtonLoginClick(Sender: TObject);
    procedure ipsIMAP1MailboxList(Sender: TObject; const Mailbox,
      Separator, Flags: String);
    procedure TreeViewMailboxesClick(Sender: TObject);
    procedure ListViewMessagesClick(Sender: TObject);
    procedure ipsIMAP1MessagePart(Sender: TObject; const PartId: String;
      Size: Int64; const ContentType, FileName, ContentEncoding,
      Parameters, MultipartMode, ContentId, ContentDisposition: String);
    procedure ipsHTMLMailer1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TBytes; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
    procedure ButtonComposeClick(Sender: TObject);
    procedure ipsIMAP1MessageInfo(Sender: TObject; const MessageId,
      Subject, MessageDate, From, Flags: String; Size: Int64);
    procedure ipsIMAP1Transfer(Sender: TObject; Direction: Integer;
      BytesTransferred: Int64; PercentDone: Integer; const Text: string);
    procedure ipsIMAP1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TBytes; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
  private
    { Private declarations }
    head_: TTreeNode;
    sep: String;
    textpart: String;
    state: GetMode;
  public
    { Public declarations }
    constructor Create( Owner: TComponent );  override;
    function treesearch( Node: TTreeNode; text: String): TTreeNode;
    procedure buildtree( Node: TTreeNode; lstr: String; rstr: String);
    function mailboxname( Node: TTreeNode ): String;
  end;

var
  FormImap: TFormImap;

implementation

uses loginf, composef;

{$R *.DFM}

constructor TFormImap.Create( Owner: TComponent );
begin
   inherited Create(Owner);
   head_ := TreeViewMailboxes.Items.Add( nil, 'Folders');
   textpart := '0';
end;


procedure TFormImap.ButtonLoginClick(Sender: TObject);
begin
// Login / Logout
   Screen.Cursor := crHourGlass;
   if ButtonLogin.Caption = 'Login' then
   begin
      if FormLogin.ShowModal() = mrOk then
      begin
         ipsIMAP1.MailServer := FormLogin.EditIMAPServer.Text;
         ipsIMAP1.MailPort := StrToInt(FormLogin.EditIMAPPort.Text);
         ipsHTMLMailer1.MailServer := FormLogin.EditSMTPServer.Text;
         ipsHTMLMailer1.MailPort := 587;
         ipsIMAP1.User := FormLogin.EditUser.Text;
         ipsHTMLMailer1.User := FormLogin.EditUser.Text;
         ipsIMAP1.Password := FormLogin.EditPassword.Text;
         ipsHTMLMailer1.Password := FormLogin.EditPassword.Text;
         try
            ipsIMAP1.Connect;
            ipsIMAP1.Mailbox := '*';
            ipsIMAP1.ListMailboxes;
         except on E: EIPWorksSSL do
            ShowMessage(E.Message);
         end;
         ButtonLogin.Caption := 'Logout';
         ButtonCompose.Enabled := true;
      end;
   end
   else
   begin
      try
         ipsIMAP1.disconnect();
      except on E: EIPWorksSSL do
         ShowMessage(E.Message);
      end;
      TreeViewMailboxes.Items.Clear();
      ListViewMessages.Items.Clear();
      ListBoxMessage.Items.Clear();
      ButtonLogin.Caption := 'Login';
      head_ := TreeViewMailboxes.Items.Add( nil, 'Folders' );
      ButtonCompose.Enabled := false;
   end;
   Screen.Cursor := crDefault;
end;


function TFormImap.treesearch( Node: TTreeNode; text: String): TTreeNode;
var
   key: TTreeNode;
   i: integer;
begin
   key := nil;
   if Node.Text = text then
      Result := Node
   else if Node.Count = 0 then
      Result := nil
   else
   begin
      for i := 0 to Node.Count - 1 do
         if key = nil then
            key := treesearch(Node.Item[i], text);
      Result := key;
   end;
end;


procedure TFormImap.buildtree( Node: TTreeNode; lstr: String; rstr: String);
var
   key: TTreeNode;
begin
   key := treesearch(node, lstr);
   if key = nil then
      key := TreeViewMailboxes.Items.AddChild(node, lstr);

   lstr := rstr;
   if Pos(sep, lstr) = 0 then
      rstr := ''
   else
   begin
      rstr := lstr;
      Delete( lstr, Pos(sep, lstr), Length(lstr) );
      Delete( rstr, 1, Length(lstr) + Length(sep) );
   end;

   if Length(lstr) > 0 then
      buildtree(key, lstr, rstr);

end;


function TFormImap.mailboxname( Node: TTreeNode ): String;
begin
  if node.Parent.Level <> 0 then
     Result := mailboxname(node.Parent) + sep + node.Text
  else
     Result := node.Text;
end;


procedure TFormImap.ipsIMAP1MailboxList(Sender: TObject; const Mailbox,
  Separator, Flags: String);
var
   ltmp: String;
   rtmp: String;

begin

   sep := Separator;
   ltmp := Mailbox;

   if Pos(sep, ltmp) = 0 then
      rtmp := ''
   else
   begin
      rtmp := ltmp;
      Delete( ltmp, Pos(sep, ltmp), Length(ltmp) );
      Delete( rtmp, 1, Length(ltmp) + Length(sep) );
   end;

   buildtree(head_, ltmp, rtmp);

end;



procedure TFormImap.TreeViewMailboxesClick(Sender: TObject);
begin
// Click the tree
   Screen.Cursor := crHourGlass;
   try
      if TreeViewMailboxes.Selected.Level > 0 then
      begin
         ipsIMAP1.Mailbox := '"' + mailboxname(TreeViewMailboxes.Selected) + '"';
         ipsIMAP1.SelectMailbox();
         ListViewMessages.Items.Clear();
         if ipsIMAP1.MessageCount > 0 then
         begin
            ipsIMAP1.MessageSet := '1:' + IntToStr(ipsIMAP1.MessageCount);
            state := HEADERS_;
            ipsIMAP1.RetrieveMessageInfo();
         end;
      end;
   except on E: EIPWorksSSL do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;
end;



procedure TFormImap.ListViewMessagesClick(Sender: TObject);
begin
// Select a message
   Screen.Cursor := crHourGlass;
   try
      ipsIMAP1.MessageSet := ListViewMessages.Selected.Caption;
      ListBoxMessage.Items.Clear();
      state := TEXT_;
      ipsIMAP1.RetrieveMessageInfo();
      if textpart = '0' then
         ipsIMAP1.RetrieveMessageText()
      else
      begin
         ipsIMAP1.RetrieveMessagePart(textpart);
         ListBoxMessage.Items.Text := ipsIMAP1.MessageText;
         textpart := '0';
      end;
      Label3.Caption := ListViewMessages.Selected.SubItems.Strings[0];
      Label4.Caption := ListViewMessages.Selected.SubItems.Strings[1];
   except on E: EIPWorksSSL do
      ShowMessage(E.Message);
   end;
   Screen.Cursor := crDefault;
end;





procedure TFormImap.ipsIMAP1MessagePart(Sender: TObject; const PartId: String;
  Size: Int64; const ContentType, FileName, ContentEncoding, Parameters,
  MultipartMode, ContentId, ContentDisposition: String);
begin
//   if (ContentType = 'text/plain') and (FileName = '' ) then
//      textpart := PartId;
end;


procedure TFormImap.ipsIMAP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TBytes; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept := true;
end;

procedure TFormImap.ipsHTMLMailer1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TBytes; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
begin
  Accept := true;
end;

procedure TFormImap.ipsIMAP1Transfer(Sender: TObject; Direction: Integer;
  BytesTransferred: Int64; PercentDone: Integer; const Text: string);
begin
  ListBoxMessage.Items.Add(Text);
end;

procedure TFormImap.ipsIMAP1MessageInfo(Sender: TObject;
  const MessageId, Subject, MessageDate, From, Flags: String;
  Size: Int64);
begin
   if state = HEADERS_ then
   begin
      ListViewMessages.Items.Add();
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].Caption := MessageId;
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].SubItems.Add(From);
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].SubItems.Add(Subject);
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].SubItems.Add(MessageDate);
      ListViewMessages.Items.Item[ListViewMessages.Items.Count - 1].SubItems.Add(IntToStr(Size));
   end;
end;

procedure TFormImap.ButtonComposeClick(Sender: TObject);
var
   i: integer;
begin
// Compose
   Screen.Cursor := crHourGlass;
   if FormCompose.ShowModal() = mrOk then
   begin
      ipsHTMLMailer1.From := FormCompose.EditFrom.Text;
      ipsHTMLMailer1.SendTo:= FormCompose.EditTo.Text;
      ipsHTMLMailer1.Cc := FormCompose.EditCc.Text;
      ipsHTMLMailer1.Subject := FormCompose.EditSubject.Text;

      if FormCompose.ComboBoxAttachments.Items.Count <> 0 then
      begin

         // Add attachments
         for i := 1 to FormCompose.ComboBoxAttachments.Items.Count do
            ipsHTMLMailer1.AddAttachment(FormCompose.ComboBoxAttachments.Items.Strings[i-1]);

         // Assign message
         ipsHTMLMailer1.MessageText := FormCompose.MemoMessage.Text;
      end
      else
      begin
         // No attachments.  Just send the message.
         ipsHTMLMailer1.MessageText := FormCompose.MemoMessage.Text;
      end;

      try
         ipsHTMLMailer1.Connect();
         ipsHTMLMailer1.Send();
         ipsHTMLMailer1.Disconnect();
      except on E: EIPWorksSSL do
         ShowMessage(E.Message);
      end;
      FormCompose.EditCc.Text := '';
      FormCompose.EditTo.Text := '';
      FormCompose.EditSubject.Text := '';
      FormCompose.ComboBoxAttachments.Items.Clear();
      FormCompose.MemoMessage.Lines.Clear();
   end;
   Screen.Cursor := crDefault;
end;

end.



//---------------------------------------------------------------------------




