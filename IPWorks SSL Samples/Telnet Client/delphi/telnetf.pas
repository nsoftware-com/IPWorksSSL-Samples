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
unit telnetf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ipscore, ipstypes,
  ExtCtrls, ipsTelnet,
  {$IF CompilerVersion >= 24 } //XE3 or higher
  Winapi.Windows;
  {$ELSE}
  Windows;
  {$IFEND}
type
  TFormTelnet = class(TForm)
    Memo1: TMemo;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    tHost: TEdit;
    bLookUp: TButton;
    Label3: TLabel;
    Memo2: TMemo;
    ipsTelnet1: TipsTelnet;
    procedure bLookUpClick(Sender: TObject);
    procedure tHostKeyPress(Sender: TObject; var Key: Char);
    procedure ipsTelnet1DataIn(Sender: TObject; Text: string;
      TextB: TArray<System.Byte>);
    procedure ipsTelnet1Command(Sender: TObject; CommandCode: Integer);
    procedure ipsTelnet1Do(Sender: TObject; OptionCode: Integer);
    procedure ipsTelnet1Disconnected(Sender: TObject; StatusCode: Integer;
      const Description: string);
    procedure ipsTelnet1Dont(Sender: TObject; OptionCode: Integer);
    procedure ipsTelnet1Will(Sender: TObject; OptionCode: Integer);
    procedure ipsTelnet1SubOption(Sender: TObject; SubOption: string;
      SubOptionB: TArray<System.Byte>);
    procedure ipsTelnet1Wont(Sender: TObject; OptionCode: Integer);
    procedure ipsTelnet1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
    procedure ipsTelnet1Connected(Sender: TObject; StatusCode: Integer;
      const Description: string);
    procedure Memo1KeyPress(Sender: TObject; var Key: Char);
    procedure Memo2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTelnet: TFormTelnet;

implementation
{$R *.DFM}

procedure TFormTelnet.bLookUpClick(Sender: TObject);
{finger user@hostname}
var
    After10Seconds: Longint;
begin
    try
        if tHost.Text = '' then
        begin
            MessageBeep($FFFF);
            exit;
        end;

        {UNCONDITIONALLY close old connections or attempts}
        ipsTelnet1.Disconnect();

        {set the remote host}
	ipsTelnet1.RemoteHost := tHost.Text;

        {attempt connection}
        Memo1.Lines.Add('Connecting to ' + ipsTelnet1.RemoteHost + '...   ');
        Memo1.Lines.Add('Requested ' + tHost.Text);
		ipsTelnet1.Connect();

        {wait until the connection is achieved
        (timeout in 10 seconds)}
        After10Seconds := GetTickCount + 10 * 1000;
        while GetTickCount < After10Seconds do
        begin
            if ipsTelnet1.Connected then break;
            Application.ProcessMessages;
        end;

        if not ipsTelnet1.Connected then
        begin
            MessageDlg( 'Connection timed out.', mtWarning, [mbOK], 0);
            Memo1.Clear;
        end
    except on E: EIPWorksSSL do
        Memo1.Lines.Add('EXCEPTION: ' + E.Message);
    end;
end;

procedure TFormTelnet.tHostKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #13 then
    begin
        bLookUp.Click;
    	Key := #0;
    end;
end;

procedure TFormTelnet.Memo2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	if mbRight = Button
    then (Sender as TMemo).Clear;
end;

procedure TFormTelnet.ipsTelnet1Command(Sender: TObject; CommandCode: Integer);
begin
	Memo2.Lines.Add('SERVER: COMMAND ' + IntToStr(CommandCode));
end;

procedure TFormTelnet.ipsTelnet1Connected(Sender: TObject; StatusCode: Integer;
  const Description: string);
begin
	if 0 <> StatusCode
	then Memo2.Lines.Add('Connection failed: ' + Description);
end;

procedure TFormTelnet.ipsTelnet1DataIn(Sender: TObject; Text: string;
  TextB: TArray<System.Byte>);
begin
  Memo1.SelStart := Length(Memo1.Text);
  Memo1.SelText := (Text);
  Memo1.SetFocus;
end;

procedure TFormTelnet.ipsTelnet1Disconnected(Sender: TObject;
  StatusCode: Integer; const Description: string);
begin
	Memo2.Lines.Add('Disconnected: ' + Description);
end;

procedure TFormTelnet.ipsTelnet1Do(Sender: TObject; OptionCode: Integer);
begin
	Memo2.Lines.Add('SERVER: DO OPTION ' + IntToStr(OptionCode));
{	if OptionCode = 24 then
    Telnet1.WillOption := OptionCode
  else}
        ipsTelnet1.SendWontOption(OptionCode);
	Memo2.Lines.Add('CLIENT: WONT OPTION ' + IntToStr(OptionCode));
end;

procedure TFormTelnet.ipsTelnet1Dont(Sender: TObject; OptionCode: Integer);
begin
	Memo2.Lines.Add('SERVER: DON''T OPTION ' + IntToStr(OptionCode));
end;

procedure TFormTelnet.ipsTelnet1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject,
  CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormTelnet.ipsTelnet1SubOption(Sender: TObject; SubOption: string;
  SubOptionB: TArray<System.Byte>);
var
  CommandString: string;
begin
	Memo2.Lines.Add('SERVER: DO SUBOPTION ' + SubOption);
  CommandString := char(24) + char(0) + 'vt100';
  ipsTelnet1.SendDoSubOption(TEncoding.UTF8.GetBytes(CommandString));
end;

procedure TFormTelnet.ipsTelnet1Will(Sender: TObject; OptionCode: Integer);
begin
	Memo2.Lines.Add('SERVER: WILL OPTION ' + IntToStr(OptionCode));
end;

procedure TFormTelnet.ipsTelnet1Wont(Sender: TObject; OptionCode: Integer);
begin
	Memo2.Lines.Add('SERVER: WON''T OPTION ' + IntToStr(OptionCode));
end;

procedure TFormTelnet.Memo1KeyPress(Sender: TObject; var Key: Char);
begin
    {send the character}
    ipsTelnet1.SendText(Key);

    {add a line feed after carriage return}
    If Key = #13 Then ipsTelnet1.SendText(#10);

    {do not allow local echo}
    Key := #0;

end;

end.


