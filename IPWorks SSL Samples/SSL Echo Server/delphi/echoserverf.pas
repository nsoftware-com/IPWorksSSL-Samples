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
unit echoserverf;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Spin, ExtCtrls, Buttons,
  ipscore, ipstypes, ipssslserver;

type
  TFormEchoserver = class(TForm)
    lTrack: TListBox;
    Label1: TLabel;
    ButtonStart: TButton;
    ButtonStop: TButton;
    EditLocalPort: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    ipsSSLServer1: TipsSSLServer;
    procedure lTrackDblClick(Sender: TObject);
    procedure ipsSSLServer1Error(Sender: TObject; ConnectionId, ErrorCode: integer;
      const Description: String);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ipsSSLServer1Connected(Sender: TObject; ConnectionId,
      StatusCode: Integer; const Description: String);
    procedure ipsSSLServer1DataIn(Sender: TObject; ConnectionId: Integer;
      Text: string; TextB: TArray<System.Byte>; EOL: Boolean);
    procedure ipsSSLServer1Disconnected(Sender: TObject; ConnectionId,
      StatusCode: Integer; const Description: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEchoserver: TFormEchoserver;

implementation

{$R *.DFM}

procedure TFormEchoserver.lTrackDblClick(Sender: TObject);
begin
	lTrack.Clear;
end;

procedure TFormEchoserver.ipsSSLServer1Error(Sender: TObject; ConnectionId, ErrorCode: integer;
  const Description: String);
begin
	lTrack.Items.Add('!!Error: ' + IntToStr(ErrorCode) + Description);
end;

procedure TFormEchoserver.ButtonStopClick(Sender: TObject);
var i: integer;
begin
        ipsSSLServer1.Shutdown;
end;



procedure TFormEchoserver.ButtonStartClick(Sender: TObject);
begin


     ipsSSLServer1.LocalPort := strtoint(EditLocalPort.Text);

    {If you get an "address already in use" error in the next line
    then your system --for instance Windows NT--
    has already implemented this service.}
    ipsSSLServer1.SSLCertStoreType := TipsCertStoreTypes.cstPFXFile;
    ipsSSLServer1.SSLCertStore := '../../test.pfx';
    ipsSSLServer1.SSLCertStorePassword := 'test';
    ipsSSLServer1.SSLCertSubject := '*';
    ipsSSLServer1.StartListening();

    lTrack.Clear;
    lTrack.Items.Add('Host ' + ipsSSLServer1.LocalHost + ' ' + inttostr(ipsSSLServer1.LocalPort));

end;



procedure TFormEchoserver.ipsSSLServer1Connected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: String);
begin
    lTrack.Items.Add('Connected from ' + ipsSSLServer1.RemoteHost[ConnectionID] +
                    ' StatusCode: ' + IntToStr(StatusCode) +
                    ' ' + Description);
    ipsSSLServer1.EOL[ConnectionID] := #10;
end;

procedure TFormEchoserver.ipsSSLServer1DataIn(Sender: TObject; ConnectionId: Integer;
  Text: string; TextB: TArray<System.Byte>; EOL: Boolean);
begin
    lTrack.Items.Add('Heard: ' + Text);
    ipsSSLServer1.SendText(ConnectionID, Text + #10);
end;

procedure TFormEchoserver.ipsSSLServer1Disconnected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: String);
begin
    lTrack.Items.Add('disconnected');
end;

end.





