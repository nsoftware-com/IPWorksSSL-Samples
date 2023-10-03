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
unit wsserverf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, wscertf, ipscore, ipstypes,
  ipswsserver, ipscertmgr;

type
  TFormWSServer = class(TForm)
    Label1: TLabel;
    txtPort: TEdit;
    btnStart: TButton;
    btnStop: TButton;
    txtLog: TMemo;
    Label2: TLabel;
    ipsWSServer1: TipsWSServer;
    Button1: TButton;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure WSServer1Error(Sender: TObject; ConnectionId, ErrorCode: Integer;
      const Description: string);
    procedure WSServer1Connected(Sender: TObject; ConnectionId: Integer);
    procedure WSServer1Disconnected(Sender: TObject; ConnectionId,
      StatusCode: Integer; const Description: string);
    procedure FormCreate(Sender: TObject);
    procedure WSServer1DataIn(Sender: TObject; ConnectionId,
      DataFormat: Integer; Text: string; TextB: TArray<System.Byte>;
      EOM: Boolean; EOL: Boolean);
    procedure WSServer1SSLClientAuthentication(Sender: TObject;
      ConnectionId: Integer; CertEncoded: string;
      CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer,
      Status: string; var Accept: Boolean);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWSServer: TFormWSServer;

implementation

{$R *.dfm}

procedure TFormWSServer.OnIdle(Sender: TObject; var Done: Boolean);
begin
  Done:=False;
  ipsWSServer1.DoEvents;
end;

procedure TFormWSServer.btnStartClick(Sender: TObject);
begin

  ipsWSServer1.LocalPort := StrToInt(txtPort.Text);
  ipsWSServer1.StartListening;
  btnStart.Enabled := False;
  btnStop.Enabled := True;

end;

procedure TFormWSServer.btnStopClick(Sender: TObject);
begin
  ipsWSServer1.Listening := False;
  btnStart.Enabled := True;
  btnStop.Enabled := False;
end;

procedure TFormWSServer.Button1Click(Sender: TObject);
begin
  FormWscert.ShowModal;
      if not (FormWscert.ipsCertMgr1.CertSubject = '') then
        begin
          ipsWSServer1.SSLCertSubject := FormWscert.ipsCertMgr1.CertSubject;
          ipsWSServer1.SSLCertStore := FormWscert.ipsCertMgr1.CertStore;
          ipsWSServer1.LocalPort := StrToInt(txtPort.Text);
        end;
      FormWscert.Close;
end;

procedure TFormWSServer.FormCreate(Sender: TObject);
begin
  Application.OnIdle := OnIdle;
end;

procedure TFormWSServer.WSServer1Error(Sender: TObject; ConnectionId,
  ErrorCode: Integer; const Description: string);
begin
  txtLog.Text := txtLog.Text + 'Error ' + IntToStr(ErrorCode) + Description;
end;

procedure TFormWSServer.WSServer1SSLClientAuthentication(
  Sender: TObject; ConnectionId: Integer; CertEncoded: string;
  CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer,
  Status: string; var Accept: Boolean);
begin
Accept := True;
end;

procedure TFormWSServer.WSServer1Connected(Sender: TObject;
  ConnectionId: Integer);
begin
    //we are connected
    txtLog.Lines.Add('Connected with ' + ipsWSServer1.WSConnectionRemoteHost[ConnectionId]);
    //send a welcome
    ipsWSServer1.WSConnectionDataToSend[ConnectionId] := 'Connected to Echo Server!';
end;

procedure TFormWSServer.WSServer1DataIn(Sender: TObject;
  ConnectionId, DataFormat: Integer; Text: string; TextB: TArray<System.Byte>;
  EOM: Boolean; EOL: Boolean);
begin
    // output what was heard
    txtLog.Lines.Add('Echoing "' + Text + '" to ' + ipsWSServer1.WSConnectionRemoteHost[ConnectionId]);
    // and echo it back
    ipsWSServer1.WSConnectionDataToSend[ConnectionId] := Text;
end;

procedure TFormWSServer.WSServer1Disconnected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: string);
begin
    txtLog.Lines.Add('Disconnected (' + Description + ')');
end;

end.



