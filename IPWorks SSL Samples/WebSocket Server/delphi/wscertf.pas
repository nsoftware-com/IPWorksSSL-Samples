unit wscertf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ipscertmgr, ipscore, ipstypes;

type
  TFormWscert = class(TForm)
    txtInfo: TMemo;
    lCerts: TListBox;
    bSelectCert: TButton;
    cbStores: TListBox;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    ipsCertMgr1: TipsCertMgr;
    procedure FormCreate(Sender: TObject);
    procedure bSelectCertClick(Sender: TObject);
    procedure cbStoresClick(Sender: TObject);
    procedure lCertsClick(Sender: TObject);
    procedure ipsCertMgr1StoreList(Sender: TObject; const CertStore: string);
    procedure ipsCertMgr1CertList(Sender: TObject; CertEncoded: string;
      CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer,
      CertSerialNumber: string; HasPrivateKey: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWscert: TFormWscert;
  CertHandle: integer;

implementation

{$R *.dfm}

procedure TFormWscert.FormCreate(Sender: TObject);
begin
  CertHandle := 0;
  ipsCertMgr1.ListCertificateStores;
  if cbStores.Items.Count > 0 then
    begin
      cbStores.ItemIndex := 0;
      cbStoresClick(sender);
    end;

end;

procedure TFormWscert.bSelectCertClick(Sender: TObject);
begin
    FormWscert.Close;
end;

procedure TFormWscert.cbStoresClick(Sender: TObject);
begin
  lCerts.Clear;
  txtInfo.Text := '';
  ipsCertMgr1.CertStore := cbStores.Items.Strings[cbStores.ItemIndex];
  ipsCertMgr1.ListStoreCertificates;
  if (lCerts.Items.Count > 0) then
    begin
      lCerts.ItemIndex := 0;
      lCertsClick(Sender);
      bSelectCert.Enabled := True;
    end;
end;

procedure TFormWscert.lCertsClick(Sender: TObject);
var x: string;
begin
    txtInfo.Text := '';
    x :=lCerts.Items.Strings[lCerts.ItemIndex];
    ipsCertMgr1.CertSubject := x;
    ipsCertMgr1.CertSubject := lCerts.Items.Strings[lCerts.ItemIndex];
    txtInfo.text := txtInfo.text + 'Issuer: ' + ipsCertMgr1.CertIssuer + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Subject: ' + ipsCertMgr1.CertSubject + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Version: ' + ipsCertMgr1.CertVersion + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Serial Number: ' + ipsCertMgr1.CertSerialNumber + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Signature Algorithm: ' + ipsCertMgr1.CertSignatureAlgorithm + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Effective Date: ' + ipsCertMgr1.CertEffectiveDate + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Expiration Date: ' + ipsCertMgr1.CertExpirationDate + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Public Key Algorithm: ' + ipsCertMgr1.CertPublicKeyAlgorithm + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Public Key Length: ' + inttostr(ipsCertMgr1.CertPublicKeyLength) + Chr(13) + Chr(10);
    txtInfo.text := txtInfo.text + 'Public Key: ' + ipsCertMgr1.CertPublicKey + Chr(13) + Chr(10);
end;

procedure TFormWscert.ipsCertMgr1CertList(Sender: TObject; CertEncoded: string;
  CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer,
  CertSerialNumber: string; HasPrivateKey: Boolean);
begin
  lCerts.Items.Add(CertSubject);
end;

procedure TFormWscert.ipsCertMgr1StoreList(Sender: TObject;
  const CertStore: string);
begin
    cbStores.Items.Add(CertStore);
end;

end.
