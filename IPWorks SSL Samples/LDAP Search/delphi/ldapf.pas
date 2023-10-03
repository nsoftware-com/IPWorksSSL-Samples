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
unit ldapf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ipscore, ipstypes, ipsldap;

type
  TFormLdap = class(TForm)
    Memo1: TMemo;
    tServer: TEdit;
    Label2: TLabel;
    tFilter: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
    tBaseDN: TEdit;
    ipsLDAP1: TipsLDAP;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ipsLDAP1SearchComplete(Sender: TObject; MessageId: Integer;
      const DN: String; ResultCode: Integer; const Description: String);
    procedure ipsLDAP1SearchResult(Sender: TObject; MessageId: Integer;
      const DN: String);
    procedure ipsLDAP1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
      var Accept: Boolean);
    procedure ipsLDAP1SSLStatus(Sender: TObject; const Message: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormLdap: TFormLdap;

implementation

{$R *.DFM}

procedure TFormLdap.Button1Click(Sender: TObject);
begin
    Memo1.Clear;
    ipsLDAP1.ServerName := tServer.Text;
    
    //Note that AD, unlike other LDAP servers, requires a bind before a search will be allowed, so the demo would 
    //have to include a few extra lines of code if you are working with AD:
    //ipsLDAP1.DN := binddn;
    //ipsDAP1.Password := password;
    //ipsLDAP1.Bind();
    
    ipsLDAP1.SearchSizeLimit := 100; // at most 100 results
    ipsLDAP1.Timeout := 0; //set timeout > 0 to use in synch mode
    //set the required attributes
    ipsLDAP1.AttrCount := 1;
    ipsLDAP1.AttrType[0] := 'mail';  // put 'email' for addresses etc...
    ipsLDAP1.DN := tBaseDN.Text;

    Memo1.Lines.Add('sending search request...');

    ipsLDAP1.Search(tFilter.Text);

    Memo1.Lines.Add('waiting for server response...');
    Screen.Cursor := crHourGlass;
end;

procedure TFormLdap.Button2Click(Sender: TObject);
begin
    ipsLDAP1.Interrupt();
end;

procedure TFormLdap.ipsLDAP1SearchComplete(Sender: TObject;
  MessageId: Integer; const DN: String; ResultCode: Integer;
  const Description: String);
begin
    Memo1.Lines.Add('search complete: ' + IntToStr(ResultCode) + ' ' + Description);
    ipsLDAP1.Unbind();
    Screen.Cursor := crDefault;
end;

procedure TFormLdap.ipsLDAP1SearchResult(Sender: TObject; MessageId: Integer;
  const DN: String);
begin
    Memo1.Lines.Add(DN);
end;


procedure TFormLdap.ipsLDAP1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TArray<System.Byte>; const CertSubject, CertIssuer, Status: string;
  var Accept: Boolean);
begin
  Accept:=true;
end;

procedure TFormLdap.ipsLDAP1SSLStatus(Sender: TObject; const Message: string);
begin
     Memo1.Lines.Add(Message);
end;

end.


