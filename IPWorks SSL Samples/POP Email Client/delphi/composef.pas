unit composef;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type
  TFormCompose = class(TForm)
    OpenDialog1: TOpenDialog;
    MemoMessage: TMemo;
    Label2: TLabel;
    EditSubject: TEdit;
    Label3: TLabel;
    EditCc: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    EditTo: TEdit;
    EditFrom: TEdit;
    Label6: TLabel;
    ButtonSend: TButton;
    ButtonCancel: TButton;

  private
    { Private declarations }

  public
    { Public declarations }
    constructor Create( Owner: TComponent ); override;

  end;

var
  FormCompose: TFormCompose;

implementation

{$R *.DFM}

//---------------------------------------------------------------------------
constructor TFormCompose.Create( Owner: TComponent );
begin
   inherited Create(Owner);
end;

end.

