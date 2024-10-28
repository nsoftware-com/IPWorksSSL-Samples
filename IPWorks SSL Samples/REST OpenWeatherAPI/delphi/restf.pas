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
unit restf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ipsrest, ipscore,
  ipstypes;

type
  TFormRest = class(TForm)
    DescriptionLabel: TLabel;
    AuthenticationLabel: TLabel;
    APIKeyEdit: TEdit;
    APIKeyLabel: TLabel;
    LocationLabel: TLabel;
    ipsREST1: TipsREST;
    CityLabel: TLabel;
    StateLabel: TLabel;
    CountryLabel: TLabel;
    CityEdit: TEdit;
    StateEdit: TEdit;
    CountryEdit: TEdit;
    LatitudeLabel: TLabel;
    LongitudeLabel: TLabel;
    LatitudeEdit: TEdit;
    LongitudeEdit: TEdit;
    WeatherLabel: TLabel;
    ConditionsLabel: TLabel;
    TempLabel: TLabel;
    PressureLabel: TLabel;
    HumidityLabel: TLabel;
    ConditionsEdit: TEdit;
    TempEdit: TEdit;
    PressureEdit: TEdit;
    HumidityEdit: TEdit;
    KelvinsLabel: TLabel;
    hPaLabel: TLabel;
    PercentLabel: TLabel;
    SearchButton: TButton;
    procedure SearchButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRest: TFormRest;

implementation

{$R *.dfm}

procedure TFormRest.SearchButtonClick(Sender: TObject);
var apiKey : string;
var address : string;
var lat : string;
var lon : string;
var mainWeather : string;
var temp : string;
var pressure : string;
var humidity : string;
begin
  try
    // We need to make two calls to get weather data.
    // You'll need an API key for authentication, which is free and available here: https://home.openweathermap.org/users/sign_up
    // The first geocodes an address (that is to get its latitude and longitude).
    // The second gets the weather at those coordinates.

    // These are needed to make the REST requests.
    apiKey := APIKeyEdit.Text;
    address := CityEdit.Text + ',' + StateEdit.Text + ',' + CountryEdit.Text;

    // Make sure the objects are in a clean state.
    ipsREST1.Reset();
    ipsREST1.Config('EncodeURL=true');

    // 1. Geocode. (Docs: https://openweathermap.org/api/geocoding-api)
    //  a. URL-encode the address.

    //  b. Make the REST request.
    ipsREST1.Get('https://api.openweathermap.org/geo/1.0/direct?q=' + address + '&appid=' + apiKey);

    //  c. Access the data in the REST response.
    ipsREST1.XPath := '/json/[1]/lat';
    lat := ipsREST1.XText;
    ipsREST1.XPath := '../lon';
    lon := ipsREST1.XText;
    LatitudeEdit.Text := lat;
    LongitudeEdit.Text := lon;

    // 2. Weather. (Docs: https://openweathermap.org/current)
    //  a. Make the REST request.
    ipsREST1.Get('https://api.openweathermap.org/data/2.5/weather?lat=' + lat + '&lon=' + lon + '&appid=' + apiKey);

    //  b. Access the data in the REST response.
    ipsREST1.XPath := '/json/weather/[1]/main';
    mainWeather := ipsREST1.XText;
    ipsREST1.XPath := '../description';
    mainWeather := mainWeather + ', ' + ipsREST1.XText;
    ConditionsEdit.Text := mainWeather;

    ipsREST1.XPath := '/json/main/temp';
    TempEdit.Text := ipsREST1.XText;

    ipsREST1.XPath := '../pressure';
    PressureEdit.Text := ipsREST1.XText;

    ipsREST1.XPath := '../humidity';
    HumidityEdit.Text := ipsREST1.XText;

    // As an exercise, try getting the rest of the values from the response.
  except
    on E : Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;
end.



