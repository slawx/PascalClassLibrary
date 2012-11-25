unit umathgame;

{$mode objfpc}{$H+}

interface

uses
  Forms, ExtCtrls, BGRAVirtualScreen, BGRALabel, BGRAImageButton, BGRABitmap;

type

  { TfrmJuego }

  TfrmJuego = class(TForm)
    btnIgualdad: TBGRAImageButton;
    btnIniciar: TBGRAImageButton;
    btnOperador: TBGRAImageButton;
    btnResultado: TBGRAImageButton;
    btnVariable1: TBGRAImageButton;
    btnVariable2: TBGRAImageButton;
    lblAciertos: TBGRALabel;
    lblIntentos: TBGRALabel;
    n_aciertos: TBGRALabel;
    n_intentos: TBGRALabel;
    n_tiempo: TBGRALabel;
    Opcion1: TBGRAImageButton;
    Opcion2: TBGRAImageButton;
    Opcion3: TBGRAImageButton;
    Opcion4: TBGRAImageButton;
    Opcion5: TBGRAImageButton;
    temporizador: TTimer;
    vsBarraEcuacion: TBGRAVirtualScreen;
    vsBarraOpciones: TBGRAVirtualScreen;
    vsFondo: TBGRAVirtualScreen;
    procedure btnIniciar_Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Opcion1Click(Sender: TObject);
    procedure Opcion2Click(Sender: TObject);
    procedure Opcion3Click(Sender: TObject);
    procedure Opcion4Click(Sender: TObject);
    procedure Opcion5Click(Sender: TObject);
    procedure temporizadorTimer(Sender: TObject);
    procedure vsBarraEcuacionRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure vsBarraOpcionesRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure vsFondoRedraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    intentos, aciertos: integer;
    opcioncorrecta: integer;
    tiempo: integer;
    var1, var2, resultado, operador: integer;
    procedure Chequear(valor: integer);
    procedure Generar;
  end;

procedure OperacionAleatoria(var Avar1, Avar2, Aresultado, AOperador: integer);
function OperadorAString(Aoperador: integer): string;
procedure SumaAleatoria(var Avar1, Avar2, Aresultado: integer);
procedure RestaAleatoria(var Avar1, Avar2, Aresultado: integer);
procedure MultiplicacionAleatoria(var Avar1, Avar2, Aresultado: integer);
procedure DivisionAleatoria(var Avar1, Avar2, Aresultado: integer);

var
  frmJuego: TfrmJuego;

resourcestring
  {txtOperacionesMatematicas = 'Operaciones Matemáticas v0.2';
  txtJuegoTerminado = 'Juego Terminado';
  txtAciertos = 'Aciertos';
  txtIntentos = 'Intentos';
  txtEficacia = 'Eficacia';
  txtIniciar = 'Iniciar';}
  txtOperacionesMatematicas = 'Math Operations';
  txtJuegoTerminado = 'Game Over';
  txtAciertos = 'Hits';
  txtIntentos = 'Attempts';
  txtEficacia = 'Effectiveness';
  txtIniciar = 'Start';

implementation

uses
  Math, SysUtils, BGRASamples, Controls, Dialogs;

{$R *.lfm}

{ Random Procedures }

procedure OperacionAleatoria(var Avar1, Avar2, Aresultado, AOperador: integer);
var
  unrango: integer;
begin
  unrango := RandomRange(0, 4);
  AOperador := unrango;
  case unrango of
    0: SumaAleatoria(Avar1, Avar2, Aresultado);
    1: RestaAleatoria(Avar1, Avar2, Aresultado);
    2: MultiplicacionAleatoria(Avar1, Avar2, Aresultado);
    3: DivisionAleatoria(Avar1, Avar2, Aresultado);
  end;
end;

function OperadorAString(Aoperador: integer): string;
begin
  case Aoperador of
    0: Result := '+';
    1: Result := '-';
    2: Result := '*';
    3: Result := '/';
  end;
end;

procedure SumaAleatoria(var Avar1, Avar2, Aresultado: integer);
begin
  Avar1 := RandomRange(1, 50);
  Avar2 := RandomRange(1, 50);
  AResultado := Avar1 + Avar2;
end;

procedure RestaAleatoria(var Avar1, Avar2, Aresultado: integer);
begin
  Avar1 := RandomRange(1, 50);
  Avar2 := RandomRange(1, 50);
  AResultado := Avar1 - Avar2;
end;

procedure MultiplicacionAleatoria(var Avar1, Avar2, Aresultado: integer);
begin
  Avar1 := RandomRange(1, 9);
  Avar2 := RandomRange(1, 9);
  AResultado := Avar1 * Avar2;
end;

procedure DivisionAleatoria(var Avar1, Avar2, Aresultado: integer);
begin
  AResultado := RandomRange(1, 9);
  Avar2 := RandomRange(1, 9);
  Avar1 := Aresultado * Avar2;
end;

{ TfrmJuego }

procedure TfrmJuego.FormShow(Sender: TObject);
begin
  Generar;
end;

procedure TfrmJuego.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  temporizador.Enabled := False;

  tiempo := 0;
  n_tiempo.Caption := '00';
end;

procedure TfrmJuego.btnIniciar_Click(Sender: TObject);
begin
  intentos := 0;
  aciertos := 0;
  tiempo := 60;

  n_intentos.Caption := IntToStr(intentos);
  n_aciertos.Caption := IntToStr(aciertos);
  n_tiempo.Caption := IntToStr(tiempo);

  Generar;

  temporizador.Enabled := True;
end;

procedure TfrmJuego.temporizadorTimer(Sender: TObject);
var
  eficacia: integer;
begin
  tiempo := tiempo - 1;
  n_tiempo.Caption := IntToStr(tiempo);

  if tiempo = 0 then
  begin
    temporizador.Enabled := False;

    if aciertos <> 0 then
      eficacia := Trunc(aciertos / intentos * 100)
    else
      eficacia := 0;

    ShowMessage(
      txtJuegoTerminado + '.' + #10 + IntToStr(aciertos) + ' ' +
      txtAciertos + '.' + #10 + IntToStr(intentos) + ' ' + txtIntentos +
      '.' + #10 + IntToStr(eficacia) + '%' + ' ' + txtEficacia + '.');

    n_tiempo.Caption := '00';
  end;
end;

procedure TfrmJuego.Generar;

  procedure AleatorizarOpciones(a, b, rand2, opcioncorrecta: integer);
  var
    i, j: integer;
    losnumeros: array [0..50] of boolean;
    isok: boolean;
  begin

    for i := 0 to 50 do
    begin
      losnumeros[i] := False;
    end;

    for i := 0 to 4 do
    begin

      if i <> rand2 then
      begin

        repeat
          isok := False;

          j := RandomRange(a, b);

          if (not losnumeros[j]) and (j <> opcioncorrecta) then
          begin
            losnumeros[j] := True;
            isok := True;
          end
          else
            isok := False;

        until isok;

        vsBarraOpciones.Controls[i].Caption := IntToStr(j);
      end;

    end;
  end;

var
  rand1, rand2: integer;
begin
  Self.BeginFormUpdate;

  OperacionAleatoria(var1, var2, resultado, operador);

  btnVariable1.Caption := IntToStr(var1);
  btnOperador.Caption := OperadorAString(operador);
  btnVariable2.Caption := IntToStr(var2);
  btnResultado.Caption := IntToStr(Resultado);

  rand1 := RandomRange(0, 3);
  rand2 := RandomRange(0, 4);

  case rand1 of
    0:
    begin
      btnVariable1.Caption := '?';
      vsBarraOpciones.Controls[rand2].Caption := IntToStr(var1);
      opcioncorrecta := var1;
    end;
    1:
    begin
      btnVariable2.Caption := '?';
      vsBarraOpciones.Controls[rand2].Caption := IntToStr(var2);
      opcioncorrecta := var2;
    end;
    2:
    begin
      btnResultado.Caption := '?';
      vsBarraOpciones.Controls[rand2].Caption := IntToStr(resultado);
      opcioncorrecta := resultado;
    end;
  end;

  case operador of
    0: AleatorizarOpciones(1, 50, rand2, opcioncorrecta);
    1: AleatorizarOpciones(1, 50, rand2, opcioncorrecta);
    2: AleatorizarOpciones(1, 9, rand2, opcioncorrecta);
    3: AleatorizarOpciones(1, 9, rand2, opcioncorrecta);
  end;

  Self.EndFormUpdate;
end;

procedure TfrmJuego.Chequear(valor: integer);
begin
  if valor = opcioncorrecta then
  begin
    aciertos := aciertos + 1;
    intentos := intentos + 1;
    n_aciertos.Caption := IntToStr(aciertos);
    n_intentos.Caption := IntToStr(intentos);
  end
  else
  begin
    intentos := intentos + 1;
    n_intentos.Caption := IntToStr(intentos);
  end;
  Generar;
end;

{ Graphics }

procedure TfrmJuego.FormCreate(Sender: TObject);
begin
  Opcion1.BitmapLoadFromFile('android_14_normal.png');

  btnVariable1.Assign(Opcion1);
  btnOperador.Assign(Opcion1);
  btnVariable2.Assign(Opcion1);
  btnIgualdad.Assign(Opcion1);
  btnResultado.Assign(Opcion1);

  Opcion2.Assign(Opcion1);
  Opcion3.Assign(Opcion1);
  Opcion4.Assign(Opcion1);
  Opcion5.Assign(Opcion1);

  btnIniciar.Assign(Opcion1);

  Caption := txtOperacionesMatematicas;
  btnIniciar.Caption := txtIniciar;
  lblAciertos.Caption := txtAciertos;
  lblIntentos.Caption := txtIntentos;
end;

procedure TfrmJuego.vsBarraEcuacionRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawiOSElement(Bitmap, iOSBar);
end;

procedure TfrmJuego.vsBarraOpcionesRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawiOSToolBar(Bitmap, False);
end;

procedure TfrmJuego.vsFondoRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawiOSElement(Bitmap, iOSBackground);
end;

{ Option Buttons }

procedure TfrmJuego.Opcion1Click(Sender: TObject);
begin
  Chequear(StrToInt(Opcion1.Caption));
end;

procedure TfrmJuego.Opcion2Click(Sender: TObject);
begin
  Chequear(StrToInt(Opcion2.Caption));
end;

procedure TfrmJuego.Opcion3Click(Sender: TObject);
begin
  Chequear(StrToInt(Opcion3.Caption));
end;

procedure TfrmJuego.Opcion4Click(Sender: TObject);
begin
  Chequear(StrToInt(Opcion4.Caption));
end;

procedure TfrmJuego.Opcion5Click(Sender: TObject);
begin
  Chequear(StrToInt(Opcion5.Caption));
end;

end.

