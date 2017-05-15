unit PilotCtrlAltDel;
{$mode delphi}{$h+}

interface
procedure PilotCtrlAltDelReceived;
procedure PilotDoPollCtrlAltDel;
procedure PilotLog(S:String);

implementation

uses
 QEMUVersatilePB,PlatformQemuVpb,VersatilePB,
 Console,GlobalConfig,GlobalConst,GlobalTypes,Logging,Platform,Serial,
 StrUtils,SysUtils,Threads,Ultibo;

var
 ThreadHandle:TThreadHandle;
 Pilot:PSerialDevice;

procedure PilotWriteLn(Line:String);
var
 Count:Cardinal;
 Status:LongWord;
begin
 Status:=SerialDeviceWrite(Pilot,PChar(Line + #13 + #10),Length(Line) + 2,SERIAL_FLAG_NONE,Count);
 if Status <> ERROR_SUCCESS then
  LoggingOutput(Format('serial write error %d',[Status]));
end;

procedure PilotSendRequest(Request:String);
begin
 if Assigned(Pilot) then
  begin
   LoggingOutput(Request);
   PilotWriteLn(Request);
  end;
end;

procedure PilotLog(S:String);
begin
 PilotSendRequest(S);
end;

procedure PilotCtrlAltDelReceived;
begin
 PilotSendRequest('ctrlaltdel');
end;

procedure SetPilot(SerialDeviceName:String);
var
 Status:LongWord;
begin
 Pilot:=SerialDeviceFindByName(SerialDeviceName);
 if Assigned(Pilot) then
  begin
   Status:=SerialDeviceOpen(Pilot,9600,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
   if Status <> ERROR_SUCCESS then
    begin
     LoggingOutput(Format('serial open error %d',[Status]));
     Pilot:=nil;
    end;
  end;
end;

procedure ParseCommandLine;
var
 I,Start:Cardinal;
 Param:String;
begin
 for I:=0 to ParamCount do
  begin
   Param:=ParamStr(I);
   LoggingOutput(Format('Param %d = %s',[I,Param]));
   if AnsiStartsStr('pilot=',Param) then
    begin
     Start:=PosEx('=',Param);
     SetPilot(MidStr(Param,Start + 1,Length(Param) - Start));
    end;
  end;
end;

function TrapCtrlAltDel(Parameter:Pointer):PtrInt;
var
 Key:Char;
begin
 TrapCtrlAltDel:=0;
 Sleep(7 * 1000);
 LoggingOutput('');
 LoggingOutput('PilotClient poll starting');
 while True do
  begin
   if ConsoleKeyPressed then
    begin
     Key:=ConsoleReadKey;
     LoggingOutput(Format('key %s <%d>',[Key,Ord(Key)]));
     if (Ord(Key) = 163) or (Key = 'q') then
      begin
       PilotSendRequest('ctrlaltdel');
      end;
    end
   else
    begin
     Sleep(100);
    end;
  end;
end;

procedure PilotDoPollCtrlAltDel;
begin
 ThreadHandle:=BeginThread(@TrapCtrlAltdel,nil,ThreadHandle,THREAD_STACK_DEFAULT_SIZE);
end;

initialization
 Pilot:=nil;
 ParseCommandLine;
end.
