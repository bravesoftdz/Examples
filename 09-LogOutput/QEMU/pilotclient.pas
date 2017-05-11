unit PilotClient;
{$mode delphi}{$h+}

interface

implementation

uses
 QEMUVersatilePB,PlatformQemuVpb,VersatilePB,
// Crt,GlobalConfig,GlobalConst,GlobalTypes,Logging,Platform,Serial,
 StrUtils,SysUtils,Threads,Ultibo;

//var
// ThreadHandle:TThreadHandle;
// Pilot:PSerialDevice;

//procedure PilotSendRequest(Request:String);
//var
// FullRequest:String;
// Count:Cardinal;
//begin
// if Assigned(Pilot) then
//  begin
//   FullRequest:=Format('pilotrequest %s',[Request]);
//   LoggingOutput(FullRequest);
//   SerialDeviceWrite(Pilot,PChar(FullRequest),Length(FullRequest),SERIAL_FLAG_NONE,Count);
//  end;
//end;

//function TrapCtrlAltDel(Parameter:Pointer):PtrInt;
//var
// Key:Char;
//begin
// TrapCtrlAltDel:=0;
// Sleep(3 * 1000);
// Pilot:=nil;
//// ParseCommandLine;
// while True do
//  begin
//   if KeyPressed then
//    begin
//     Key:=ReadKey;
//     if Ord(Key) = 163 then
//      begin
////       PilotSendRequest('restartwithnewestkernel');
//      end;
//    end
//   else
//    begin
//     Sleep(100);
//    end;
//  end;
//end;

//procedure SetPilot(SerialDeviceName:String);
//begin
// Pilot:=SerialDeviceFindByName(SerialDeviceName);
// if Assigned(Pilot) then
//  begin
//   if SerialDeviceOpen(Pilot,9600,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0) <> ERROR_SUCCESS then
//    begin
//     Pilot:=nil;
//    end;
//  end;
//end;

//procedure ParseCommandLine;
//var
// I,Start:Cardinal;
// Param:String;
//begin
// for I:=0 to ParamCount do
//  begin
//   Param:=ParamStr(I);
//   if AnsiStartsStr('pilotrequestsserialdevice=',Param) then
//    begin
//     Start:=PosEx('=',Param);
//     SetPilot(MidStr(Param,Start + 1,Length(Param) - Start));
//    end;
//  end;
//end;

initialization
// ThreadHandle:=BeginThread(@TrapCtrlAltdel,nil,ThreadHandle,THREAD_STACK_DEFAULT_SIZE);
end.
