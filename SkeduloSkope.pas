PROGRAM SkeduloSkope; // Massively multi-threaded you say ??  Well ... maybe.
                     // Run the program without parameters to get a write-up.
                    // This is BETA software, a hack, more for the utility of
                   // seeing the scheduler in action than for a "nice" program
                  // Use at your own risk,
                 // though it is very unlikely it will damage your system.
                // Sant Coetzer 2002 Apr 27 V0.7á
{&PMTYPE VIO}

USES CRT,OS2Base,VPUtils,SysUtils;

CONST
  CurrentThread=0;

TYPE ThreadControl=RECORD
                     Attr:BYTE;              // Char Attribs (color), x & y posn
                     PC,PD,tX,tY,MC:INTEGER  // PriorityClass,PriorityDelta...MaxCount
                   END;
     TCArray = ARRAY[0..1023]OF ThreadControl;

VAR
  VIODat : VioModeInfo;
  P : POINTER;
  TC : TCArray;
  evGo,evSched : hEv;
  Master,doLabel:BOOLEAN;
  Threads,LegendLine,BG,FG,Resolution,CurThread,rc,MaxCountAbs,MaxCountDyn,
  MaxCount,Line,Priority,X,Tid,PriorityClass,PriorityDelta,Color,Tn:INTEGER;
  ReadyCount : INTEGER=0;
  prtyC:ARRAY[1..4]OF CHAR=('I','R','T','F');

  PROCEDURE WaitKey(Col:INTEGER); // Prompt for a keypress to continue
  VAR Prompt:STRING; X,I:INTEGER;
  BEGIN
    INC(Color);  I:=Color;
    Prompt[0]:=#52;  X:=53;
    Prompt:=Prompt+'Press a key to continue ...';
    TextBackGround(Col);
    REPEAT
      IF (I MOD 8)=Col THEN INC(I);
      TextColor({Blink+}I MOD 8);   HighVideo;  INC(I);
      GotoXY(X,2);  Write(Prompt[X]);  INC(X);  IF X>79 THEN X:=53;
      DOSSleep(1);
    UNTIL KeyPressed;  ReadKey
  END; // WaitKey

PROCEDURE Schpiel;  // Tell the user what it's for
BEGIN
  HideCursor;
  TextBackGround(White);  TextColor(Blue);  WriteLn;  HideCursor;
  WriteLn('                    SkeduloSkope  -  OS/2 Scheduler exposition'#10);
  Write('  This program exposes the OS/2 scheduler''s activities to some degree.');
  TextBackGround(RED);  TextColor(WHITE);  WriteLn(#10);
  WriteLn('  Some phases of this HOGs the CPU, so do not run it while you are also');
  WriteLn('  running critical operations depending on regular access to the CPU.');
  WriteLn('  (No need to ^C here, this run will only show instructions and info.)');
  WriteLn('  Note: OS/2 will NOT honour PAUSE or ^C or even Watchcat or C-A-D');
  Write  ('        keys (at least not any time soon) when running TC threads.');
  TextBackGround(GREEN);  TextColor(WHITE);  WriteLn(#10);
  WriteLn('  The OS/2 scheduler recognizes 128 priorities, divided into 4 classes, being:');
  WriteLn('  Idle(I)=1, Regular(R)=2, ForegroundServer(F)=4 and TimeCritical(T)=3.');
  WriteLn('  The numbers are out of sequence; probably because (F) was added later.'#10);
  WriteLn('  Each priority class is further qualified by a "delta".');
  WriteLn('  This is a number 0..31, the higher the better.');
  WriteLn('  In this program I write priorities as I19 for Class:Idle; Delta:19 &c.'#10);
  WriteLn('  In summary, CPU attention is equally shared between identical priorities;');
  WriteLn('  (T) threads run first, with no periodic priority boost given to any other;');
  WriteLn('  (F) threads follow, except when the app has focus; (then (F)=(R) exactly;)');
  WriteLn('  (R) threads follow and lastly, any (I) threads;');
  WriteLn('  When no (T) threads are "ready", and more than one (F)|(R) thread is "ready",');
  WriteLn('  a (miniscule) time slice will be given to (F)&(R) threads that have been');
  Write  ('  waiting longer than Config.SYS:MaxWait= seconds. (T)&(I) never get boosts.');
  WaitKey(White);  TextBackGround(BLUE);  TextColor(WHITE);  GotoXY(80,25);
  WriteLn('                                 Program operation.'#10#10);
  WriteLn('  Several threads will be started, each at a different priority; All that the');
  WriteLn('  threads do, is to count down from a maximum count that you can specify and');
  WriteLn('  to display each number on the console. Each counter on the screen is labeled');
  WriteLn('  with the priority at which it''s thread is running, so you can see which');
  WriteLn('  priorities get CPU attention.'#10);
  WriteLn('  The labels are constructed as follows: "Cdd:" where C is the priority class:');
  WriteLn('  I=Idle(1), R=Regular(2), T=TimeCritical(3) and F=ForegroundServer(4);');
  WriteLn('  dd is the "delta" (0..31). This is then followed by two or more counters.'#10);
  WriteLn('  The number of counters (and therefore threads) depend on the screen size.');
  WriteLn('  The program provides for 3 "resolutions", and it decides on which one to use');
  WriteLn('  depending on the screen size it finds itself running in.'#10);
  WriteLn('  I don''t know how to programmatically change the screen font, or I would have');
  WriteLn('  done it from inside the program. So you run "Mode" from the command prompt');
  WriteLn('  before running SkeduloSkope. The three screen sizes supported are: 80xLL,');
  WriteLn('  160xLL and 160xKK where LL IN [21..36] and KK IN [37..51].'#10);
  WriteLn('  So, if you run "MODE 160,37" (and then set a small enough font, and stretch');
  WriteLn('  the VIO window until the scroll bars disappear), you''ll get the best display.');
  WaitKey(Blue);  TextBackGround(BLUE);  TextColor(WHITE);  GotoXY(80,25); WriteLn;
  WriteLn('                       Program operation and parameters.'#10);
  WriteLn(' To see the threads racing one another on your screen, first prepare your');
  WriteLn(' environment by running, say, "Mode 160,21"; then setting a font size that will');
  WriteLn(' make the window fit on your screen; and stretching the VIO window to get rid');
  WriteLn(' of the scroll bars, and then run SkeduloSkope again. Running in the 80x25');
  WriteLn(' window will produce a less informative but still enlightening display.'#10);
  WriteLn(' There are two parameters: MaxCountAbs and MaxCountDyn; The first specifies');
  WriteLn(' the start count for (T)&(I) threads, (Abs) because scheduling for these are');
  WriteLn(' absolute; i.e. no priority boosts. The second specifies the start count for');
  WriteLn(' the (F)&(R) threads (Dyn) because scheduling here is affected by priority');
  WriteLn(' boosts which one can only see if the threads run for many seconds.'#10);
  WriteLn(' Using too large a value for MaxCountAbs produces a "boring" display, but too');
  WriteLn(' small a value for MaxCountDyn makes you miss out on seeing the priority boosts.');
  WriteLn(' The numbers you use very much depend on your CPU speed.');
  WriteLn(' A ggod starting point for a 100Mhz 486 would be SkeduloSkope 1000 2000');
  WriteLn(' and for a 1GHz Athlon, SkeduloSkope 5000 10000.');
  WriteLn(' For a later run, you may make the Abs count very much smaller than the Dyn one.');
  WriteLn(' Caveat: Check your Config.SYS:THREADS=NNN. NNN must be large (>1024).');
  Write  ('         While your''e at it, set MAXWAIT=1 unless your machine is a slow 386');
  Halt($FF);
END;

FUNCTION WatchDog(P:POINTER):INTEGER; // This thing just halts the program after a timeout
VAR ev : hev;
BEGIN
  DosCreateEventSem(nil,ev,0,FALSE);  DosWaitEventSem(ev,60000);  // 1 Minute
  DOSBeep(4567,750);
  Halt($F0);
END;

FUNCTION Thread(P:INTEGER):INTEGER;cdecl; // This gets dispatched with different P's
VAR S : String[4];         T:INTEGER;
BEGIN
  WITH TC[P] DO BEGIN
    IF Resolution=0 THEN Str(MC MOD 99:2,S) ELSE S:=Int2Hex(MC,4);
{ "Resolution=2" means we're running in 160x37+ mode, i.e. 6 threads/priority level
  The program goes Zombie if certain threads write to the screen in this mode
  and therefore the masses of IF statements below to skip writing for those
  (experimentally determined) priority levels. If you can make this proggie
  run without those IFs (and/or without the DOSSleep(1); below) do let me know
  how you do it <mailto:sant@OS2World.com>    }
    IF Resolution=2 THEN  //Problems !!
      BEGIN
        IF NOT(
           ((PC=1)AND((PD=26)OR(PD=25)OR(PD=24)OR(PD=23)OR(PD=21)OR(PD=19)))OR
           ((PC=2)AND((PD=02)OR(PD=01)OR(PD=00))))THEN
         VIOWrtCharStrAtt(@S[1],Ord(S[0]),tY,tX,Attr,0);
      END
    ELSE VIOWrtCharStrAtt(@S[1],Ord(S[0]),tY,tX,Attr,0);

    DOSSetPriority(prtys_Thread,PC,PD,CurrentThread);
    INC(ReadyCount); // Count this thread as "Ready"
//    DOSWaitEventSem(evSched,sem_Indefinite_Wait); // Wait for all other threads to be made "Ready to run"
    DOSWaitEventSem(evGo,sem_Indefinite_Wait);  // Wait for all other programs to be made "Ready to run"
    DOSSleep(1); // Kludge! but take this out and it HANGS !
    REPEAT
        CurThread:=P;  // For T1 display
        IF (Resolution=2)AND
           ((PC=1)AND( (PD=26)OR(PD=25)OR(PD=24)OR(PD=21)OR(PD=19)) OR
           ((PC=2)AND( (PD=02)OR(PD=01)OR(PD=00))))THEN // Nothing !
        ELSE
          BEGIN
            IF MC=0 THEN Attr:=$33
              ELSE IF Resolution=0 THEN Str(MC MOD 99:2,S) ELSE S:=Int2Hex(MC,4);
            DOSEnterCritSec; // Don't disturb while writing to the screen
            VIOWrtCharStrAtt(@S[1],Ord(S[0]),tY,tX,Attr,0);
            DOSExitCritSec;
          END;
      DEC(MC);
    UNTIL MC<0;
  END; // WITH
  DEC(ReadyCount); // Count this thread as completed
END; // Thread

PROCEDURE LaunchThreads;
VAR rc:INTEGER;
  PROCEDURE StartThread;
  BEGIN
    TC[Tn].Attr:=(Priority AND $7) SHL 4;
    IF (Priority AND $7)=Lightgray THEN TC[Tn].Attr:=TC[Tn].Attr OR Blue
                                   ELSE TC[Tn].Attr:=TC[Tn].Attr OR Lightgray;
    TC[Tn].PC:=PriorityClass;  TC[Tn].PD:=PriorityDelta;
    IF Priority IN[0..31,64..95]THEN TC[Tn].MC:=MaxCountAbs ELSE TC[Tn].MC:=MaxCountDyn;
    TextBackGround(TC[Tn].Attr SHR 4);  TextColor(TC[Tn].Attr AND $7);
    IF doLabel THEN
      BEGIN
        GoToXY(X,Line);  INC(X,4);
        Write(prtyC[PriorityClass]+Int2StrZ(PriorityDelta,2)+':');
      END;
    TC[Tn].tX:=Pred(X);  TC[Tn].tY:=Pred(Line);
    rc:=DOSCreateThread(Tid,Thread,Tn,create_Ready+Stack_Committed,16384);
    IF rc=Error_Max_Thrds_Reached THEN
      BEGIN
        ClrScr;  WriteLn('Not enough threads available in the system.');
        WriteLn(#10'See accompanying Readme about changing your Config.SYS to increase threads.');
        Halt($D0);
      END;
    IF rc<>No_Error THEN
      BEGIN
        ClrScr;  WriteLn('DOSCreateThread returned ',rc);
        Halt($D1);
      END;
    DEC(Tn);  // Next thread
  END; //StartThread

BEGIN
//BeginThread(nil,8192,WatchDog,P,0,Tid);  // Halt the program if it hangs
  Tn:=Pred(Threads);  // Thread number
  FOR Priority:=127 DOWNTO 0 DO  // All defined priorities
    BEGIN
      doLabel:=TRUE;
      PriorityClass:=Succ(Priority SHR 5);  PriorityDelta:=Priority AND $1F;
      StartThread; // First thread for this priority - with label
      IF Resolution=2 THEN BEGIN DEC(X,4); INC(Line); StartThread; DEC(Line) END;
      doLabel:=FALSE;
      IF Resolution=0 THEN INC(X,3) ELSE INC(X,5);
      StartThread; // Second thread
      IF Resolution=2 THEN BEGIN INC(Line); StartThread; DEC(Line) END;
      IF Resolution=0 THEN
        BEGIN
          INC(X,3);
          IF X>80 THEN BEGIN DEC(X,80); INC(Line) END;
        END
      ELSE
        BEGIN
          INC(X,5);
          StartThread; // Third thread
          IF Resolution=2 THEN BEGIN INC(Line); StartThread; DEC(Line) END;
          INC(X,6);
          IF X>160 THEN BEGIN DEC(X,160); IF Resolution=2 THEN INC(Line,2) ELSE INC(Line) END;
        END;
      IF PriorityDelta=0 THEN
        BEGIN
          IF Resolution<>2 THEN
            IF Line=10 THEN Line:=0 ELSE IF Line= 5 THEN Line:=10 ELSE
          ELSE
            IF Line=18 THEN Line:=0 ELSE IF Line= 9 THEN Line:=18;
          DEC(PriorityClass); INC(Line)
        END;
      GotoXY(1,LegendLine+1);
      Write('PriorityClass = ',PriorityClass,', PriorityDelta = ',PriorityDelta:2,', Active Threads = ',ReadyCount,'    ');
    END;
  DOSPostEventSem(evSched);
END; // LaunchThreads

PROCEDURE UpdateReadyCountDisplay;
// This gives a very "approximate" display; there is no guarantee that this
// thread will run everytime one of the "worker threads" gets pre-empted
VAR Prev,Temp,L,F:INTEGER; S:STRING;
BEGIN
  CASE Resolution OF
   0: BEGIN L:= 64; F:=2 END;
   1: BEGIN L:=144; F:=3 END;
   2: BEGIN L:=144; F:=6 END;
  END;
  S[0]:=Chr(L); FillChar(S[1],L,' ');
  DEC(L,4);  Prev:=0;
  REPEAT
    DOSSleep(1);
    IF Prev<>CurThread THEN
      BEGIN
        DOSEnterCritSec;
        Prev:=CurThread;
        Temp:=INTEGER(@TC[CurThread].Attr);  // Let VP compute address in EAX
        ASM ROR BYTE[EAX],4 END; // Reverse display attributes
    // This reverses the colors of the "just pre-empted"(Probably) thread.
    // Enables one to spot re-schedules of the same thread (with a very fast eye)
        Move(S[6],S[1],L);
        S[Succ(L)]:=prtyC[TC[CurThread].PC];
        S[L+4]:=Chr(CurThread MOD F + $61);
{ For the non-assemblites: The following 4 lines converts the PD into ASCII and
  inserts it into the string. Assumption: VP uses EAX to compute the addresses.
  If you don't like that insert "MOV EAX,Temp" right after the "ASM"s }
        Temp:=INTEGER(@TC[CurThread].PD); // VP leaves address in EAX
        ASM MOV EAX,[EAX];  AAM 10;  ADD AX,'00'; XCHG AH,AL; PUSH AX END;
        Temp:=INTEGER(@S[L+2]); // Ditto
        ASM POP WORD[EAX] END;
        GotoXY(1,LegendLine);  Write(ReadyCount:3,' Threads, ',S,'  ');
        DOSExitCritSec;
      END
  UNTIL ReadyCount=0;
  GotoXY(1,LegendLine);
END; // UpdateReadyCountDisplay

BEGIN  // Main
  IF ParamCount<>2 THEN Schpiel;
  BG:=Cyan; FG:=White;  TextBackGround(BG); TextColor(FG);  ClrScr;
  LegendLine:=20;  Line:=6;  X:=1;  Master:=TRUE;  Color:=0;  CurThread:=0;
  MaxCountAbs:=StrToInt(ParamStr(1));
  MaxCountDyn:=StrToInt(ParamStr(2));
  VIODat.cb:=SizeOf(VioModeInfo);    VIOGetMode(VIODat,0);
  IF VIODat.Col<160 THEN Resolution:=0 ELSE Resolution:=1+ORD(VIODat.Row>36);
  IF Resolution=0 THEN       Threads:=256;
  IF Resolution=1 THEN       Threads:=384;
  IF Resolution=2 THEN BEGIN Threads:=768; LegendLine:=36; Line:=10 END;
  GotoXY(1,LegendLine); Write('Using ',Threads,' threads.');

  rc:=DosCreateEventSem(Nil,evSched,0,FALSE);
  IF rc<>No_Error THEN BEGIN Writeln('DosCreateEventSem error code = ',rc); Halt($FC) END;
  rc:=DosCreateEventSem('\SEM32\SkeduloSkopeGo',evGo,0,FALSE);
  IF rc=Error_Duplicate_Name THEN
    BEGIN               // Sem already there
      rc:=DosOpenEventSem('\SEM32\SkeduloSkopeGo',evGo);
      IF rc<>No_Error THEN BEGIN Writeln('DosOpenEventSem error code = ',rc); Halt($FE) END;
      Master:=FALSE;
    END;
  LaunchThreads;  TextBackGround(BG); TextColor(FG);  GotoXY(1,LegendLine);
  DOSSetPriority(prtys_Thread,3,31,CurrentThread); // For UpdateReadyCountDisplay;
  IF Master THEN
    BEGIN
      WriteLn(Threads,' threads "ready to go". If you are running another/more instance(s) of');
      Write('SkeduloSkope, now is the time to get them ready. <Enter> starts the whole lot.');
      ReadKey;
      IF ReadyCount<Threads THEN DOSSleep(33);
      DOSPostEventSem(evGo);
    END
  ELSE
    BEGIN
      WriteLn(Threads,' threads "ready to go". Waiting on master to let us run ...');
      rc:=DosQueryEventSem(evGo,Tn);  // See if already posted
      IF Tn=0 THEN DOSWaitEventSem(evGo,sem_Indefinite_Wait) ELSE Halt($E0);
    END;
//TextBackGround(BG);  TextColor(FG);
  GotoXY(1,LegendLine+1);  ClrEOL;   GotoXY(1,LegendLine);  ClrEOL;
  UpdateReadyCountDisplay;
END.
