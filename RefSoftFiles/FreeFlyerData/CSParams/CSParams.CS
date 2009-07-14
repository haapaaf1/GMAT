[COMMAND1]
Command1=Enabled\/SPACE\/FreeForm\/SPACE\/Freeform Script\/SPACE\/FreeformScript2\/SPACE\/\/SPACE\/
[COMMAND2]
Command1=Enabled\/SPACE\/While\/SPACE\/Expanded\/SPACE\/ISS.ElapsedDays <= 1\/Delimit\/Object Parameter\/Operator\/Number\/SPACE\/\/SPACE\/
[COMMAND3]
Command1=Enabled\/SPACE\/Report\/SPACE\/ISS.ElapsedDays<15,9>, ISS.Position<15,9>, ISS.Velocity<15,9>, ISS.VMag<15,9>, ISS.RA<15,9>, ISS.Momentum<15,9>, ISS.W<15,9>, ISS.DEC<15,9>, ISS.DEC<15,9>, ISS.I<15,9>, ISS.RA<15,9>, ISS.RAAN<15,9>\/Delimit\/ISS.ElapsedDays<15,9>\/Delimit\/ISS.Position<15,9>\/Delimit\/ISS.Velocity<15,9>\/Delimit\/ISS.VMag<15,9>\/Delimit\/ISS.RA<15,9>\/Delimit\/ISS.Momentum<15,9>\/Delimit\/ISS.W<15,9>\/Delimit\/ISS.DEC<15,9>\/Delimit\/ISS.DEC<15,9>\/Delimit\/ISS.I<15,9>\/Delimit\/ISS.RA<15,9>\/Delimit\/ISS.RAAN<15,9>\/Delimit\/Report Headers\/Delimit\/Include Headers\/SPACE\/CSParams_FF_ISS_2Body_EarthMJ2000Eq.report\/SPACE\/\/SPACE\/
[COMMAND4]
Command1=Enabled\/SPACE\/For\/SPACE\/Expanded\/SPACE\/I = 1 to 120 step 1\/Delimit\/Variable\/Operator\/Number\/Operator\/Number\/Operator\/Number\/SPACE\/\/SPACE\/
[FORMAT_SECTION]
Position=29
ProjectDir=C:\GMAT_results\TruthFiles\FF\CSParams\
ProjectType=1
NumLines=11
0CWObjectList0=228
0CWObjectListLast0=199
0CWObjectList1=228
0CWObjectListLast1=199
0CWObjectList2=-1
0CWObjectListLast2=-1
0CWObjectList3=530
0CWObjectListLast3=460
1CWObjectList0=225
1CWObjectListLast0=199
1CWObjectList1=225
1CWObjectListLast1=199
1CWObjectList2=-1
1CWObjectListLast2=-1
1CWObjectList3=520
1CWObjectListLast3=460
2CWObjectList0=228
2CWObjectListLast0=199
2CWObjectList1=228
2CWObjectListLast1=199
2CWObjectList2=-1
2CWObjectListLast2=-1
2CWObjectList3=530
2CWObjectListLast3=460
3CWObjectList0=231
3CWObjectListLast0=201
3CWObjectList1=-1
3CWObjectListLast1=-1
3CWObjectList2=154
3CWObjectListLast2=134
3CWObjectList3=587
3CWObjectListLast3=509
4CWObjectList0=83
4CWObjectListLast0=72
4CWObjectList1=222
4CWObjectListLast1=193
4CWObjectList2=333
4CWObjectListLast2=289
4CWObjectList3=333
4CWObjectListLast3=289
CWMDS0=69
CWMDSLast0=61
CWMDS1=335
CWMDSLast1=295
CWMDS2=355
CWMDSLast2=312
CWMDS3=169
CWMDSLast3=148
ActivePage=1
MDSRecord=1
[COMMAND5]
Command1=Enabled\/SPACE\/Step\/SPACE\/ISS\/SPACE\/\/SPACE\/
[COMMAND6]
Command1=Enabled\/SPACE\/End\/SPACE\/\/SPACE\/
[COMMAND7]
Command1=Enabled\/SPACE\/End\/SPACE\/\/SPACE\/
[COMMAND8]
Command1=Disabled\/SPACE\/If\/SPACE\/Expanded\/SPACE\/CountTime = 0\/Delimit\/Variable\/Operator\/Number\/SPACE\/\/SPACE\/
[COMMAND9]
Command1=Disabled\/SPACE\/Report\/SPACE\/ISS.ElapsedDays<15,9>, ISS.X<15,9>, ISS.Y<15,9>, ISS.Z<15,9>, ISS.VX<15,9>, ISS.VY<15,9>, ISS.VZ<15,9>, CountTime<15,9>, ISS.ElapsedSeconds<15,9>\/Delimit\/ISS.ElapsedDays<15,9>\/Delimit\/ISS.X<15,9>\/Delimit\/ISS.Y<15,9>\/Delimit\/ISS.Z<15,9>\/Delimit\/ISS.VX<15,9>\/Delimit\/ISS.VY<15,9>\/Delimit\/ISS.VZ<15,9>\/Delimit\/CountTime<15,9>\/Delimit\/ISS.ElapsedSeconds<15,9>\/Delimit\/Report Headers\/Delimit\/Include Headers\/SPACE\/FF_ISS_Earth_0_0_0.Report\/SPACE\/\/SPACE\/
[COMMAND10]
Command1=Disabled\/SPACE\/End\/SPACE\/\/SPACE\/
[COMMAND11]
Command1=Disabled\/SPACE\/Step\/SPACE\/ISS\/SPACE\/\/SPACE\/
[COMMAND12]
Command1=Disabled\/SPACE\/Assignment\/SPACE\/Dummy = ( ISS.ElapsedSeconds )\/Delimit\/Variable\/Operator\/Object Parameter\/Operator\/Number\/SPACE\/\/SPACE\/
[COMMAND13]
Command1=Disabled\/SPACE\/If\/SPACE\/Expanded\/SPACE\/CountTime > 0 and Flag = 0\/Delimit\/Variable\/Operator\/Number\/Delimit\/Variable\/Operator\/Number\/SPACE\/\/SPACE\/
[COMMAND14]
Command1=Disabled\/SPACE\/Assignment\/SPACE\/Flag = ( 1 )\/Delimit\/Variable\/Operator\/Number\/Operator\/Number\/SPACE\/\/SPACE\/
[COMMAND15]
Command1=Disabled\/SPACE\/Assignment\/SPACE\/SS = ( Dummy )\/Delimit\/Variable\/Operator\/Variable\/Operator\/Object Parameter\/SPACE\/\/SPACE\/
[COMMAND16]
Command1=Disabled\/SPACE\/End\/SPACE\/\/SPACE\/
[COMMAND17]
Command1=Disabled\/SPACE\/If\/SPACE\/Expanded\/SPACE\/CountTime = 0\/Delimit\/Variable\/Operator\/Number\/SPACE\/\/SPACE\/
[COMMAND18]
Command1=Disabled\/SPACE\/Report\/SPACE\/ISS.ElapsedDays<15,9>, ISS.X<15,9>, ISS.Y<15,9>, ISS.Z<15,9>, ISS.VX<15,9>, ISS.VY<15,9>, ISS.VZ<15,9>, CountTime<15,9>, ISS.ElapsedSeconds<15,9>\/Delimit\/ISS.ElapsedDays<15,9>\/Delimit\/ISS.X<15,9>\/Delimit\/ISS.Y<15,9>\/Delimit\/ISS.Z<15,9>\/Delimit\/ISS.VX<15,9>\/Delimit\/ISS.VY<15,9>\/Delimit\/ISS.VZ<15,9>\/Delimit\/CountTime<15,9>\/Delimit\/ISS.ElapsedSeconds<15,9>\/Delimit\/Report Headers\/Delimit\/Include Headers\/SPACE\/FF_ISS_Earth_0_0_0.Report\/SPACE\/\/SPACE\/
[COMMAND19]
Command1=Disabled\/SPACE\/End\/SPACE\/\/SPACE\/
[COMMAND20]
Command1=Disabled\/SPACE\/Assignment\/SPACE\/ReportTime = ( CountTime * SS )\/Delimit\/Variable\/Operator\/Variable\/Operator\/Variable\/SPACE\/\/SPACE\/
[COMMAND21]
Command1=Disabled\/SPACE\/If\/SPACE\/Expanded\/SPACE\/ReportTime = 600\/Delimit\/Variable\/Operator\/Number\/SPACE\/\/SPACE\/
[COMMAND22]
Command1=Disabled\/SPACE\/Report\/SPACE\/ISS.ElapsedDays<15,9>, ISS.X<15,9>, ISS.Y<15,9>, ISS.Z<15,9>, ISS.VX<15,9>, ISS.VY<15,9>, ISS.VZ<15,9>, CountTime<15,9>, ISS.ElapsedSeconds<15,9>\/Delimit\/ISS.ElapsedDays<15,9>\/Delimit\/ISS.X<15,9>\/Delimit\/ISS.Y<15,9>\/Delimit\/ISS.Z<15,9>\/Delimit\/ISS.VX<15,9>\/Delimit\/ISS.VY<15,9>\/Delimit\/ISS.VZ<15,9>\/Delimit\/CountTime<15,9>\/Delimit\/ISS.ElapsedSeconds<15,9>\/Delimit\/Report Headers\/Delimit\/Include Headers\/SPACE\/FF_ISS_Earth_0_0_0.Report\/SPACE\/\/SPACE\/
[COMMAND23]
Command1=Disabled\/SPACE\/Assignment\/SPACE\/CountTime = ( 0 )\/Delimit\/Variable\/Operator\/Number\/Operator\/Number\/SPACE\/\/SPACE\/
[COMMAND24]
Command1=Disabled\/SPACE\/End\/SPACE\/\/SPACE\/
[COMMAND25]
Command1=Disabled\/SPACE\/Assignment\/SPACE\/CountTime = ( CountTime + 1 )\/Delimit\/Variable\/Operator\/Variable\/Operator\/Number\/SPACE\/\/SPACE\/
[COMMAND26]
Command1=Disabled\/SPACE\/End\/SPACE\/\/SPACE\/
[CopyOfISS]
EphemFileType=Unknown
UseEphem=0
[COMMAND27]
Command1=Disabled\/SPACE\/While\/SPACE\/Expanded\/SPACE\/CopyOfISS.ElapsedDays < 1\/Delimit\/Object Parameter\/Operator\/Number\/SPACE\/\/SPACE\/
[COMMAND28]
Command1=Disabled\/SPACE\/View\/SPACE\/CopyOfISS\/SPACE\/\/SPACE\/
[COMMAND29]
Command1=Disabled\/SPACE\/Report\/SPACE\/CopyOfISS.ElapsedMinutes<15,9>, CopyOfISS.X<15,9>, CopyOfISS.Y<15,9>, CopyOfISS.Z<15,9>, CopyOfISS.VX<15,9>, CopyOfISS.VY<15,9>, CopyOfISS.VZ<15,9>\/Delimit\/CopyOfISS.ElapsedMinutes<15,9>\/Delimit\/CopyOfISS.X<15,9>\/Delimit\/CopyOfISS.Y<15,9>\/Delimit\/CopyOfISS.Z<15,9>\/Delimit\/CopyOfISS.VX<15,9>\/Delimit\/CopyOfISS.VY<15,9>\/Delimit\/CopyOfISS.VZ<15,9>\/Delimit\/Report Headers\/Delimit\/Include Headers\/SPACE\/FF_ISS_Earth_0_0_0_check.Report\/SPACE\/\/SPACE\/
[COMMAND30]
Command1=Disabled\/SPACE\/Step\/SPACE\/CopyOfISS to (CopyOfISS.ElapsedMinutes < 10)\/SPACE\/Object Parameter\/Operator\/Number\/SPACE\/\/SPACE\/
[COMMAND31]
Command1=Disabled\/SPACE\/End\/SPACE\/\/SPACE\/
[COMMAND32]
Command1=Disabled\/SPACE\/Report\/SPACE\/CopyOfISS.ElapsedMinutes<15,9>, CopyOfISS.X<15,9>, CopyOfISS.Y<15,9>, CopyOfISS.Z<15,9>, CopyOfISS.VX<15,9>, CopyOfISS.VY<15,9>, CopyOfISS.VZ<15,9>\/Delimit\/CopyOfISS.ElapsedMinutes<15,9>\/Delimit\/CopyOfISS.X<15,9>\/Delimit\/CopyOfISS.Y<15,9>\/Delimit\/CopyOfISS.Z<15,9>\/Delimit\/CopyOfISS.VX<15,9>\/Delimit\/CopyOfISS.VY<15,9>\/Delimit\/CopyOfISS.VZ<15,9>\/Delimit\/Report Headers\/Delimit\/Include Headers\/SPACE\/FF_ISS_Earth_0_0_0_check.Report\/SPACE\/\/SPACE\/
[COMMAND33]
Command1=Disabled\/SPACE\/Step\/SPACE\/CopyOfISS to (CopyOfISS.ElapsedMinutes < 10)\/SPACE\/Object Parameter\/Operator\/Number\/SPACE\/\/SPACE\/
[COMMAND34]
Command1=Disabled\/SPACE\/End\/SPACE\/\/SPACE\/
[ISS]
EphemFileType=Unknown
UseEphem=0
[START_SECTION]
Start1=Enabled\/SPACE\/Spacecraft\/SPACE\/ISS\/SPACE\/\/SPACE\/0\/SPACE\/0\/SPACE\/0\/SPACE\/\/SPACE\/Unknown\/SPACE\/
Start2=Enabled\/SPACE\/Variable\/SPACE\/I\/SPACE\/\/SPACE\/
Start3=Enabled\/SPACE\/Variable\/SPACE\/SS\/SPACE\/Step Size\/SPACE\/
[FreeformScript2]
FreeFormLine1=\/SPACE\/FF_SolarSystem.COUpdateInterval = 0;\/END\/
FreeFormLine2=\/SPACE\/FF_SolarSystem.NPUpdateInterval = 60;\/END\/
FreeFormLine3=\/SPACE\/FF_SolarSystem.ComputePolarMotion = 1;\/END\/
[COMMAND_SECTION]
Number_Commands=7
