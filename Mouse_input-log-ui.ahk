;"Desktop wallpaper" Mouse-input feedback ui  ; by M.Wolff
; https://autohotkey.com/boards/viewtopic.php?f=6&t=26059
; Re-Worked code. Custom-ui, Desktop-mount. Scintilla lex; Flicker eliminated : 

#NoEnv
#Notrayicon
#Persistent
#MouseHistory(mhist:=10)
#Singleinstance force
OnMessage(0x0404,"AHK_NOTIFYICON")
#Include <SCIaa>

SciLexer:= A_ScriptDir . (A_PtrSize == 8 ? "\SciLexer64.dll" : "\SciLexer32.dll")
If (!LoadSciLexer(SciLexer)) {
	MsgBox,0x10,%g_AppName% - Error
	, % "Failed to load library """ . SciLexer . """.`n`n"
	ExitApp,
}

(_:=winexist("ahk_class AutoHotkey","KeyHistory_input-log-uiSCI.ahk"))? (
, A_Y_HC:= 60) : A_Y_HC:= 389, TxtSize:= 11
global A_X_HC:= 8, A_MarginX:= 0, A_MarginY:= 1			; Ordinals ( see-above )
, text_p1_Colour:="c5540aa", text_p1_Font:= "MS gothic"	; Main font;(Monospace working best atm)
, Dtop:= True, behind_icon:= False, MERGE_MOVE := True	; D-Top-host;!BehindD-Top-icons?
,A_Y_HC, A_Y_HC, gui_hh, gui_xx, gui_yy, gui_hh_old,gui_visible 	; MERGE_MOVE -> Merge-Mouse-Move-2next-Update
global ismovable, gui_hw, iniwrite_Queued, mhist, rect
A_H_HC:= (mhist *(TxtSize*2))
VarSetCapacity(RECT,16)

fileexist(A_scriptdir "\" A_ScriptName ".ini") ? readini() : ()

;=-------=====-==========----------=====-==========----------=====-==========----------=====-==========---

OnExit("AtExit")
OnMessage(0x0201,"WM_LBUTTONDOWN")
gosub("menutray") ; lgui,new
global wtemp:= strlen( SizingTemplate:= " *WHEEL UP   0.000   9999    9999") *10 ; <-<-------do-not-disturb
Gui,+LastFound +E0x0a090028 +Hwndgui_hw -DPIScale +toolwindow -caption
sci_init()
Gui,Margin,% A_MarginX,% A_MarginY
Gui,Font,s11,MS gothic
Gui,Add,Text, vKH,% Format(formatting,"ea!u","1000.00","Browooes","KEY_IaGNORE_")
GuiControlGet,KHT,Pos
GuiControlGet,KH,Pos
GuiControlGet,scint,Pos
GuiControl,,KH		; clear dummy sizing text ;sci.SetText(unused, text,0xEE0000)
if !headers
	GuiControl,,KHT		; clear dummy sizing text
gui,Margin,MarginX:=0,MarginY:=1 	;l;guiControl,+e0x02000000,	MH,
gosub,Resize
return,

;=-------=====-==========----------=====-==========----------=====-==========----------=====-==========---

Resize: 							; Resize label to fit mouse history.
gui_hh:= A_H_HC, gui_ww:= wtemp+8
guiControl,Move,MH,h%gui_hh%
gui_hh +=40
gui,+LastFound 						; Determine visibility.
WinGet,style,Style
gui_visible:= style ^0x10000000 	; Determine current position and height.
WinGetPos,gui_xx,gui_yy,,gui_hh_old
SysGet,	wa_,	MonitorWorkArea
SysGet,	twc_h,	51					;SM_CYSMCAPTION
SysGet,	bdr_h,	8					;SM_CYFIXEDFRAME Initially on the left side.
( !gui_visible)? (gui_xx:= 10, gui_yy:= wa_bottom-(gui_hh +twc_h +bdr_h *2 +10)) :
,((gui_yy +gui_hh //2 > (wa_bottom -wa_top) //2)? gui_yy:= 410)
gshow(gui_hw,A_X_HC,A_Y_HC,gui_ww,gui_hh) ; Move relative to bottom edge when closer to the bottom.
guiControl,+e0x0a000020 +0x06000000,MH
return,

#MouseHistory(NewSize="") {
	global MouseBuffer
	static MouseHook, MouseHookProc
	if NewSize =	; Get current history length.
	return,(cap:= VarSetCapacity(MouseBuffer) //24)>0 ? cap -1 : 0
	if NewSize	{
	if !MouseHook {	; Register mouse hook.
			MouseHookProc:= RegisterCallback("Mouse")
			MouseHook:= DllCall("SetWindowsHookEx","int",14,"ptr",MouseHookProc,"uint",0,"uint",0,"ptr")
		} ; sizeof(MSLLHOOKSTRUCT)=24
		((cap:= VarSetCapacity(MouseBuffer))>new_cap? cap:= (new_cap:= (NewSize +1) *24))
		VarSetCapacity(old_buffer,cap) ; Back up previous history.
		DllCall("RtlMoveMemory","ptr",&old_buffer,"ptr",&MouseBuffer,"ptr",cap)
		VarSetCapacity(MouseBuffer,0) ; FORCE SHRINK
		VarSetCapacity(MouseBuffer,new_cap,0) ; Set new history length.
		DllCall("RtlMoveMemory","ptr",&MouseBuffer,"ptr",&old_buffer,"ptr",cap) /* ; Restore previous history.; (Remember N+1 mouse events to simplify calculation of the Nth mouse event's elapsed Time.); Put tick count so the initial mouse event has a meaningful value for "elapsed". */
		NumPut(A_TickCount,MouseBuffer,16,"uint")
	} else { ; Unregister the mouse hook.
		(MouseHook? DllCall("UnhookWindowsHookEx","ptr",MouseHook) ;Clear history entirely.
		,DllCall("GlobalFree","ptr",MouseHookProc), MouseHook:="")
		VarSetCapacity(MouseBuffer, 0)
}	}

Show:
SetFormat,	FloatFast,.2
text:= "",	buf_size	:= #MouseHistory()
Loop,%	(	buf_size  ) {
	SetFormat,IntegerFast,D
	if MouseHistory(A_Index,msg,x,y,mouseData,flags,Time,elapsed) {
		SetFormat,IntegerFast,H
		msg:= (msg +0) ""
		SetFormat,IntegerFast,D
		switch msg {
			case 0x201,0x202,0x203,0xA1,0xA2,0xA3 : btn:= "Left" ;		WM_LBUTTONDOWN/UP/DBLCLK, WM_NC..
			case 0x204,0x205,0x206,0xA4,0xA5,0xA6 : btn:= "Right" ; 	WM_RBUTTONDOWN/UP/DBLCLK, WM_NC..
			case 0x207,0x208,0x209,0xA7,0xA8,0xA9 : btn:= "Middle" ; 	WM_MBUTTONDOWN/UP/DBLCLK, WM_NC..
			case 0x20B,0x20C,0x20D,0xAB,0xAC,0xAD : btn:= (mouseData &0x10000)? "X1(Back)" : "X2"(Fwd) ;XBUTTDOWNUP/DBLCLK+NC
			case 0x20A : mouseData:= mouseData << 32 >> 48, btn:= (mouseData < 0)? "WheelDn" : "WheelUp"	;WM_MOUSEWHEEL
			case 0x20E : mouseData:= mouseData << 32 >> 48, btn:= (mouseData < 0) ? "WheeLeft" : "WheelRt"	;WM_MOUSEHWHEEL
			case 0x20A, 0x20E: clickCount:= Abs(mouseData), (!clickCount? clickCount:="")	;WM_MOUSEWHEEL,WM_MOUSEHWHEEL
			case 0x200: btn:= clickCount:= ""		; 	WM_MOUSEMOVE
			case 0x203,0xA3,0x206,0xA6,0x209,0xA9,0x20D,0xAD,: btn:= msg, clickCount:= 2		;LBUTTDBLCLK+_NC/M/XBUTTDBLCLK
			case 0x201,0x204,0x207,0x20B,0xA1,0xA4,0xA7,0xAB : btn:= msg, clickCount:= "Down"	;WM_MOUSEWHEEL WM_MOUSEHWHEEL
			case 0x202,0x205,0x208,0x20C,0xA2,0xA5,0xA8,0xAC : clickCount:= "Up", btn:= msg		;WM_L/R/M/XBUTTONUP WM_NC
			default: TT("Error unhandled message from mouse...`n" msg ":`n" mouseData ":`n" flags)
		}
		text.= ((flags & 1) ? "*" : " ") ; formatting:="{:-8}{:-8}{:-10}{:-9}`n"
		;.	SubStr" ", 1, 6)(a:=(msg=0x201)?"Dn": (a:=(msg=0x202)?"Up")); (a:=(msg=0x202)?"Up")));" ",1,8)
		.	SubStr(btn . (a:= (msg=0x201)? "Dn" : (msg=0x202?"Up" : "  " )) .  "     ",1,7)
		.	SubStr(" "elapsed/1000.0, -4) "  " SubStr("  " x,-4)
		.	SubStr(" " clickCount, -5)
		.	SubStr(" "y, -4 ) "`n"
	}
	else,break,
}
settext(text)
return,

gshow(hw="",xx="",yy="",ww="",hh="") {
	global	;global DTOP,gui_xx,gui_yy,gui_hh,gui_hw,txt_hw,SizingTemplate
	wtemp:= strlen(SizingTemplate) *10
	winset exstyle,+0x02000020,ahk_id %txt_hw%
	winset style,-0x80080000,ahk_id %hw%
	;winset exstyle,+0x02000020,ahk_id %txt_hw%
	;winset style,+0x46000000,ahk_id %hw%
	;winset style,-0x82080080,ahk_id %hw%
	loop,parse,% "hw,xx,yy,ww,hh",`,
	{
		if ((%a_loopfield%)="") {
			dky:= ("gui_" . a_loopfield)
			(("gui_" . a_loopfield)
			?dky:= ("gui_" . a_loopfield)
			,(%a_loopfield%) :=	(%dky%))
		}	
	}
	Window2Dtop(hw,gui_xx,gui_yy,ww,gui_hh)
	sleep,30
	gui,Show,x%xx% y%yy% w%ww% h%hh% NA ,% "no_glass"
	gui,color,0x000000
	winset,transcolor,off,ahk_id %hw%
	winset,transcolor,0x000000,ahk_id %hw%
	;msgbox,%   xx " " yy  " "  ww " " hh  " "
	guicontrol,,MH,e0x02000020
	guicontrol,,MH,w%wtemp%
	;win_move(hw,"","",wtemp,"","")
	winset,style,+0x42000000,ahk_id %hw%
	winset,style,-0x8a0a0080,ahk_id %hw%
	winset,ExStyle,+0x02080020,ahk_id %txt_hw%
	winset,ExStyle,+0x02080020,ahk_id %hw%
	if ismovable
		winset,style,+0x80000000,ahk_id %hw%
	else {
		winset,style,-0x80000000,ahk_id %hw%
		winset,exstyle,+0x20,ahk_id %hw%
	}
	sleep,35
}

MouseHistory(N,ByRef msg,ByRef x,ByRef y,ByRef mouseData,ByRef flags,ByRef Time, ByRef elapsed=-1) {
	global MouseBuffer	; a more straightforward scope method?
	static mbuf:= MouseBuffer
	if N is not integer
	return,false
	buf_max:= #MouseHistory()
	if (N < 1 || N > buf_max)
	return,false
	x			:= NumGet(MouseBuffer,ofs:= (N-1) *24,"int")
	y			:= NumGet(MouseBuffer,ofs+4, "int" )
	mouseData	:= NumGet(MouseBuffer,ofs+8, "uint")
	flags		:= NumGet(MouseBuffer,ofs+12,"uint")
	Time		:= NumGet(MouseBuffer,ofs+16,"uint")
	msg			:= NumGet(MouseBuffer,ofs+20,"uint")
	elapsed:=	Time -((Time2:= NumGet(MouseBuffer,N*24+16,"uint"))? Time2 : Time)
	return,!!msg
}

sci_init() {
	global ;gui,Add,Text,+E0x00000020 +hwndtxthw w%wtemp% h%A_H_HC% vMH
	sci:= new scintilla(gui_hw) ;,headers,KH,KHT,Jew_wind,scint
	winset,transcolor,000000
	Sci.SetCodePage(65001) 	; UTF-8
	sci.SetWrapMode(true) 	; set default font up;STYLE_DEFAULT := 32
	sci.StyleSetFont(32,"ms gothic","ms gothic"),sci.stylesetsize(32,11), sci.StyleSetFore(STYLE_DEFAULT,0x995566), sci.STYLESETBACK(STYLE_DEFAULT,0x000000), sci.SETWHITESPACEback(STYLE_DEFAULT,0x000000),sci.StyleSetBold(STYLE_DEFAULT, False) ;sci.DELETEBACK()
	sci.SETUSETABS(STYLE_DEFAULT, true)
	sci.SCI_SETTABWIDTH(STYLE_DEFAULT, 3)

	sci.SetLexer(STYLE_DEFAULT)
	sci.SETselectionback(STYLE_DEFAULT,0x00aaff),sci.SETSELBACK(STYLE_DEFAULT,0x000000)
	sci.StyleClearAll()

	sci.StyleSetSize(SCE_AHKL_USERDEFINED1, 10)
	sci.StyleSetFore(SCE_AHKL_USERDEFINED1, 0x664433)
	Sci.StyleSetBack(SCE_AHKL_USERDEFINED1, 0x000000)
	sci.StyleSetFont(SCE_AHKL_USERDEFINED1, "segoe mdl2 assets","segoe mdl2 assets")
	sci.SETUSETABS(SCE_AHKL_USERDEFINED1, true)
	sci.SCI_SETTABWIDTH(SCE_AHKL_USERDEFINED1, 3)

	sci.StyleSetSize(SCE_AHKL_USERDEFINED2, 9)
	sci.StyleSetFore(SCE_AHKL_USERDEFINED2, 0xaa4466)
	sci.StyleSetFont(SCE_AHKL_USERDEFINED2,"armada light","armada light")

	Sci.StyleSetBack(SCE_AHKL_USERDEFINED2, 0x000000)
	sci.SETUSETABS(SCE_AHKL_USERDEFINED2, true)
	sci.SCI_SETTABWIDTH(SCE_AHKL_USERDEFINED2, 3)

	sci.StyleSetSize(SCE_AHKL_USERDEFINED3, 10)
	sci.StyleSetBold(SCE_AHKL_USERDEFINED3, false)
	sci.StyleSetFore(SCE_AHKL_USERDEFINED3, 0x885555)
	Sci.StyleSetBack(SCE_AHKL_USERDEFINED3, 0x000000)
	sci.StyleSetFont(SCE_AHKL_USERDEFINED3,"armada light","armada light")
	sci.SETUSETABS(SCE_AHKL_USERDEFINED3, true)
	sci.SCI_SETTABWIDTH(SCE_AHKL_USERDEFINED3, 3)

	sci.StyleSetSize(SCE_AHKL_USERDEFINED4, 12)
	sci.StyleSetFore(SCE_AHKL_USERDEFINED4, 0x448800)
	Sci.StyleSetBack(SCE_AHKL_USERDEFINED4, 0x000000)

	sci.StyleSetSize(SCE_AHKL_USERDEFINED5, 12)
	sci.StyleSetFore(SCE_AHKL_USERDEFINED5, 0x0044aa)
	Sci.StyleSetBack(SCE_AHKL_USERDEFINED5, 0x000000)

	sci.StyleSetSize(SCE_AHKL_USERDEFINED6, 12)
	sci.StyleSetFore(SCE_AHKL_USERDEFINED6, 0x0066ff)
	Sci.StyleSetBack(SCE_AHKL_USERDEFINED6, 0x000000)
	sci.SetReadOnly(true)
	Gui,Add,Custom,ClassScintilla vSCint +e0x02080000 +hwndtxthw x0 y100 w400 h300 r0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12,
}

settext(byref txt,byref obj="sci") {
	global (%obj%)
	(%obj%).SetReadOnly(false)
	(%obj%).SetText(unused,txt,0xfffffff)
	searchrep(keynames,"3")
	searchrep(bold,"2") 
	searchrep(injecteds,"2") 
	searchrep(INjectionCHAR,"1") 
	searchrep(notlogignoreCHAR,"1") 
	searchrep(notphysCHAR,"1") 
	(%obj%).SetReadOnly(true)
}

searchrep(TheNeedle,userstylenum="1",byref obj="sci") {
	global (%obj%)
	pSearch:= StrSplit(TheNeedle,",")
	loop,% (pSearch.Length(), cnt:=0) {
		if ((SearchText:= pSearch[A_Index]) = "")
			Continue
		If (!StringLength := StrPut(SearchText, "UTF-8") - 1) {
			(%obj%).IndicatorClearRange(0, (%obj%).GetLength())
			Return
		}
		TextLength:= (%obj%).GetLength(), cnt++
		(%obj%).SetTargetRange(0,TextLength)
		Length:= StrPut(SearchText, "UTF-8") - 1
		VarSetCapacity(StrBuf,Length)
		StrPut(SearchText, &StrBuf, "UTF-8")
		
		While ((%obj%).SearchInTarget(StringLength, &StrBuf) != -1) { ; Algumist, RRR
			TargetStart:= 	(%obj%).GetTargetStart()
			TargetEnd:= 	(%obj%).GetTargetEnd()
			TargetLength:= 	TargetEnd-TargetStart
			
			If (!TargetLength) {
				(%obj%).SetTargetRange(++TargetEnd, TextLength)
				continue, ; Zero-length match (Scintilla RegEx)
			}
			(%obj%).StartStyling(TargetStart) ; colors from start to end
			anus:=Mod(cnt, 7)
			switch anus {
				;case "1","2","3","4","5","6","7","8","9","0":
				default:
					(%obj%).SetStyling(Length, SCE_AHKL_USERDEFINED%userstylenum%)
					(%obj%).SetTargetRange(TargetEnd, TextLength)
			}
		}
	}
}

MENSpunction() {
	global
	trayActiv:= True
	Menu,Tray,Show
	trayActiv:= False
}

AHK_NOTIFYICON(wParam, lParam) {	; 0x201: ; WM_LBUTTONDOWN   ; 0x202:; WM_LBUTTONUP
	;Thread,Priority,0 || ;Thread,Priority,7 ; 0x020B:; WM_XBUTTONDOWN
	switch lParam {
		;Case 0x0200 : refresh_uptime_(True)	 ; WM_MOUSEmove
		; return,% Refresh_uptime_(True)
		Case 0x204 : return,% MENSpunction() ; WM_RBUTTONDN
		;MENSpunction()
			return,
		Case 0x203 : TT("Loading...") ; timer("ID_VIEW_VARIABLES",-1);	WM_LBUTTONDBLCLK
			PostMessage,0x0111,65407,,,% (A_ScriptName " - AutoHotkey")
		winget,h,id,WinEvent.ahk - AutoHotkey
		;Aero_BlurWindow(h)
		;Aero_BlurWindow(h)
		Case 0x205 : return,(trayActiv?MENSpunction()) ;WM_RBUTTONUP
		;	Case 0x0208:;	WM_MBUTTONUP	;;timer("ID_TRAY_RELOADSCRIPT",-1); TT("Reloading... 1 sec",900); sleep,900; reload			; return
	}
	return,
}

movabletoggle() {
	global gui_hw,iniwrite_Queued ;	(hwnd=""?hwnd:= gui_hw)
	winset,transcolor,off,ahk_id %gui_hw%
	winget,poopold,style,ahk_id %gui_hw%
	;msgbox % _:= (wi:= wingetpos(gui_hw)).y
	;if (poopold & 0x80000000)
	if ( !((_:= (wi:= wingetpos(gui_hw)).y)=A_Y_HC)) {
		A_Y_HC:= wi.y, A_X_HC:= wi.x
		msgb0x("New Pos?","Save new Pos?`nX: " A_X_HC "`nY: " A_Y_HC,5,0x43040+289) ; (if moved)
		ifmsgbox,ok
			IniRightz()				;iniwrite_Queued:=True
	}	;winset,style,^0x80000000,ahk_id %gui_hw%	;winset,exstyle,^0x20,ahk_id %gui_hw%
	winget,poop,style,ahk_id %gui_hw%
	sleep,200
	(poop & 0x80000000? movable:= true : moveable:= False)
	menu,tray,% movable?"check":"uncheck",ismovable,% "movabletoggle",winset,transcolor,0x000000,ahk_id %gui_hw%
	return,
}

Mouse(nCode,wParam,lParam) { ; Mouse hook callback - records mouse events into MouseBuffer.
global MouseBuffer, MERGE_MOVE
Critical ;if MERGE_MOVE && wParam = 0x200 && NumGet(MouseBuffer, 20, "uint") = 0x200
if ((buf_max:= #MouseHistory()) > 0) {
	if (MERGE_MOVE && NumGet(MouseBuffer,20,"uint")=0x200) { ; Update the most recent (mouse-move) event.
		DllCall("RtlMoveMemory","ptr",&MouseBuffer,"ptr",lParam,"ptr",20)
	} else {	; Push older mouse events to the back.; Copy current mouse event to the buffer.
		((buf_max>1)? DllCall("RtlMoveMemory","ptr",&MouseBuffer+24,"ptr",&MouseBuffer,"ptr",buf_max*24))
		DllCall("RtlMoveMemory","ptr",&MouseBuffer,"ptr",lParam,"ptr",20)
	}
	NumPut(wParam,MouseBuffer,20,"uint") ; Put wParam in place of dwEventInfo.
	SetTimer,Show,-10
}	; "gosub Show" slows down the mouse hook and causes problems, so use a Timer.
return,DllCall("CallNextHookEx","uint",0,"int",nCode,"ptr",wParam,"ptr",lParam,"ptr")
}

Window2Dtop(byref Child="",x:=1,y:=45,w:=480,h:=480){		; Window2Dtop(byref Child="",x:=1,y:=45,w:=480,h:=480){
	WinGetPos,ChildX,ChildY,Child_W,Child_H,ahk_id %Child%	; global gui_hw;	( !Child? Child:= gui_hw ); stylemen invoked
	Surrogate:= DESKTOP("blah")								; WinGetPos,ChildX,ChildY,Child_W,Child_H, ahk_id %Child%
	DllCall("SetParent","ptr",Child,"ptr",Surrogate)		; (Surrogate:= behind_icon?DesktopAC : (DesktoP("Main")))
	sleep(100)												; DllCall("SetParent","ptr",Child,"ptr",Surrogate)
	Win_Move(Child,x,y,"","","")							; msgbox %  Child " " ChildX " " ChildY " " Child_W " " Child_H;
	return,													; sleep(100)
}															; WinMove,ahk_id %Child%,,1,45,480,480swa; return,errorlevel ;};

WinGetPos(byref WinTitle="") { ; ,WinText="",ExcludeTitle="",ExcludeText="") {
	(!detecthiddenwindows? (detecthiddenwindows,"on", timer("detecthiddenwindows",-300)))
	(!detecthiddentext? (detecthiddentext,"on", timer("detecthiddentext",-300)))
	WinGetPos, wX, wY, wWidth, wHeight,ahk_id %WinTitle% ;,% WinText,% ExcludeTitle,% ExcludeText
	return,_:= ({	"X" : wx
				,	"Y" : wY
				,	"W" : wWidth
	,				"H" : wHeight })
}

mentoggla() {
	global
	varn:= a_thismenuitem
	switch a_thismenuitem {
		case "Pulse Alpha" : varn:= "Pulse"
		default	: varn:= a_thismenuitem
	}
	(%varn%:= !%varn%)
	if varn
	try,menu,% A_ThisMenu,check,% a_thismenuitem
	else,try menu,% A_ThisMenu,uncheck,% a_thismenuitem
	return,varn
}

menutray:
menu,tray,icon
menu,tray,noStandard
menu,tray,add,ismovable,% "movabletoggle"
menu,tray,add,hide me pls, hidetray
menu,tray,icon,hide me pls, C:\Icon\32\32.ico
menu,tray,add,dont hide pls, donthidetray
menu,tray,icon,dont hide pls, C:\Icon\32\32.ico
menu,tray,Add,%	"Open",				MenHandlr
menu,tray,Icon,% "Open",%			"C:\Icon\64ribbon\regview3264.ico",,32
menu,tray,Add,%	"Open Containing",	MenHandlr
menu,tray,Icon,% "Open Containing",% "C:\Icon\256\#86_2.ico",,32
menu,tray,Add,% "Edit Script",		MenHandlr
menu,tray,Icon,% "Edit Script",%	"C:\Icon\256\np++Hackjob.ico",,32
menu,tray,Add,% "Reload",			MenHandlr
menu,tray,Icon,% "Reload",%			"C:\Icon\256\IDI_ICON1.ico",,32
menu,tray,Add,% "Suspend VKs",		MenHandlr
menu,tray,Icon,% "Suspend VKs",%	"C:\Icon\256\invert_goatse_256.ico",0,32
menu,tray,Add,% "Pause",			MenHandlr
menu,tray,Icon,% "Pause",%			"C:\Icon\24\head_fk_a_24_c2b.ico",,32
menu,tray,Add,% "Exit",				MenHandlr
menu,tray,Icon,% "Exit",%			"C:\Icon\256\DOO0m.ico",,32
timer("HideTray",-36000)
return,

hidetray:
donthidetray:
switch a_thislabel {
case "hidetray"		:	menu,tray,noicon
case "donthidetray"	:	timer("hidetray",off)
}
return,

WM_LBUTTONDOWN(wParam, lParam,byref RECT, mDC) {
	tt("aadawad")
	global gui_hw,rbutton_cooldown:=false
	;settimer rbutton_cooldown_reset, -670
	xs:= (lParam & 0xffff), ys:= (lParam >> 16)
	;DllCall("SetWindowBand", "ptr",gui_hw, "ptr", 0, "uint", 15)
	;tt(a_lasterror)
	While   GetKeyState("lbutton","P") {
		If !GetKeyState("lbutton","P")
			return,
		else,tt(a_now)
		DllCall("GetCursorPos","Uint",&RECT)
		vWinX   :=  NumGet(&RECT,   0, "Int") - xs
		vWinY   :=  NumGet(&RECT,   4, "Int") - ys
		win_move(gui_hw,vWinX,vWiny,"","","")
	sleep, 4
}	}

MenHandlr(isTarget="") {
	global
	listlines,off
	switch	nus:= a_thismenuitem {
		case "Open Containing": TT("Opening "   a_scriptdir "..." Open_Containing(A_scriptFullPath),1)
		case "edit","SUSPEND","pAUSE": PostMessage,0x0111,(%a_thismenuitem%),,
		,% A_ScriptName " - AutoHotkey"
		case "Open" : PostMessage,0x0111,%open%,,
		case "reload" : reload
		case "exit" : exitApp,
		;winset,style,+0x568F0000,ahk_id %ldr_hWnd%
		;winset,transcolor,off,	 ahk_id %ldr_hWnd%
		;winset,transcolor,0x000000,ahk_id %ldr_hWnd%
		default: islabel(a_thismenuitem)? timer(a_thismenuitem,-10) : ()
	}
	return,
}

IniRightz() {
	global
	settingsString:= "A_X_HC," . A_X_HC . ",A_Y_HC," . A_Y_HC .  ","
	IniWrite,%settingsString%,%A_scriptdir%\%A_ScriptName%.ini,settings,settingsall
	return,!errorlevel
}

readini() {
	local varname
	IniRead,settingsall,%A_Scriptfullpath%.ini,settings,settingsall
	if !settingsall {
		msgbox,% "no setings"
		exit
	} else,loop,parse,settingsall,`,
	varname? (global (%varname%):= a_loopfield, varname:= "" ) : varname:= a_loopfield
}

AtExit() {
	global
	(iniwrite_Queued? IniRightz())
	sleep,800
	return,
}

detecthiddentext:
detecthiddenwindows:
(%a_thislabel%),off
return,

guiClose:
ExitApp,