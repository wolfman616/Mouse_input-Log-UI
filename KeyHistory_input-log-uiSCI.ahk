; if !winexist("Mouse_input-log-ui.ahk ahk_class AutoHotkey")
#NoEnv
#Persistent
#Notrayicon
#InstallKeybdHook
#Singleinstance force
#KeyHistory(13)
#Include <SCIaa>
DetectHiddenWindows,On
DetectHiddenText,	On
SetTitleMatchMode,	2
SetTitleMatchMode,	Slow
	SetControlDelay,-1
		SetWinDelay,-1

SciLexer:= A_ScriptDir . (A_PtrSize == 8 ? "\SciLexer64.dll" : "\SciLexer32.dll")
If (!LoadSciLexer(SciLexer)) {
	MsgBox,0x10,%g_AppName% - Error
, % "Failed to load library """ . SciLexer . """.`n`n"
	ExitApp,
}

menutray()

global spos:= 1, A_X_HC:= 1,A_Y_HC, A_MarginX:= 0, A_MarginY:= 2 ; master metrics
, bold:= "0.,1.,2.,3.,4.,5.,6.,7.,8.,9..0,.1,.2,.3,.4,.5,.6,.7,.8,.9", injecteds:= "↰,,"

(_:= winexist("Mouse_input-log-ui.ahk"))? A_Y_HC:= 280 : A_Y_HC:= 60

fileexist(A_scriptdir "\" A_ScriptName ".ini") ?readini() : ()

global colour1:= 0xEE0000, dtop:= true, behind_icon:= False
, INjectionCHAR:= "", notlogignoreCHAR:= "", notphysCHAR:= ""
, formatting:="{:1}`t{:1}`t{:1}`n", keynames:= "lctrl,"
, keytextold, dtop,behind_icon, our_pid:= our_hWnd:= ""
, ID_TRAY_PAUSE,ID_TRAY_EXIT, ID_VIEW_VARIABLES:= 65407,ID_TRAY_EDITSCRIPT:= 65304
, ID_TRAY_SUSPEND:= 65305, ID_TRAY_PAUSE:= 65306,ID_TRAY_EXIT:= 65307,ID_TRAY_RELOADSCRIPT:= 65303 

Process,Exist ; OnMessage(0x200, "WM_MOUSEMOVE") ;OnMessage(0x201, "WM_LBUTTONDOWN")
OnExit("AtExit")
OnMessage(0x100,"WM_KEYDOWN")
OnMessage(0x404,"AHK_NOTIFYICON") ;timer("menutray",-1)	ssdasassadaaa

hHookKeybd:= DllCall("SetWindowsHookEx","int",13 ;WH_KEYBOARD_LL=13
			,"ptr",RegisterCallback("Keyboard")
			,"ptr",DllCall("GetModuleHandle","ptr",0,"ptr")
			,"uint",0,"ptr") ; dwThreadId
gui, Color,0,0 ; +AlwaysOnTop; WS_EX_COMPOSITED = E0x02000000 & WS_EX_LAYERED= E0x00080000
Gui,+LastFound +E0x0a090028 +Hwndgui_hw -DPIScale +toolwindow -caption
sci_init()
gosub,Resize
return,
sci_init(){
	global
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
	Gui,Add,Custom,ClassScintilla vSCint +e0x02080000 +hwndJew_wind x0 y100 w400 h300 r0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12,
	Gui,Margin,% A_MarginX,% A_MarginY
	Gui,Font,s11,MS gothic 
	Gui,Add,Text, vKH,% Format(formatting,"ea!u","1000.00","Browooes","KEY_IaGNORE_")
	GuiControlGet,KHT,Pos
	GuiControlGet,KH,Pos 
	GuiControlGet,scint,Pos
	GuiControl,,KH		; clear dummy sizing text ;sci.SetText(unused, text,0xEE0000)
	if !headers 
		GuiControl,,KHT		; clear dummy sizing text 
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

#MaxThreadsBuffer,On
!WheelDown::
!WheelUp::
#MaxThreadsBuffer,Off
history_size:= #KeyHistory() +((A_ThisHotkey="!WheelUp")? +1 : -1)
#KeyHistory(history_size>0 ? history_size : 1)
; Delay resize to improve hotkey responsiveness.
SetTimer,Resize,-10
return,

Resize:
; Resize label to fit key history.
gui_hh:= KHH*#KeyHistory()
GuiControl, Move, KH, h%gui_hh%
gui_hh +=KHY + 10
;GuiControl, Move, scint, w100 x-10
Gui, +LastFound
; Determine visibility.
WinGet, style, Style
gui_visible:= style & 0x10000000
WinGetPos,gui_xx,gui_yy,,gui_hh_old
SysGet,wa_,MonitorWorkArea
SysGet,twc_h,51 ; SM_CYSMCAPTION
SysGet,bdr_h,8  ; SM_CYFIXEDFRAME
if (!gui_visible) {
	gui_xx = 0 ; Initially on the left side.
	gui_yy:= wa_bottom-(gui_hh+twc_h+bdr_h*2+10)
} else,((gui_yy+gui_hh//2 > (wa_bottom-wa_top)//2)? gui_yy:= 410) ; Move relative to bottom edge when closer to the bottom.
gshow("",A_X_HC,A_Y_HC,420)	;gshow("",10,405) ;win_move(gui_hw,2,,,,"") ;WinGetPos,gui_xx,gui_yy,,gui_hh_oldNN
return, ; Gui,Show,x10 y10 h%gui_hh% w340 NA,KeyHist ; Window2Dtop(gui_hw) ; return, GuiSize:

if (A_EventInfo = 1)
	Gui,Hide
return,

Keyboard(nCode,wParam,lParam) {
	global KeyBuffer
	static sz:= 16+A_PtrSize
	Critical
	if KeyHistory(1,vk,sc,flags)
		&& NumGet(lParam+0,"uint") = vk
		&& NumGet(lParam+4,"uint") = sc
		&& NumGet(lParam+8,"uint") = flags
		 buf_max:= 0 ; Don't show key-repeat.
	else,buf_max:= #KeyHistory()
	if ( buf_max>0 ) {	; Push older key events to the back.
		if (buf_max>1)
			DllCall("RtlMoveMemory","ptr",&KeyBuffer+sz,"ptr",&KeyBuffer,"ptr",buf_max*sz)
		; Copy current key event to the buffer.
		DllCall("RtlMoveMemory","ptr",&KeyBuffer,"ptr",lParam,"ptr",sz)
		SetTimer,Show,-20
	}
	return,DllCall("CallNextHookEx","ptr",0,"int",nCode,"ptr",wParam,"ptr", lParam,"ptr")
}

KeyHistory(N,ByRef vk,ByRef sc,ByRef flags:=0,ByRef time:=0,ByRef elapsed:=0,ByRef info:=0) {
	global KeyBuffer,text,keytext,INjectionCHAR,notlogignoreCHAR,notphysCHAR
	static sz:= 16+A_PtrSize
	if N is not integer
		return,false
	buf_max:= #KeyHistory()
	if (N < 0) 
		N += buf_max + 1
	if (N < 1 or N > buf_max)
		return,false
	vk		:= NumGet(KeyBuffer, (N-1)*sz,   "uint")
	sc		:= NumGet(KeyBuffer, (N-1)*sz+4, "uint")
	flags	:= NumGet(KeyBuffer, (N-1)*sz+8, "uint")
	time	:= NumGet(KeyBuffer, (N-1)*sz+12,"uint")
	info	:= NumGet(KeyBuffer, (N-1)*sz+16)
	elapsed	:= time - ((time2:= NumGet(KeyBuffer, N*sz+12, "uint")) ? time2 : time)
	if (info =0xFFC3D44F) 
		info:= notlogignoreCHAR
	else if (info=0xFFC3D44E)
		info:= notphysCHAR
	else if (info=0xFFC3D44D) {
		spos:= -70
		((flags & 0x10)? (info:=INjectionCHAR) : (info := ""))
	} else.(!info=0x0? info:= "Error" : info:= "")
	return,(vk || sc) 
}
 
GetKeyFlagText(flags) { ;													;"↑":"↓")
																			;"↗" :"↙")
	return,( (flags & 0x01) ? "e" : "") ; LLKHF_EXTENDED					;"⇗" :"⇙")
	. 		((flags & 0x30) ? "" : "") ; LLKHF_ALTDOWN 						;"↿ ":" ⇂")
	. 		((flags & 0x20) ? "!" : " ") ; LLKHF_ALTDOWN					;"☆":"★")
	. 		((flags & 0x10) ? "⇝" : " ") ; LLKHF_INJECTED (artificial) 		;"↥" :"↧")
	.		((flags & 0x80) ? ((flags & 0x10) ? "▲" : "△" ) : ((flags & 0x10) ?  "▼" : "▽" ))
	. 		((flags & 0x10) ? "⇜" : " ") ; LLKHF_INJECTED (artificial)
	. 		((flags & 0x20) ? "!" : " ") ; LLKHF_ALTDOWN
	. 		((flags & 0x30) ? "" : "") ; LLKHF_ALTDOWN
} 

#KeyHistory(NewSize="") {
	global KeyBuffer
	static sz := 16+A_PtrSize
	; Get current history length.
	if (NewSize="")
		return,(cap:= VarSetCapacity(KeyBuffer)//sz)>0 ? cap-1 : 0
	if (NewSize) {
		new_cap:= (NewSize+1)*sz
		cap:= VarSetCapacity(KeyBuffer)
		if (cap>new_cap)
			cap:=new_cap
		VarSetCapacity(old_buffer, cap)
		; Back up previous history.
		DllCall("RtlMoveMemory", "ptr", &old_buffer, "ptr", &KeyBuffer, "ptr", cap)
		; Set new history length.
		VarSetCapacity(KeyBuffer, 0) ; FORCE SHRINK
		VarSetCapacity(KeyBuffer, new_cap,  0)
		; Restore previous history.
		DllCall("RtlMoveMemory", "ptr", &KeyBuffer, "ptr", &old_buffer, "ptr", cap)
		; (Remember N+1 key events to simplify calculation of the Nth key event's elapsed time.)
		; Put tick count so the initial key event has a meaningful value for "elapsed".
		NumPut(A_TickCount, KeyBuffer, 12, "uint")
	} else, VarSetCapacity(KeyBuffer, 0) ; Clear history entirely.
}

GetKeyNameText(vkCode, scanCode, isExtendedKey) { ; Gets readable key name, usually identical to the name in KeyHistory.
	k:=GetKeyName(format("vk{1:02x}sc{3}{2:02x}", vkCode, scanCode, isExtendedKey))	; if ( Strlen(k)<4 )			; StringUpper, k, k
	switch K {
		case "up":			return,k:="￪"
		case "down":			return,k:="￬"
		case "left":			return,k:="￩"
		case "right":			return,k:="￫"
		case "pgdn":			return,k:= "PgDn"
		case "enter":			return,k:= "⏎"
		case "backspace":			k:= "↰" ; "⟸" ;""BkSp"
		case "printscreen":			return,k:= "PrtScr"
		case "numpadenter":	return,k:= "NumPad⏎" ; ⏎⇦⇚⇐↰←		
		case "numpadadd":			return,k:= "NumPad+"
		case "numpadclear":			return,k:= "NumPad￮"
		case "numpaddivide":			return,k:= "NumPad/"
		case "numpadsubtract":			return,k:= "NumPad-"
		case "numpadmultiply":  			return,k:= "NumPad-"
		case "lcontrol":  			return,k:= "LCtrl"
		default:		if strlen(k)<4 ;keynames .="↰BkSp↰","PrtScr","NumPad￮","NumPad/","NumPad-","⏎","pgdn","￫","￩","￬","￪",,,,,,
			StringUpper,k,k
	}
	keynames.=k . ","
	return,k ;else if ( Strlen(k)>8 )	; return,GetKeyName(format("vk{1:02x}sc{3}{2:02x}", vkCode, scanCode, isExtendedKey))
}		

Show:
SetFormat,FloatFast,.2
SetFormat,IntegerFast,H
text:= ""
buf_size:= #KeyHistory()
Loop,% (buf_size) {
	if (KeyHistory(buf_size+1-A_Index, vk, sc, flags, time, elapsed, info)) {
		keytext:= GetKeyNameText(vk, sc, flags & 0x1)
		(elapsed<0)? (elapsed:= "#err#"):(dt:= elapsed*0.001)
		sc_a:= sc ; AHK-style SC
		((flags & 1)?(sc_a |= 0x100))
		flags &= ~1
		sc_a:= SubStr("000" SubStr(sc_a, 3), -2)
		vk_a:= SubStr(vk+0, 3)
		if (StrLen(vk_a)<2)
			vk_a = 0%vk_a%
		StringUpper,vk_a,vk_a
		StringUpper,sc_a,a
		flags:= GetKeyFlagText(flags & ~0x1)	;(s!flags? flags:="Down ")
		; text .= Format("{:-4}{:-5}{:-7}{:-9}{:-10}{:-30}`n", vk_a, sc_a, flags, dt, keytext, info)
		text .= Format(formatting,flags,dt,keytext,info)
	}
}  
settext(text)

; text3:=text 
; sci.SetReadOnly(false)
; sci.SetText(unused,byref text3,0xfffffff)
; sci.SetReadOnly(true)
;colour1 := colour1 + 0x111111
;if (colour1=0xFFFFFF)
;	colour1 :=0x0000ff
	;sci.STYLESETFONT(SCLEX_CONTAINER, "STYLE_DEFAULT")
;sci.STYLESETfore(SCLEX_CONTAINER,colour1)
;msgbox % text2
;GuiControl,,scnt,% text
return,

return(){
	exit,
}

gshow(hw="",xx="",yy="",ww="",hh="") {
	global DTOP,gui_xxx,gui_yyy,gui_hhh,gui_hhw	;static Peee:="hw,x,y,w,h"
	loop,parse,% "hw,xx,yy,ww,hh",`,
	{
		if ((%a_loopfield%)="")			{
			dicks:=("gui_" . a_loopfield)
			if ("gui_" . a_loopfield)	{
				dicks:=("gui_" . a_loopfield)
				(%a_loopfield%):=%dicks%
	}	}	}
	
	Window2Dtop(hw,1,"","","")
	sleep,30
	gui, Show,x%xx% y%yy% h%hh% w%ww% NA,% "no_glass"
	win_move(hw,"","","","","")
}

Window2Dtop(byref Child="",x:=1,y:=45,w:=480,h:=480){
	;global hgui;	( !Child? Child:= hgui ) ; stylemen invoked
	WinGetPos,ChildX,ChildY,Child_W,Child_H, ahk_id %Child%
	Surrogate:=DesktoP()
	DllCall("SetParent","ptr",Child,"ptr",Surrogate) ;msgbox %  Child " " ChildX " " ChildY " " Child_W " " Child_H
	sleep(100)
	WinMove,ahk_id %Child%,,1,45,480,480swa
	return,
}

EncodeInteger(ref, val) {
	return,DllCall("ntdll\RtlFillMemoryUlong", "Uint", ref, "Uint", 4, "Uint", val)
}

SendUnicodeChar(charCode) {
	VarSetCapacity(ki, 28 * 2, 0)
	EncodeInteger(&ki + 0, 1)
	EncodeInteger(&ki + 6, charCode)
	EncodeInteger(&ki + 8, 4)
	EncodeInteger(&ki +28, 1)
	EncodeInteger(&ki +34, charCode)
	EncodeInteger(&ki +36, 4|2)
	DllCall("SendInput", "UInt", 2, "UInt", &ki, "Int", 28)
}

; WM_KEYDOWN() {
	; if A_Gui
		; return,true
; }

WM_MOUSEMOVE() {
	if (A_GuiControl="KHT")
		ToolTip In Flags`ne%A_Tab%=%A_Tab%Extended`na%A_Tab%=%A_Tab%Artificial`n!%A_Tab%=%A_Tab%Alt-Down`nu%A_Tab%=%A_Tab%Key-Up
	else,ToolTip
}

WM_LBUTTONDOWN(wParam, lParam){
	global text
	StringReplace, Clipboard, text, `n, `r`n, All
}


GuiClose:
ExitApp
dtop_dock:
(dwnd:= behind_icon? DesktopAC : (DesktoP()))
return

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
			PostMessage,0x0111,%ID_VIEW_VARIABLES%,,,% (A_ScriptName " - AutoHotkey")
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
	msgbox % _:= (wi:= wingetpos(gui_hw)).y
;	if (poopold & 0x80000000)
;	msgbox fucking sootcunt
	 if ( !((_:= (wi:= wingetpos(gui_hw)).y)=A_Y_HC)) {
		A_Y_HC:= wi.y
		A_X_HC:= wi.x
		msgb0x("New Pos?","Save new Pos?`nX: " A_X_HC "`nY: " A_Y_HC,5,0x43040+289) ; (if moved)
		ifmsgbox,ok
		IniRightz()				;iniwrite_Queued:=True
	}
	;winset,style,^0x80000000,ahk_id %gui_hw%
	;winset,exstyle,^0x20,ahk_id %gui_hw%
	winget,poop,style,ahk_id %gui_hw%
	sleep,200
	(poop & 0x80000000? movable:= true : moveable:= False)
 	menu,tray,% movable?"check":"uncheck",ismovable,% "movabletoggle",winset,transcolor,0x000000,ahk_id %gui_hw%	

	return
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

menutray() {
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
	;timer("HideTray",-36000)
	return,
}

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


return,
gshow("",A_X_HC,A_Y_HC,400,280)	;gshow("",10,405)

mentoggla() {
	global
	varn:= a_thismenuitem
	switch a_thismenuitem {
		case "Pulse Alpha" : varn:= "Pulse"
		default	: varn:= a_thismenuitem
	}
	(%varn%) := !(%varn%)
	if varn
		 try,menu,% A_ThisMenu,check,% a_thismenuitem
	else,try menu,% A_ThisMenu,uncheck,% a_thismenuitem
	return,varn
}

;	_TRAY_WM_	;^^^^^;
ID_TRAY_EXIT:
ID_TRAY_PAUSE:
ID_TRAY_SUSPEND:
ID_VIEW_VARIABLES:
ID_TRAY_EDITSCRIPT:
ID_TRAY_RELOADSCRIPT:
PostMessage,0x0111,(%a_thislabel%),,,% A_ScriptName " - AutoHotkey"
hidetray:
donthidetray:
switch a_thislabel {
	case "donthidetray"	:	timer("hidetray",off)
	case "hidetray"		:	menu,tray,noicon
}
return, ; END / TRAY MENU / END / TRAY MENU / END / TRAY MENU / END / TRAY | / END / TRAY |
return, ; END / TRAY MENU / END / TRAY MENU / END / TRAY MENU / END / TRAY | / END / TRAY |