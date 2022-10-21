; https://autohotkey.com/boards/viewtopic.php?f=6&t=26059
; Change Log:
; 2022.05.26
;     Fixed incomplete display of history content.
;     Fixed flicker.
;     Added title and tips.
;     Can run in the background.

#NoEnv
#Persistent
#Notrayicon
#InstallKeybdHook
#Singleinstance force
	SetControlDelay,-1
	    SetWinDelay,-1

dtop:= true, behind_icon:= False
global spos:=1,A_X_HC:=8,A_Y_HC:=250, A_MarginX:=0, A_MarginY:=1 ; master metrics
global formatting:="{:-8}{:-8}{:-10}{:-9}`n"
global ed,gay,keytextold,dtop,behind_icon,gui_hw
global ID_VIEW_VARIABLES:=65407,ID_TRAY_EDITSCRIPT:=65304,ID_TRAY_SUSPEND:=65305,ID_TRAY_PAUSE:=65306,ID_TRAY_EXIT:=65307,ID_TRAY_RELOADSCRIPT:=65303 ; ID_VIEW_VARIABLES,ID_TRAY_EDITSCRIPT,ID_TRAY_RELOADSCRIPT,ID_TRAY_SUSPEND,ID_TRAY_PAUSE,ID_TRAY_EXIT
Process, Exist 
our_pid:=
our_hWnd:= 
hHookKeybd := DllCall("SetWindowsHookEx", "int", 13 ; WH_KEYBOARD_LL = 13
    , "ptr", RegisterCallback("Keyboard")
    , "ptr", DllCall("GetModuleHandle", "ptr", 0, "ptr")
    , "uint", 0, "ptr") ; dwThreadId
#KeyHistory(13)  ; 虽然它用 # 开头，实际却是一个函数

;OnMessage(0x200, "WM_MOUSEMOVE")
;OnMessage(0x201, "WM_LBUTTONDOWN")
;OnMessage(0x100, "WM_KEYDOWN")
OnMessage(0x404,"AHK_NOTIFYICON")
;timer("menutray",-1)
menutray()
; WS_EX_COMPOSITED = E0x02000000 & WS_EX_LAYERED = E0x00080000 -> Double Buffer
;gui, 	APCBackMain:  New,  -DPIScale +toolwindow +owner -SysMenu +AlwaysOnTop, APCBackMain

Gui,+LastFound +E0x02080020 +Hwndgui_hw -DPIScale +toolwindow -caption ;+AlwaysOnTop
Gui,Margin,% A_MarginX,% A_MarginY
Gui,Font,s11,MS gothic 

;Gui, Add, Text, vKHT +0x100, % Format("{:-4}{:-5}{:-7}{:-9}{:-19}{:-30}", "VK", "SC", "Flags", "Elapsed", "Key", "Extra")
Gui,Add,Text,vKHT +0x100,% Format("{:-7}{:-7}{:-9}{:-15}", "Flags", " T", "Key", "Extra")
Gui,Add,Text,vKH,% Format("{:-8}{:-2}{:-10}{:-10}","ea!u","1000.00","Browser_Favtes","KEY_IGNORE_")
GuiControlGet,KHT,Pos
GuiControlGet,KH,Pos
GuiControl,,KH  ; clear dummy sizing text 
if !headers
GuiControl,,KHT  ; clear dummy sizing text 
gosub,Resize
return

#MaxThreadsBuffer, On
!WheelUp::
!WheelDown::
#MaxThreadsBuffer, Off
	history_size := #KeyHistory() + ((A_ThisHotkey="!WheelUp") ? +1 : -1)
	#KeyHistory(history_size>0 ? history_size : 1)
	; Delay resize to improve hotkey responsiveness.
	SetTimer, Resize, -10
return

Resize:
; Resize label to fit key history.
gui_hh:= KHH*#KeyHistory()
GuiControl, Move, KH, h%gui_hh%
gui_hh +=KHY + 10

Gui, +LastFound
; Determine visibility.
WinGet, style, Style
gui_visible:= style & 0x10000000

WinGetPos,gui_xx,gui_yy,,gui_hh_old
SysGet,wa_,MonitorWorkArea
SysGet,twc_h,51 ; SM_CYSMCAPTION
SysGet,bdr_h,8  ; SM_CYFIXEDFRAME
if (!gui_visible) {
	gui_xx = 10 ; Initially on the left side.
	gui_yy:= wa_bottom-(gui_hh+twc_h+bdr_h*2+10)
} else    ; Move relative to bottom edge when closer to the bottom.
	((gui_yy+gui_hh//2 > (wa_bottom-wa_top)//2)? gui_yy := 410)
gshow("",A_X_HC,A_Y_HC)	;gshow("",10,405)
return,

; Gui,Show,x10 y10 h%gui_hh% w340 NA,KeyHist
; Window2Dtop(gui_hw)
; return,

GuiSize:
if (A_EventInfo = 1)
	Gui,Hide
return,

Keyboard(nCode,wParam,lParam) {
	global KeyBuffer
	static sz := 16+A_PtrSize
	Critical
	if KeyHistory(1,vk,sc,flags)
		&& NumGet(lParam+0,"uint") = vk
		&& NumGet(lParam+4,"uint") = sc
		&& NumGet(lParam+8,"uint") = flags
		 buf_max:= 0 ; Don't show key-repeat.
	else,buf_max:= #KeyHistory()
	if (buf_max > 0)
	{	; Push older key events to the back.
		if (buf_max > 1)
			DllCall("RtlMoveMemory","ptr",&KeyBuffer+sz,"ptr",&KeyBuffer,"ptr",buf_max*sz)
		; Copy current key event to the buffer.
		DllCall("RtlMoveMemory","ptr",&KeyBuffer,"ptr",lParam,"ptr",sz)
		; "gosub Show" slows down the keyboard hook and causes problems, so use a timer.        
		SetTimer,Show,-20
	}
	return,DllCall("CallNextHookEx","ptr",0,"int",nCode,"ptr",wParam,"ptr", lParam,"ptr")
}

KeyHistory(N,ByRef vk,ByRef sc,ByRef flags:=0,ByRef time:=0,ByRef elapsed:=0,ByRef info:=0) {
	global KeyBuffer,text,keytext
	static sz:= 16+A_PtrSize
	if N is not integer
		return,false
	buf_max:= #KeyHistory()
	if (N < 0) 
		N += buf_max + 1
	if (N < 1 or N > buf_max)
		return,false
	vk    := NumGet(KeyBuffer, (N-1)*sz,   "uint")
	sc    := NumGet(KeyBuffer, (N-1)*sz+4, "uint")
	flags := NumGet(KeyBuffer, (N-1)*sz+8, "uint")
	time  := NumGet(KeyBuffer, (N-1)*sz+12,"uint")
	info  := NumGet(KeyBuffer, (N-1)*sz+16)
	elapsed:= time - ((time2:= NumGet(KeyBuffer, N*sz+12, "uint")) ? time2 : time)
	if (info=0xFFC3D44F) 
		info:= "!IgnoreL"
	else if (info=0xFFC3D44E)
		info:= "!IgnoreP"
	else if (info=0xFFC3D44D) {
		spos:=-70
		; if keytext {
			; global ed:="(\s*\⇝[\▲*\▼*]+\⇜*\s*(?!\s*[\d*\!*\w*\s*\.*.*]*injected[\s*\n*.*]*$))"
			; global Bd:="(\s*[\▲*\▼*]+\s*(?!\s*[\d*\!*\w*\s*\.*.*]*[\s*\n*.*]*$))"
			; while spos:=regexmatch(text,ed,cap,spos) {
				; switch cap {
					; case " ▲ ":	
						; ups:= " 	 "
						; text:=regexreplace(text,Bd, ups ,,2,spos-50)
					; case " ▼ ":
						; downs:= " ▽ "
						; text:=regexreplace(text,Bd,downs,,2,spos-50)
				; }
				; spos++ ;inc 1
			; }
			; cap:= ""
			; keytextold:= keytext
		; } else,return
		((flags & 0x10)? (info:="injected") : (info := ""))
	} else {
		    (!info=0x0?info:="Error":info:="")
		; if (flags ^ 0x10) ; {
				; info := "Supplanted prev. " 
			; } else if flags & 0x90 {
				; info := "p" ;"!IGNORED
		} ;info := "n
	return,(vk or sc) 
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
		case "up": k:="￪"
		case "down": k:="￬"
		case "left": k:="￩"
		case "right": k:="￫"
		case "pgdn": k:= "PgDn"
		case "enter": k:= "⏎"
		case "backspace": k:= "↰BkSp↰" ; "⟸" ;""BkSp"
		case "printscreen": k:= "PrtScr"
		case "numpadenter": k:= "NumPad⏎" ; ⏎⇦⇚⇐↰←
		case "numpadadd": k:= "NumPad+"
		case "numpadclear": k:= "NumPad￮"
		case "numpaddivide": k:= "NumPad/"
		case "numpadsubtract": k:= "NumPad-"
		case "numpadmultiply": k:= "NumPad-"
		default: StringUpper,k,k
	}
	return,k
}		;else if ( Strlen(k)>8 )	; return,GetKeyName(format("vk{1:02x}sc{3}{2:02x}", vkCode, scanCode, isExtendedKey))

Show:
SetFormat,FloatFast,.2
SetFormat,IntegerFast,H
buf_size:= #KeyHistory(), text:= ""
Loop,% (buf_size) {
	if (KeyHistory(buf_size+1-A_Index, vk, sc, flags, time, elapsed, info)) {
		keytext:= GetKeyNameText(vk, sc, flags & 0x1)
		(elapsed<0)?(elapsed:="#err#"):(dt:=elapsed*0.001)
		sc_a:= sc ; AHK-style SC
		((flags & 1)?(sc_a |= 0x100))
		flags &= ~1
		sc_a:= SubStr("000" SubStr(sc_a, 3), -2)
		vk_a:= SubStr(vk+0, 3)
		((StrLen(vk_a)<2)? vk_a:= ("0" . vk_a))
		StringUpper,vk_a,vk_a
		StringUpper,sc_a,sc_a
		flags:= GetKeyFlagText(flags & ~0x1)	;(s!flags? flags:="Down ")
		; text .= Format("{:-4}{:-5}{:-7}{:-9}{:-10}{:-30}`n", vk_a, sc_a, flags, dt, keytext, info)
		text .= Format(formatting,  flags, dt, keytext, info)
	}
}
GuiControl,,KH,% text
return,

return(){
	exit,
}

GuiClose:
ExitApp
dtop_dock:
(dwnd:= behind_icon? DesktopAC : (DesktoP()))
menutray() {
	Menu,Tray,NoStandard
	Menu,Tray,Add,Show,MenuHandler
	Menu,Tray,Add,dTop d0ck,MenuHandler
	Menu,Tray,Add,Freeze,MenuHandler
	Menu,Tray,Add,Exit,MenuHandler
	Menu,Tray,Standard ;Menu, Tray, Default, Show
	return,
}

MenuHandler() {
global gui_hw
	switch A_ThisMenuItem {
		case "Freeze":
			if (A_IsPaused)	{
				WinSetTitle,ahk_id %gui_hw%,,Key History
				Menu,Tray,UnCheck,Freeze
			} else {
				WinSetTitle, ahk_id %gui_hw%,,Key History - Freezed
				Menu,Tray,Check,Freeze
			}
			Pause,Toggle
		case "Show":
			Gui, Show, NA,no_glass 
			if (!A_IsPaused)	{
				Menu,Tray,Check,Freeze
				Pause On
			}
		case "dTop d0ck":
			Window2Dtop(gui_hw)
		case "Exit":
			ExitApp,
		default:
			(islabel(A_thismenuitem)?Timer(A_thismenuitem,-1),return())
			Traytip,Hangler Error,Uknown menu item please consult the master,3,33
}	}
gshow("",A_X_HC,A_Y_HC)	;gshow("",10,405)
return

gshow(hw="",xx="",yy="",ww="",hh="") {
	global DTOP,gui_xxx,gui_yyy,gui_hhh,gui_hhw	;static Peee:="hw,x,y,w,h"
	loop,parse,% "hw,xx,yy,ww,hh",`,
	{
		if ((%a_loopfield%)="") 	  {
			dicks:= ("gui_" . a_loopfield)
			if ("gui_" . a_loopfield) {
				dicks:= ("gui_" . a_loopfield)
				(%a_loopfield%):= %dicks%
	}	}	}
	
	Window2Dtop(hw,1,"","","")
	sleep,30
	gui,Show,x%xx% y%yy% h%hh% NA,% "no_glass" ;win_move(hw,"","","","","")
	gui,color,0x000000
	winset,transcolor,off,ahk_id %gui_hw%
	winset,transcolor,0x000000,ahk_id %gui_hw%	

}

Window2Dtop(byref Child="",x:=1,y:=45,w:=480,h:=480,behind_icon:=False){
	;global gui_hw
;	( !Child? Child:= gui_hw ) ; stylemen invoked
	WinGetPos,ChildX,ChildY,Child_W,Child_H, ahk_id %Child%
	(Surrogate:=behind_icon?DesktopAC:(DesktoP()))
	DllCall("SetParent","ptr",Child,"ptr",DesktopAC)
	sleep(100)
	WinMove,ahk_id %Child%,,1,45,480,480swa
	return,errorlevel
}


WM_KEYDOWN() {
	if A_Gui
		return,true
}

WM_MOUSEMOVE() {
	if (A_GuiControl="KHT")
		ToolTip In Flags`ne%A_Tab%=%A_Tab%Extended`na%A_Tab%=%A_Tab%Artificial`n!%A_Tab%=%A_Tab%Alt-Down`nu%A_Tab%=%A_Tab%Key-Up
	else ToolTip
}

WM_LBUTTONDOWN(wParam, lParam){
    global text
    StringReplace, Clipboard, text, `n, `r`n, All
}

AHK_NOTIFYICON(wParam, lParam) { ; 0x201: ; WM_LBUTTONDOWN   ; 0x202:; WM_LBUTTONUP
	;static global TrayWMinit:= OnMessage(0x404,"AHK_NOTIFYICON") ;Thread,Priority,0 || ;Thread,Priority,7
	switch lParam { 
	;	case 0x0021: ;	WM_MOUSEACTIVATE
		;   return,% (ret:=refresh_uptime_(True))
	   ;case 0x204:  ;	WM_RBUTTONDN
		;	return,% MENSpunction()
			;MENSpunction()
			;return,
		case 0x203:  ;	WM_LBUTTONDBLCLK
			tt("Loading...") ; timer("ID_VIEW_VARIABLES",-1)
			PostMessage,0x0111,65407,,,% (A_ScriptName "  - AutoHotkey")
		case 0x0208: ;	WM_MBUTTONUP
			tt("Reloading... 1 sec",900)
			settimer,ID_TRAY_RELOADSCRIPT,-1
;	    case 0x205:  ;	WM_RBUTTONUP
	;		return,(trayActiv?MENSpunction())
			; menuTrayUndermouse() experimental fail 
}	}

;  TRAY WM_ ;^^^^^;
ID_TRAY_EXIT:
ID_TRAY_PAUSE:
ID_TRAY_SUSPEND:
ID_VIEW_VARIABLES:
ID_TRAY_EDITSCRIPT:
ID_TRAY_RELOADSCRIPT:
PostMessage,0x0111,(%a_thislabel%),,,% A_ScriptName " - AutoHotkey"
return, ; END / TRAY MENU / END / TRAY MENU / END / TRAY MENU / END / TRAY | / END / TRAY |

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