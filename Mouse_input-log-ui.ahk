;"Desktop wallpaper" Mouse-input feedback ui  ; by M.Wolff
; https://autohotkey.com/boards/viewtopic.php?f=6&t=26059
; Re-Worked code. Custom-ui, Desktop-mount. Flicker eliminated
; To-do : implement Scintilla ;optimize ;add more options
#NoEnv
;#Notrayicon
#Persistent
#MouseHistory(10)
#Singleinstance force
(_:=winexist("ahk_class AutoHotkey","KeyHistory_input-log-uiSCI.ahk"))? (
, A_Y_HC:= 60) : A_Y_HC:= 389, TxtSize:= 11
global A_X_HC:= 8, A_MarginX:= 0, A_MarginY:= 1 			; ordinals ( see-above )
, text_p1_Colour:="c5540aa", text_p1_Font:= "MS gothic" 	; Main font  ; (Monospace working best atm)
, Dtop:= True, behind_icon:= False, MERGE_MOVE := True 		; D-Top host ;		!Behind D-Top-icons?
,A_Y_HC,A_Y_HC,gui_hh,gui_xx,gui_yy,gui_hh_old,gui_visible 	; MERGE_MOVE -> Merge-Mouse-Move-2next-Update
gosub("menutray")
;lgui,new
Gui,+LastFound +E0x02000028 +Hwndgui_hw -DPIScale +toolwindow -caption 
gui,Font,s%TxtSize% %text_p1_Colour%,% text_p1_Font

gui,Add,Text,+0x06000000 +E0x02000028 w400 vMH,% sTrSpacer:=".                                 ." ; <---< Do-Not-Disturb
	guiControlGet,	MH,		Pos		;		-	-get dummy size. -	-	-	(Will have to come out probably)
	guiControl,,	MH 
;WinSet,Transcolor,000000


;winset,ExStyle,-0x80000,ahk_id %Gui_hW%
gui,Margin,MarginX:=0,MarginY:=1
;winset,ExStyle,+0x02000020,ahk_id %txthwnd%

	;l;guiControl,+e0x02000000,	MH, 
gosub,Resize
return,

Resize: 							; Resize label to fit mouse history.
gui_hh:= MHH*(#MouseHistory())
guiControl,Move,MH,h%gui_hh%
gui_hh +=20

gui,+LastFound 						; Determine visibility.
WinGet,style,Style
gui_visible := style ^ 0x10000000 	; Determine current position and height.
WinGetPos, gui_xx, gui_yy, , gui_hh_old

SysGet,	wa_,	MonitorWorkArea
SysGet,	twc_h,	51					;SM_CYSMCAPTION
SysGet,	bdr_h,	8					;SM_CYFIXEDFRAME Initially on the left side.
( !gui_visible)? (gui_xx:= 10, gui_yy:= wa_bottom-(gui_hh +twc_h +bdr_h *2 +10)) :
,((gui_yy +gui_hh //2 > (wa_bottom -wa_top) //2)? gui_yy:= 410)
gshow(gui_hw,A_X_HC,A_Y_HC)			; Move relative to bottom edge when closer to the bottom.
return,

gshow(hw="",xx="",yy="",ww="",hh="") {
	global DTOP,gui_xx,gui_yy,gui_hh,gui_hw
	loop,parse,% "hw,xx,yy,ww,hh",`,
	{
		if ((%a_loopfield%)="") {
			dky:= ("gui_" . a_loopfield)
			(("gui_" . a_loopfield)? dky:= ("gui_" . a_loopfield)
			, (%a_loopfield%):= (%dky%))
	}	}
	Window2Dtop(hw,1,gui_xx,"",gui_hh)
	sleep,30
	gui,Show,x%xx% y%yy% w400 h%gui_hh% NA,% "no_glass b"
	win_move(hw,"","","","","")
}

Show:
SetFormat,FloatFast,.2
text:= "",buf_size:= #MouseHistory()
Loop,% ( buf_size )	{
	SetFormat,IntegerFast,D
	if MouseHistory(A_Index,msg,x,y,mouseData,flags,Time,elapsed) {
		SetFormat,IntegerFast,H
		msg:= (msg + 0) ""
		SetFormat,IntegerFast,D
		switch msg {
			case 0x201,0x202,0x203,0xA1,0xA2,0xA3 : btn := "Left"	; 	WM_LBUTTONDOWN/UP/DBLCLK, WM_NC..
			case 0x204,0x205,0x206,0xA4,0xA5,0xA6 : btn := "Right"	; 	WM_RBUTTONDOWN/UP/DBLCLK, WM_NC..
			case 0x207,0x208,0x209,0xA7,0xA8,0xA9 : btn := "Middle" ; 	WM_MBUTTONDOWN/UP/DBLCLK, WM_NC..
			case 0x20B,0x20C,0x20D,0xAB,0xAC,0xAD : btn := (mouseData & 0x10000) ? "X1(Back)" : "X2"(Fwd) 	; 	WM_XBUTTONDOWN/UP/DBLCLK, WM_NC..
			case 0x20A : mouseData:= mouseData << 32 >> 48, btn:= (mouseData < 0) ? "WheelDn" : "WheelUp"	;	WM_MOUSEWHEEL
			case 0x20E : mouseData:= mouseData << 32 >> 48, btn:= (mouseData < 0) ? "WheeLeft" : "WheelRt"	;	WM_MOUSEHWHEEL
			case 0x20A, 0x20E: clickCount:= Abs(mouseData), (!clickCount? clickCount:="")		; 	WM_MOUSEWHEEL, WM_MOUSEHWHEEL
			case 0x200: btn:= clickCount:= ""		; 	WM_MOUSEMOVE
			case 0x203,0xA3,0x206,0xA6,0x209,0xA9,0x20D,0xAD,: btn:= msg, clickCount:= 2		;	WM_LBUTTONDBLCLK, WM_NC.., ..R/M/XBUTTONDBLCLK
			case 0x201,0x204,0x207,0x20B,0xA1,0xA4,0xA7,0xAB : btn:= msg, clickCount:= "Down"	;	WM_MOUSEWHEEL, WM_MOUSEHWHEEL
			case 0x202,0x205,0x208,0x20C,0xA2,0xA5,0xA8,0xAC : clickCount:= "Up", btn:= msg		;	WM_L/R/M/XBUTTONUP, WM_NC..
			default: TT("Error unhandled message from mouse...`n" msg ":`n" mouseData ":`n" flags)
		}
		text .=((flags & 1) ? "*" : " ") ; formatting:="{:-8}{:-8}{:-10}{:-9}`n"
			;.	SubStr"	  ", 1, 6)(a:=(msg=0x201)?"Dn": (a:=(msg=0x202)?"Up")); (a:=(msg=0x202)?"Up")));"  ",1,8)
			.	SubStr(btn . (a:=(msg=0x201)? "Dn" : (msg=0x202?"Up":"  " )) .  "     ", 1, 7) 
			.	SubStr(" "elapsed/1000.0, -4 ) "  " SubStr("  " x, -4)
			.	SubStr(" " clickCount, -5)
			.	SubStr(" "y, -4 ) "`n"
	}
	else,break,
}
guiControl,,MH,% text
return,

MouseHistory(N,ByRef msg,ByRef x,ByRef y,ByRef mouseData,ByRef flags,ByRef Time, ByRef elapsed=-1) {
	global MouseBuffer	; a more straightforward scope method?
	static mbuf:= MouseBuffer
	if N is not integer
		return,false
	buf_max := #MouseHistory()
	if (N < 1 or N > buf_max)
		return,false
	x			:= NumGet(MouseBuffer, ofs:= (N-1) *24,"int")
	y			:= NumGet(MouseBuffer, ofs+4, "int")
	mouseData	:= NumGet(MouseBuffer, ofs+8, "uint")
	flags		:= NumGet(MouseBuffer, ofs+12,"uint")
	Time		:= NumGet(MouseBuffer, ofs+16,"uint")
	msg			:= NumGet(MouseBuffer, ofs+20,"uint")
	elapsed:= Time - ((Time2:= NumGet(MouseBuffer,N*24+16,"uint"))? Time2 : Time)
	return,!!msg
}

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
	} else	{ ; Unregister the mouse hook.
		if MouseHook	{
			DllCall("UnhookWindowsHookEx","ptr",MouseHook)
			DllCall("GlobalFree","ptr",MouseHookProc)
			MouseHook:= ""
		} ; Clear history entirely.
		VarSetCapacity(MouseBuffer, 0)
}	}

Mouse(nCode,wParam,lParam) { ; Mouse hook callback - records mouse events into MouseBuffer.
	global MouseBuffer,MERGE_MOVE
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
	} ; "gosub Show" slows down the mouse hook and causes problems, so use a Timer.
	return,DllCall("CallNextHookEx","uint",0,"int",nCode,"ptr",wParam,"ptr",lParam,"ptr")
}

Window2Dtop(byref Child="",x:=1,y:=45,w:=480,h:=480){		; Window2Dtop(byref Child="",x:=1,y:=45,w:=480,h:=480){	
	WinGetPos,ChildX,ChildY,Child_W,Child_H, ahk_id %Child%	; global gui_hw;	( !Child? Child:= gui_hw ); stylemen invoked
	Surrogate:=DESKTOP("turds")								; WinGetPos,ChildX,ChildY,Child_W,Child_H, ahk_id %Child%
	DllCall("SetParent","ptr",Child,"ptr",Surrogate)		; (Surrogate:= behind_icon?DesktopAC : (DesktoP("Main")))
	sleep(100)												; DllCall("SetParent","ptr",Child,"ptr",Surrogate)	
	WinMove,ahk_id %Child%,,1,45,480,480swa					; msgbox %  Child " " ChildX " " ChildY " " Child_W " " Child_H; sleep(100)
	return,													; WinMove,ahk_id %Child%,,1,45,480,480swa; return,errorlevel
}															; }

WM_LBUTTONDOWN(wParam, lParam) {
	global text
	StringReplace,Clipboard,text,`n,`r`n,All
}

menutray:
timer("HideTray",-18000)
menu,tray,	add, hide me pls, hidetray
menu,tray,	icon, hide me pls, C:\Icon\32\32.ico
menu,tray,	add, dont hide pls, donthidetray
menu,tray,	icon, dont hide pls, C:\Icon\32\32.ico
return,

hidetray:
donthidetray:
switch a_thislabel {
	case "hidetray"		:	menu,tray,noicon
	case "donthidetray"	:	timer("hidetray",off)
}
return,

guiClose:
ExitApp,