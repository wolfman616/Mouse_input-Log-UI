#NoEnv
#Persistent
;#Notrayicon
#InstallKeybdHook
#Singleinstance Force
#KeyHistory(KEY_HIST_MAX:= 12)
#Include <SCIaa>
DetectHiddenWindows,On
DetectHiddenText,	On
SetTitleMatchMode,	2
SetTitleMatchMode,	Slow
	SetControlDelay,-1
		SetWinDelay,-1
	  Setbatchlines,-1
fileexist(A_scriptdir "\" A_ScriptName ".ini") ?readini() : ()
global autohidetray:= True, AutoHideTray_T:= -96000
, spos:= 1, A_X_HC:= 1, A_Y_HC ;Master X/Y Pos;
, A_MarginX:= 0, A_MarginY:= 2 ; Master-<<etrics ;
, wtemp:= strlen( SizingTemplate:= " *WHEEL UP   0.000   9999    9999") *10 ; <-<-------do-not-disturb
;(_:= winexist("Mouse_input-log-ui.ahk"))? A_X_HC:= 280 : A_Y_HC:= 60
SciLexer:= A_ScriptDir . (A_PtrSize==8? "\SciLexer64.dll" : "\SciLexer32.dll")
if(!LoadSciLexer(SciLexer)) {
	MsgBox,0x10,%g_AppName% - Error
, % "Failed to load library """ . SciLexer . """.`n`n"
	exitapp,
}

	gosub,Varz
	gosub,MenuTray
	gosub,onmsgz
;=-------=====-==========----------=====-==========----------=====-==========----------====

Gui,+LastFound +E0x0a090028 +Hwndgui_hw -DPIScale +toolwindow -caption

sci_init()
Gui,Margin,% A_MarginX,% A_MarginY
Gui,Font,s11,MS gothic ;Gui,Add,Text, vKH,% Format(formatting,"ea!u","1000.00","Browooes","KEY_IaGNORE_")
GuiControlGet,scint,Pos
GuiControlGet,KHT,Pos
GuiControlGet,KH,Pos 
GuiControl,,KH		;clear dummy sizing text ;sci.SetText(unused, text,0xEE0000)
if(!headers)
	GuiControl,,KHT	;clear dummy sizing text 
gosub,Resize
return,

Resize: ; Resize label to fit key history.
gui_hh:= KHH*#KeyHistory()
GuiControl,Move,KH, h%gui_hh%
gui_ww:= wtemp +8
gui_hh += KH +10
;GuiControl, Move, scint, w100 x-10
Gui, +LastFound
; Determine visibility.
WinGet, style, Style
gui_visible:= style & 0x10000000
WinGetPos,gui_xx,gui_yy,,gui_hh_old
SysGet,wa_,MonitorWorkArea
SysGet,twc_h,51 ; SM_CYSMCAPTION
SysGet,bdr_h,8  ; SM_CYFIXEDFRAME
if(!gui_visible) {
	gui_xx = 0 ; Initially on the left side.
	gui_yy:= wa_bottom-(gui_hh+twc_h+bdr_h*2+10)
} else,((gui_yy+gui_hh//2 > (wa_bottom-wa_top)//2)? gui_yy:= 410) ; Move relative to bottom edge when closer to the bottom.
gShow("",A_X_HC,A_Y_HC,gui_ww,420)	;gShow("",10,405) ;win_move(gui_hw,2,,,,"") ;WinGetPos,gui_xx,gui_yy,,gui_hh_oldNN
return, ; Gui,Show,x10 y10 h%gui_hh% w340 NA,KeyHist ; Window2Dtop(gui_hw) ; return, GuiSize:


;=--- --- -=== ==-= === ====== - -- -- -- -=====- ====== ====-- ---- ----== ==  =-== == = ===  ==- --- --- -- -=== = =-== == == ===---
  
GuiClose:
ExitApp,

OnMsgz:
rPiD := rP_ID_Gui("KHIST")OnExit("AtExit")
OnMessage(0x100,"WM_KEYDOWN")
OnMessage(0x404,"AHK_NOTIFYICON")
hHookKeybd:= DllCall("SetWindowsHookEx","int",13 ;WH_KEYBOARD_LL=13
			,"ptr",RegisterCallback("Keyboard")
			,"ptr",DllCall("GetModuleHandle","ptr",0,"ptr")
			,"uint",0,"ptr") ; dwThreadId
return,


DTOP_Dock:
(dwnd:= behind_icon? DesktopAC : (DesktoP()))
return,

sci_init(){
	global
	Sci:= New scintilla(gui_hw) ;,headers,KH,KHT,txt_hw,scint
		winset,transcolor,000000

	Sci.SetCodePage(65001) 	; UTF-8
	sci.SetWrapMode(true) 	; set default font up;STYLE_DEFAULT := 32
	sci.StyleSetFont(32,"ms gothic","ms gothic"),sci.stylesetsize(32,11)
	, sci.StyleSetFore(STYLE_DEFAULT,0x995566), sci.STYLESETBACK(STYLE_DEFAULT,0x000000)
	, sci.SETWHITESPACEback(STYLE_DEFAULT,0x000000),sci.StyleSetBold(STYLE_DEFAULT, False)
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
	;Gui,Add,Custom,ClassScintilla vSCint +e0x02080000 +hwndtxt_hw x0 y100 w400 h300 r0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12
}

settext(byref txt,byref obj="sci") {
	global (%obj%)
	,(%obj%).SetReadOnly(false)
	,(%obj%).SetText(unused,txt,0xfffffff)
	; ,searchrep(keynames,"3")
	; ,searchrep(bold,"2") 
	; ,searchrep(injecteds,"2") 
	; ,searchrep(INjectionCHAR,"1") 
	; ,searchrep(notlogignoreCHAR,"1") 
	; ,searchrep(notphysCHAR,"1") 
	Needle:=" 	
	(LTrim Join Comments
	ODims)
	((?:^|\s);[^\n]+)					; Comments
		|(^\s*\/\*.+?\n\s*\*\/)			; Multiline comments
		|((?:^|\s)#[^ \t\r\n,]+)		; Directives
		|([+*!~&\/\\<>^|=?:
			,().```%{}\[\]\-]+)			; Punctuation
		|\b(0x[0-9a-fA-F]+|[0-9]+)		; Numbers
		|(""[^""\r\n]*"")				; Strings
		|\b(A_\w*|" keynames ")\b		; A_Builtins
		|\b(" bold ")\b					; bold
		|\b(" injecteds ")\b			; injecteds
		|\b(" INjectionCHAR ")\b		 j; INjectionCHAR
		|\b(" notlogignoreCHAR ")\b		; notlogignoreCHAR
		|\b(" notphysCHAR ")\b			; notphysCHAR
		|(([a-zA-Z_$]+)(?=\())			; Functions
		|(^\s*[A-Z()-\s]+\:\N)			; Descriptions
	)"
	searchrep(Needle,byref obj="sci") 

	,(%obj%).SetReadOnly(true)
}


matchval(Pos="",Len="",IDNum="",Colour="",byref match="") {
	static obj:="sci"
	critical
	(IDNum=16)? sci_style:= "Style_DEFAULT" : sci_style:= "SCE_AHKL_USERDEFINED" . IDNum
	; ((Font!=""?)	(%obj%).StyleSetFont(sci_style, Font , Font ))
	; (Size!=""?	(%obj%).StyleSetSize(sci_style, Size ))
	; (Italic!=""?	(%obj%).StyleSetItalic(sci_style,(( Italic ="off")? False : (Italic="on"?True))))
	; (Bold!=""?	(%obj%).StyleSetBold(sci_style,(( Bold ="off")? False : (Bold="on"?True))))
	; (Underline!=""? (%obj%).STYLESETUNDERLINE(sci_style,(( Underline ="off")? False : (Underline="on"?True))))
	return,((colour)? sci.StyleSetFore((%sci_style%),dix[IDNum].colour):())
		,sci.StartStyling(Pos-1)
		,sci.SetTargetRange(Pos,Pos+Len)
		,sci.SetStyling(Len,(%sci_style%))
}

/* 
;if(matchval(Pos,Len,a_index,colz[ a_index ])) {;if(matchval(FoundPos,Match.Len(),a_index,Dix[ a_index ].Colour,Dix[ a_index ].Font, Dix[ a_index ].Size, Dix[ a_index ].Italic, Dix[ a_index ].Bold, Dix[ a_index ].Underline)) { 
*/

searchrep(TheNeedle,userstylenum="1",byref obj="sci") {
	while(FoundPos:= regexmatch(txt,TheNeedle,Match,pos)) {
		loop,16
			if(Match.Value(a_index)){
				matchval(FoundPos,Match.Len(),a_index,dix[ a_index ].Colour,Match)
				break,
			}
		Pos:= FoundPos +Match.Len()+1
		continue,
		global (%obj%)
		pSearch:= StrSplit(TheNeedle,",")
		loop,% (pSearch.Length(), cnt:=0) {
			if((SearchText:= pSearch[A_Index]) = "")
				Continue
			If(!StringLength:= StrPut(SearchText, "UTF-8") - 1) {
				(%obj%).IndicatorClearRange(0, (%obj%).GetLength())
				Return
			}
			TextLength:= (%obj%).GetLength(), cnt++
			,(%obj%).SetTargetRange(0,TextLength)
			,Length:= StrPut(SearchText,"UTF-8") -1
			,VarSetCapacity(StrBuf,Length)
			,StrPut(SearchText,&StrBuf,"UTF-8")
			While((%obj%).SearchInTarget(StringLength,&StrBuf)!=-1) { ; Algumist, RRR
				TargetStart:= 	(%obj%).GetTargetStart()
				, TargetEnd:= 	(%obj%).GetTargetEnd()
				, TargetLen:= 	TargetEnd-TargetStart
				If(!TargetLen) {
					(%obj%).SetTargetRange(++TargetEnd,TextLength)
					continue, ; Zero-length match (Scintilla RegEx)
				}
				(%obj%).StartStyling(TargetStart) ; colors from start to end
				,anus:=Mod(cnt,7)
				switch,anus { ;case "1","2","3","4","5","6","7","8","9","0":
					default:
						(%obj%).SetStyling(Length,SCE_AHKL_USERDEFINED%userstylenum%)
						,(%obj%).SetTargetRange(TargetEnd,TextLength)
				}
			}
		}
	}
}

#MaxThreadsBuffer,On
!WheelUp::
!WheelDown::
#MaxThreadsBuffer,Off
history_size:= #KeyHistory() +((A_ThisHotkey="!WheelUp")? +1 : -1)
(#KeyHistory(history_size>0)? history_size:= 1)
SetTimer,Resize,-10 ; Delay resize to improve hotkey responsiveness.
return,

; Resize: 							; Resize label to fit mouse history.
; gui_hh:= A_H_HC, gui_ww:= wtemp+8
; guiControl,Move,MH,h%gui_hh%
; gui_hh +=40
; gui,+LastFound 						; Determine visibility.
; WinGet,style,Style
; gui_visible:= style ^0x10000000 	; Determine current position and height.
; WinGetPos,gui_xx,gui_yy,,gui_hh_old
; SysGet,wa_,	MonitorWorkArea
; SysGet,twc_h,	51					;SM_CYSMCAPTION
; SysGet,bdr_h,	8					;SM_CYFIXEDFRAME Initially on the left side.
; ( !gui_visible)? (gui_xx:= 10, gui_yy:= wa_bottom-(gui_hh +twc_h +bdr_h *2 +10)) :
; ,((gui_yy +gui_hh //2 > (wa_bottom -wa_top) //2)? gui_yy:= 410)
; gshow(gui_hw,A_X_HC,A_Y_HC,gui_ww,gui_hh) ; Move relative to bottom edge when closer to the bottom.
; guiControl,+e0x0a000020 +0x06000000,MH
; return,


Show:
SetFormat,FloatFast,.2
SetFormat,IntegerFast,H
text:= "", buf_size:= #KeyHistory()
Loop,% 	 ( buf_size ) {
	if(KeyHistory(buf_size+1-A_Index,vk, sc, flags, time, elapsed, info)) {
		(elapsed<0)? (elapsed:= "#err#"):(dt:= elapsed*0.001)
		, keytext:= GetKeyNameText(vk,sc,flags &0x1)
		, sc_a:= sc ; AHK-style SC
		, ((flags &1)?(sc_a|=0x100))
		, flags&=~1
		, sc_a:= SubStr("000" SubStr(sc_a,3),-2)
		, vk_a:= SubStr(vk+0,3)
		if(StrLen(vk_a)<2)
			vk_a=0%vk_a%
		StringUpper,vk_a,vk_a
		, StringUpper,sc_a,a
		flags:= GetKeyFlagText(flags&~0x1)	;(s!flags? flags:="Down ") ; text .= Format("{:-4}{:-5}{:-7}{:-9}{:-10}{:-30}`n", vk_a, sc_a, flags, dt, keytext, info)
		, text.=Format(formatting,flags,dt,keytext,info)
}	}
settext(text)
return,

Keyboard(nCode,wParam,lParam) {
	global KeyBuffer
	static sz:= 16+A_PtrSize
	Critical
	if(KeyHistory(1,vk,sc,flags))
		&& NumGet(lParam+0,"uint") = vk
		&& NumGet(lParam+4,"uint") = sc
		&& NumGet(lParam+8,"uint") = flags
		 buf_max:= 0 ; Don't show key-repeat.
	else,buf_max:= #KeyHistory()
	if(  buf_max>0  ) {	; Push older key events to the back.
		if(MERGE_MOVE && NumGet(KeyBuffer,20,"uint")=0x200) { ; Update the most recent (mouse-move) event.
		DllCall("RtlMoveMemory","ptr",&KeyBuffer,"ptr",lParam,"ptr",20)
	  } else,if(buf_max>1)
			DllCall("RtlMoveMemory","ptr",&KeyBuffer+sz,"ptr",&KeyBuffer,"ptr",buf_max*sz)
			; Copy current key event to the buffer.
		DllCall("RtlMoveMemory","ptr",&KeyBuffer,"ptr",lParam,"ptr",sz)
		SetTimer,Show,-10
	}
	return,DllCall("CallNextHookEx","ptr",0,"int",nCode,"ptr",wParam,"ptr", lParam,"ptr")
}

KeyHistory(N,ByRef vk,ByRef sc,ByRef flags:=0,ByRef time:=0,ByRef elapsed:=0,ByRef info:=0) {
	global KeyBuffer,text,keytext,INjectionCHAR,notlogignoreCHAR,notphysCHAR
	static sz:= 16+A_PtrSize
	if N is not integer
		return,false
	buf_max:= #KeyHistory()
	if(N<0) 
		N+=buf_max +
	if(N<1 || N>buf_max)
		return,false
	vk		:= NumGet(KeyBuffer,(N-1)*sz,   "uint")
	,sc		:= NumGet(KeyBuffer,(N-1)*sz+4, "uint")
	,flags	:= NumGet(KeyBuffer,(N-1)*sz+8, "uint")
	,time	:= NumGet(KeyBuffer,(N-1)*sz+12,"uint")
	,info	:= NumGet(KeyBuffer,(N-1)*sz+16)
	elapsed	:= time -((time2:= NumGet(KeyBuffer,N*sz+12,"uint"))? time2 : time)
	((info =0xFFC3D44F)? info:= notlogignoreCHAR : ((info=0xFFC3D44E)? info:= notphysCHAR :((info=0xFFC3D44D)? (spos:= -70, ( ( flags &0x10)? (info:= INjectionCHAR) : (info= ""))) :(!info=0x0? info:= "Error" : info:= ""))))
	return,(vk||c) 
}
 
GetKeyFlagText(flags) { ;													;"↑":"↓")
																			;"↗" :"↙")
	return,( (flags & 0x01) ? "e" : "")		; LLKHF_EXTENDED				;"⇗" :"⇙")
	. 		((flags & 0x30) ? "" : "")		; LLKHF_ALTDOWN 				;"↿ ":" ⇂")
	. 		((flags & 0x20) ? "!" : " ")	; LLKHF_ALTDOWN					;"☆":"★")
	. 		((flags & 0x10) ? "⇝" : " ")	; LLKHF_INJECTED (artificial) 	;"↥" :"↧")
	.		((flags & 0x80) ? ((flags & 0x10) ? "▲" : "△" ) : ((flags & 0x10) ?  "▼" : "▽" ))
	. 		((flags & 0x10) ? "⇜" : " ")	; LLKHF_INJECTED (artificial)
	. 		((flags & 0x20) ? "!" : " ")	; LLKHF_ALTDOWN
	. 		((flags & 0x30) ? "" : "")		; LLKHF_ALTDOWN
} 

#KeyHistory(NewSize="") {
	global KeyBuffer
	;critical
	static sz:= 12+A_PtrSize
	if(NewSize="") ; Get current history length.
		return,(cap:= VarSetCapacity(KeyBuffer)//sz)>0 ? cap-1 : 0
	if(NewSize) {
		new_cap:= (NewSize+1)*sz
		,cap:= VarSetCapacity(KeyBuffer)
		,(cap>new_cap? cap:= new_cap)
		varSetCapacity(old_buffer,cap)	; Back up previous history.
		DllCall("RtlMoveMemory","ptr",&old_buffer,"ptr",&KeyBuffer,"ptr",cap) ; Set new history length.
		VarSetCapacity(KeyBuffer,0) ; FORCE SHRINK
		VarSetCapacity(KeyBuffer,new_cap,0) ; Restore previous history.
		DllCall("RtlMoveMemory","ptr",&KeyBuffer,"ptr",&old_buffer,"ptr",cap) ; (Remember N+1 key events to simplify calculation of the Nth key event's elapsed time.)
		; Put tick count so the initial key event has a meaningful value for "elapsed".
		NumPut(A_TickCount,KeyBuffer,12, "uint")
	} else,VarSetCapacity(KeyBuffer,0) ; Clear history entirely.
}

GetKeyNameText(vkCode, scanCode, isExtendedKey) { ;Gets readable key name,usually identical to the name in KeyHistory.
		; if ( Strlen(k)<4 ) ;StringUpper, k, k
	switch,k:= GetKeyName(format("vk{1:02x}sc{3}{2:02x}",vkCode,scanCode,isExtendedKey)) {
		case "up" :				return,k:="￪"
		case "down" :			return,k:="￬"
		case "left" :			return,k:="￩"
		case "right" :			return,k:="￫"
		case "pgdn" :			return,k:= "PgDn"
		case "enter" :			return,k:= "⏎"
		case "backspace" :	k:= "↰" ; "⟸" ;""BkSp"
		case "printscreen" :	return,k:= "PrtScr"
		case "numpadenter" :	return,k:= "NumPad⏎" ; ⏎⇦⇚⇐↰←		
		case "numpadadd" :		return,k:= "NumPad+"
		case "numpadclear" :	return,k:= "NumPad￮"
		case "numpaddivide" :	return,k:= "NumPad/"
		case "numpadsubtract" :	return,k:= "NumPad-"
		case "numpadmultiply" :	return,k:= "NumPad-"
		case "lcontrol":		return,k:= "LCtrl"
		default : if(strlen(k)<4) ;keynames .="↰BkSp↰","PrtScr","NumPad￮","NumPad/","NumPad-","⏎","pgdn","￫","￩","￬","￪",,,,,,
					StringUpper,k,k
	}
	keynames.=k . ","
	return,k ;else if ( Strlen(k)>8 )	; return,GetKeyName(format("vk{1:02x}sc{3}{2:02x}", vkCode, scanCode, isExtendedKey))
}

gShow(hw="",xx="",yy="",ww="",hh="") {
	global DTOP,gui_xxx,gui_yyy,gui_hhh,gui_hhw	;static Peee:="hw,x,y,w,h"
	loop,parse,% "hw,xx,yy,ww,hh",`,
	{
		if((%a_loopfield%)="") {
			dicks:=("gui_" . a_loopfield)
			if("gui_" . a_loopfield)	{
				dicks:=("gui_" . a_loopfield)
				,(%a_loopfield%):=%dicks%
	}	}	}
	Window2Dtop(hw,1,"","","")
	sleep,30
	gui,Show,x%xx% y%yy% h%hh% w%ww% NA,% "no_glass"
	,win_move(hw,"","","","","")
}

Window2Dtop(byref Child="",x:=1,y:=45,w:=480,h:=480){
	;global hgui;	( !Child? Child:= hgui ) ; stylemen invoked
	WinGetPos,ChildX,ChildY,Child_W,Child_H, ahk_id %Child%
	Surrogate:= DesktoP()
	DllCall("SetParent","ptr",Child,"ptr",Surrogate) ;msgbox %  Child " " ChildX " " ChildY " " Child_W " " Child_H
	sleep(100)
	WinMove,ahk_id %Child%,,1,45,480,480swa
	return,
}

EncodeInteger(ref,val) {
	return,DllCall("ntdll\RtlFillMemoryUlong", "Uint", ref, "Uint", 4, "Uint", val)
}

SendUnicodeChar(charCode) {
	VarSetCapacity(ki,28 *2,0)
	,EncodeInteger(&ki +0,1)
	,EncodeInteger(&ki +6,charCode)
	,EncodeInteger(&ki +8,4)
	,EncodeInteger(&ki +28,1)
	,EncodeInteger(&ki +34,charCode)
	,EncodeInteger(&ki +36,4|2)
	DllCall("SendInput","UInt",2,"UInt",&ki,"Int",28)
}

movtogl() {
	global
	winget,p2old,style,ahk_id %gui_hw% 
	winset,transcolor,off,ahk_id %gui_hw%
	_:= (wi:= wingetpos(gui_hw)).y ; if (p2old & 0x80000000) ; &&
	 if(!((_:= (wi:= wingetpos(gui_hw)).y)=A_Y_HC)) {
		A_Y_HC:= wi.y, A_X_HC:= wi.x, msgb0x("New Pos?"
		,"Save new Pos?`nX: " A_X_HC "`nY: " A_Y_HC,5,0x43040+289) ; (if moved)
		ifmsgbox,ok
			IniRightz() ; iniwrite_Queued:=True
	} sleep,200
	winget,p2,style,ahk_id %gui_hw%
	(p2 &0x80000000? movable:= true : moveable:= False)
	(movable?buggeroff:="check":buggeroff:="uncheck")
	menu,tray,% buggeroff,ismovable,% "movabletoggle",winset,transcolor,0x000000,ahk_id %gui_hw%
	return,
}


IniRightz() {
	global
	g:= wingetpos(gui_hw)
	settingsString:= "A_X_HC," . g.x . ",A_Y_HC," . g.y . ","
	msgbox
	IniWrite,%settingsString%,%A_scriptdir%\%A_ScriptName%.ini,settings,settingsall
	return,!errorlevel
}
 
readini() {
	local varname
	IniRead,settingsall,%A_Scriptfullpath%.ini,settings,settingsall
	if !settingsall {
		msgbox,% "no setings"
		exit,
	} else,loop,parse,settingsall,`,
	{
		 (varname? (global (%varname%):= a_loopfield, varname:="") : varname:= a_loopfield)
	}
}

WM_MOUSEMOVE() {
	if (A_GuiControl="KHT")
		ToolTip In Flags`ne%A_Tab%=%A_Tab%Extended`na%A_Tab%=%A_Tab%Artificial`n!%A_Tab%=%A_Tab%Alt-Down`nu%A_Tab%=%A_Tab%Key-Up
	else,ToolTip
}

WM_LBUTTONDOWN(wParam, lParam){
	global text
	StringReplace,Clipboard,Text,`n,`r`n,All
}

rP_ID_Gui(gui_name="") { ; Attain Identity of this script ;
	(gui_name=""? gui_name:= a_ScriptName)
	global WtL:=  gui_name . " ahk_Class AutoHotkeyGUI"
	gui,higGireR: New,,XLab
	gui,higGireR: Show,Hide w10 h10
	return,DllCall("GetCurrentProcessId")
}

AHK_NOTIFYICON(wParam, lParam) {	; 0x201: ; WM_LBUTTONDOWN ; 0x202:; WM_LBUTTONUP
	;Thread,Priority,0 || ;Thread,Priority,7 ; 0x020B:; WM_XBUTTONDOWN
	switch lParam {
	;	Case 0x0200 : refresh_uptime_(True) ; WM_MOUSEmove ; return,% Refresh_uptime_(True)
		Case 0x204 : 
			PostMessage,0x0111,65300,,,% A_ScriptName " - AutoHotkey"
			tt("koons")
			;MENS()
		Case 0x203 : TT("Loading...") ; timer("ID_VIEW_VARIABLES",-1);	WM_LBUTTONDBLCLK
			PostMessage,0x0111,%ID_VIEW_VARIABLES%,,,% (A_ScriptName " - AutoHotkey")
			winget,h,id,WinEvent.ahk - AutoHotkey
	; 	Case 0x205 :  PostMessage,0x0111,65300,,,% A_ScriptName " - AutoHotkey" ;WM_RBUTTONUP
	;	Case 0x0208:; WM_MBUTTONUP ;;timer("ID_TRAY_RELOADSCRIPT",-1); TT("Reloading... 1 sec",900); sleep,900; reload ; return
		;default:
	}
}

menutray:
menu,tray,noStandard
menu,tray,add,ismovable,movtogl
menu,tray,add,hide me pls, hidetray
menu,tray,icon,hide me pls, C:\Icon\32\32.ico
menu,tray,add,dont hide pls, donthidetray
menu,tray,icon,dont hide pls, C:\Icon\32\32.ico
menu,tray,Add,%	"Open",			MenHandlr
menu,tray,Icon,% "Open",%		 "C:\Icon\64ribbon\regview3264.ico",,32
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
if(autohidetray)
	settimer,HideTray,% autohidetray_T ; Tray AutoHide ;
return,


/*  ; Notes for popup: NP++; ahk_id 0x2e1120 PID: 8332; process name AutoHotkey.exe; Title Get Parameters; AHK_Class AutoHotkeyGUI; Style / ExStyle 0x940A0000 - 0x00000088; Control Edit1 C_hwnd: 0x130c78 ; Style / ExStyle 0x50010080 - 0x00000200
ID_TRAY_OPEN := 65300
ID_VIEW_LINES := 65406
ID_FILE_RELOADSCRIPT := 65400 ;ID_TRAY_RELOADSCRIPT := 65303
ID_FILE_EDITSCRIPT := 65401 ;ID_TRAY_EDITSCRIPT := 65304
ID_FILE_WINDOWSPY := 65402 ;ID_TRAY_WINDOWSPY := 65302
ID_FILE_PAUSE := 65403 ;ID_TRAY_PAUSE := 65306
ID_FILE_SUSPEND := 65404 ;ID_TRAY_SUSPEND := 65305
ID_FILE_EXIT := 65405 ;ID_TRAY_EXIT := 65307
ID_VIEW_VARIABLES := 65407
ID_VIEW_HOTKEYS := 65408
ID_VIEW_KEYHISTORY := 65409
ID_VIEW_REFRESH := 65410
ID_HELP_USERMANUAL := 65411 ;ID_TRAY_HELP := 65301
ID_HELP_WEBSITE := 65412
*/

MenHandlr(isTarget="") {
	global
	listlines,off
	switch,nus:= a_thismenuitem {
		case "Open Containing": TT("Opening "   a_scriptdir "..." Open_Containing(A_scriptFullPath),1)
		case "Edit Script": PostMessage,0x0111,65304,,,% A_ScriptName " - AutoHotkey"
		case "Suspend VKs": sendMessage,0x0111,65305,,,% A_ScriptName " - AutoHotkey"
		case "pAUSE": PostMessage,0x0111,65306,,,% A_ScriptName " - AutoHotkey"
		case "Open" : PostMessage,0x0111,65300,,,% A_ScriptName " - AutoHotkey"
		case "reload" : PostMessage,0x0111,65400,,,% A_ScriptName " - AutoHotkey"
		case "exit" : PostMessage,0x0111,65307,,,% A_ScriptName " - AutoHotkey"
		default: islabel(a_thismenuitem)? timer(a_thismenuitem,-10) : ()
	}
	return,
}

mentoggla() {
	global
	varn:= a_thismenuitem
	switch,a_thismenuitem {
		case "Pulse Alpha" : varn:= "Pulse"
		default	: varn:= a_thismenuitem
	}
	(%varn%):=!(%varn%)
	if(varn)
		 try,menu,% A_ThisMenu,check,% a_thismenuitem
	else,try,menu,% A_ThisMenu,uncheck,% a_thismenuitem
	return,varn
}
 


Varz:
global colour1:= 0xEE0000, dtop:= true, behind_icon:= False
, INjectionCHAR:= "", notlogignoreCHAR:= "", notphysCHAR:= ""
, formatting:="{:1}`t{:1}`t{:1}`n", keynames:= "lctrl,"
, keytextold, dtop,behind_icon, our_pid:= our_hWnd:= ""
, ID_TRAY_PAUSE,ID_TRAY_EXIT, ID_VIEW_VARIABLES:= 65407,ID_TRAY_EDITSCRIPT:= 65304
, ID_TRAY_SUSPEND:= 65305, ID_TRAY_PAUSE:= 65306,ID_TRAY_EXIT:= 65307,ID_TRAY_RELOADSCRIPT:= 65303
, bold:= "0.,1.,2.,3.,4.,5.,6.,7.,8.,9..0,.1,.2,.3,.4,.5,.6,.7,.8,.9", injecteds:= "↰,,"
,KEY_HIST_MAX
return,

AtExit() {
	global Sci,iniwrite_Queued
	Sci:= destroy scintilla
	(iniwrite_Queued? IniRightz())
	sleep,800
	return,
}

return() {
	exit,
}

;	_TRAY_WM_	;^^^^^;
ID_TRAY_EXIT:
ID_TRAY_PAUSE:
ID_TRAY_SUSPEND:
ID_VIEW_VARIABLES:
ID_TRAY_EDITSCRIPT:
ID_TRAY_RELOADSCRIPT:
PostMessage,0x0111,% %a_thislabel%,,,% A_ScriptName " - AutoHotkey"
donthidetray:
hidetray:
switch a_thislabel {
	case "donthidetray"	:	timer("hidetray",off)
	case "hidetray"		:	menu,tray,noicon
}
return, ; END / TRAY MENU / END / TRAY MENU / END / TRAY MENU / END / TRAY | / END / TRAY |
