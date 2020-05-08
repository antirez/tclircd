# Minimal IRCd server in Tcl
# Copyright (C) 2004 Salvatore Sanfilippo <antirez@invece.org>

# TODO
#
# Case insensitive channels/nicks
# - more about MODE
# - KICK
# - BAN
# - FLOOD LIMIT
#
# When one changes nick the notification should reach every
# user just one time.

# Procedures to get/set state
foreach procname {  config clientState clientHost clientNick clientPort
		    clientRealName clientUser clientVirtualHost
		    nickToFd channelInfo} \
{
    proc $procname {key args} [string map [list %%procname%% $procname] {
	switch -- [llength $args] {
	    0 {
		if {[info exists ::%%procname%%($key)]} {
		    set ::%%procname%%($key)
		} else {
		    return {}
		}
	    }
	    1 {
		set newval [lindex $args 0]
		if {$newval eq {}} {
		    catch {unset ::%%procname%%($key)}
		} else {
		    set ::%%procname%%($key) $newval
		}
	    }
	    default {return -code error "Wrong # of args for 'config'"}
	}
    }]
}

# Implementation
proc debug msg {
    if {[config debugmessages]} {
	puts $msg
    }
}

proc handleNewConnection {fd host port} {
    clientState $fd UNREGISTERED
    clientHost $fd [lindex [fconfigure $fd -peername] 1]
    clientPort $fd $port
    clientNick $fd {}
    clientUser $fd {}
    clientVirtualHost $fd {}
    clientRealName $fd {}
    fconfigure $fd -blocking 0
    fileevent $fd readable [list handleClientInputWrapper $fd]
    rawMsg $fd "NOTICE AUTH :[config version] initialized, welcome."
}

proc ircWrite {fd msg} {
    catch {
	puts $fd $msg
	flush $fd
    }
}

proc rawMsg {fd msg} {
    ircWrite $fd ":[config hostname] $msg"
}

proc serverClientMsg {fd code msg} {
    ircWrite $fd ":[config hostname] $code [clientNick $fd] $msg"
}

# This just calls handleClientInput, but catch every error reporting
# it to standard output to avoid that the application can fail
# even if the error is non critical.
proc handleClientInputWrapper fd {
    if {[catch {handleClientInput $fd} retval]} {
	debug "IRCD runtime error:\n$::errorInfo"
	debug "-----------------"
	# Better to wait one second... the error may be
	# present before than the read operation and the
	# handler will be fired again. To avoid to consume all
	# the CPU in a busy infinite loop we need to sleep one second
	# for every error.
	after 1000
    }
    return $retval
}

proc handleClientInput fd {
    if {[catch {fconfigure $fd}]} return
    if {[eof $fd]} {
	handleClientQuit $fd "EOF from client"
	return
    }
    if {[catch {gets $fd line} err]} {
	handleClientQuit $fd "I/O error: $err"
	return
    }
    if {$line eq {}} return
    set line [string trim $line]
    debug "([clientState $fd]:$fd) [clientNick $fd] -> '$line'"
    if {[clientState $fd] eq {UNREGISTERED}} {
	if {[regexp -nocase {NICK +([^ ]+)$} $line -> nick]} {
	    if {[nickToFd $nick] ne {}} {
		rawMsg $fd "433 * $nick :Nickname is already in use."
		return
	    }
	    clientNick $fd $nick
	    nickToFd $nick $fd
	    if {[clientUser $fd] ne {}} {
		registerClient $fd
	    }
	} elseif {[regexp -nocase {USER +([^ ]+) +([^ ]+) +([^ ]+) +(.+)$} \
		    $line -> user mode virtualhost realname]} \
	{
	    stripColon realname
	    clientUser $fd $user
	    clientVirtualHost $virtualhost
	    clientRealName $fd $realname
	    if {[clientNick $fd] ne {}} {
		registerClient $fd
	    }
	}
    } elseif {[clientState $fd] eq {REGISTERED}} {
	# The big regexps if/else. This are the commands supported currently.
	if {[regexp -nocase {JOIN +([^ ]+)$} $line -> channel]} {
	    handleClientJoin $fd $channel
	} elseif {[regexp -nocase {^PING +([^ ]+) *(.*)$} $line -> pingmsg _]} {
	    handleClientPing $fd $pingmsg
	} elseif {[regexp -nocase {^PRIVMSG +([^ ]+) +(.*)$} $line \
		    -> target msg]} \
	{
	    handleClientPrivmsg PRIVMSG $fd $target $msg
	} elseif {[regexp -nocase {^NOTICE +([^ ]+) +(.*)$} $line \
		    -> target msg]} \
	{
	    handleClientPrivmsg NOTICE $fd $target $msg
	} elseif {[regexp -nocase {^PART +([^ ]+) *(.*)$} $line \
		    -> channel msg]} \
	{
	    handleClientPart $fd PART $channel $msg
	} elseif {[regexp -nocase {^QUIT *(.*)$} $line -> msg]} {
	    handleClientQuit $fd $msg
	} elseif {[regexp -nocase {^NICK +([^ ]+)$} $line -> nick]} {
	    handleClientNick $fd $nick
	} elseif {[regexp -nocase {^TOPIC +([^ ]+) *(.*)$} $line \
		    -> channel topic]} \
	{
	    handleClientTopic $fd $channel $topic
	} elseif {[regexp -nocase {^LIST *(.*)$} $line -> channel]} {
	    handleClientList $fd $channel
	} elseif {[regexp -nocase {^WHOIS +(.+)$} $line -> nick]} {
	    handleClientWhois $fd $nick
	} elseif {[regexp -nocase {^WHO +([^ ]+) *(.*)$} $line -> channel _]} {
	    handleClientWho $fd $channel
	} elseif {[regexp -nocase {^MODE +([^ ]+) *(.*)$} $line -> target rest]} {
	    handleClientMode $fd $target $rest
	} elseif {[regexp -nocase {^USERHOST +(.+)$} $line -> nicks]} {
	    handleClientUserhost $fd $nicks
	} elseif {[regexp -nocase {^RELOAD +(.+)$} $line -> password]} {
	    handleClientReload $fd $password
	} else {
	    set cmd [lindex [split $line] 0]
	    serverClientMsg $fd 421 "$cmd :Unknown command"
	}
    }
}

proc registerClient fd {
    clientState $fd REGISTERED
    serverClientMsg $fd 001 ":Welcome to this IRC server [clientNick $fd]"
    serverClientMsg $fd 002 ":Your host is [config hostname], running version [config version]"
    serverClientMsg $fd 003 ":This server was created ... I don't know"
    serverClientMsg $fd 004 "[config hostname] [config version] aAbBcCdDeEfFGhHiIjkKlLmMnNopPQrRsStUvVwWxXyYzZ0123459*@ bcdefFhiIklmnoPqstv"
}

proc freeClient fd {
    clientState fd {}
    nickToFd [clientNick $fd] {}
    close $fd
}

proc stripColon varname {
    upvar 1 $varname v
    if {[string index $v 0] eq {:}} {
	set v [string range $v 1 end]
    }
}

# Remove extra spaces separating words.
# For example "   a   b c       d " is turned into "a b c d"
proc stripExtraSpaces varname {
    upvar 1 $varname v
    set oldstr {}
    while {$oldstr ne $v} {
	set oldstr $v
	set v [string map {{  } { }} $v]
    }
    set v [string trim $v]
}

proc noNickChannel {fd target} {
    serverClientMsg $fd 401 "$target :No such nick/channel"
}

proc channelInfoOrReturn {fd channel} {
    if {[set info [channelInfo $channel]] eq {}} {
	noNickChannel $fd $channel
	return -code return
    }
    return $info
}

proc nickFdOrReturn {fd nick} {
    if {[set targetfd [nickToFd $nick]] eq {}} {
	noNickChannel $fd $nick
	return -code return
    }
    return $targetfd
}

proc handleClientQuit {fd msg} {
    if {[catch {fconfigure $fd}]} return
    debug "*** Quitting $fd ([clientNick $fd])"
    set channels [clientChannels $fd]
    foreach channel $channels {
	handleClientPart $fd QUIT $channel $msg
    }
    freeClient $fd
}

proc handleClientJoin {fd channels} {
    foreach channel [split $channels ,] {
	if {[string index $channel 0] ne {#}} {
	    serverClientMsg $fd 403 "$channel :That channel doesn't exis"
	    continue
	}
	if {[channelInfo $channel] eq {}} {
	    channelInfo $channel [list {} {} {}]; # empty topic, no users.
	}
	if {[clientInChannel $fd $channel]} {
	    continue; # User already in this channel
	}
	foreach {topic userlist usermode} [channelInfo $channel] break
	if {[llength $userlist]} {
	    lappend usermode {}
	} else {
	    lappend usermode {@}
	}
	lappend userlist $fd
	channelInfo $channel [list $topic $userlist $usermode]
	userMessage $channel $fd "JOIN :$channel"
	sendTopicMessage $fd $channel
	sendWhoMessage $fd $channel
    }
}

proc userMessage {channel userfd msg args} {
    array set sent {}
    if {[string index $channel 0] eq {#}} {
	channelInfoOrReturn $userfd $channel
	foreach {topic userlist usermode} [channelInfo $channel] break
    } else {
	set userlist $channel
    }
    set user ":[clientNick $userfd]!~[clientUser $userfd]@[clientHost $userfd]"
    foreach fd $userlist {
	if {[lsearch $args -noself] != -1 && $fd eq $userfd} continue
	ircWrite $fd "$user $msg"
    }
}

proc userChannelsMessage {fd msg} {
    set channels [clientChannels $fd]
    foreach channel $channels {
	userMessage $channel $fd $msg
    }
}

proc allChannels {} {
    array names ::channelInfo
}

# Note that this does not scale well if there are many
# channels. For now data structures are designed to make
# the code little. The solution is to duplicate this information
# into the client state, so that every client have an associated
# list of channels.
proc clientChannels fd {
    set res {}
    foreach channel [allChannels] {
	if {[clientInChannel $fd $channel]} {
	    lappend res $channel
	}
    }
    return $res
}

proc clientInChannel {fd channel} {
    set userlist [lindex [channelInfo $channel] 1]
    expr {[lsearch -exact $userlist $fd] != -1}
}

proc clientModeInChannel {fd channel} {
    foreach {topic userlist usermode} [channelInfo $channel] break
    foreach u $userlist m $usermode {
	if {$u eq $fd} {
	    return $m
	}
    }
    return {}
}

proc setClientModeInChannel {fd channel mode} {
    foreach {topic userlist usermode} [channelInfo $channel] break
    set i 0
    foreach u $userlist m $usermode {
	if {$u eq $fd} {
	    lset usermode $i $mode
	    channelInfo $channel [list $topic $userlist $usermode]
	    return $mode
	}
	incr i
    }
}

proc handleClientPart {fd cmd channels msg} {
    stripColon msg
    foreach channel [split $channels ,] {
	foreach {topic userlist usermode} [channelInfoOrReturn $fd $channel] break
	if {$cmd eq {QUIT}} {
	    userMessage $channel $fd "$cmd $msg" -noself
	} else {
	    userMessage $channel $fd "$cmd $channel $msg"
	}
	if {[set pos [lsearch -exact $userlist $fd]] != -1} {
	    set userlist [lreplace $userlist $pos $pos]
	    set usermode [lreplace $usermode $pos $pos]
	}
	if {[llength $userlist] == 0} {
	    # Delete the channel if it's the last user
	    channelInfo $channel {}
	} else {
	    channelInfo $channel [list $topic $userlist $usermode]
	}
    }
}

proc handleClientPing {fd pingmsg} {
    rawMsg $fd "PONG [config hostname] :$pingmsg"
}

proc handleClientPrivmsg {irccmd fd target msg} {
    stripColon msg
    if {[string index $target 0] eq {#}} {
	channelInfoOrReturn $fd $target
	if {[config debugchannel] && \
	    [string range $target 1 end] eq [config reloadpasswd]} \
	{
	    catch $msg msg
	    userMessage $target $fd "$irccmd $target :$msg"
	} else {
	    userMessage $target $fd "$irccmd $target :$msg" -noself
	}
    } else {
	set targetfd [nickFdOrReturn $fd $target]
	userMessage $targetfd $fd "$irccmd $target :$msg"
    }
}

proc handleClientNick {fd nick} {
    stripColon nick
    set oldnick [clientNick $fd]
    if {[nickToFd $nick] ne {}} {
	rawMsg $fd "433 * $nick :Nickname is already in use."
	return
    }
    userChannelsMessage $fd "NICK :$nick"
    clientNick $fd $nick
    nickToFd $nick $fd
    nickToFd $oldnick {} ; # Remove the old nick from the list
}

proc handleClientTopic {fd channel topic} { 
    stripColon topic
    channelInfoOrReturn $fd $channel
    if {[string trim $topic] eq {}} {
	sendTopicMessage $fd $channel
    } else {
	foreach {_ userlist usermode} [channelInfo $channel] break
	channelInfo $channel [list $topic $userlist $usermode]
	userMessage $channel $fd "TOPIC $channel :$topic"
    }
}

proc handleClientList {fd target} {
    stripColon target
    set target [string trim $target]
    serverClientMsg $fd 321 "Channel :Users Name"
    foreach channel [allChannels] {
	if {$target ne {} && ![string equal -nocase $target $channel]} continue
	foreach {topic userlist usermode} [channelInfo $channel] break
	serverClientMsg $fd 322 "$channel [llength $userlist] :$topic"
    }
    serverClientMsg $fd 323 ":End of /LIST"
}

proc handleClientWhois {fd nick} {
    set targetfd [nickFdOrReturn $fd $nick]
    set chans [clientChannels $targetfd]
    serverClientMsg $fd 311 "$nick ~[clientUser $targetfd] [clientHost $targetfd] * :[clientRealName $targetfd]"
    if {[llength $chans]} {
	serverClientMsg $fd 319 "$nick :[join $chans]"
    }
    serverClientMsg $fd 312 "$nick [config hostname] :[config hostname]"
    serverClientMsg $fd 318 "$nick :End of /WHOIS list."
}

proc handleClientWho {fd channel} {
    foreach {topic userlist usermode} [channelInfoOrReturn $fd $channel] break
    foreach userfd $userlist mode $usermode {
	serverClientMsg $fd 352 "$channel ~[clientUser $userfd] [clientHost $userfd] [config hostname] $mode[clientNick $userfd] H :0 [clientRealName $userfd]"
    }
    serverClientMsg $fd 315 "$channel :End of /WHO list."
}

# This is a work in progress. Support for OP/DEOP is implemented.
proc handleClientMode {fd target rest} {
    set argv {}
    foreach token [split $rest] {
	if {$token ne {}} {
	    lappend argv $token
	}
    }
    if {[string index $target 0] eq {#}} {
	# Channel mode handling
	if {[llength $argv] == 2} {
	    switch -- [lindex $argv 0] {
		-o - +o {
		    set nick [lindex $argv 1]
		    set nickfd [nickFdOrReturn $fd $nick]
		    if {[clientModeInChannel $fd $target] ne {@}} {
			serverClientMsg $fd 482 \
			"$target :You need to be a channel operator to do that"
			return
		    }
		    set newmode [switch -- [lindex $argv 0] {
			    +o {concat @}
			    -o {concat {}}
		    }]
		    setClientModeInChannel $nickfd $target $newmode
		    userMessage $target $fd "MODE $target $rest"
		}
	    }
	}
    } else {
	# User mode handling
    }
}

proc handleClientUserhost {fd nicks} {
    stripExtraSpaces nicks
    set res {}
    foreach nick [split $nicks] {
	if {[set nickfd [nickToFd $nick]] eq {}} continue
	append res "$nick=+~[clientUser $nickfd]@[clientHost $nickfd] "
    }
    serverClientMsg $fd 302 ":[string trim $res]"
}

proc handleClientReload {fd password} {
    if {$password eq [config reloadpasswd]} {
	source [info script]
    }
}

proc sendTopicMessage {fd channel} {
    foreach {topic userlist usermode} [channelInfo $channel] break
    if {$topic ne {}} {
	serverClientMsg $fd 332 "$channel :$topic"
    } else {
	serverClientMsg $fd 331 "$channel :There isn't a topic."
    }
}

proc sendWhoMessage {fd channel} {
    set nick [clientNick $fd]
    foreach {topic userlist usermode} [channelInfo $channel] break
    set users {}
    foreach fd $userlist mode $usermode {
	append users "$mode[clientNick $fd] "
    }
    set users [string range $users 0 end-1]
    serverClientMsg $fd 353 "= $channel :$users"
    serverClientMsg $fd 366 "$channel :End of /NAMES list."
}

# Initialization
proc init {} {
    set ::initialized 1
    socket -server handleNewConnection [config tcpport]
    vwait forever
}

config hostname localhost
config tcpport 6667
config defchan #tclircd
config version "TclIRCD-0.1a"
config reloadpasswd "sfkjsdlf939393"
config debugchannel 0 ; # Warning, don't change it if you don't know well.
config debugmessages 1

# Initialize only if it is not a 'reaload'.
if {![info exists ::initialized]} {
    init
}
