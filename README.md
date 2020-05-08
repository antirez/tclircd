(Note: this is code from 2004! Here only to archive it)

Welcome to the Tcl IRCd!

This is a pure Tcl IRCd, not a full IRC protocol implementation but enough
to be used as a real IRC server to open some channel to the public.

You can find more information visiting:

    http://www.hping.org/tclircd/

## HOW TO RUN IT

Using Tcl8.4 or greater, type:

  $ tclsh ircd.tcl

The server will run using the port 6667 for default. You can
modify it editing the ircd.tcl file, the configuration stuff is
at the end.

You may change the password used to reload the server at runtime
from the same part of the ircd.tcl file.

If you plan to hack with this program you should now that it's
possible to reload the server at runtime using the following
command from the IRC client:

  /quote reload <password>

The `<password>` can be set from the ircd.tcl file. For default
it is "betterIfYouChangeThis".

Have fun!
antirez
