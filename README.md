Simple Port Management
======================

This is a simple library, meant to work together with tcp-stream 
and wire-streams to run a server with multiple ports, each with
multiple connections. 

Quick Note
----------

This is more of a simple project for me, it is not (yet)
meant to be used as a proper package. Maybe it will some
day, but not at the moment (2021-04-30).

Compilation
-----------

To compile the program, usually you would run

	cabal install --only-dependencies
	cabal build

However, in older versions of cabal, you may have
to run

	cabal sandbox init
	cabal install --only-dependencies
	cabal build

To get the documentation, run

	cabal haddock --html

How it works
------------

### Basic Idea

Each server has multiple ports. There is one 'listen' port
that accepts new connections. There is also a number of
'communication' ports that each have a maximum number of
connections, and are the main communication port between
the server and a client.

### More Details

A client connects to the server on the 'listen' port. The
server then looks through its open ports to see if any
has any open spaces. If it does, it sends that port's
number to the client, closes the initial connection,
and connects to the client on the new port it just found.
If all of the server's ports are full, then it opens a new
port, and sends this new port number to the client, closes
the initial connection, and connects to the client on the
new port. When a client is finished, it disconnects from
the server, and frees up a slot on the port it was 
connected to. If a port has no open connections, it is 
closed and removed from the list of potential connection
ports, and can no longer be used (unless re-opened by the
server).

Examples
--------

### Basic Server

A sample client and server is provided by the package, they
are in the modules 'SampleClient' and 'SampleServer' respectively,
and compile to 'client' and 'server' respectively. The sample client
sends strings to the server, and the server returns the string in
reverse. However, if the string starts with 'exit', then the 
connection is closed.

### Chat Server

A chat server and client is also provided by the package. They can
be found in the 'ChatServer' and 'ChatClient' Modules respectively.
To run the server, find chat-server\[.exe\] and invoke it like:

	chat-server <port-number>

For example, to invoke on port 3001, use

	chat-server 3001

Then, for a client to connect to it, invoke it like:

	chat-client <ip-address> <port-number>
	
And, to connect to a local chat server on port 3001, use

	chat-client 127.0.0.1 3001

#### Chat Client Usage

##### Getting a username

To send and receive messages, you first have request a username
and receive an id number. To do this, type

	$<name>
	
e.g.

	$my_boring_name
	
(Note that spaces are not allowed; only the first word will be
used for your username if you include spaces)

This will connect you to server, and you will start to receive
messages from other clients. 

Note that, at the moment, once you have a name, you cannot
change it. This should change in a future update.

##### Seeing who else is online

You can see who else is online by using the following command

	? n m
	
This will show you m users, starting from the n-th user; usually,
you will use this command like so:

	? 0 <max_number_of_users_to_show>
	
Then, if you want to see if there are even more users, you can start
from the value you left off at. Note that the numbers represent
the n-th user, *not* the user with user id n. So if you use a command
like

	? 0 10

The first id could be greater than 10.

##### Leaving the chat room

You can leave the chatroom simply by typing 'exit' or 'quit'.
If for some reason that doesn't work, you could try to force an exit
by pressing CTRL+C.

Note that, at the moment, there is no way to send a public message that
is just "exit" or "quit"; you'd have to append some other text. So just
make sure you don't accidentally type "quit" when you don't want to quit.

##### Sending Public Messages

You can send a message just by typing. So long as the message isn't
recognised as one of the other formats of message, it will be posted
as a public message. So the following messages should work:

	???
	quit it!
	#@$&

##### Sending Private Messages

You can send a private message to any user by typing \# followed by their
user id. e.g.

	#12 Are you available right now?

Would send the message to user 12. To look up a user's id, use the ? lookup
command.
