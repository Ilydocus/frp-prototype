frp-prototype
=============

#Description

Implementation of a simplified version of the connection setup procedure of the RRC protocol in LTE, using the functional reactive programming (FRP) paradigm and Haskell.

The Haskell libray used for FRP is [Reactive-banana](http://www.haskell.org/haskellwiki/Reactive-banana).

The protoype is composed of three programs that represent the behaviour of the three types of actors in the procedure:

- the UEs (in ue.hs)
- the eNodeB (in eNodeB.hs)
- the MME (in mme.hs)

Different information about the UEs are stored in the three actors.

The three programs communicate using TCP sockets and the incoming of new data on the sockets is handled as an event in Reactive-banana's event network.

Serialization of the messages is done using the [binary package](http://hackage.haskell.org/package/binary).

To simulate the behavior of the SecurityMode messages, part of the SecurityModeCommand message is encrypted using the [AES package](http://hackage.haskell.org/package/AES).

#RRC connection setup procedure

Several sequence diagrams describing the messaging sequence according to different use cases can be found in a document in the doc folder.

#How does it work?

##UE program

The UEs are created using the function powerOn (ue.hs, l.45).

The first message (RaPreamble) is sent from within the function eventLoop, line 80 (ue.hs).
Subsequent messages are handled by the FRP event network which contains all the actions that executes when events occur (either an external event (a new message is received), or an internal one (a RaResponse is ready to be sent)). The event network is  described in the function setupNetwork (ue.hs, l.98).

##eNodeB program

The messages coming from the UEs and the MME are handled in the FRP event network. It is described in the function setupNetwork (eNodeB.hs, l.69).

##MME program

The messages coming from the eNodeB are handled in the FRP event network. It is described in the function setupNetwork (mme.hs, l.51).

##Messages

RRC and S1 messages are described in the RrcMessages.hs and S1Messages.hs files.
