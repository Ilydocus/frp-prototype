frp-prototype
=============

#Description

Implementation of a simplified version of the "attach UE" procedure in the RRC protocol of LTE, using the functional reactive programming (FRP) paradigm and Haskell.

The Haskell libray used for FRP is [Reactive-banana](http://www.haskell.org/haskellwiki/Reactive-banana).

The protoype is composed of three programs that represent the behaviour of the three types of actors in the procedure:

- the UEs (in ue.hs)
- the eNodeB (in eNodeB.hs)
- the MME (in mme.hs)

Different information about the UEs are stored in the three actors.

The three programs communicate using TCP sockets and the incoming of new data on the sockets is handled as an event in Reactive-banana's event network.

Serialization of the messages is done using the [binary package](http://hackage.haskell.org/package/binary).

To simulate the behavior of the SecurityMode messages, part of the SecurityModeCommand message is encrypted using the [AES package](http://hackage.haskell.org/package/AES).