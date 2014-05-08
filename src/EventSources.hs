module EventSources (EventSource (..)
                    ,addHandler
                    ,fire)
       where

import Reactive.Banana.Frameworks as R

type EventSource a = (AddHandler a, a -> IO())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO()
fire = snd
