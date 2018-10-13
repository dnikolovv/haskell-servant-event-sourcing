# Haskell Servant with Event-Sourcing [![Build Status](https://travis-ci.com/dnikolovv/haskell-servant-event-sourcing.svg?branch=master)](https://travis-ci.com/dnikolovv/haskell-servant-event-sourcing)

A very simple but interesing application made using Haskell [Servant](http://hackage.haskell.org/package/servant) and [Eventful](https://github.com/jdreaver/eventful).

It stores a stream of events in-memory that represent an integer counter.

It supports 3 types of events: increment (POST counter/increment), decrement (POST counter/decrement) and reset (POST counter/reset).

Example:

```
POST counter/increment 123

GET counter/latest
{
    "count": 123
}

GET counter/events
[
    [
        {
            "tag": "CounterIncremented",
            "contents": 123
        },
        {
            "count": 123
        }
    ]
]
```
