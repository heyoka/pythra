pythra
=====

pythra translates the erlport wrapper parts of [lfex/py](https://github.com/lfex/py) to erlang

I did this mainly to make it easy to work with python objects and methods.


Python3 only.


Requirements
------------

    + erlang >= 17
    + python 3
    + the python lib cytoolz


Build
-----

    $ rebar3 compile
    
Use
---
    # start
    $ rebar3 shell
    
Example
-------

### erlang
Object instantiation and method call

    1> {ok, P} = pythra:start_link().            
    {ok,<0.156.0>}
    2> Obj = pythra:init(P, 'user.pyclass', 'Pyclass', [7]).
    pyClass __init__
    {'$erlport.opaque',python,
                       <<128,2,99,117,115,101,114,46,112,121,99,108,97,115,115,
                         10,80,121,99,108,97,115,115,10,113,0,...>>}
    3> pythra:method(P, Obj, get_value).               
    7
    4> pythra:stop(P).
### python

    from erlport.erlterms import Map


    class Pyclass:

        def __init__(self, value):
            print("pyClass __init__")
            self.value = value

        def get_value(self):
            return self.value



