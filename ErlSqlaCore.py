###############################################################################
# module: ErlSqlaCore.py
# description: Python sibling of erlsqlacore.erl, handles message passing 
# between Erlang node/s and SQLACore module (simple SQLAlchemy Core api api)
# ...fully dependent upon erlport, by github.com/hdima/erlport
# author: snwight, northwight@gmail.com
# repos: github.com/snwight/erlsqlacore
# license: dbad
# date: oct 2012
###############################################################################
from SqlaCore import *
from erlport import Port, Protocol
from erlport import Atom, String

class ErlSqlaCore(Protocol):
    def __init__(self):
        pass

    def handle(self, port, msg):
        port.write(msg)

# forthcoming dispatch funcs:
#    def handle_start(self, source):
#        self.sqlaconfig = string(source)
#        self.sqlacore = sqlacore(self.sqlaconfig)
#        port.write([1234])
#    def handle_get(self, port, argList):
#        rows = self.sqlaCore.get(argList)
#        port.write(rows)
#    def handle_add(self, port, argList):
#        rowcount = self.sqlaCore.get(argList)
#        port.write(rowcount)
#    def handle_remove(self, port, argList):
#        rowcount = self.sqlaCore.get(argList)
#        port.write(rowcount)

if __name__ == "__main__":
    proto = ErlSqlaCore()
    proto.run(Port(use_stdio=True))


