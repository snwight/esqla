###############################################################################
# module: esqla.py
# description: Python sibling of esqla.erl, handles message passing 
# between Erlang node/s and SQLACore module (simple SQLAlchemy Core api api)
# ...fully dependent upon erlport, by github.com/hdima/erlport
# author: snwight, northwight@gmail.com
# repos: github.com/snwight/esqla
# license: dbad
# date: oct 2012
###############################################################################
from SqlaCore import *
from erlport import Port, Protocol
from erlport import Atom, String

class esqla(Protocol):
    def __init__(self):
        pass

    def handle_port(self, port=None):
        # never mind 
        return "ported"

    def handle_start(self, configString=None):
        # crank up SQLAlchemy engine
        self.sqlacore = SqlaCore(configString, "../priv/schema1.sql")
        if not self.sqlacore:
            return "failed start"
        return "started"

    def handle_get(self, argList=None):
        rowSet = self.sqlacore.get(argList)
        # return FULL rowset result, for now!
        return rowSet

    def handle_upsert(self, argList=None):
        rowCount = self.sqlacore.upsert(argList)
        return rowCount

    def handle_remove(self, argList=None):
        rowCount = self.sqlacore.remove(argList)
        return rowCount

if __name__ == "__main__":
    proto = ErlSqlaCore()
    proto.run(Port(packet=4, use_stdio=False))
