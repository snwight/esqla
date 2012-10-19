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

    def handle_port(self, port=None):
        # store active port descriptor
        self.activePort = port
        return "ported"

    def handle_start(self, configString=None):
        # crank up SQLAlchemy engine
        #        self.sqlaconfig = String(configString)
        self.sqlaconfig = "sqlite:////tmp/test.db"
        self.sqlacore = SqlaCore(self.sqlaconfig)
        if self.sqlacore:
            self.sqlacore.loadSchema("/Users/snwight/erlsqlacore/schema1.sql")
        return "started"
        
    def handle_get(self, argList=None):
        rowSet = self.sqlacore.get(argList)
        # return FULL rowset result, for now!
        self.activePort.write(rowSet)
        return rowSet

    def handle_upsert(self, argList=None):
        rowCount = self.sqlacore.upsert(argList)
        self.activePort.write(rowCount)
        return rowCount

    def handle_remove(self, argList=None):
        rowCount = self.sqlacore.remove(argList)
        self.activePort.write(rowCount)
        return rowCount

if __name__ == "__main__":
    proto = ErlSqlaCore()
    proto.run(Port(use_stdio=True))


