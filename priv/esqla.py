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
from sqla import *
from erlport import Port, Protocol
from erlport import Atom, String

class esqla(Protocol):
    def __init__(self):
        pass

    def handle_start(self, argList):
        # crank up SQLAlchemy engine
        [configString, testDB] = argList

        print argList

        self.sqlacore = sqla(configString, testDB)
        if not self.sqlacore:
            return "failed start"
        return "started"

    def handle_schemata(self):
        # return C-formatted string 
        return ("schema", self.sqlacore.schemata())

    def handle_get(self, argList=None):
        # return FULL rowset result, for now!
        return ("result", self.sqlacore.get(argList))

    def handle_upsert(self, argList=None):
        # return modified rows count
        return ("updatecount", self.sqlacore.upsert(argList))

    def handle_remove(self, argList=None):
        # return deleted rows count
        return ("delcount", self.sqlacore.remove(argList))

if __name__ == "__main__":
    proto = esqla()
    proto.run(Port(packet=4, use_stdio=False))
