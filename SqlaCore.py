###############################################################################
# module: SqlaCore.py
# description: Harshly reduced subset of SQLAlchemy Core API api
# author: snwight, northwight@gmail.com
# repos: github.com/snwight/erlsqlacore
# license: dbad
# date: oct 2012
###############################################################################
from sqlalchemy import engine, create_engine, text
from sqlalchemy.types import String
from sqlalchemy.sql import select
from sqlalchemy.schema import Table, Column, MetaData
from sqlalchemy.engine import reflection

from subprocess import call
import os 

class SqlaCore():
    '''
    encapsulate a limited subset of the SQLAlchemy Core API
    '''
    def __init__(self, source=None):
        # hook ourselves up to SQLAlchemy using Python/DB interface "source"
        self.engine = create_engine(source, echo=False)
        # reflect the existing SQL schema
        self.md = MetaData(self.engine)
        self.md.reflect(views=True)
        # private matters
        self.conn = None
        self.trans = None
        self.autocommit = True

    def _checkConnection(self):
        if self.conn is None:
            self.conn = self.engine.connect()
        if self.autocommit is False:
            if not self.conn.in_transaction():
                self.trans = self.conn.begin()
        self.conn.execution_options(autocommit=self.autocommit)

    def _getTableObject(self, tName=None):
        '''
        extract table name, match in reflected schema, return SQLA Table object
        '''
        if tName:
            for t in self.md.sorted_tables:
                if t.name == tName:
                    return t
        return None

    def loadSchema(self, schemaFile=None):
        '''
        create and load db - primarily useful for testing
        '''
        ## create postgres db
        #        cmd = '''psql -q -U vesper -d jsonmap_db 
        #                 < {0} 2>/dev/null'''.format(schemaFile)
        ## create mysql db
        #        cmd = '''mysql -p've$per' -u vesper jsonmap_db 
        #                 < {0}'''.format(self.schemaFile)
        cmd = "sqlite3 /tmp/test.db < {0}".format(schemaFile)
        call(cmd, shell=True)
        return True

    def get(self, argList):
        '''
        QUERY implementation - 
        criminally simplistic single-table retrieval for now!
        nb: --- empty entries match all ---
        argList[tableName => self-evident
                pKeyDict => {primary_key_1: match_value1, ...}
                colDict => None | {colname1: val1, colname2: val2,...}]
        '''
        if argList is None:
            return None
        [tableName, pKeyDict, colDict, hints] = argList
        table = self._getTableObject(tableName)
        hints = hints or {}
        limit = hints.get('limit')
        offset = hints.get('offset')
        query = select([table])
        for pk, pv in pKeyDict.items():
            query = query.where(table.c[pk] == pv)
        for ck, cv in pKeyDict.items():
            query = query.where(table.c[ck] == cv)
        if limit:
            query = query.limit(limit)
        if offset:
            query = query.offset(offset)
        self._checkConnection()
        result = self.conn.execute(query)
        return result

    def upsert(self, argList):
        '''
        UPSERT implementation
        argList[tableName => self-evident
                pKeyDict => {primary_key_1: match_value1, ...}
                colDict => None | {colname1: val1, colname2: val2,...}]
        '''
        if argList is None:
            return None
        [tableName, pKeyDict, colDict] = argList
        table = self._getTableObject(tableName)
        self._checkConnection()
        # try update first - if it fails we'll drop through to insert
        upd = table.update()
        for pk, pv in pKeyDict.items():
            upd = upd.where(table.c[pk] == pv)
        result = self.conn.execute(upd, colDict)
        if result.rowcount:
            return result.rowcount
        # update failed - try inserting new row
        for pk, pv in pKeyDict.items():
            # yeah why not?
            colDict[pk] = pv
        ins = table.insert()
        result = self.conn.execute(ins, argDict)
        return result.rowcount

    def remove(self, argList):
        '''
        DELETE implementation
        argList[tableName => self-evident
                pKeyDict => {primary_key_1: match_value1, ...}
                colDict => None | {colname1: val1, colname2: val2,...}]
        '''
        if argList is None:
            return None
        [tableName, pKeyDict, colDict] = argList
        table = self._getTableObject(tableName)
        cmd = table.delete()
        for pk, pv in pKeyDict.items():
            cmd = cmd.where(table.c[pk] == pv)
        for ck, cv in pKeyDict.items():
            cmd = cmd.where(table.c[ck] == cv)
        self._checkConnection()
        result = self.conn.execute(cmd)
        return result.rowcount

    def commit(self):
        if self.conn is not None:
            if self.conn.in_transaction():
                self.trans.commit()

    def rollback(self):
        if self.conn is not None:
            if self.conn.in_transaction():
                self.trans.rollback()

    def close(self):
        if self.conn is not None:
            self.conn.close()
            self.conn = None
