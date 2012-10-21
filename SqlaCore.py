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
    def __init__(self, source=None, testSchema=None):
        if testSchema:
            self.loadSchema(testSchema)
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
        nb: SQLA knows primary keys, no need for special list here
        argList[string tableName => "table_name",
                tuple list keyVals => None | [("col1", val), ...],
                tuple list hints => None | [("limit", N)], [("offset", N)]]
                ]
        '''
        if argList is None:
            return None
        [tableName, kvs, h] = argList
        table = self._getTableObject(tableName)
        query = select([table])
        if kvs:
            for (k, v) in kvs:
                query = query.where(table.c[k] == v)
#        if h:
#            limit = hints.get('limit')
#            if limit:
#                query = query.limit(limit)
#            offset = hints.get('offset')
#            if offset:
#                query = query.offset(offset)
        self._checkConnection()
        result = self.conn.execute(query)
        return result

    def upsert(self, argList):
        '''
        UPSERT implementation
        nb: require primary key/s here
        argList[string tableName => "table_name",
                tuple pKeyVal => ("primary_key_name", match_value1),
                tuple list keyVals => None | [("col1", val), ...],
        '''
        if argList is None:
            return None
        [tableName, pKeyVals, keyVals] = argList
        table = self._getTableObject(tableName)
        self._checkConnection()
        # try update first - if it fails we'll drop through to insert
        upd = table.update()
        for (pk, pv) in pKeyVals:
            upd = upd.where(table.c[pk] == pv)
           
        #!!!!!!!!!! 
        keyValDict = dict(keyVals)
        #!!!!!!!!!!

        result = self.conn.execute(upd, keyValDict)
        if result.rowcount:
            return result.rowcount
        # update failed - try inserting new row, with those same pri keys
        for (pk, pv) in pKeyVals:
`            keyValDict[pk] = pv
        ins = table.insert()
        result = self.conn.execute(ins, argDict)
        return result.rowcount

    def remove(self, argList):
        '''
        DELETE implementation
        nb: require primary key/s here
        argList[string tableName => "table_name",
                tuple pKeyVal => ("primary_key_name", match_value1),
                tuple list keyVals => None | [("col1", val), ...],
                colDict => None | [("colname1", val1), ...]
        '''
        if argList is None:
            return None
        [tableName, pKeyVals, keyVals] = argList
        table = self._getTableObject(tableName)
        cmd = table.delete()
        for (pk, pv) in pKeyVals:
            cmd = cmd.where(table.c[pk] == pv)
        for (ck, cv) in keyVals:
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
