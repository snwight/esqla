###############################################################################
# module: sqla.py
# description: Harshly reduced subset of SQLAlchemy Core API api
# author: snwight, northwight@gmail.com
# repos: github.com/snwight/erlsqlacore
# license: dbad
# date: oct 2012
###############################################################################
import sqlalchemy.types
from sqlalchemy import engine, create_engine
from sqlalchemy.sql import select
from sqlalchemy.schema import Table, Column, MetaData
from sqlalchemy.engine import reflection
from subprocess import call
from erlport import String


class sqla():
    '''
    encapsulate a limited subset of the SQLAlchemy Core API
    '''
    def __init__(self, source=None, testDB=None):
        if testDB:
            self.loadSchema(testDB)
        # hook ourselves up to SQLAlchemy using Python/DB interface "source"
        self.engine = create_engine(source, echo=False)
        # reflect the existing SQL schema
        self.md = MetaData(self.engine)
        self.md.reflect(views=True)
        self.insp = reflection.Inspector.from_engine(self.engine)
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


    def schemata(self):        
        s = 'SQL schema:: '
        for tbl in self.insp.get_table_names():
            s += '~nTABLE: ' + tbl
            for c in self.insp.get_columns(tbl):
                s += '~n\t' + c['name']
            for pk in self.insp.get_primary_keys(tbl):
                s += '~n\tPRIMARY KEY: ' + pk
            for i in self.insp.get_indexes(tbl):
                s += '~n\tINDEX: ' + i['name'] 
                s += ' UNIQUE ' if 'unique' in i else '' + ' ON ' 
                for ic in i['column_names']:
                    s += ic + ' '
            for fk in self.insp.get_foreign_keys(tbl):
                s += '~n\tFOREIGN KEY: '
                for cc in fk['constrained_columns']:
                    s += cc + ' '
                s += ' ON ' + fk['referred_table']  + ': '
                for rc in fk['referred_columns']:
                    s += rc + ' '
        for vw in self.insp.get_view_names():
            s += '~nVIEW: ' + vw
            for c in self.insp.get_columns(vw):
                s += '~n\t' + c['name']
            s += '~nVIEW DEFINITION: '
            s += '~n' + self.insp.get_view_definition(vw)+'~n'
        return s


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
        [tnm, kvs, hints] = argList
        tableName = String(tnm)
        table = self._getTableObject(tableName)
        # nb: SELECT * for now
        query = select([table])
        # strip empty tuples now
        for t in kvs:
            if not len(t):
                continue
            (k, v) = t
            kStr = String(k)
            if isinstance(table.c[kStr].type, sqlalchemy.types.INTEGER):
                query = query.where(table.c[kStr] == v)
            else:
                query = query.where(table.c[kStr] == String(v))
        for h in hints:
            if not len(h):
                continue
            (k, v) = h
            sk = String(k)
            if sk == "limit":
                query = query.limit(v)
            if sk == "offset":
                query = query.offset(v)
        self._checkConnection()
        # print "DEBUG query:", query
        result = self.conn.execute(query)
        rows = []
        for r in result:
            [rows.append([table.name, c.name, r[c.name]]) for c in table.c]
        return rows


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
        [tableName, pKeyVal, keyVals] = argList
        table = self._getTableObject(String(tableName))
        self._checkConnection()
        # try update first - if it fails we'll drop through to insert
        upd = table.update()
        # format our K,V tuples as acceptable types - key is always a string
        (pk, pv) = pKeyVal
        pkStr = String(pk)
        # but val is a variant thing
        if isinstance(table.c[pkStr].type, sqlalchemy.types.INTEGER):
            pvStr = pv
        else:
            pvStr = String(pv)
        upd = upd.where(table.c[pkStr] == pvStr)
        kvDict = dict()
        for t in keyVals:
            if not len(t):
                continue
            (k, v) = t
            kStr = String(k)
            if isinstance(table.c[kStr].type, sqlalchemy.types.INTEGER):
                kvDict[kStr] = v
            else:
                kvDict[kStr] = String(v)
        result = self.conn.execute(upd, kvDict)
        if result.rowcount:
            return result.rowcount
        # update failed - try inserting new row, with that same pri key
        kvDict[pkStr] = pvStr
        ins = table.insert()
        result = self.conn.execute(ins, kvDict)
        return result.rowcount


    def remove(self, argList):
        '''
        DELETE implementation
        argList[string tableName => "table_name",
                tuple list keyVals => None | [("col1", val), ...],
                colDict => None | [("colname1", val1), ...]
        '''
        if argList is None:
            return None
        [tableName, keyVals] = argList
        table = self._getTableObject(String(tableName))
        cmd = table.delete()
        kvDict = dict()
        for t in keyVals:
            if not len(t):
                continue
            (k, v) = t
            kStr = String(k)
            if isinstance(table.c[kStr].type, sqlalchemy.types.INTEGER):
                kvDict[kStr] = v
            else:
                kvDict[kStr] = String(v)
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
