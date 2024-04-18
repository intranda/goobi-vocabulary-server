import logging
import mysql.connector

class DB:
    def __init__(self, host, port, database, user, password):
        self.db = mysql.connector.connect(
            host=host,
            port=port,
            database=database,
            user=user,
            password=password
        )
    
    def query(self, q):
        c = self.db.cursor()
        c.execute(q)
        return c.fetchall()
        