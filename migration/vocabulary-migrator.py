import logging
import argparse
from lib.context import Context
from lib.db import DB
from lib.api import API
from lib.migrator import Migrator
import traceback

def main():
    args = parse_args()
    logging.basicConfig(format='%(levelname)s :: %(asctime)s - %(filename)s:%(funcName)s - %(message)s', level=args.log)

    db = DB(
        host=args.goobi_database_host,
        port=args.goobi_database_port,
        database=args.goobi_database_name,
        user=args.goobi_database_user,
        password=args.goobi_database_password
    )
    api = API(
        args.vocabulary_server_host,
        args.vocabulary_server_port
    )
    ctx = Context(db, api, args.fallback_language, args.continue_on_error)

    try:
        migrator = Migrator(ctx)
        migrator.migrate()
    except Exception as e:
        if args.verbose:
            traceback.print_exc()
            logging.critical(e)
        else:
            logging.critical(f'An unhandled error occurred')
        exit(1)
            
class RawTextDefaultsHelpFormatter(argparse.RawTextHelpFormatter, argparse.ArgumentDefaultsHelpFormatter):
    pass

def parse_args():
    parser = argparse.ArgumentParser(prog='vocabulary-migrator.py', formatter_class=RawTextDefaultsHelpFormatter, description='Vocabulary migration tool.')
    parser.add_argument('--log', required=False, default='INFO', help='Logger level (possible values are: NOTSET, DEBUG, INFO, WARNING, ERROR, CRITICAL)')
    parser.add_argument('--verbose', required=False, default=False, action='store_const', const=True, help='Verbose output')
    parser.add_argument('--vocabulary-server-host', type=str, default='localhost', help='vocabulary server host')
    parser.add_argument('--vocabulary-server-port', type=str, default='8080', help='vocabulary server port')
    parser.add_argument('--goobi-database-host', type=str, default='localhost', help='Goobi database host')
    parser.add_argument('--goobi-database-port', type=str, default='3306', help='Goobi database port')
    parser.add_argument('--goobi-database-name', type=str, default='goobi', help='Goobi database name')
    parser.add_argument('--goobi-database-user', type=str, default='goobi', help='Goobi database username')
    parser.add_argument('--goobi-database-password', type=str, default='goobi', help='Goobi database password')
    parser.add_argument('--continue-on-error', default=False, action='store_const', const=True, help='Should the migration continue on errors?')
    parser.add_argument('--fallback-language', type=str, help='In case some schema contains optional, translatable fields and none of the languages is required, which language should be used as a fallback language?')
    
    #parser.add_argument('action', help='''action to be performed, possible actions:
    #    status\t\tprint the state of all plugins
    #    clean\t\tclean all installed plugins
    #    install\t\tinstall all plugins that are not yet installed
    #    upgrade\t\tupgrade all outdated plugins''')

    return parser.parse_args()

if __name__ == '__main__':
    main()
