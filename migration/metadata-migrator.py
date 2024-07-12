import logging
import argparse
from lib.api import API
from lib.mets_migrator import MetsMigrator
from lib.mets_context import Context
import traceback

def main():
    args = parse_args()
    logging.basicConfig(format='%(levelname)s :: %(asctime)s - %(filename)s:%(funcName)s - %(message)s', level=args.log)

    api = API(
        args.vocabulary_server_host,
        args.vocabulary_server_port
    )
    ctx = Context(api, args.verbose, args.continue_on_error, args.metadata_directory, args.mapping_file)

    try:
        migrator = MetsMigrator(ctx)
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
    parser = argparse.ArgumentParser(prog='metadata-migrator.py', formatter_class=RawTextDefaultsHelpFormatter, description='Metadata migration tool.')
    parser.add_argument('--metadata-directory', '-d', required=True, help='Directory to recursively scan for metadata to update')
    parser.add_argument('--mapping-file', '-m', required=True, help='Vocabulary and record mapping file')
    parser.add_argument('--vocabulary-server-host', type=str, default='localhost', help='vocabulary server host')
    parser.add_argument('--vocabulary-server-port', type=str, default='8081', help='vocabulary server port')
    parser.add_argument('--log', required=False, default='INFO', help='Logger level (possible values are: NOTSET, DEBUG, INFO, WARNING, ERROR, CRITICAL)')
    parser.add_argument('--verbose', required=False, default=False, action='store_const', const=True, help='Verbose output')
    parser.add_argument('--continue-on-error', default=False, action='store_const', const=True, help='Should the migration continue on errors?')

    return parser.parse_args()

if __name__ == '__main__':
    main()
