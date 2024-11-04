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
        args.vocabulary_server_port,
        args.vocabulary_server_token
    )
    ctx = Context(api, args.dry, args.verbose, args.continue_on_error, args.metadata_directory, args.mapping_file, args.preferred_mets_main_value_language, args.manual_id_fix, args.trust)

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
    parser.add_argument('--dry', required=False, default=False, action='store_const', const=True, help='Don\'t persist changes but only print replacements to the console')
    parser.add_argument('--metadata-directory', '-d', required=True, help='directory to recursively scan for metadata to update')
    parser.add_argument('--mapping-file', '-m', required=True, help='vocabulary and record mapping file')
    parser.add_argument('--vocabulary-server-host', type=str, default='localhost', help='vocabulary server host')
    parser.add_argument('--vocabulary-server-port', type=str, default='8081', help='vocabulary server port')
    parser.add_argument('--vocabulary-server-token', type=str, default=None, help='vocabulary server security token')
    parser.add_argument('--preferred-mets-main-value-language', type=str, default='eng', help='Default language to use for mets value writing, if present and prior value invalid')
    parser.add_argument('--trust', type=str, default='ID', help='Set the data source to trust for the migration. Possible values are: "ID" and "Value". If "ID" is set, the record ID is parsed from the valueURI and used to find the migrated record. If "Value" is set, the XML elements value is used to find the newly migrated record by value. Defaults to "ID".')
    parser.add_argument('--manual-id-fix', type=str, default=None, help='Manually fix the record ID of elements whose name attribute matches this parameter. Caution, this must not be executed twice!')
    parser.add_argument('--log', required=False, default='INFO', help='logger level (possible values are: NOTSET, DEBUG, INFO, WARNING, ERROR, CRITICAL)')
    parser.add_argument('--verbose', required=False, default=False, action='store_const', const=True, help='verbose output')
    parser.add_argument('--continue-on-error', default=False, action='store_const', const=True, help='should the migration continue on errors?')

    return parser.parse_args()

if __name__ == '__main__':
    main()
