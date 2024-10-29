import logging
import datetime
import shutil
from lxml import etree as ET
import sys
import re

VOCABULARY_ENDPOINT = 'http://{{HOST}}:{{PORT}}/api/v1/vocabularies/{{ID}}'
RECORD_ENDPOINT = 'http://{{HOST}}:{{PORT}}/api/v1/records/{{ID}}'

class MetsManipulator:
    def __init__(self, ctx, file_path):
        self.ctx = ctx
        self.file_path = file_path
        self.changed = False
        self.vocabulary_endpoint = VOCABULARY_ENDPOINT.replace('{{HOST}}', ctx.api.host).replace('{{PORT}}', ctx.api.port)
        self.record_endpoint = RECORD_ENDPOINT.replace('{{HOST}}', ctx.api.host).replace('{{PORT}}', ctx.api.port)
        self.vocabulary_uri_regex = re.compile(self.vocabulary_endpoint.replace('{{ID}}', '\\d+'))
        self.record_uri_regex = re.compile(self.record_endpoint.replace('{{ID}}', '\\d+'))

    def process(self):
        logging.debug(f'Processing mets file {self.file_path}')
        self.process_mets_file()

    def create_backup(self):
        now = datetime.datetime.now()
        suffix = now.strftime('%Y-%m-%d-%H%M%S') + now.strftime('%f')[:3]
        backup_filename = self.file_path + '.' + suffix
        shutil.copyfile(self.file_path, backup_filename)
        logging.debug(f'Backed up mets file: {backup_filename}')

    def process_mets_file(self):
        tree = ET.parse(self.file_path)
        root = tree.getroot()
        self.process_node(root)
        
        if self.changed and not self.ctx.dry:
            self.create_backup()
            tree.write(self.file_path, encoding='utf-8', xml_declaration=True)
            self.ctx.log_processed(self.file_path)

    def process_node(self, node):
        if self.is_vocabulary_reference(node) and not self.is_already_migrated(node):
            self.process_vocabulary_reference(node)
            if self.ctx.dry:
                dump_node(node)
        if self.is_manual_id_reference(node):
            self.process_manual_id_reference(node)
            if self.ctx.dry:
                dump_node(node)
        for child in node:
            self.process_node(child)

    def is_vocabulary_reference(self, node):
        return node.tag.endswith('metadata') and 'authority' in node.attrib and 'authorityURI' in node.attrib and 'valueURI' in node.attrib

    def is_manual_id_reference(self, node):
        return self.ctx.manual_id_fix != None and node.tag.endswith('metadata') and 'name' in node.attrib and node.attrib['name'] == self.ctx.manual_id_fix
    
    def is_already_migrated(self, node):
        return re.match(self.vocabulary_uri_regex, node.attrib['authorityURI']) != None and re.match(self.record_uri_regex, node.attrib['valueURI']) != None

    def generate_vocabulary_uri(self, vocabulary_id):
        return self.vocabulary_endpoint.replace('{{ID}}', str(vocabulary_id))

    def generate_record_uri(self, record_id):
        return self.record_endpoint.replace('{{ID}}', str(record_id))

    def process_vocabulary_reference(self, node):
        try:
            # Extract old vocabulary and record ids
            valueURI = node.attrib['valueURI']

            # Expect the "valueURI" attribute to be formatted like:
            # https://example.com/vocabulary/records/vocabularies/6/451
            parts = valueURI.split('/')
            
            vocabulary_id_old = None
            record_id_old = None

            # First, try to read the record id part as an int
            try:
                vocabulary_id_old = int(parts[-2])
                record_id_old = int(parts[-1])

                vocabulary_id_new = self.ctx.lookup_vocabulary_id(vocabulary_id_old)
                record_id_new = self.ctx.lookup_record_id(record_id_old)
            except Exception as e:
                try:
                    # If it is not a valid number, try to clean the valueURI and find the correct id
                    repaired_vocabulary_id, repaired_record_id = self.ctx.robust_find_record_id(parts)
                    if repaired_vocabulary_id != None and repaired_record_id != None and (repaired_vocabulary_id != vocabulary_id_old or repaired_record_id != record_id_old):
                        vocabulary_id_old = int(repaired_vocabulary_id)
                        record_id_old = int(repaired_record_id)
                        vocabulary_id_new = self.ctx.lookup_vocabulary_id(vocabulary_id_old)
                        record_id_new = self.ctx.lookup_record_id(record_id_old)
                    else:
                        # If that fails, try to intreprete the last part as a main value and search for a record containing this value
                        if vocabulary_id_old == None:
                            raise Exception('No vocabulary ID found')
                        vocabulary_id_new = self.ctx.lookup_vocabulary_id(vocabulary_id_old)
                        record_id_new = self.ctx.api.find_record(self.ctx, vocabulary_id_new, parts[-1])
                except Exception as e:
                    raise e
            
            if vocabulary_id_new == None or record_id_new == None:
                raise Exception(f'Unable to read old IDs from mets')

            if not self.is_manual_id_reference(node):
                record_data = self.ctx.api.lookup_record(record_id_new)

                main_field_id = self.ctx.find_main_field_id_for_vocabulary(record_data['vocabularyId'])
                main_field_data = [f for f in record_data['fields'] if f['definitionId'] == main_field_id]
                if len(main_field_data) != 1:
                    logging.critical(f'Record [{record_id_new}] has no unique main entry field')
                    sys.exit(1)

                # Replace node text if not matching any translation of main value
                translated_main_values = self.ctx.extract_language_values(main_field_data[0])
                if not self.ctx.record_contains_value(record_data, node.text.strip()):
                    node.text = self.ctx.extract_preferred_language(translated_main_values)

            vocabulary_name = self.ctx.lookup_vocabulary_name(vocabulary_id_new)

            # Set all attributes accordingly
            node.attrib['authority'] = vocabulary_name
            node.attrib['authorityURI'] = self.generate_vocabulary_uri(vocabulary_id_new)
            node.attrib['valueURI'] = self.generate_record_uri(record_id_new)

            self.changed = True
        except Exception as e:
            error = f'Unable to retrieve vocabulary and record id from valueURI: {valueURI}\n\t\t{e}'
            logging.debug(error)
            self.ctx.log_issue(self.file_path, error)

    def process_manual_id_reference(self, node):
        try:
            record_id_old = int(node.text)
            record_id_new = self.ctx.lookup_record_id(record_id_old)
            node.text = str(record_id_new)
            self.changed = True
        except Exception as e:
            msg = f'Unable to read ID {node.text}!'
            logging.critical(msg)
            raise Exception(msg)

def dump_node(node):
    attributes = ' '.join(f'{k}="{v}"' for k, v in node.attrib.items())
    logging.info(f'<{node.tag} {attributes} />')