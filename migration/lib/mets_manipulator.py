import logging
import datetime
import shutil
from lxml import etree as ET

VOCABULARY_ENDPOINT = 'http://localhost:8081/api/v1/vocabularies/{{ID}}'
RECORD_ENDPOINT = 'http://localhost:8081/api/v1/records/{{ID}}'

class MetsManipulator:
    def __init__(self, ctx, file_path):
        self.ctx = ctx
        self.file_path = file_path
        self.changed = False

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
        
        if self.changed:
            self.create_backup()
            tree.write(self.file_path, encoding='utf-8', xml_declaration=True)
            self.ctx.log_processed(self.file_path)

    def process_node(self, node):
        if self.is_vocabulary_reference(node):
            self.process_vocabulary_reference(node)
        for child in node:
            self.process_node(child)

    def is_vocabulary_reference(self, node):
        return 'authority' in node.attrib and 'authorityURI' in node.attrib and 'valueURI' in node.attrib
    
    def process_vocabulary_reference(self, node):
        try:
            # Extract old vocabulary and record ids
            valueURI = node.attrib['valueURI']
            parts = valueURI.split('/')
            vocabulary_id_old = int(parts[-2])
            record_id_old = int(parts[-1])
        
            vocabulary_id_new = self.ctx.lookup_vocabulary_id(vocabulary_id_old)
            record_id_new = self.ctx.lookup_record_id(record_id_old)
            vocabulary_name = self.ctx.lookup_vocabulary_name(vocabulary_id_new)

            node.attrib['authority'] = vocabulary_name
            node.attrib['authorityURI'] = generate_vocabulary_uri(vocabulary_id_new)
            node.attrib['valueURI'] = generate_record_uri(record_id_new)

            self.changed = True
        except Exception as e:
            error = f'Unable to retrieve vocabulary and record id from valueURI: {valueURI}\n\t\t{e}'
            logging.debug(error)
            self.ctx.log_issue(self.file_path, error)


def generate_vocabulary_uri(vocabulary_id):
    return VOCABULARY_ENDPOINT.replace('{{ID}}', str(vocabulary_id))

def generate_record_uri(record_id):
    return RECORD_ENDPOINT.replace('{{ID}}', str(record_id))