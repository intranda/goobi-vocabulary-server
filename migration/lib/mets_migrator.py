import os
import logging
from lib.mets_manipulator import MetsManipulator
from alive_progress import alive_bar

CSV_DELIMITER = ','

class MetsMigrator:
    def __init__(self, ctx):
        self.ctx = ctx
    
    def migrate(self):
        self.load_mapping_file()
        self.mets_files = self.scan_for_mets_files()
        logging.info(f'{len(self.mets_files)} mets file(s) found!')
        logging.info(f'Start processing ...')
        with alive_bar(len(self.mets_files)) as bar:
            for f in self.mets_files:
                m = MetsManipulator(self.ctx, f)
                m.process()
                bar()
    
    def load_mapping_file(self):
        header = None
        with open(self.ctx.mapping_file, 'r') as f:
            for line in f:
                line = line.strip()
                if header == None:
                    header = line
                    if header != CSV_DELIMITER.join(['vocabulary_name', 'vocabulary_id_old', 'vocabulary_id_new', 'record_id_old', 'record_id_new']):
                        raise Exception('Header mismatch in mapping file!')
                    continue

                parts = line.split(CSV_DELIMITER)
                if len(parts) != 5:
                    raise Exception(f'Wrong number of fields in line: {line}')
                
                vocabulary_name = parts[0]
                vocabulary_id_old = int(parts[1])
                vocabulary_id_new = int(parts[2])
                record_id_old = int(parts[3])
                record_id_new = int(parts[4])

                if not vocabulary_id_new in self.ctx.vocabulary_id_name_map:
                    self.ctx.vocabulary_id_name_map[vocabulary_id_new] = vocabulary_name
                if not vocabulary_name in self.ctx.vocabulary_name_id_map:
                    self.ctx.vocabulary_name_id_map[vocabulary_name] = vocabulary_id_new
                if not vocabulary_id_old in self.ctx.vocabulary_id_map:
                    self.ctx.vocabulary_id_map[vocabulary_id_old] = vocabulary_id_new
                if not record_id_old in self.ctx.record_id_map:
                    self.ctx.record_id_map[record_id_old] = record_id_new
                else:
                    raise Exception(f'Mapping file contains duplicate entry for old record {record_id_old}')

    def scan_for_mets_files(self):
        results = []
        for root, dirs, files in os.walk(self.ctx.metadata_directory):
            for name in files:
                if name != 'meta.xml':
                    continue
                full_path = os.path.join(root, name)
                logging.debug(f'Found meta file {full_path}')
                results.append(full_path)
        return results
                