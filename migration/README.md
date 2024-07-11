# Vocabulary migration tool
## Set-Up instructions
```bash
python -m venv vmenv
. vmenv/bin/activate
pip install requests mysql-connector-python==8.4.0 alive_progress
```

## Run vocabulary data migration
```bash
. vmenv/bin/activate
python vocabulary-migrator.py
```

See `python vocabulary-migrator.py --help` for all available options.

## Run mets file migration
```bash
. vmenv/bin/activate
python metadata-migrator.py -m migration.csv -d /opt/digiverso/goobi/metadata
```
