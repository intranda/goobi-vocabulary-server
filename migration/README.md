# Vocabulary migration tool
## Set-Up instructions
```bash
python -m venv vmenv
. vmenv/bin/activate
pip install requests mysql-connector-python alive_progress
```

## Run
```bash
. vmenv/bin/activate
python vocabulary-migrator.py
```

See `python vocabulary-migrator.py --help` for all available options.
