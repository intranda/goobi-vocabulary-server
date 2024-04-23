# Vocabulary migration tool
## Set-Up instructions
```bash
python -m venv vmenv
. vmenv/bin/activate
pip install requests mysql-connector-python progressbar2
```

## Run
```bash
. vmenv/bin/activate
python vocabulary-migrator.py
```

See `python vocabulary-migrator.py --help` for all available options.