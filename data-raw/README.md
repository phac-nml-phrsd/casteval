# How to reproduce `ensemble.csv` and `historic.csv`

Dependencies:
- Python 3
- Pandas
- Numpy

Clone the repository:

```
git clone https://github.com/jonassjuul/curvestat.git
cd curvestat
```

Then copy and run `getdata.py`:
```
cp -i /path/to/casteval/data-raw/getdata.py .
python getdata.py
```

`ensemble.csv` and `historic.csv` will be written into the toplevel of `curvestat/`.