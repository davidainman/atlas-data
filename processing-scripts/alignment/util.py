import unicodedata
import logging
import tqdm
import typing as t
from csv import writer
from csv import reader

import openpyxl as op
import attr
import pandas as pd

@attr.s(auto_attribs=True)
class SimpleContainer:
    file: str = (attr.ib(),)
    values: t.List = (attr.ib(),)
    @classmethod
    def from_excel(cls, excel: str):
        values = [("Feature", "Language", "Glottocode", "Coder", "Error Message", "Comment")]
        container = cls(file=excel, values=values)
        return container

    def log(self, row):
        self.values.append(row)

    def to_excel(self):
        wb = op.Workbook()
        ws = wb.active
        for row in self.values:
            ws.append(row)
        wb.save(filename=self.file)





# other helper functions
logger = logging.getLogger("alignment")


def tq(iter, task, logger=logger, total: t.Optional[t.Union[int, float]] = None):
    if logger.getEffectiveLevel() >= logging.INFO:
        # print(task)
        print(task)
        return tqdm.tqdm(iter, total=total)
    else:
        return iter


def clean_cell_value(cell: op.cell.cell.Cell):
    if cell.value is None:
        return ""
    if type(cell.value) == float or type(cell.value) == int:
        if cell.value == int(cell.value):
            v = str(int(cell.value))
        else:
            v = str(cell.value)
    else:
        v = unicodedata.normalize("NFC", (cell.value or "").strip())
    try:
        return v.replace("\n", "")
    except TypeError:
        return str(v)


def get_excel_header_to_column(
    ws: op.Workbook,
):
    header_to_column_index = {}
    header = []
    for c, r in enumerate(next(ws.iter_rows(1, 1)), 0):
        value = clean_cell_value(r)
        header.append(value)
        header_to_column_index[value] = c
    return header_to_column_index, header


if __name__ == "__main__":
    pass
