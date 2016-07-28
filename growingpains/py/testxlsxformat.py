from openpyxl import load_workbook
wb = load_workbook(filename='dbfile.xlsx', read_only=True)

print(wb.get_sheet_names())
# ws = wb.active # ws is now an IterableWorksheet

def printtablevalues(table):
    for row in table.rows:
        for cell in row:
            print(cell.value)



for row in ws.iter_rows('A1:C2'):
    for cell in row:
        print (cell)



def writerowdata(row):
    rowval = '';
    for cell in row:
        rowval = rowval + cell.value
    reutrn rowval


def writerowdata(f, row):
    rowval = '';
    for cell in row:
        rowval = rowval + ' -- ' + cell.value
    print(rowval)
    row




def writesheet(f, sheet):
    for row in sheet.rows:
        writerowdata(f, row)





a = open('out.txt','w')
a.close() 

