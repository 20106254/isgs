import pyodbc
import csv

# Define the connection string
conn_str = (
    r'DRIVER={MDBTools};'
    r'DBQ=/home/john/Documents/private/msc/isgs/ISGS13_Database_01a.mdb'
)

# Establish the connection
conn = pyodbc.connect(conn_str)
cursor = conn.cursor()

# Execute the query
#query = """
#SELECT RELEVE_SP_DATA.Site_ID,
#       RELEVE_SP_DATA.Releve_ID,
#       RELEVE_SP_DATA.Species_name,
#       RELEVE_SP_DATA.Domin
#FROM RELEVE_SP_DATA
#"""

query = """SELECT Site_ID FROM RELEVE_SP_DATA"""

cursor.execute(query)

# Fetch all rows from the executed query
rows = cursor.fetchall()

# Define the CSV file path
csv_file_path = 'output.csv'

# Open the CSV file for writing
with open(csv_file_path, mode='w', newline='') as file:
    writer = csv.writer(file)

    # Write the column headers
    writer.writerow([column[0] for column in cursor.description])

    # Write the data rows
    for row in rows:
        writer.writerow(row)

# Close the connection
cursor.close()
conn.close()
