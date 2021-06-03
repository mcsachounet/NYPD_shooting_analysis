

df= pd.read_csv(r'C:\Users\Administrator\PycharmProjects\NY_Shootings\venv\Lib\site-packages\pandas\io\NYPD_Shooting_Incident_Data__Historic_.csv')
df_lat = df['Latitude']

df_long = df['Longitude']
api_key = 'AIzaSyA9VHqz8yayHaprbh6rvEdfDuVbXmTndgo'

Xpy

lst_zip = []
for i in range(0, len(df)):
    lat = df['Latitude'][i]
    long = df['Longitude'][i]
    temp_json_data = requests.get(getAPIUrl(lat, long, 'yourkey')).json()
    try:
        temp_json_data['results'][0]['address_components'][-1]['types']== ['postal_code']
        if temp_json_data['results'][0]['address_components'][-1]['types'] == ['postal_code']:
            zip = temp_json_data['results'][0]['address_components'][-1]['long_name']
            print(zip)
            lst_zip.append(zip)
        else:
            zip = temp_json_data['results'][0]['address_components'][-2]['long_name']
            print(zip)
            lst_zip.append(zip)
    except:
        zip = 'Naan'
        print(zip)
        lst_zip.append(zip)

# workbook = Workbook()
# sheet = workbook.active
df_zip = pd.DataFrame(lst_zip)
# for row in dataframe_to_rows(df_zip, index=False, header=True):
#     sheet.append(row)
writer = pd.ExcelWriter('zip.xlsx', engine='xlsxwriter')
df_zip.to_excel(writer, sheet_name='zip', index=False)
writer.save()

# workbook.save("zip.xlsx")

