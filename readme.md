#SDSU-STAT-API
GMT database API

'''
docker-compose build\\

docker-compose up -d\\
'''

## API

List GMT data files
* @get /gmtFiles


Extract List ID from start to limit
* @get /gmtColNames/<start:int>/<limit:int>

Method: GET
Params:
  * start
  * limit
example: /gmtColNames/2/5


Extract Target List by ID
* @get /gmtColData/<id>

Method: GET
Params:
  * id

example: /gmtColData/1
returns
  id
  name
  data
