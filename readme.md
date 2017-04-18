#SDSU-STAT-API
Plubmer Restful API Framework

```
# make a clone
git clone https://github.com/kruny1001/STAT-SDSU-API.git
cd STAT-SDSU-API
```

```
# Create container
docker-compose build\\

# Run Docker container
docker-compose up -d\\
```

API Examples

localhost:8000/gmtFiles
localhost:8000/gmtColNames/2/5
localhost:8000/gmtColData/2

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


###

'''
# update clone
git reset --hard HEAD #if there are conflic merge problem
git pull
docker-compose restart
'''
