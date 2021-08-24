# just-json-server

Command to run a API server supporting CRUD with just a json file.

## Getting started

Create a json file with name, say `db.json`

```json
{
  "articles": [
    {
      "id": 1,
      "title": "title 1"
    },
    {
      "id": 2,
      "title": "title 2"
    }
  ],
  "posts": [
    {
      "id": 1,
      "title": "post 1"
    }
  ],
  
}
```

Then, build and start an API server

```bash
stack build

stack exec just-json-server db.json
```

which supports CRUD operations on the given data, e.g.

```
GET /articles
GET /articles/1
POST /articles
PATCH /articles/1
PUT /articles/1
DELETE /articles/1
```